;; src/vix/routes.clj core routes for the application.
;; Copyright 2011-2012, Vixu.com, F.M. (Filip) de Waard <fmw@vixu.com>.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns vix.routes
  (:use compojure.core
        vix.auth
        [slingshot.slingshot :only [try+]]
        [clojure.data.json :only [read-json json-str]]
        [ring.util.response :only [redirect]])
  (:require [vix.config :as config]
            [vix.db :as db]
            [vix.lucene :as lucene]
            [vix.views :as views]
            [vix.util :as util]
            [com.ashafa.clutch :as clutch]
            [clj-time.format :as time-format]
            [clj-time.core :as time-core]
            [compojure.route :as route]
            [compojure.handler :as handler]))

(def available-languages
  "Atom; Sequence of available languages used for language selection."
  (atom (db/get-available-languages config/database)))

(defn reset-available-languages!
  "Updates the available-languages atom to the currently available
   languages in the given database."
  [database]
  (reset! available-languages
          (db/get-available-languages database)))

(def search-allowed-feeds
  "Sequence of feeds that have :searchable set to true."
  (atom (try
          (db/get-searchable-feeds (db/list-feeds config/database))
          (catch Exception e
            nil))))

(defn reset-search-allowed-feeds!
  "Resets the search-allowed-feeds atom to the current state of the
   feeds in the database (i.e. all feeds that currently
   have :searchable set to true)."
  [database]
  (reset! search-allowed-feeds
          (db/get-searchable-feeds (db/list-feeds database))))

(def index-reader
  "Atom containing a Lucene IndexReader."
  (atom (lucene/create-index-reader lucene/directory)))

(defn reset-index-reader!
  "Closes the active IndexReader (if any) in the index-reader atom and
   creates a new one."
  []
  (when @index-reader
    (.close @index-reader))
  
  (reset! index-reader
          (lucene/create-index-reader lucene/directory)))

(defn response
  "Returns a map containing the given body that is provided to Ring as
   a response. Optionally accepts :status (HTTP status code integer)
   and :content-type (HTTP Content-Type header) keywords, followed by
   their value. The :status value defaults to 200, unless the body
   value is nil, in which case it defaults to 404. The content-type
   value defaults to text/html; charset=UTF-8."
  [body & {:keys [status content-type]}]
  {:status (or status (if (nil? body) 404 200))
   :headers {"Content-Type" (or content-type "text/html; charset=UTF-8")}
   :body body})

(defn json-response
  "Calls the response function with an application/json; charset=UTF-8
   content-type and the provided body. Optionally accepts a :status
   keyword, followed by a HTTP status code integer. When the provided
   body is not nil it is converted to a json-str. The status defaults
   to 200, unless the body is nil, in which case it defaults to 404."
  [body & {:keys [status]}]
  (response (when-not (nil? body) (json-str body))
            :status (or status (if (nil? body) 404 200))
            :content-type "application/json; charset=UTF-8"))

;; FIXME: add a nice 404 page
(defn page-not-found-response
  "Calls the response function with a 404 :status and <h1>Page not
   found</h1> as the body."
  []
  (response "<h1>Page not found</h1>" :status 404))

(defn image-response
  "Returns the attachment for the :original key for the given document
   in database, or a page-not-found-response if the attachment wasn't
   found. Uses the CouchDB revision identifier as the value for the HTTP
   ETag header and uses the most recent date from the document (either
   :updated or published) as the value of the Last-Modified header."
  [database document]
  (if-let [attachment (clutch/get-attachment database
                                             document
                                             :original)]
    (let [resp (response attachment
                         :content-type (:content_type
                                        (:original
                                         (:_attachments
                                          document))))]
      (assoc resp :headers
             (assoc (:headers resp)
               "ETag"
               (:_rev document))
             "Last-Modified"
             (time-format/unparse (time-format/formatters :rfc822)
                                  (util/rfc3339-to-jodatime
                                   (or (:updated document)
                                       (:published document))
                                   "UTC"))))
    (page-not-found-response)))

(defmulti get-segment
  "Multimethod that retrieves the data associated with a page segment
   based on the segment :type (e.g. :document, :string).

   Segments are used for coupling external entities with documents
   (e.g. recent news items, but not the related-pages that are already
   stored in a document attribute).

   They are stored in config/page-segments."
  (fn [segment-details database language timezone]
    (:type segment-details)))

(defmethod get-segment :document
  [segment-details database language timezone]
  (assoc segment-details
    :data
    (db/get-document database ((:slug segment-details) language))))

(defmethod get-segment :most-recent-events
  [segment-details database language timezone]
  (let [docs (db/get-most-recent-event-documents database
                                                 language
                                                 (:feed segment-details)
                                                 (:limit segment-details))]
    (assoc segment-details
      :data
      (if (= (count docs) 1)
        (first docs)
        docs))))

(defmethod get-segment :feed
  [segment-details database language timezone]
  (let [docs (db/get-documents-for-feed database
                                        language
                                        (:feed segment-details)
                                        (:limit segment-details))]
    (assoc segment-details
      :data
      (if (= (count docs) 1)
        (first docs)
        docs))))

(defmethod get-segment :string
  [segment-details database language timezone]
  segment-details)

(defn get-segments [page-segments database language timezone]
  (into {}
        (for [[k v] page-segments]
          [k (get-segment v
                          database
                          language
                          timezone)])))

(defn get-frontpage-for-language!
  "Renders the views/frontpage-view for the given language, using the
   provided database and timezone. Uses the value corresponding to
   the :frontpage key in the config/page-segments map to populate the
   segments."
  [database language timezone]
  (views/frontpage-view
   language
   timezone
   (get-segments (:frontpage config/page-segments)
                 database
                 language
                 timezone)))

(def frontpage-cache
  "Atom pointing to a map that stores frontpages for each language
   identified by the language code as a key."
  (atom {}))

(defn get-cached-frontpage!
  "Returns the frontpage for the given language from the
   frontpage-cache map. If it hasn't been cached yet, the function
   loads the frontpage from given database using the provided timezone
   and caches it, before recurring."
  [database language timezone]
  (if-let [fp (get @frontpage-cache language)]
    fp
    (do
      (swap! frontpage-cache
             assoc
             language
             (response (get-frontpage-for-language! database
                                                    language
                                                    timezone)))
      (get-cached-frontpage! database language timezone))))

(defn reset-frontpage-cache!
  "Takes the the frontpage-cache map, removes the value corresponding
   to the given language key and points the atom to the resulting
   map."
   [language]
  (swap! frontpage-cache dissoc language))

(def page-cache
  "Atom pointing to a map with slugs as a keyword and the cached pages
   as a value."
  (atom {}))

(defn get-cached-page!
  "Returns the page for provided slug from the page-cache map, or
   saves it to the atom using the given database and timezone and
   recurs. Images skip the cache. When the document isn't found a
   page-not-found-response is returned."
  [database slug timezone]
  (if-let [p (get @page-cache slug)]
    p
    (if-let [document (db/get-document database slug)]
      (cond
       ;; files always skip the cache
       (:original (:_attachments document))
       (image-response database document)
       ;; for event-like documents
       ;;(not (nil? (:end-time-rfc3339 document)))
       ;; for all other documents
       :default
       (do
         (swap! page-cache
                assoc
                slug
                (response
                 (views/page-view (:language document)
                                  timezone
                                  document
                                  (get-segments (:default-page
                                                 config/page-segments)
                                                database
                                                (:language document)
                                                timezone))))
         (get-cached-page! database slug timezone)))   
      (page-not-found-response))))

(defn reset-page-cache!
  "Resets the page-cache atom to an empty map."
  []
  (reset! page-cache {}))

(defn logout
  "Returns a map that can be passed to Ring, which clears
   the :username and :permissions keys from the given session map and
   redirects the request to /."
  [session]
  {:session (dissoc session :username :permissions)
   :status 302
   :headers {"Location" "/"}})

(defn login
  "Attempts a login for given username and password. Authenticates the
  provided session if successful (i.e. associating :username to the
  particular user and :permissions to the relevant permissions) and
  redirects to /admin. Redirects to /login if the user doesn't exist
  or in case of a username/password mismatch."
  [session username password]
  (try+
   (when-let [authenticated-session (authenticate config/database
                                                  session
                                                  username
                                                  password)]
     {:session authenticated-session
      :status 302
      :headers {"Location" "/admin/"}})
   (catch [:type :vix.auth/user-does-not-exist] _
     (redirect "/login"))
   (catch [:type :vix.auth/username-password-mismatch] _
     (redirect "/login"))))

(defn reset-all!
  "Resets caches."
  [database language]
  (reset-frontpage-cache! language)
  (reset-page-cache!)
  (reset-index-reader!)
  (reset-search-allowed-feeds! database)
  (reset-available-languages! database))

(defmulti feed-request
  "Handles json feed CRUD or GET request based on HTTP method."
  (fn [method & _]
    method))

(defmethod feed-request :GET [method new-doc language feed-name]
  (json-response (db/get-feed config/database language feed-name)))

(defmethod feed-request :POST [method new-doc language feed-name]
  (let [feed (db/create-feed config/database new-doc)]
    (reset-all! config/database language)
    (json-response feed :status 201)))

(defmethod feed-request :PUT [method new-doc language feed-name]
  (let [feed (db/update-feed config/database language feed-name new-doc)]
    (reset-all! config/database language)
    (json-response feed)))

(defmethod feed-request :DELETE [method new-doc language feed-name]
  (if-let [feed (db/delete-feed config/database language feed-name)]
    (do
      (reset-all! config/database language)
      (json-response feed))
    (json-response nil)))

(defmulti document-request
  "Handles json document CRUD or GET request based on HTTP method."
  (fn [method & _]
    method))

(defmethod document-request :GET
  [method new-doc existing-doc language feed-name]
  (json-response existing-doc))

(defmethod document-request :POST
  [method new-doc existing-doc language feed-name]
  (let [document (db/create-document config/database
                                     language
                                     feed-name
                                     config/default-timezone
                                     new-doc)]
    (reset-index-reader!)
    (lucene/add-documents-to-index! lucene/directory [new-doc])
    (reset-all! config/database language)
    (json-response document :status 201)))

(defmethod document-request :PUT
  [method new-doc {:keys [slug feed] :as existing-doc} language feed-name]
  (if (and existing-doc
           (= slug (:slug new-doc))
           (= feed (:feed new-doc))
           (= language (:language existing-doc) (:language new-doc)))
    (let [document (db/update-document config/database
                                       config/default-timezone
                                       slug
                                       new-doc)]
      (reset-index-reader!)
      (lucene/update-document-in-index! lucene/directory slug document)
      (reset-all! config/database language)
      (json-response document))
    (json-response nil)))

(defmethod document-request :DELETE
  [method new-doc {:keys [slug] :as existing-doc} language feed-name]
  (if existing-doc
    (let [document (db/delete-document config/database slug)]
      (reset-index-reader!)
      (lucene/delete-document-from-index! lucene/directory slug)
      (reset-all! config/database language)
      (json-response document))
    (json-response nil)))

(def http-methods
  "HTTP method as keywords, with lowercase keys and uppercase values."
  {:get :GET :post :POST :put :PUT :delete :DELETE})

(defroutes main-routes
  (GET "/"
       {headers :headers}
       (let [preferred-language (or
                                 (util/get-preferred-language
                                  (util/parse-accept-language-header
                                   (get headers "accept-language"))
                                  @available-languages)
                                 config/default-language)]
         (if (= preferred-language config/default-language)
           (get-cached-frontpage! config/database
                                  config/default-language
                                  config/default-timezone)
           (redirect (str config/base-uri preferred-language)))))
  (GET "/admin*"
       {session :session {feed "feed"} :params}
       (when (authorize session :DELETE nil :*)
         (response (views/admin-template {}))))
  (GET "/login"
       []
       (response (views/login-page-template "")))
  (POST "/login"
        {session :session
         {username "username" password "password"} :form-params}
        (login session username password))
  (GET "/logout"
       {session :session}
       (logout session))
  (GET "/:language"
       {{language :language} :params}
       (if (<= (count language) 3)
         (get-cached-frontpage! config/database
                                language
                                config/default-timezone)
         (page-not-found-response)))
  (GET "/:language/search"
       {{language :language
         q :q
         after-doc-id :after-doc-id
         after-score :after-score
         pp-after-doc-id :pp-aid
         pp-after-score :pp-as}
        :params}
       (let [after-doc-id-int (util/read-int after-doc-id)
             after-score-float (util/read-float after-score)]
         (response
          (views/search-results-view
           language
           config/search-results-per-page
           (lucene/search q
                          (lucene/create-filter
                           {:language language
                            :draft false
                            :feed (get @search-allowed-feeds language)})
                          (inc config/search-results-per-page)
                          after-doc-id-int
                          after-score-float
                          @index-reader
                          lucene/analyzer)
           q
           pp-after-doc-id
           pp-after-score
           after-doc-id-int
           after-score-float
           (not (and after-doc-id-int after-score-float))
           (get-segments (:search-page
                          config/page-segments)
                         config/database
                         language
                         config/default-timezone)))))
  (GET "/json/:language/:feed/list-documents"
       {{language :language
         feed-name :feed
         limit :limit
         startkey-published :startkey-published
         startkey_docid :startkey_docid} :params
         session :session}
       (when (authorize session :GET language feed-name)
         (json-response
          (db/get-documents-for-feed config/database
                                     language
                                     feed-name
                                     (when limit
                                       (Integer/parseInt limit))
                                     startkey-published
                                     startkey_docid))))
  (GET "/json/list-feeds"
       {session :session
        {ddt :default-document-type
         language :language} :params}
       (when (authorize session :GET nil :*)
         (json-response
          (if ddt
            (db/list-feeds-by-default-document-type config/database
                                                    ddt
                                                    language)
            (db/list-feeds config/database language)))))
  (ANY "/json/feed/:language/:name"
       {lowercase-method :request-method
        {language :language feed-name :name} :params
        body :body
        session :session}
       (let [method (http-methods lowercase-method)]
         (when (if (= method :POST)
                 (authorize session :POST nil :*)
                 (authorize session method language feed-name))
           (feed-request method
                         (try
                           (read-json (slurp body))
                           (catch java.io.EOFException _
                             nil))
                         language
                         feed-name))))
  (ANY "/json/document/*"
       {lowercase-method :request-method
        {raw-slug :*} :params
        body :body
        session :session}
       (let [method
             (http-methods lowercase-method)
             {:keys [slug feed language] :as new-doc}
             (try
               (read-json (slurp body))
               (catch java.io.EOFException _
                 nil))
             existing-doc
             (db/get-document config/database
                              (util/force-initial-slash raw-slug)
                              true)]
         (when (if (= method :GET)
                 (authorize session
                            method
                            (:language existing-doc)
                            (:feed existing-doc))
                 (authorize session method language feed))
           (document-request method
                             new-doc
                             existing-doc
                             language
                             feed))))
  (route/resources "/static/")
  (GET "/*"
       {{slug :*} :params}
       (get-cached-page! config/database
                         (util/force-initial-slash slug)
                         config/default-timezone))
  (ANY "/*"
       []
       (page-not-found-response)))

(defn handle-authentication-errors [handler]
  "Middleware function that redirects on insufficient privileges."
  (fn [request]
    (try+
     (handler request)
     (catch [:type :vix.auth/insufficient-privileges] _
         (redirect "/permission-denied"))
     (catch [:type :vix.auth/authentication-required] _
         (redirect "/login")))))

(defn wrap-caching-headers [handler]
  "Middleware function that adds Cache-Control: public and Expires
   headers to for image/png, image/jpeg, image/gif, text/css and
   text/javascript requests. Adds an Expires header with a date that
   is surely in the past for all other requests."
  (fn [request]
    (let [now (time-core/now)
          response (handler request)]
      (if (some #{(get (:headers response) "Content-Type")}
                ["image/png"
                 "image/jpeg"
                 "image/gif"
                 "text/css"
                 "text/javascript"])
        (assoc response :headers
               (assoc (:headers response)
                 "Cache-Control"
                 "public"
                 "Expires"
                 (time-format/unparse (time-format/formatters :rfc822)
                                      (time-core/plus now
                                                      (time-core/years 3)))))
        (assoc response :headers
               (assoc (:headers response)
                 "Expires" "Mon, 26 Mar 2012 09:00:00 GMT"))))))

(defn redirect-301 [to]
  "Returns a 'Moved Permanently' redirect with HTTP status 301."
  {:status 301
   :body "Moved Permanently"
   :headers {"Location" to}})

(defn redirection-handler [handler]
  "Deals with the following redirects:
   - redirects all relevant requests to config/default-host
     (e.g. vixu.com -> www.vixu.com),
   - redirects all non-SSL requests to /admin to SSL,
   - any custom redirects specified in config/redirects."
  (fn [request]
    (cond
     ;; redirect requests to default-host if necessary
     (not (= (:server-name request) config/server-name))
     (redirect-301 (str "http://" config/default-host (:uri request)))
     ;; redirect /admin on non-localhost to https
     (and (= (apply str (take 6 (:uri request))) "/admin")
          (= (:scheme request) :http)
          (not (= (:server-name request) "localhost")))
     (redirect-301 (str "https://" config/default-host (:uri request)))
     ;; perform custom redirects configured in config/redirects
     (string? (get config/redirects (:uri request)))
     (redirect-301 (get config/redirects (:uri request)))
     ;; otherwise, move on to default handler and serve the page
     :default
     (handler request))))

(def app
  (-> (handler/site main-routes)
      (redirection-handler)
      (wrap-caching-headers)
      (handle-authentication-errors)))