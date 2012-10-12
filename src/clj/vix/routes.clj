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
        [slingshot.slingshot :only [try+ throw+]]
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

(def invalid-request-body-error
  {:type ::invalid-request-body
   :message "The JSON or Clojure request body is invalid."})

(def method-action-mismatch-error
  {:type ::method-action-mismatch
   :message (str "The :action value in the provided map must correspond "
                 "to the right HTTP method (i.e. POST & :create, "
                 "PUT & :update and DELETE and :delete).")})

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

(defn data-response
  "Returns API response map for body in either JSON or a native
   Clojure datastructure that is converted to a string using the
   pr-str function, depending on whether :json or :clj is passed as a
   type. Uses a native Clojure datastructure by default. The status
   code is either the provided HTTP status code integer. Otherwise 200
   or 404 is used by default depending on whether the body is nil. The
   value of the HTTP Content-Type header is either 'application/json;
   charset=UTF-8' or 'text/plain; charset=UTF-8', depending on
   the provided type."
  [body & {:keys [type status]}]
  (response (if (= type :json)
              (when-not (nil? body)
                (json-str body))
              (pr-str body))
            :status (or status (if (nil? body) 404 200))
            :content-type (get {:json "application/json; charset=UTF-8"
                                :clj "text/plain; charset=UTF-8"}
                               type
                               "text/plain; charset=UTF-8")))

(defn data-error-response
  "Returns a plain text response with the provided message and status 400 "
  [message]
  {:status 400
   :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body message})

;; FIXME: add a nice 404 page
(defn page-not-found-response
  "Calls the response function with a 404 :status and <h1>Page not
   found</h1> as the body."
  []
  (response "<h1>Page not found</h1>" :status 404))

(defn image-response
  "Returns the attachment for the :original key of the most recent
   state in the given document-state sequence in database, or a
   page-not-found-response if the attachment wasn't found. Uses the
   CouchDB revision identifier as the value for the HTTP ETag header
   and uses the most recent date from the document (either
   :updated or published) as the value of the Last-Modified header."
  [database document-states]
  (let [{:keys [_id attachments datestamp] :as document}
        (first document-states)]
    (if-let [attachment (clutch/get-attachment database _id :original)]
      (let [resp (response attachment
                           :content-type
                           (:type (:original attachments)))]
        (assoc resp :headers
               (assoc (:headers resp)
                 "ETag"
                 (:_rev document)
                 "Last-Modified"
                 (time-format/unparse (time-format/formatters :rfc822)
                                      (util/rfc3339-to-jodatime datestamp
                                                                "UTC")))))
      (page-not-found-response))))

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
    (first (db/get-document database
                            ((:slug segment-details) language)
                            {:limit 1}))))

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
  (views/frontpage-view language
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
    (let [{:keys [attachments language] :as document}
          (first (db/get-document database slug {:limit 1}))]
      (if (and document (not (= (keyword (:action document)) :delete)))
        (cond
         ;; files always skip the cache
         (:original attachments)
         (image-response database [document])
         ;; for event-like documents
         ;;(not (nil? (:end-time-rfc3339 document)))
         ;; for all other documents
         :default
         (do
           (swap! page-cache
                  assoc
                  slug
                  (response
                   (views/page-view language
                                    timezone
                                    document
                                    (get-segments (:default-page
                                                   config/page-segments)
                                                  database
                                                  language
                                                  timezone))))
           (get-cached-page! database slug timezone)))   
        (page-not-found-response)))))

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

(defn feed-request
  "Handles feed API request based. Accepts method (HTTP method),
   response-type (response Content-Type indicator, i.e. :json
   or :clj), new-state (the new state of the feed document),
   language (feed language) and feed-name. Returns a 400 response if
   the action value of the new-state map (e.g. :create) and HTTP
   method (e.g. :POST) don't match. All calls (including action/method
   mismatches) reset the cache, unless they're GET requests ."
  [database method response-type new-state language feed-name]
  (let [action (keyword (:action new-state)) ; need to convert for json
        feed (if (= method :GET)
               (db/get-feed config/database language feed-name)
               (db/append-to-feed config/database
                                  (assoc new-state :action action)))]
    (when (not (= method :GET))
      (reset-all! database language))
    
    (if (= action (method {:POST :create
                           :PUT :update
                           :DELETE :delete
                           :GET nil}))
      (data-response feed
                     :status (if (= method :POST) 201 200)
                     :type response-type)
      (data-error-response (str "The HTTP method (e.g. :POST) "
                                "doesn't match the :action (e.g. :create) "
                                "value of the provided feed document.")))))

(defmulti document-request
  "Handles document API request based on HTTP method.
   Accepts method (HTTP method), response-type (response Content-Type
   indicator, either :json or :clj), new-doc (new document),
   existing-doc (old document), language and feed-name."
  (fn [method & _]
    method))

(defmethod document-request :GET
  [method response-type doc]
  (data-response doc :type response-type))

(defmethod document-request :POST
  [method response-type {:keys [language] :as doc}]
  (let [document (db/append-to-document config/database
                                        config/default-timezone
                                        doc)]
    (reset-index-reader!)
    (lucene/add-documents-to-index! lucene/directory [(first document)])
    (reset-all! config/database language)
    (data-response document :status 201 :type response-type)))

(defmethod document-request :PUT
  [method response-type {:keys [language slug] :as doc}]
  (let [document (db/append-to-document config/database
                                        config/default-timezone
                                        doc)]
    (reset-index-reader!)
    (lucene/update-document-in-index! lucene/directory slug (first document))
    (reset-all! config/database language)
    (data-response document :type response-type)))

(defmethod document-request :DELETE
  [method response-type {:keys [language slug] :as doc}]
  (let [document (db/append-to-document config/database
                                        config/default-timezone
                                        doc)]
    (reset-index-reader!)
    (lucene/delete-document-from-index! lucene/directory slug)
    (reset-all! config/database language)
    (data-response document :type response-type)))

(def http-methods
  "HTTP method as keywords, with lowercase keys and uppercase values."
  {:get :GET :post :POST :put :PUT :delete :DELETE})

(def method-action-matches
  "Mapping of HTTP methods to feed/document map :action values."
  {:POST :create :PUT :update :DELETE :delete})

(defn read-body
  "Returns a string representation of body, treated like the provided
   type. Supported types are json and clj (with the lattter as the
   default option). Types are passed as strings, not keywords. Returns
   nil on empty input, throws invalid-request-error if input is
   malformed."
  [type body]
  (let [data (try
               (str (slurp body))
               (catch java.lang.NullPointerException _ nil))]
    (when (pos? (count data))
      (try
        (if (= type "json")
          (read-json data)
          (read-string data))
        (catch java.lang.Exception _
          (throw+ invalid-request-body-error))))))

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
         pp-after-doc-id :pp-aid
         pp-after-score :pp-as
         after-doc-id :after-doc-id
         after-score :after-score} :params}
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
           (if (string? pp-after-doc-id)
             (vector pp-after-doc-id)
             pp-after-doc-id)
           (if (string? pp-after-score)
             (vector pp-after-score)
             pp-after-score)
           after-doc-id-int
           after-score-float
           (not (and after-doc-id-int after-score-float))
           (get-segments (:search-page
                          config/page-segments)
                         config/database
                         language
                         config/default-timezone)))))
  (GET "/_api/:type/:language/:feed/_list-documents"
       {headers :headers
        session :session
        {type :type
         language :language
         feed-name :feed
         limit :limit
         startkey-published :startkey-published
         startkey_docid :startkey_docid} :params}
       (when (authorize session :GET language feed-name)
         (data-response
          (db/get-documents-for-feed config/database
                                     language
                                     feed-name
                                     (when limit
                                       (Integer/parseInt limit))
                                     startkey-published
                                     startkey_docid)
          :type (if (= type "json") :json :clj))))
  (GET "/_api/:type/_list-feeds"
       {session :session
        {type :type ddt :default-document-type language :language} :params}
       (when (authorize session :GET nil :*)
         (data-response
          (if ddt
            (db/list-feeds-by-default-document-type config/database
                                                    ddt
                                                    language)
            (db/list-feeds config/database language))
          :type (if (= type "json") :json :clj))))
  (ANY "/_api/:type/_feed/:language/:name"
       {lowercase-method :request-method
        {type :type language :language feed-name :name} :params
        body :body
        session :session}
       (let [method (http-methods lowercase-method)
             {:keys [action] :as new-doc} (read-body type body)]
         (when (if (= method :POST)
                 (authorize session :POST nil :*)
                 (authorize session method language feed-name))
           (if (or (= method :GET)
                   (= (keyword action) (method-action-matches method)))
             (feed-request config/database
                           method
                           (keyword type)
                           new-doc
                           language
                           feed-name)
             (throw+ method-action-mismatch-error)))))
  (ANY "/_api/:type/_document/*"
       {lowercase-method :request-method
        {type :type raw-slug :*} :params
        body :body
        session :session}
       (let [method
             (http-methods lowercase-method)
             {:keys [action language feed] :as doc}
             (if (= method :GET)
               (db/get-document
                config/database
                (util/force-initial-slash raw-slug))
               (read-body type body))]
         (when (authorize session method language feed)
           (if (or (= method :GET)
                   (= (keyword action) (method-action-matches method)))
             (document-request method (keyword type) doc)
             (throw+ method-action-mismatch-error)))))
  (route/resources "/static/")
  (GET "/*"
       {{slug :*} :params}
       (get-cached-page! config/database
                         (util/force-initial-slash slug)
                         config/default-timezone))
  (ANY "/*"
       []
       (page-not-found-response)))

(defn handle-exceptions
  "Middleware function that catches and handles exceptions. Performs
   redirect to /login on authentication errors and returns a data
   response on API errors."
  [handler]
  (fn [request]
    (try+
     (handler request)
     (catch [:type :vix.auth/insufficient-privileges] _
       (redirect "/permission-denied"))
     (catch [:type :vix.auth/authentication-required] _
       (redirect "/login"))
     (catch [:type :vix.routes/invalid-request-body] e
       (data-error-response (:message e)))
     (catch [:type :vix.routes/method-action-mismatch] e
       (data-error-response (:message e)))
     (catch [:type :vix.db/feed-already-deleted] e
       (data-error-response (:message e)))
     (catch [:type :vix.db/feed-update-conflict] e
       (data-error-response (:message e)))
     (catch [:type :vix.db/feed-already-exists-conflict] e
       (data-error-response (:message e)))
     (catch [:type :vix.db/document-already-exists-conflict] e
       (data-error-response (:message e)))
     (catch [:type :vix.db/document-update-conflict] e
       (data-error-response (:message e)))
     (catch [:type :vix.db/document-already-deleted] e
       (data-error-response (:message e)))
     (catch [:type :vix.db/document-missing-required-keys] e
       (data-error-response (:message e))))))

(defn wrap-caching-headers
  "Middleware function that adds Cache-Control: public and Expires
   headers to for image/png, image/jpeg, image/gif, text/css and
   text/javascript requests. Adds an Expires header with a date that
   is surely in the past for all other requests."
  [handler]
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

(defn redirect-301
  "Returns a 'Moved Permanently' redirect with HTTP status 301."
  [to]
  {:status 301
   :body "Moved Permanently"
   :headers {"Location" to}})

(defn redirection-handler
  "Deals with the following redirects:
   - redirects all relevant requests to config/default-host
     (e.g. vixu.com -> www.vixu.com),
   - redirects all non-SSL requests to /admin to SSL,
   - any custom redirects specified in config/redirects."
   [handler]
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
      (handle-exceptions)))