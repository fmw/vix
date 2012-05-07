;; src/vix/routes.clj core routes for the application.
;;
;; Copyright 2011-2012, F.M. (Filip) de Waard <fmw@vix.io>.
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
        [clojure.contrib.json :only [read-json json-str]]
        [clojure.contrib.duck-streams :only [slurp*]]
        [ring.util.response :only [redirect]])
  (:require [vix.config :as config]
            [vix.db :as db]
            [vix.lucene :as lucene]
            [vix.views :as views]
            [vix.util :as util]
            [clj-time.format :as time-format]
            [clj-time.core :as time-core]
            [clojure.contrib [error-kit :as kit]]
            [couchdb [client :as couchdb]]
            [compojure.route :as route]
            [compojure.handler :as handler]))

(def search-allowed-feeds
  (atom (try
          (db/get-searchable-feeds (db/list-feeds config/db-server
                                                  config/database))
          (catch Exception e
            nil))))

(def *index-reader*
  (atom (lucene/create-index-reader lucene/directory)))

(defn reset-index-reader! []
  (when @*index-reader*
    (.close @*index-reader*))
  
  (compare-and-set! *index-reader*
                    @*index-reader*
                    (lucene/create-index-reader lucene/directory)))

(defn response [body & {:keys [status content-type]}]
  {:status (or status (if (nil? body) 404 200))
   :headers {"Content-Type" (or content-type "text/html; charset=UTF-8")}
   :body body})

(defn json-response [body & {:keys [status]}]
  (response (when-not (nil? body) (json-str body))
            :status (or status (if (nil? body) 404 200))
            :content-type "application/json; charset=UTF-8"))

;; FIXME: add a nice 404 page
(defn page-not-found-response []
  (response "<h1>Page not found</h1>" :status 404))

(defn image-response [db-server database document]
  (try
    (let [resp (response (new java.io.ByteArrayInputStream
                              (:body (couchdb/attachment-get db-server
                                                             database
                                                             document
                                                             "original")))
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
    (catch java.io.FileNotFoundException e
      (page-not-found-response))))

(defmulti get-segment
  "Multimethod that retrieves the data associated with a page segment
   based on the segment :type (e.g. :document, :string).

   Segments are used for coupling external entities with documents
   (e.g. recent news items, but not the related-pages that are already
   stored in a document attribute).

   They are stored in config/page-segments."
  (fn [segment-details db-server database language timezone]
    (:type segment-details)))

(defmethod get-segment :document
  [segment-details db-server database language timezone]
  (assoc segment-details
    :data
    (db/get-document
     db-server
     database
     ((:slug segment-details) language))))

(defmethod get-segment :most-recent-events
  [segment-details db-server database language timezone]
  (let [docs (db/get-most-recent-event-documents db-server
                                                 database
                                                 language
                                                 (:feed segment-details)
                                                 (:limit segment-details))]
    (assoc segment-details
      :data
      (if (= (count docs) 1)
        (first docs)
        docs))))

(defmethod get-segment :feed
  [segment-details db-server database language timezone]
  (let [docs (db/get-documents-for-feed db-server
                                        database
                                        language
                                        (:feed segment-details)
                                        (:limit segment-details))]
    (assoc segment-details
      :data
      (if (= (count docs) 1)
        (first docs)
        docs))))

(defmethod get-segment :string
  [segment-details db-server database language timezone]
  segment-details)

(defn get-segments [page-segments db-server database language timezone]
  (into {}
        (for [[k v] page-segments]
          [k (get-segment v
                          db-server
                          database
                          language
                          timezone)])))

(defn get-frontpage-for-language! [db-server database language timezone]
  (views/frontpage-view
   language
   timezone
   (get-segments (:frontpage config/page-segments)
                 db-server
                 database
                 language
                 timezone)))

(def *frontpage-cache* (atom {}))

(defn get-cached-frontpage! [db-server database language timezone]
  (if-let [fp (get @*frontpage-cache* language)]
    fp
    (do
      (swap! *frontpage-cache*
             assoc
             language
             (response (get-frontpage-for-language! db-server
                                                    database
                                                    language
                                                    timezone)))
      (get-cached-frontpage! db-server database language timezone))))

(defn reset-frontpage-cache! [language]
  (swap! *frontpage-cache* dissoc language))

(def *page-cache* (atom {}))

(defn get-cached-page! [db-server database slug timezone]
  (if-let [p (get @*page-cache* slug)]
    p
    (if-let [document (db/get-document db-server database slug)]
      (cond
       ;; files always skip the cache
       (:original (:_attachments document))
       (image-response db-server database document)
       ;; for event-like documents
       ;;(not (nil? (:end-time-rfc3339 document)))
       ;; for all other documents
       :default
       (do
         (swap! *page-cache*
                assoc
                slug
                (response
                 (views/page-view (:language document)
                                  timezone
                                  document
                                  (get-segments (:default-page
                                                 config/page-segments)
                                                db-server
                                                database
                                                (:language document)
                                                timezone))))
         (get-cached-page! db-server database slug timezone)))
      
      (page-not-found-response))))

(defn reset-page-cache! []
  (compare-and-set! *page-cache* @*page-cache* {}))

;; FIXME: add authorization
(defn catch-all [db-server database slug timezone]
  (get-cached-page! db-server database slug timezone))

(defn logout [session]
  {:session (dissoc session :username)
   :status 302
   :headers {"Location" "/"}})

(defn login [session username password]
  (kit/with-handler
    (when-let [authenticated-session (authenticate
                                       config/db-server
                                       config/database
                                       session
                                       username
                                       password)]
      {:session authenticated-session
       :status 302
       :headers {"Location" "/admin/"}})
    (kit/handle UserDoesNotExist []
      (redirect "/login"))
    (kit/handle UsernamePasswordMismatch []
                (redirect "/login"))))

;; FIXME: change order of authorize/json-response calls, so attackers
;; can't get a 404 from an unauthorized request to mine for existing docs
;; -
;; consider escaping json calls (e.g. /json/document/_new
;; instead of just /new)
(defroutes main-routes
  (GET "/"
       []
       (get-cached-frontpage! config/db-server
                              config/database
                              config/default-language
                              config/default-timezone))
  (GET "/admin*"
       {session :session {feed "feed"} :params}
       (when (authorize session nil :* :DELETE)
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
         (get-cached-frontpage! config/db-server
                                config/database
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
          (if (and after-doc-id-int after-score-float)
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
                            (lucene/create-index-reader
                             lucene/directory)
                            lucene/analyzer)
             q
             pp-after-doc-id
             pp-after-score
             after-doc-id-int
             after-score-float
             false
             (get-segments (:search-page
                            config/page-segments)
                           config/db-server
                           config/database
                           language
                           config/default-timezone))
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
                            @*index-reader*
                            lucene/analyzer)
             q
             pp-after-doc-id
             pp-after-score
             after-doc-id-int
             after-score-float
             true
             (get-segments (:search-page
                            config/page-segments)
                           config/db-server
                           config/database
                           language
                           config/default-timezone))))))
  (GET "/json/:language/:feed/list-documents"
       {{language :language
         feed-name :feed
         limit :limit
         startkey-published :startkey-published
         startkey_docid :startkey_docid} :params
         session :session}
       (when (authorize session language feed-name :GET)
         (json-response
          (db/get-documents-for-feed config/db-server
                                     config/database
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
       (when (authorize session nil :* :GET)
         (json-response
          (if ddt
            (db/list-feeds-by-default-document-type config/db-server
                                                    config/database
                                                    ddt
                                                    language)
            (db/list-feeds config/db-server config/database language)))))
  (POST "/json/new-feed"
        request
        (when (authorize (:session request) nil :* :POST)
          (let [feed (db/create-feed config/db-server
                                     config/database
                                     (read-json (slurp* (:body request))))]
            (reset-frontpage-cache! (:language feed))
            (reset-index-reader!)
            (compare-and-set! search-allowed-feeds
                              @search-allowed-feeds
                              (db/get-searchable-feeds
                               (db/list-feeds config/db-server
                                              config/database)))
            (json-response feed :status 201))))
  (GET "/json/feed/:language/:name"
       {{language :language feed-name :name} :params session :session}
       (if-let [feed (db/get-feed config/db-server
                                  config/database
                                  language
                                  feed-name)]
         (when (authorize session language feed-name :GET)
           (json-response feed))
         (json-response nil)))
  (PUT "/json/feed/:language/:name"
       {{language :language feed-name :name}
        :params body :body session :session}
       (if-let [feed (db/get-feed config/db-server
                                  config/database
                                  language
                                  feed-name)]
         (when (authorize session language feed-name :PUT)
           (let [feed (db/update-feed config/db-server
                                      config/database
                                      language
                                      feed-name
                                      (read-json (slurp* body)))]
             (reset-frontpage-cache! language)
             (reset-index-reader!)
             (compare-and-set! search-allowed-feeds
                               @search-allowed-feeds
                               (db/get-searchable-feeds
                                (db/list-feeds config/db-server
                                               config/database)))
             (json-response feed)))
         (json-response nil)))
  (DELETE "/json/feed/:language/:name"
          {{language :language feed-name :name} :params session :session}
          (if-let [feed (db/get-feed config/db-server
                                     config/database
                                     language
                                     feed-name)]
            (when (authorize session language feed-name :DELETE)
              (json-response 
               (db/delete-feed
                config/db-server
                config/database
                language
                feed-name))
              (reset-frontpage-cache! language)
              (reset-index-reader!)
              (json-response nil))
            (json-response nil)))
  (POST "/json/:language/:feed/new"
        {{language :language feed-name :feed} :params
         session :session
         body :body}
        (when (authorize session language feed-name :POST)
          (let [document (db/create-document config/db-server
                                             config/database
                                             language
                                             feed-name
                                             config/default-timezone
                                             (read-json (slurp* body)))]
            (lucene/add-documents-to-index! lucene/directory [document])
            (reset-frontpage-cache! language)
            (reset-index-reader!)
            (json-response document :status 201))))
  (GET "/json/document/*"
       {{slug :*} :params session :session}
       (if-let [document (db/get-document config/db-server
                                          config/database
                                          (util/force-initial-slash slug)
                                          true)]
         (when (authorize session (:language document) (:feed document) :GET)
           (json-response document))
         (json-response nil)))
  (PUT "/json/document/*"
       {{slug :*} :params body :body session :session}
       (let [slug (util/force-initial-slash slug)]
         (if-let [document (db/get-document config/db-server
                                            config/database slug)]
           (when (authorize session
                            (:language document)
                            (:feed document)
                            :PUT)
             (let [document (db/update-document config/db-server
                                                config/database
                                                config/default-timezone
                                                slug
                                                (read-json (slurp* body)))]
               (lucene/update-document-in-index! lucene/directory
                                                 slug
                                                 document)
               (reset-frontpage-cache! (:language document))
               (reset-index-reader!)
               (reset-page-cache!)
               (json-response document)))
           (json-response nil))))
  (DELETE "/json/document/*"
          {{slug :*} :params session :session}
          (let [slug (util/force-initial-slash slug)]
            (if-let [document (db/get-document config/db-server
                                               config/database
                                               slug)]
              (when (authorize session
                               (:language document)
                               (:feed document)
                               :DELETE)
                (reset-page-cache!)
                (reset-frontpage-cache! (:language document))
                (reset-index-reader!)
                (let [document (db/delete-document config/db-server
                                                   config/database
                                                   slug)]
                  (lucene/delete-document-from-index! lucene/directory slug)
                  (json-response document)))
              (json-response nil))))
  (route/resources "/static/")
  (GET "/*"
       {{slug :*} :params}
       (catch-all config/db-server
                  config/database
                  (util/force-initial-slash slug)
                  config/default-timezone)))

(defn handle-authentication-errors [handler]
  "Middleware function that redirects on insufficient privileges."
  (fn [request]
    (kit/with-handler
      (handler request)
      (kit/handle InsufficientPrivileges []
        (redirect "/permission-denied"))
      (kit/handle AuthenticationRequired []
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
     (not (= (:server-name request) config/default-host))
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