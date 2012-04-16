;; src/vix/routes.clj core routes for the application.
;;
;; Copyright 2011, F.M. (Filip) de Waard <fmw@vix.io>.
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
            [clojure.contrib [error-kit :as kit]]
            [couchdb [client :as couchdb]]
            [compojure.route :as route]
            [compojure.handler :as handler]))

(def search-allowed-feeds
  (atom (db/get-searchable-feeds (db/list-feeds config/db-server
                                                config/database))))

; FIXME: test character encoding issues
(defn response [body & {:keys [status content-type]}]
  {:status (or status (if (nil? body) 404 200))
   :headers {"Content-Type" (or content-type "text/html; charset=UTF-8")}
   :body body})

; FIXME: test character encoding issues
(defn json-response [body & {:keys [status]}]
  (response (when-not (nil? body) (json-str body))
            :status (or status (if (nil? body) 404 200))
            :content-type "application/json; charset=UTF-8"))

; FIXME: add a nice 404 page
; FIXME: add authorization
(defn catch-all [db-server database slug]
  (if-let [document (db/get-document db-server database slug)]
    (if-let [original (:original (:_attachments document))]
      (response (new java.io.ByteArrayInputStream
                     (:body (couchdb/attachment-get db-server
                                                    database
                                                    (:_id document)
                                                    "original")))
                :content-type (:content_type original))
      (response (views/blog-article-view document config/default-timezone)))
    (response "<h1>Page not found</h1>" :status 404)))

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
  (GET "/test"
       {{a :a} :params}
       (json-str a))
  (GET "/"
       []
       (response (views/blog-frontpage-view
                  (db/get-documents-for-feed config/db-server
                                             config/database
                                             ;; FIXME: make configurable
                                             "en"
                                             "blog")
                  config/default-timezone)))
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
             false)
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
             true)))))
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
                feed-name)))
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
                                            config/database
                                            slug)]
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
                  (util/force-initial-slash slug))))

(defn handle-authentication-errors [handler]
  (fn [request]
    (kit/with-handler
      (handler request)
      (kit/handle InsufficientPrivileges []
        (redirect "/permission-denied"))
      (kit/handle AuthenticationRequired []
        (redirect "/login")))))

(def app
  (-> (handler/site main-routes)
      (handle-authentication-errors)))