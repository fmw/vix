; src/vix/routes.clj core routes for the application.
;
; Copyright 2011, F.M. (Filip) de Waard <fmw@vix.io>.
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

(ns vix.routes
  (:use compojure.core
        vix.core
        vix.views
        vix.auth
        [clojure.contrib.json :only [read-json json-str]]
        [clojure.contrib.duck-streams :only [slurp*]]
        [ring.util.response :only [redirect]])
  (:require [vix.db :as db]
            [clojure.contrib [error-kit :as kit]]
            [compojure.route :as route]
            [compojure.handler :as handler]))

(def db-server "http://localhost:5984/")
(def database "vix")

; FIXME: switch to utf-8, but need to fix conversion problems first
(defn response [body & [status]]
  {:status (or status (if (nil? body) 404 200))
   :headers {"Content-type" "text/html; charset=ISO-8859-1"}
   :body body})

; FIXME: switch to utf-8, but need to fix conversion problems first
(defn json-response [body & [status]]
  (let [json-body (when-not (nil? body) (json-str body))
        status (if status status (if (nil? body) 404 200))]
    (assoc-in (response json-body status)
              [:headers "Content-type"]
              "application/json; charset=ISO-8859-1")))

; FIXME: add a nice 404 page
; FIXME: add authorization
(defn catch-all [db-server database slug]
  (if-let [document (db/get-document db-server database slug)]
    (response (blog-article-template document))
    (response "<h1>Page not found</h1>" 404)))

(defn logout [session]
  {:session (dissoc session :username)
   :status 302
   :headers {"Location" "/"}})

(defn login [session username password]
  (kit/with-handler
    (when-let [authenticated-session (authenticate
                                       db-server
                                       database
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

(defroutes main-routes
  (GET "/" [] (response (blog-frontpage-template
                        (db/get-documents-for-feed db-server database "blog"))))
  (GET "/admin*" {session :session {feed "feed"} :params}
       (when (authorize session :* :DELETE)
         (response (new-document-template {}))))
  (GET "/login" [] (response (login-page-template "")))
  (POST "/login"
      {session :session {username "username" password "password"} :form-params}
        (login session username password))
  (GET "/logout" {session :session} (logout session))
  (GET "/json/:feed/list-documents" request
       (when (authorize (:session request) (:feed (:params request)) :GET)
         (json-response
          (db/get-documents-for-feed db-server
                                     database
                                     (:feed (:params request))))))
  (GET "/json/list-feeds" {session :session {feed "feed"} :params}
       (when (authorize session :* :GET)
         (json-response (db/list-feeds db-server database))))
  (POST "/json/new-feed" request
        (when (authorize (:session request) :* :POST)
          (json-response (db/create-feed
                           db-server
                           database
                           (read-json (slurp* (:body request))))
                         201)))
  (GET "/json/feed/:name" {{feed-name :name} :params session :session}
       (if-let [feed (db/get-feed db-server database feed-name)]
         (when (authorize session feed-name :GET)
           (json-response feed))
         (json-response nil)))
  (PUT "/json/feed/:name" {{feed-name :name} :params body :body session :session}
       (if-let [feed (db/get-feed db-server database feed-name)]
         (when (authorize session feed-name :PUT)
           (json-response 
            (db/update-feed
             db-server
             database
             feed-name
             (read-json (slurp* body)))))
         (json-response nil)))
  (DELETE "/json/feed/:name" {{feed-name :name} :params session :session}
          (if-let [feed (db/get-feed db-server database feed-name)]
            (when (authorize session feed-name :DELETE)
              (json-response 
               (db/delete-feed
                db-server
                database
                feed-name)))
            (json-response nil)))
  (POST "/json/:feed/new" request
        (when (authorize (:session request) (:feed (:params request)) :POST)
          (json-response (db/create-document
                           db-server
                           database
                           (:feed (:params request))
                           (read-json (slurp* (:body request))))
                         201)))
  (GET "/json/document/*" {{slug :*} :params session :session}
       (if-let [document (db/get-document
                           db-server database (force-initial-slash slug))]
         (when (authorize session (:feed document) :GET)
           (json-response document))
         (json-response nil)))
  (PUT "/json/document/*" {{slug :*} :params body :body session :session}
       (let [slug (force-initial-slash slug)]
         (if-let [document (db/get-document db-server database slug)]
           (when (authorize session (:feed document) :PUT)
             (json-response 
               (db/update-document
                 db-server
                 database
                 slug
                 (read-json (slurp* body)))))
           (json-response nil))))
  (DELETE "/json/document/*" {{slug :*} :params session :session}
          (let [slug (force-initial-slash slug)]
            (if-let [document (db/get-document db-server database slug)]
              (when (authorize session (:feed document) :DELETE)
                (json-response 
                  (db/delete-document
                    db-server
                    database
                    slug)))
              (json-response nil))))
  (route/resources "/static/")
  (GET "/*" {{slug :*} :params}
       (catch-all db-server database (force-initial-slash slug))))

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