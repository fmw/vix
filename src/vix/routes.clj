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
        vix.db
        vix.views
        vix.auth
        [clojure.contrib.json :only [read-json json-str]]
        [clojure.contrib.duck-streams :only [slurp*]]
        [ring.util.response :only [redirect]])
  (:require [clojure.contrib [error-kit :as kit]]
            [compojure.route :as route]
            [compojure.handler :as handler]))

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
(defn catch-all [db-server db-name slug]
  (if-let [document (get-document db-server db-name slug)]
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
                                       db-name
                                       session
                                       username
                                       password)]
      {:session authenticated-session
       :status 302
       :headers {"Location" "/admin/blog"}})
    (kit/handle UserDoesNotExist []
      (redirect "/login"))
    (kit/handle UsernamePasswordMismatch []
      (redirect "/login"))))

(defroutes main-routes
  (GET "/" [] (response (blog-frontpage-template
                        (get-feed db-server db-name "blog"))))
  (GET "/admin*" {session :session {feed "feed"} :params}
       (when (authorize session :* :DELETE)
         (response (new-document-template {}))))
  (GET "/login" [] (response (login-page-template "")))
  (POST "/login"
      {session :session {username "username" password "password"} :form-params}
        (login session username password))
  (GET "/logout" {session :session} (logout session))
  (GET "/json/:feed" request
       (when (authorize (:session request) (:feed (:params request)) :GET)
         (json-response
           (get-feed db-server db-name (:feed (:params request))))))
  (POST "/json/:feed/new" request
        (when (authorize (:session request) (:feed (:params request)) :POST)
          (json-response (create-document
                           db-server
                           db-name
                           (:feed (:params request))
                           (read-json (slurp* (:body request))))
                         201)))
  (GET "/json/document/*" {{slug :*} :params session :session}
       (if-let [document (get-document
                           db-server db-name (force-initial-slash slug))]
         (when (authorize session (:feed document) :GET)
           (json-response document))
         (json-response nil)))
  (PUT "/json/document/*" {{slug :*} :params body :body session :session}
       (let [slug (force-initial-slash slug)]
         (if-let [document (get-document db-server db-name slug)]
           (when (authorize session (:feed document) :PUT)
             (json-response 
               (update-document
                 db-server
                 db-name
                 slug
                 (read-json (slurp* body)))))
           (json-response nil))))
  (DELETE "/json/document/*" {{slug :*} :params session :session}
          (let [slug (force-initial-slash slug)]
            (if-let [document (get-document db-server db-name slug)]
              (when (authorize session (:feed document) :DELETE)
                (json-response 
                  (delete-document
                    db-server
                    db-name
                    slug)))
              (json-response nil))))
  (route/resources "/static/")
  (GET "/*" {{slug :*} :params}
       (catch-all db-server db-name (force-initial-slash slug))))

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
