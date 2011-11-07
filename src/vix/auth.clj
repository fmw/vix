; src/vix/auth.clj: authentication and authorization functionality.
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

(ns vix.auth
  (:require [vix.db :as db]
            [ring.util.response :only [redirect]]
            [clojure.contrib [error-kit :as kit]]
            [couchdb [client :as couchdb]])
  (:import (org.mindrot.jbcrypt BCrypt)))

(kit/deferror InvalidUsername [] []
  {:msg (str "Username's can only contain alphanumeric characters "
             "and '-', '_', '.' and '@'")
   :unhandled (kit/throw-msg Exception)})

(kit/deferror UserExists [] [username]
  {:msg (str "The user '" username "' already exists.")
   :unhandled (kit/throw-msg Exception)})

(kit/deferror UserDoesNotExist [] [username]
  {:msg (str "The user '" username "' doesn't exist.")
   :unhandled (kit/throw-msg Exception)})

(kit/deferror UsernamePasswordMismatch [] []
  {:msg "The provided username and password do not match."
   :unhandled (kit/throw-msg Exception)})

(kit/deferror InsufficientPrivileges [] []
  {:msg (str "You have insufficient privileges to perform "
             "this action on the requested resource.")
   :unhandled (kit/throw-msg Exception)})

(kit/deferror AuthenticationRequired [] []
  {:msg "You need to authenticate before you can perform this action."
   :unhandled (kit/throw-msg Exception)})

(defn fix-complex-keys
  "Keys (including vectors) are returned as keywords from CouchDB.
  This function converts keys that are supposed to be vectors back."
  [m]
  (let [transform-key (fn [old-key]
                        (let [converted-key (read-string (name old-key))]
                          (if (vector? converted-key)
                            converted-key
                            old-key)))]
    (zipmap (map transform-key (keys m)) (vals m))))

(defn get-user [db-server db-name username]
  (kit/with-handler
    (let [document (couchdb/view-get
                     db-server
                     db-name
                     "views"
                     "by_username"
                     {:include_docs true
                      :key username})]
      (let [user (:doc (first (:rows document)))]
        (when (map? user)
          (assoc user :permissions (fix-complex-keys (:permissions user))))))
    ; Create views if they don't exist yet.
    (kit/handle couchdb/DocumentNotFound []
      (do
        (db/create-views db-server db-name "views" db/views)
        (get-user db-server db-name username)))))

(defn add-user [db-server db-name username password permissions-map]
  (if (get-user db-server db-name username)
    (kit/raise UserExists username)
    (if (re-matches #"^[\w-.@]{2,}" username)
      (couchdb/document-create
        db-server
        db-name
        {:type "user"
         :username username
         :password (BCrypt/hashpw password (BCrypt/gensalt 12))
         :permissions permissions-map})
      (kit/raise InvalidUsername))))

(defn authenticate [db-server db-name session username password]
  (if-let [user (get-user db-server db-name username)]
    (if (BCrypt/checkpw password (:password user))
      (assoc session :username username :permissions (:permissions user))
      (kit/raise UsernamePasswordMismatch))
    (kit/raise UserDoesNotExist username)))

(defn authorize-for-feed
  ([permissions-map feed-name method] ; used for global permissions (:*)
     (authorize-for-feed permissions-map nil feed-name method))
  ([permissions-map language feed-name method]
     (let [feed-key (if (= feed-name :*)
                      :*
                      [(keyword language) (keyword feed-name)])
           feed-permissions-set (set (get permissions-map feed-key))]
       (contains? feed-permissions-set (name method)))))

(defn authorize [session language feed-name method]
  (if (:username session)
    (let [permissions-map (:permissions session)]
      (if (or (authorize-for-feed permissions-map
                                  language
                                  feed-name
                                  method)
              ;; only fall back to global permissions if there are no
              ;; feed-specific permissions provided for this username
              (and (not (contains? permissions-map [(keyword language)
                                                    (keyword feed-name)]))
                   (authorize-for-feed permissions-map
                                       language
                                       :*
                                       method)))
        true
        (kit/raise InsufficientPrivileges)))
      (kit/raise AuthenticationRequired)))


