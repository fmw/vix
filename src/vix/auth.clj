;; src/vix/auth.clj: authentication and authorization functionality.
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

(ns vix.auth
  (:require [vix.db :as db]
            [clojure.contrib [error-kit :as kit]]
            [com.ashafa.clutch :as clutch])
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
  "Returns a new map for provided input m, with regular keys kept as is,
   but with complex keys (e.g. :[:en :blog]) converted back. This
   is necessary, because all keys are stored as strings in CouchDB and
   converted to keywords as is on retrieval. This results in complex
   keys (e.g. vectors like [:en :blog]) being converted to keywords
   like :[:en :blog]. This is not the desired behaviour, so this
   function restores them."
  [m]
  (zipmap (map (fn [old-key]
                 (let [converted-key (read-string (name old-key))]
                   (if (symbol? converted-key)
                     old-key
                     converted-key)))
               (keys m))
          (vals m)))

(defn get-user
  "Returns user document for username from the provided database, with
   a permissions vector associated to the :permissions key. "
  [database username]
  (try
    (when-let [user (:value (first (clutch/get-view database
                                                    "views"
                                                    :by_username
                                                    {:key username})))]
      (when (map? user)
        (assoc user :permissions (fix-complex-keys (:permissions user)))))
    ;; Create views if they don't exist yet.
    (catch java.lang.IllegalStateException e
      (do
        (db/create-views "http://localhost:5984/"
                         database
                         "views"
                         db/views)
        (get-user database username)))))

(defn add-user
  "Adds the user using the provided username and password with the
   given permission-map to the provided database on the given database
   server. The keys in the permissions-map are either vectors of
   language and feed-name pairs (e.g. [\"en\", \"images\"]) or the
   :* keyword designated for global permissions. Each value in the
   permissions-map is a vector of capitalized strings representing
   HTTP methods that are allowed for the particular key. Legal
   usernames have at least two characters and may contain alphanumeric
   characters, dots (i.e. .), dashes (i.e. -) and at
   symbols (i.e. @). Passwords are hashed with Bcrypt."
  [database username password permissions-map]
  (if (get-user database username)
    (kit/raise UserExists username)
    (if (re-matches #"^[\w-.@]{2,}" username)
      (clutch/put-document
       database
       {:type "user"
        :username username
        :password (BCrypt/hashpw password (BCrypt/gensalt 12))
        :permissions permissions-map})
      (kit/raise InvalidUsername))))

(defn authenticate
  "Returns a copy of the provided session map with the :username
   mapped to the provided username and a :permissions key associated
   to the permissions as stored in the database if the username and
   password can be successfully authenticated to a user document
   stored in the provided database name on the given database server.
   Otherwise, a UserDoesNotExist error is raised when the user wasn't
   found and a UsernamePasswordMismatch error if the username/password
   combination isn't correct. Note that you probably shouldn't
   distinguish between these two errors on the login page, because
   providing a UserDoesNotExist error message would enable attackers
   to mine for existing usernames on the system."
  [database session username password]
  (if-let [user (get-user database username)]
    (if (BCrypt/checkpw password (:password user))
      (assoc session :username username :permissions (:permissions user))
      (kit/raise UsernamePasswordMismatch))
    (kit/raise UserDoesNotExist username)))

(defn- authorize-for-feed
  "Private convenience function that checks the value in the
   permissions-map for either the :* key (for global permissions) or a
   corresponding language and feed-name key (e.g. [:en :pages]) to see
   if that collection contains the desired method. Returns a string
   with the requested method, or nil if not authorized."
  [permissions-map method & [language feed-name]]
  (some #{(name method)}
        (get permissions-map
             (if (= feed-name :*)
               :*
               [language feed-name]))))

(defn authorize
  "Returns true if the provided session map contains a string value
   for the :username key and the string value of the requested method
   keyword in its :permissions-map value for a vector of the provided
   language and feed-name or global permission collection (if
   any). Global permissions (associated to :* in the permissions map)
   are only checked if there is no specific permission map for the
   provided language and feed-name. If neither succeeds an
   InsufficientPrivileges error is raised. If there is no string value
   mapped to the :username key of the provided session map an
   AuthenticationRequired error is raised."
  [session method language feed-name]
  (if (string? (:username session))
    (if (or (authorize-for-feed (:permissions session)
                                method
                                language
                                feed-name)
            ;; only fall back to global permissions if there are no
            ;; feed-specific permissions provided for this username
            (and (not (contains? (:permissions session)
                                 [language feed-name]))
                 (authorize-for-feed (:permissions session)
                                     method
                                     language
                                     :*)))
      true
      (kit/raise InsufficientPrivileges))
    (kit/raise AuthenticationRequired)))