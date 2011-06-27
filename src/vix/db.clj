; src/vix/db.clj functionality that provides database interaction
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

(ns vix.db
  (:use vix.core)
  (:require [clojure.contrib [error-kit :as kit]]
            [couchdb [client :as couchdb]]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [clj-time.core]
            [clj-time.format])
  (:import [java.net URLEncoder]))

(def db-server "http://localhost:5984/")
(def db-name "vix")

(def views {:by_feed  {:map (slurp (str "/home/fmw/clj/vix/src/"
                                        "database-views/"
                                        "map_document_by_feed.js"))}
            :by_slug  {:map (slurp (str "/home/fmw/clj/vix/src/"
                                        "database-views/"
                                        "map_document_by_slug.js"))}
            :by_username  {:map (slurp (str "/home/fmw/clj/vix/src/"
                                        "database-views/"
                                        "map_user_by_username.js"))}})

(defn #^{:rebind true} view-sync
  [server db design-doc view-name view-functions]
  "Reimplementation of clojure-couchdb's view-add function."
  (log-hide!)
  (let [doc-path (str "_design/" design-doc)]
    (kit/with-handler
      (let [document (couchdb/document-get server db doc-path)]
        (couchdb/document-update
          server
          db
          doc-path
          (assoc-in document [:views (keyword view-name)] view-functions)))
        (kit/handle couchdb/DocumentNotFound []
          (couchdb/document-create
            server
            db
            doc-path
            {:language "javascript"
             :views {(keyword view-name) view-functions}}))))
  (log-restore!))

(defn view-get [server db design-doc view-name & [view-options]]
  (json/read-json (:body (http/get
                          (str server
                               db
                               "/_design/"
                               design-doc
                               "/_view/"
                               view-name
                               "?"
                               (couchdb/url-encode view-options))))))

(defn create-views [db-server db-name design-doc views]
  (doseq [view views]
    (view-sync
      db-server db-name design-doc (key view) (val view))))

(defn get-document [db-server db-name slug]
  (kit/with-handler
    (let [document (couchdb/view-get
                     db-server
                     db-name
                     "views"
                     "by_slug"
                     {:include_docs true
                      :key slug})]
      (:doc (first (:rows document))))
    ; Create views if they don't exist yet.
    (kit/handle couchdb/DocumentNotFound []
      (do
        (create-views db-server db-name "views" views)
        (get-document db-server db-name slug)))))

(defn get-unique-slug [db-server db-name slug]
  (loop [slug slug]
    (let [document (get-document db-server db-name slug)]
      (if document
        (recur (increment-slug slug))
        slug))))

(defn create-document [db-server db-name feed document]
  (couchdb/document-create
    db-server
    db-name
    (assoc document
           :type "document"
           :feed feed
           :slug (get-unique-slug db-server db-name (:slug document))
           :published (now-rfc3339))))

(defn get-feed [db-server db-name feed]
  (if-let [entries
    (:rows
     (view-get
      db-server
      db-name
      "views"
      "by_feed"
      {:endkey (str "[\"" feed "\"]")
       :startkey (str "[\"" feed "\", \"2999\"]")
       :descending "true"}))]
    (map #(:value %) entries)
    ; Create views if they don't exist yet.
    (do
      (create-views db-server db-name "views" views)
      (get-feed db-server db-name feed))))


(defn update-document [db-server db-name slug new-document]
  (if-let [document (get-document db-server db-name slug)]
    (couchdb/document-update
      db-server
      db-name
      (:_id document)
      (assoc document
             :updated-at (now-rfc3339)
             :title (:title new-document)
             :content (:content new-document)
             :draft (:draft new-document)))))

(defn delete-document [db-server db-name slug]
  (if-let [document (get-document db-server db-name slug)]
    (couchdb/document-delete db-server db-name (:_id document))))
