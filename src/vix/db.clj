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

(def views {:by_feed  {:map (slurp (str "/home/fmw/clj/vix/src/"
                                        "database-views/"
                                        "map_document_by_feed.js"))}
            :by_slug  {:map (slurp (str "/home/fmw/clj/vix/src/"
                                        "database-views/"
                                        "map_document_by_slug.js"))}
            :by_username  {:map (slurp (str "/home/fmw/clj/vix/src/"
                                        "database-views/"
                                        "map_user_by_username.js"))}
            :feeds {:map (slurp (str "/home/fmw/clj/vix/src/"
                                          "database-views/"
                                          "map_feeds_by_name.js"))}})

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

(defn create-views [db-server db-name design-doc views]
  (doseq [view views]
    (view-sync
     db-server db-name design-doc (key view) (val view))))

(defn encode-view-options [options]
  (let [encoded-values (map #(URLEncoder/encode % "UTF-8")
                            (map json/json-str (vals options)))
        opt-seq (interleave (map name (keys options)) encoded-values)]
    (apply str (butlast (interleave opt-seq (flatten (repeat ["=" "&"])))))))

(defn view-get [db-server db-name design-doc view-name & [view-options]]
  (try
    (let [uri (str db-server
                   db-name
                   "/_design/"
                   design-doc
                   "/_view/"
                   view-name
                   "?"
                   (encode-view-options view-options))]

      (json/read-json (:body (http/get uri))))
      (catch java.lang.Exception e
        (if (= (.getMessage e) "404")
          (do
            ; automatically add missing views and run again
            (create-views db-server db-name "views" views)
            (view-get db-server db-name design-doc view-name view-options))
          (.printStackTrace e)))))

(defn list-feeds [db-server db-name]
  (if-let [feeds (:rows (view-get db-server
                                  db-name
                                  "views"
                                  "feeds"
                                  {:descending true}))]
    (map #(:value %) feeds)))

(defn get-feed [db-server db-name feed-name]
  (let [feed (view-get db-server
                       db-name
                       "views"
                       "feeds"
                       {:include_docs true
                        :key feed-name})]
    (:value (first (:rows feed)))))

(defn create-feed [db-server db-name data-map]
  (couchdb/document-create
    db-server
    db-name
    (assoc data-map
           :type "feed"
           :created (now-rfc3339))))

(defn update-feed [db-server db-name feed-name data-map]
  (if-let [feed (get-feed db-server db-name feed-name)]
    (couchdb/document-update
      db-server
      db-name
      (:_id feed)
      (assoc feed
             :feed-updated (now-rfc3339)
             :title (:title data-map)
             :subtitle (:subtitle data-map)
             :default-slug-format (:default-slug-format data-map)
             :default-document-type (:default-document-type data-map)))))

; TODO: delete/flag feed content
(defn delete-feed [db-server db-name feed-name]
  (kit/with-handler
    (if-let [feed (get-feed db-server db-name feed-name)]
      (couchdb/document-delete db-server db-name (:_id feed)))
    (kit/handle couchdb/DocumentNotFound []
                nil)
    (kit/handle couchdb/ResourceConflict []
                nil)))

(defn get-documents-for-feed [db-server db-name feed]
  (if-let [entries (:rows (view-get db-server
                                    db-name
                                    "views"
                                    "by_feed"
                                    {:endkey [feed]
                                     :startkey [feed "2999"]
                                     :include_docs true
                                     :descending true}))]
    (map #(:value %) entries)))

(defn get-document [db-server db-name slug]
  (let [document (view-get db-server
                           db-name
                           "views"
                           "by_slug"
                           {:include_docs true
                            :key slug})]
    (:value (first (:rows document)))))

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

(defn update-document [db-server db-name slug new-document]
  (if-let [document (get-document db-server db-name slug)]
    (couchdb/document-update
      db-server
      db-name
      (:_id document)
      (assoc document
             :updated (now-rfc3339)
             :title (:title new-document)
             :content (:content new-document)
             :draft (:draft new-document)))))

(defn delete-document [db-server db-name slug]
  (kit/with-handler
    (if-let [document (get-document db-server db-name slug)]
      (couchdb/document-delete db-server db-name (:_id document)))
    (kit/handle couchdb/DocumentNotFound []
                nil)
    (kit/handle couchdb/ResourceConflict []
                nil)))