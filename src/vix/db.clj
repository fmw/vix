;; src/vix/db.clj functionality that provides database interaction
;;
;; Copyright 2011-2012 , F.M. (Filip) de Waard <fmw@vix.io>.
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

(ns vix.db
  (:require [clojure.contrib [error-kit :as kit]]
            [clojure.contrib.base64 :as base64]
            [couchdb [client :as couchdb]]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [vix.util :as util])
  (:import [java.net URLEncoder]
           [org.apache.commons.codec.binary Base64]))

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
                                     "map_feeds.js"))}
            :feeds_by_default_document_type {:map
                                             (slurp
                                              (str "/home/fmw/clj/vix/"
                                                   "src/database-views/"
                                                   "map_feeds_by_"
                                                   "default_document_"
                                                   "type.js"))}})

(defn #^{:rebind true} view-sync
  [server db design-doc view-name view-functions]
  "Reimplementation of clojure-couchdb's view-add function."
  (util/log-hide!)
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
  (util/log-restore!))

(defn create-views [db-server db-name design-doc views]
  (doseq [view views]
    (view-sync
     db-server db-name design-doc (key view) (val view))))

(defn encode-view-options [options]
  (let [encode-option (fn [option]
                        (if (or (= (key option) :startkey_docid)
                                (= (key option) :endkey_docid))
                          (val option) ; don't quote docids
                          (json/json-str (val option))))
        encoded-values (map #(URLEncoder/encode % "UTF-8")
                            (map encode-option options))
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

(defn list-feeds-by-default-document-type
  [db-server db-name default-document-type]
  (if-let [feeds (:rows (view-get db-server
                                  db-name
                                  "views"
                                  "feeds_by_default_document_type"
                                  {:descending true
                                   :key default-document-type}))]
    (map #(:value %) feeds)))

(defn get-feed [db-server db-name language feed-name]
  (let [feed (view-get db-server
                       db-name
                       "views"
                       "feeds"
                       {:include_docs true
                        :key [language feed-name]})]
    (:value (first (:rows feed)))))

(defn create-feed [db-server db-name data-map]
  (couchdb/document-create
    db-server
    db-name
    (assoc data-map
           :type "feed"
           :created (util/now-rfc3339))))

(defn update-feed [db-server db-name language feed-name data-map]
  (if-let [feed (get-feed db-server db-name language feed-name)]
    (couchdb/document-update
      db-server
      db-name
      (:_id feed)
      (assoc feed
             :feed-updated (util/now-rfc3339)
             :title (:title data-map)
             :subtitle (:subtitle data-map)
             :language (:language data-map)
             :default-slug-format (:default-slug-format data-map)
             :default-document-type (:default-document-type data-map)))))

; TODO: delete/flag feed content
(defn delete-feed [db-server db-name language feed-name]
  (kit/with-handler
    (if-let [feed (get-feed db-server db-name language feed-name)]
      (couchdb/document-delete db-server db-name (:_id feed)))
    (kit/handle couchdb/DocumentNotFound []
                nil)
    (kit/handle couchdb/ResourceConflict []
                nil)))

(defn get-documents-for-feed
  ([db-server db-name language feed-name]
     (get-documents-for-feed db-server
                             db-name
                             language
                             feed-name
                             nil
                             nil
                             nil))
  ([db-server db-name language feed-name limit]
     (get-documents-for-feed db-server
                             db-name
                             language
                             feed-name
                             limit
                             nil
                             nil))
  ([db-server db-name language feed-name limit startkey startkey_docid]
     (let [feed [language feed-name]
           options {:endkey [feed nil]
                    :startkey [feed (or startkey "2999")]
                    :include_docs true
                    :descending true}
           options (if (nil? limit)
                     options
                     (assoc options :limit (+ 1 limit))) ; +1 for extra doc
           options (if (nil? startkey_docid)
                    options
                    (assoc options :startkey_docid startkey_docid))
           result (view-get db-server db-name "views" "by_feed" options)]
       (if-let [entries (:rows result)]
         (let [docs (map #(:value %) entries)]
           (if (or (nil? limit) (<= (count docs) limit)) ; has next page?
             {:next nil
              :documents docs}
             {:next {:startkey_docid (:_id (last docs))
                     :published (:published (last docs))}
              :documents (butlast docs)}))))))

(defn get-document
  ([db-server db-name slug] (get-document db-server db-name slug false))
  ([db-server db-name slug include-attachment?]
     (let [document (view-get db-server
                              db-name
                              "views"
                              "by_slug"
                              {:include_docs true
                               :key slug})
           doc-row (:value (first (:rows document)))]
       (if (and include-attachment? (:original (:_attachments doc-row)))
         (let [content-type (:content_type (:original
                                            (:_attachments doc-row)))
               f (:body (couchdb/attachment-get db-server
                                                db-name
                                                (:_id doc-row)
                                                "original"))]
           (assoc doc-row :attachment {:type content-type
                                       :data (Base64/encodeBase64String f)}))
         doc-row))))

(defn get-unique-slug [db-server db-name slug]
  (loop [slug slug]
    (let [document (get-document db-server db-name slug)]
      (if document
        (recur (util/increment-slug slug))
        slug))))

(defn create-document [db-server db-name language feed-name document]
  (let [slug (get-unique-slug db-server db-name (:slug document))
        doc (couchdb/document-create db-server
                                     db-name
                                     (assoc (dissoc document :attachment)
                                       :type "document"
                                       :feed feed-name
                                       :language language
                                       :slug slug
                                       :published (util/now-rfc3339)))]

    (if-not (and (nil? (:data (:attachment document)))
                   (nil? (:type (:attachment document))))
      (do
        (couchdb/attachment-create db-server
                                   db-name
                                   (:_id doc)
                                   "original"
                                   (Base64/decodeBase64
                                    (:data (:attachment document)))
                                   (:type (:attachment document)))
        ;; return newly fetched doc from db (including attachment)
        (get-document db-server db-name (:slug doc)))
      ;; when there is no attachment we don't need to refetch
      doc)))

(defn update-document [db-server db-name slug new-document]
  (if-let [document (get-document db-server db-name slug)]
    (let [doc (couchdb/document-update
               db-server
               db-name
               (:_id document)
               (assoc (dissoc document :attachment)
                 :updated (util/now-rfc3339)
                 :title (:title new-document)
                 :content (:content new-document)
                 :draft (:draft new-document)))]
      
      (if-not (and (nil? (:data (:attachment new-document)))
                   (nil? (:type (:attachment new-document))))
        (do
          (couchdb/attachment-create db-server
                                     db-name
                                     (:_id doc)
                                     "original"
                                     (Base64/decodeBase64
                                      (:data (:attachment new-document)))
                                     (:type (:attachment new-document)))
          ; return newly fetched doc from db (including attachment)
          (get-document db-server db-name (:slug doc)))
        ; when there is no attachment we don't need to refetch
        doc))))

(defn delete-document [db-server db-name slug]
  (kit/with-handler
    (if-let [document (get-document db-server db-name slug)]
      (couchdb/document-delete db-server db-name (:_id document)))
    (kit/handle couchdb/DocumentNotFound []
                nil)
    (kit/handle couchdb/ResourceConflict []
                nil)))