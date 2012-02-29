;; cljs/src/document.cljs: document model that interacts with the backend.
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

(ns vix.document
  (:require [vix.util :as util]
            [goog.net.XhrIo :as xhrio]
            [goog.net.EventType :as event-type]
            [goog.json :as gjson]
            [goog.events :as events]
            [goog.structs.Map :as Map]))

(defn add-initial-slash [slug]
  (if (= (first slug) "/")
    slug
    (str "/" slug)))

(defn request [uri callback method content]
  (let [req (new goog.net.XhrIo)
        content (if (map? content)
                  (goog.json/serialize (util/map-to-obj content))
                  content)]
    (events/listen req goog.net.EventType/COMPLETE callback)
    (. req (send
            uri
            method
            content
            (new goog.structs.Map "Content-Type"
                                  "application/json;charset=utf-8")))))

(defn request-doc-with-slug [slug callback method content]
  (let [slug (str "/json/document" slug)]
    (request slug callback method content)))

(defn get-doc [slug callback]
  (request-doc-with-slug slug callback "GET" nil))

(defn delete-doc [slug callback]
  (request-doc-with-slug slug callback "DELETE" nil))

(defn create-doc [callback language feed-name json-map]
  (request (str "/json/" language "/" feed-name "/new")
           callback
           "POST"
           json-map))

(defn update-doc [slug callback json-map]
  (request-doc-with-slug slug callback "PUT" json-map))

(defn get-documents-for-feed
  ([language feed-name callback]
     (get-documents-for-feed language feed-name callback nil nil nil))
  ([language feed-name callback limit]
     (get-documents-for-feed language feed-name callback limit nil nil))
  ([language feed-name callback limit startkey-published startkey_docid]
     (let [base-uri (str "/json/" language "/" feed-name "/list-documents")
           uri  (if (or (nil? startkey-published)
                        (nil? startkey_docid) (nil? limit))
                  (if limit
                    (str base-uri "?limit=" limit)
                    base-uri)
                  (str base-uri
                       "?limit=" limit
                       "&startkey-published=" startkey-published
                       "&startkey_docid=" startkey_docid))]
       (xhrio/send uri callback))))

(defn get-feeds-list
  ([callback]
     (get-feeds-list callback nil nil))
  ([callback default-document-type]
     (get-feeds-list callback default-document-type nil))
  ([callback default-document-type language]
     (let [uri (str "/json/list-feeds"
                    (when default-document-type
                      (str "?default-document-type=" default-document-type))
                    (when language
                      (if default-document-type
                        (str "&language=" language)
                        (str "?language=" language))))]
       (xhrio/send uri callback))))

(defn get-feed [language feed-name callback]
  (request (str "/json/feed/" language "/" feed-name) callback "GET" nil))

(defn create-feed [callback json-map]
  (request "/json/new-feed" callback "POST" json-map))

(defn update-feed [language feed-name callback json-map]
  (request (str "/json/feed/" language "/" feed-name)
           callback
           "PUT"
           json-map))

(defn delete-feed [language feed-name callback]
  (request (str "/json/feed/" language "/" feed-name) callback "DELETE" nil))