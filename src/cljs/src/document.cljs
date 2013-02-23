;; cljs/src/document.cljs: document model that interacts with the backend.
;; Copyright 2011-2013, Vixu.com, F.M. (Filip) de Waard <fmw@vixu.com>.
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
  (:require [cljs.reader :as reader]
            [clojure.string :as string]
            [vix.util :as util]
            [goog.net.XhrIo :as xhrio]
            [goog.net.EventType :as event-type]
            [goog.events :as events]
            [goog.structs.Map :as Map]))

(def action-to-method {:create :POST :update :PUT :delete :DELETE})

(defn add-initial-slash [slug]
  (if (= (first slug) "/")
    slug
    (str "/" slug)))

(defn request
  "Sends a XMLHttpRequest to uri using the given method (provided as a
   keyword) and data. Calls the given callback fn with the integer
   status code as the first argument and the response data as the
   second."
  [method uri callback & [data]]
  (xhrio/send uri
              (fn [evt]
                (let [xhr (.-target evt)
                      status (. xhr (getStatus))
                      response (. xhr (getResponseText))]
                  (callback status
                            (if (or (= status 200) (= status 201))
                              (reader/read-string response)
                              response))))
              (name method)
              ;; replacing unicode characters until read-string
              ;; can deal with \x (which is equivalent to \u00).
              ;; See: http://dev.clojure.org/jira/browse/CLJ-1025
              (when data
                (string/replace (pr-str data) #"\\x" "\\u00"))
              (when data
                (new goog.structs.Map
                     "Content-Type" "text/plain; charset=utf-8"))))

(defn request-doc [method slug callback & [data]]
  (request method (str "/_api/clj/_document" slug) callback data))

(defn request-feed [method language feed-name callback & [data]]
  (request method
           (str "/_api/clj/_feed/" language "/" feed-name)
           callback
           data))

(defn get-doc [slug callback]
  (request-doc :GET  slug callback))

(defn append-to-document
  [{:keys [action slug] :as doc} callback]
  (request-doc (action-to-method action)
               slug
               callback
               doc))

(defn delete-document-shortcut [slug callback]
  (get-doc slug
           (fn [status [previous-state _]]
             (when (= status 200)
               (append-to-document
                (assoc (dissoc previous-state :_id :_rev)
                  :previous-id (:_id previous-state)
                  :action :delete)
                callback)))))

(defn get-documents-for-feed
  [language feed-name callback & [limit startkey-published startkey_docid]]
  (let [base-uri (str "/_api/clj/"
                      language
                      "/"
                      feed-name
                      "/_list-documents")]
    (request :GET
             (if (some nil? [startkey-published startkey_docid limit])
               (if limit
                 (str base-uri "?limit=" limit)
                 base-uri)
               (str base-uri
                    "?limit=" limit
                    "&startkey-published=" startkey-published
                    "&startkey_docid=" startkey_docid))
             callback)))

(defn get-feeds-list
  [callback & [default-document-type language]]
  (request :GET
           (str "/_api/clj/_list-feeds"
                (when default-document-type
                  (str "?default-document-type=" default-document-type))
                (when language
                  (if default-document-type
                    (str "&language=" language)
                    (str "?language=" language))))
           callback))

(defn get-feed [language feed-name callback]
  (request-feed :GET language feed-name callback))

(defn append-to-feed
  [{:keys [action language name] :as feed-doc} callback]
  (request-feed (action-to-method action)
                language
                name
                callback
                feed-doc))

(defn delete-feed-shortcut [language feed-name callback]
  (get-feed language
            feed-name
            (fn [status [previous-state _]]
              (if (= status 200)
                (append-to-feed
                 (assoc (dissoc previous-state :_id :_rev)
                   :previous-id (:_id previous-state)
                   :action :delete)
                 callback)
                (callback status feed-states)))))