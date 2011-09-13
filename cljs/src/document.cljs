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

(defn create-doc [callback feed json-map]
  (request (str "/json/" feed "/new") callback "POST" json-map))

(defn update-doc [slug callback json-map]
  (request-doc-with-slug slug callback "PUT" json-map))

(defn get-documents-for-feed
  ([name callback]
     (get-documents-for-feed name callback nil nil nil))
  ([name callback limit]
     (get-documents-for-feed name callback limit nil nil))
  ([name callback limit startkey-published startkey_docid]
     (let [base-uri (str "/json/" name "/list-documents")
           uri  (if (or (nil? startkey-published) (nil? startkey_docid) (nil? limit))
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
     (get-feeds-list callback nil))
  ([callback default-document-type]
     (let [uri (if default-document-type
                 (str "/json/list-feeds?default-document-type="
                      default-document-type)
                 "/json/list-feeds")]
       (xhrio/send uri callback))))

(defn get-feed [feed-name callback]
  (request (str "/json/feed/" feed-name) callback "GET" nil))

(defn create-feed [callback json-map]
  (request "/json/new-feed" callback "POST" json-map))

(defn update-feed [feed-name callback json-map]
  (request (str "/json/feed/" feed-name) callback "PUT" json-map))

(defn delete-feed [feed-name callback]
  (request (str "/json/feed/" feed-name) callback "DELETE" nil))