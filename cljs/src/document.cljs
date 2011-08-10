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

(defn request-doc [uri callback method content]
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
    (request-doc slug callback method content)))

(defn get-doc [slug callback]
  (request-doc-with-slug slug callback "GET" nil))

(defn delete-doc [slug callback]
  (request-doc-with-slug slug callback "DELETE" nil))

(defn create-doc [callback json-map]
  (request-doc "/json/blog/new" callback "POST" json-map))

(defn update-doc [slug callback json-map]
  (request-doc-with-slug slug callback "PUT" json-map))

(defn get-feed [name callback]
  (xhrio/send (str "/json/" name) callback))