(ns vix.document
  (:require [goog.net.XhrIo :as xhrio]))

(defn add-initial-slash [slug]
  (if (= (first slug) "/")
    slug
    (str "/" slug)))

(defn get-doc [slug callback]
  (let [slug (add-initial-slash slug)]
    (xhrio/send (str "/json/document" slug) callback)))