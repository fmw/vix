(ns vix.util
  (:require [goog.global :as global]
            [goog.dom :as dom]
            [goog.date :as date]
            [clojure.string :as string]))

;; FIXME: support nested maps
(defn map-to-obj [m]
  (let [o (js-obj)]
    (doall (map #(aset o (name (first %)) (second %)) m)) o))

(defn set-page-title! [title]
  (set! global/document.title title))

(defn has-consecutive-dashes-or-slashes? [slug]
  (if (re-find #"[\-/]{2,}" slug) true false))

(defn get-element [el-id-or-obj]
  (if (string? el-id-or-obj)
    (dom/getElement el-id-or-obj)
    el-id-or-obj))

(defn date-now! []
  (let [d (new date/Date)]
    {:year (. d (getUTCFullYear))
     :month (inc (. d (getUTCMonth)))
     :day (. d (getDate))}))

(defn slugify-document-title [title]
  (string/join "-" (filter #(not (string/blank? %))
                           (.split title #"[^a-zA-Z0-9]"))))

(defn create-slug [raw-slug title feed date extension]
  (loop [slug raw-slug
         substitutions [["{document-title}" (slugify-document-title title)]
                        ["{feed-name}" (:name feed)]
                        ["{language}" (:language feed)]
                        ["{year}" (:year date)]
                        ["{month}" (:month date)]
                        ["{day}" (:day date)]
                        ["{ext}" extension]]]
    (if (pos? (count substitutions))
      (recur (string/replace slug
                             (first (first substitutions))
                             (last (first substitutions)))
             (rest substitutions))
      slug)))