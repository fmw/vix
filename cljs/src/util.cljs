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

(defn date-now []
  (let [d (new date/Date)]
    {:year (. d (getUTCFullYear))
     :month (inc (. d (getUTCMonth)))
     :day (. d (getDate))}))