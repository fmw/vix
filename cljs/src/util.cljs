;; cljs/src/util.cljs: collection of client-side utility functions.
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

(ns vix.util
  (:require [goog.array :as goog.array]
            [goog.global :as global]
            [goog.dom :as dom]
            [goog.date :as date]
            [clojure.string :as string]))

(defn col-to-js [c]
  (cond
   (map? c) (map-to-obj c)
   (convertable-to-array? c) (convert-to-array c)
   :default c))

(defn map-to-obj [m]
  (when (map? m)
    (loop [keys (keys m) values (vals m) o (js-obj)]
      (if (pos? (count keys))
        (let [k (first keys)
              v (first values)
              key (if (keyword? k)
                    (name k)
                    k)
              value (cond
                     (map? v) (map-to-obj v)
                     (convertable-to-array? v) (convert-to-array v)
                     :default v)]
          (aset o key value)
          (recur (rest keys) (rest values) o))
        o))))

(defn convertable-to-array? [col]
  (or (vector? col) (seq? col) (set? col)))

(defn convert-to-array [col]
  (when (convertable-to-array? col)
    (loop [c (vec col) rv []] ; "todo" nodes c and result vector rv
      (if (pos? (count c))
        (recur
         (rest c)
         (conj ; add value to the result vector
          rv
          (cond ; handle value differently depending on what it is
           (keyword? (first c)) (name (first c))
           (map? (first c)) (map-to-obj (first c))
           (convertable-to-array? (first c)) (convert-to-array (first c))
           :default (first c))))
        (to-array rv))))) ; convert result vector to array when done

(defn pair-from-string [s]
  (let [matches (re-matches #"\['([a-z]+)','([a-zA-Z\-/ ]+)'\]" s)]
    (when matches
      (rest matches))))

(defn set-page-title! [title]
  (set! global/document.title title))

(defn has-consecutive-dashes-or-slashes? [slug]
  (if (re-find #"[\-/]{2,}" slug) true false))

(defn get-element [el-id-or-obj]
  (if (string? el-id-or-obj)
    (dom/getElement el-id-or-obj)
    el-id-or-obj))

(defn get-elements-by-class [class-name]
  (goog.array/toArray (dom/getElementsByClass class-name)))

(defn get-elements-by-tag-and-class [tag class-name]
  (goog.array/toArray (dom/getElementsByTagNameAndClass tag class-name)))

(defn get-children-by-class [el class-name]
  (goog.array/toArray (dom/getElementsByClass class-name el)))

(defn get-children [el]
  (goog.array/toArray (dom/getChildren el)))

(defn get-children-by-tag [el tag-name]
  (goog.array/toArray (. el (getElementsByTagName tag-name))))

(defn get-parent [el]
  (dom/getAncestor el #(identity true)))

(defn get-position-in-parent [el parent-el]
  (let [enumerated-children (map-indexed vector (get-children parent-el))]
    (ffirst (filter #(= el (last %)) enumerated-children))))

(defn get-distance-to-ancestor
  ([ancestor node]
     (get-distance-to-ancestor ancestor node 25))
  ([ancestor node max-distance]
     (loop [distance 0
            current-node node]
       (if (or (= current-node ancestor) (>= distance max-distance))
         (when (<= distance max-distance)
           distance)
         (recur (inc distance) (get-parent current-node))))))

(defn is-sibling? [el-x el-y]
  (= (get-parent el-x) (get-parent el-y)))

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