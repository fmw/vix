;; cljs/src/util.cljs: collection of client-side utility functions.
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

(ns vix.util
  (:require [clojure.string :as string]
            [domina :as domina]
            [goog.array :as goog.array]
            [goog.dom :as dom]
            [goog.date :as date]
            [goog.events :as events]
            [goog.history.EventType :as event-type]
            [goog.history.Html5History :as Html5History]
            [goog.Uri :as Uri]))

(defn convertable-to-array? [col]
  (or (vector? col) (seq? col) (set? col)))

(defn convert-to-array [col]
  (when (convertable-to-array? col)
    (loop [c (vec col) rv []]    ; "todo" nodes c and result vector rv
      (if (pos? (count c))
        (recur (rest c)
               (conj rv
                     (let [node (first c)]
                       (cond
                        (keyword? node) (name node)
                        (map? node) (map-to-obj node)
                        (convertable-to-array? node) (convert-to-array node)
                        :default node))))
        (to-array rv)))))   ; convert result vector to array when done

(defn map-to-obj [m]
  (when (map? m)
    (loop [keys (keys m) values (vals m) o (js-obj)]
      (if (pos? (count keys))
        (let [k (first keys)
              v (first values)
              key (if (keyword? k)
                    (string/replace (name k) "-" "_")
                    k)
              value (cond
                     (map? v) (map-to-obj v)
                     (convertable-to-array? v) (convert-to-array v)
                     :default v)]
          (aset o key value)
          (recur (rest keys) (rest values) o))
        o))))

(defn col-to-js [c]
  (cond
   (map? c) (map-to-obj c)
   (convertable-to-array? c) (convert-to-array c)
   :default c))

(defn pair-from-string [s]
  (let [matches (re-matches #"\['([a-z]+)','([a-zA-Z0-9\-/ ]+)'\]" s)]
    (when matches
      (rest matches))))

(defn set-page-title! [title]
  (set! js/document.title title))

(defn has-consecutive-dashes-or-slashes? [slug]
  (if (re-find #"[\-/]{2,}" slug) true false))

(defn get-element
  "Accepts a string, Google Closure DOM node or a Domina node and
   returns a DOM element."
  [el-id-or-obj]
  (when-not (nil? el-id-or-obj)
    (if (string? el-id-or-obj)
      (dom/getElement el-id-or-obj)
      (if (not (nil? (.-DOCUMENT_NODE el-id-or-obj)))
        el-id-or-obj
        (domina/single-node el-id-or-obj)))))

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

(defn get-select-option-name-by-value [el value]
  (.-textContent
   (first
    (filter #(= (.-value %) value) (goog.array/toArray (.-options el))))))

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

(defn create-slug [slug-template title feed date extension]
  (loop [slug slug-template
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

(defn get-path [uri]
  (. (new goog.Uri uri false) (getPath)))

(defn get-internal-links! []
  (filter #(= (.substr (get-path (.-href %)) 0 7) "/admin/")
          (cljs.core.Vector/fromArray
           (get-children-by-tag js/document.body "a"))))

(def *h* (atom nil))

(defn start-history! [route-fn]
  (compare-and-set! *h* nil (new goog.history.Html5History))
  (.setUseFragment @*h* false)
  (.setPathPrefix @*h* "/admin/")
  (.setEnabled @*h* true)
  (events/listen @*h*
                 event-type/NAVIGATE
                 route-fn))

(defn navigate [token title]
  (. @*h* (setToken token title)))

(defn navigate-replace-state [token title]
  (. @*h* (replaceToken token title)))

; FIXME: figure out why only nested calls (e.g. in
; create-document-list-events and render-editor-template) work and
; replace with a centralized call
(defn xhrify-internal-links! [link-elements]
  (doseq [element link-elements]
    (events/listen element
                   "click"
                   (fn [e]
                     (. e (preventDefault))
                     (navigate (.substr (get-path (.-href (.-target e))) 7)
                               (.-title (.-target e)))))))