;; cljs/macros/vix/macros.clj: Useful macros for ClojureScript code.
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

(ns vix.crossover.macros
  (require [net.cgrand.enlive-html :as html]
           [vix.config :as config]))

(comment
  (deftest test-routes
    (is (= (routes "/admin/blog/edit"
                   ["admin" "foo"] :unmatched
                   ["admin" :feed "edit"] (identity (:feed params))
                   ["admin" "bar"] :unmatched)
           "blog"))

    (is (= (routes "/admin/blog/edit/foo"
                   ["admin" "foo"] :unmatched
                   ["admin" :feed "edit"] (identity (:feed params))
                   ["admin" "bar"] :unmatched
                   :fallback :fallen-back)
           :fallen-back))))

(defmacro routes [uri-path & clauses]
  (when-let [[route-segments body & clauses] clauses]
    `(if-let [~'params (vix.core/route-matches ~route-segments ~uri-path)]
      ~body
      (routes ~uri-path ~@clauses))))

(defmacro defhandler
  "defn wrapper that calls (events/prevent-default evt) before
   executing the function body."
  [fname & args]
  (let [docstring (when (string? (first args))
                    (first args))
        evt (if (string? (first args))
              (second args)
              (first args))
        body (if (string? (first args))
               (drop 2 args)
               (rest args))]
    `(defn ~fname ~docstring ~evt
       (do
         (~(symbol (str "events/prevent-default")) ~(symbol (str "evt")))
         (~(symbol (str "events/stop-propagation")) ~(symbol (str "evt"))))
       ~@body)))

(defn read-snippet
  "Extracts the snippet with given identifier from the provided enlive
   resource and returns it as a string. If the identifier starts with
   a dot it is used as-is (as a class value), otherwise it is assumed
   to be an id attribute value and prepended by a #-character."
  [resource identifier]
  (apply str
         (html/emit*
          (html/select resource
                       [(let [name- (name identifier)]
                          (if (= (first name- ) \.)
                            identifier
                            (keyword (str "#" name-))))]))))

(defn read-snippets
  "Returns a map with the extract snippets from the provided filename
   using the given ids sequence as snippet id values, prefixed by the
   provided ns (resulting in keys like :feed/add-feed-button)."
  [ns filename ids]
  (zipmap (map (fn [id]
                 (keyword (str ns "/" (name id))))
               ids)
          (map (partial read-snippet (html/html-resource filename)) ids)))

(defmacro get-snippets
  "Expands to a map of HTML snippets for the feed UI."
  []
  (merge
   (read-snippets "feed"
                  "templates/cljs/feed-overview.html"
                  [:.feed-list-for-language-container
                   :.feed-list-row
                   :add-feed-button
                   :add-document-button
                   :document-list
                   :.document-list-row
                   :add-document
                   :feed-name-row
                   :feed-title-row
                   :feed-subtitle-row
                   :feed-default-slug-format-row
                   :feed-custom-slug-format-row
                   :feed-default-document-type-row
                   :feed-searchable-row
                   :feed-language-row
                   :save-feed])
   (read-snippets "ui"
                  "templates/cljs/ui.html"
                  [:status-message
                   :caption
                   :date-widget
                   :datepicker-time-row])
   (read-snippets "editor"
                  "templates/cljs/editor.html"
                  [:back-to-overview-link
                   :image-drop-target
                   :image-information-container
                   :title-row
                   :subtitle-row
                   :slug-row
                   :start-time-row
                   :end-time-row
                   :icon-container
                   :document-relations
                   :image-relations
                   :editor-images
                   :description-container
                   :content-container
                   :save-button-container
                   :.feed-select-option
                   :.document-select-option
                   :.related-page
                   :.related-image
                   :add-related-page-dialog-form
                   :add-image-dialog-form
                   :.image-icon-container
                   :image-preview-in-dialog-container
                   :menu-builder
                   :.top-level-menu-item
                   :.nested-menu-item
                   :.nested-menu-category
                   :.item-details
                   :.add-item-node
                   :.add-sub-item
                   :add-menu-item-container
                   :menu-instructional-paragraph
                   :add-menu-item-dialog-form])))

(defmacro get-cdn-hostname
  "Expands to the CDN hostname."
  []
  config/cdn-hostname)