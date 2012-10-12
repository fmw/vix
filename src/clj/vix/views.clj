;; src/vix/views.clj template views
;; Copyright 2011-2012, Vixu.com, F.M. (Filip) de Waard <fmw@vixu.com>.
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

(ns vix.views
  (:use [slingshot.slingshot :only [try+]])
  (:require [net.cgrand.enlive-html :as html]
            [clj-time.core :as time-core]
            [clj-time.format :as time-format]
            [vix.util :as util]
            [vix.config :as config]))

(defmacro deftemplates
  "Defines multiple Enlive templates. The same as Enlive's deftemplate,
  but using a map with template sources as values instead of just a
  single source string."
  [name sources args & forms]
  `(defn ~name [key# & fn-args#]
     (let [templates# (zipmap (keys ~sources)
                              (map (fn [source#]
                                     (html/template source# ~args ~@forms))
                                   (vals ~sources)))]
       (try+
        (apply (get templates# key#) fn-args#)
        (catch java.lang.NullPointerException e# nil)))))

(defmacro defsnippets
  "Defines multiple Enlive templates. The same as Enlive's deftemplate,
  but using a map with template sources as values instead of just a
  single source string."
  [name sources selector args & forms]
  `(defn ~name [key# & fn-args#]
     (let [templates# (zipmap (keys ~sources)
                              (map (fn [source#]
                                     (html/snippet source#
                                                   ~selector
                                                   ~args
                                                   ~@forms))
                                   (vals ~sources)))]
       (try+
        (apply (get templates# key#) fn-args#)
       (catch java.lang.NullPointerException e# nil)))))

(html/deftemplate login-page-template "templates/en/login.html"
  [message]
  [:div#status-message] (html/content message))

(html/deftemplate admin-template "templates/en/admin.html"
  [ctxt]
  [:div#status-message] (html/content (:message ctxt)))

(deftemplates layout
  {:nl "templates/nl/layout.html"
   :en "templates/en/layout.html"}
  [language {:keys [menu title main background-image]}]
  [:#menu] (if menu
             (html/substitute menu)
             identity)
  [:title] (if title
             (html/content (str config/title-prefix title)) identity)
  [[:#search-form-container] [:form]]
  (html/set-attr :action (str "/" language "/search"))
  [:html]
  (html/set-attr :lang language)
  [:div#content-bar] (when main
                       (html/html-content
                        (apply str (drop 2 (butlast (butlast main)))))))

(defsnippets screenshot-model
  {:nl "templates/nl/frontpage.html"
   :en "templates/en/frontpage.html"}
  [[:.screenshot-container (html/nth-of-type 1)]]
  [{:keys [slug title]}]
  [[:img (html/nth-of-type 1)]]
  (if slug
    (html/do->
     (html/set-attr :src (str config/cdn-hostname slug))
     (html/set-attr :title title)
     (html/set-attr :alt title))
    (html/substitute nil))
  [[:h5 (html/nth-of-type 1)]]
  (html/content title))

(deftemplates frontpage
  {:nl "templates/nl/frontpage.html"
   :en "templates/en/frontpage.html"}
  [timezone
   language
   {:keys [frontpage]}]
  [:div#leader-text]
  (when (map? (:data frontpage))
    (html/html-content
     (:description (:data frontpage))))
  [:div#advantages-container]
  (when (map? (:data frontpage))
    (html/html-content
     (:content (:data frontpage))))
  [:.screenshot-container]
  (html/substitute nil)
  [:div#teaser-image-container]
  (when (pos? (count (:related-images (:data frontpage))))
    (html/prepend
     (map (partial screenshot-model (keyword language))
          (:related-images (:data frontpage))))))

(defn transform-menu [original]
  (if original
    (let [r (html/html-resource (java.io.StringReader. original))]
      (html/select
       (html/transform
        (html/transform
         (html/transform
          (html/transform
           (html/transform
            (html/transform
             r
             [:ul]
             (html/set-attr :id "menu"))
            [[:li]]
            (html/set-attr :class
                           "block menu-category"))
           [[:ul] [:ul] [:li]]
           (html/remove-attr :class))
          [[:ul] [:ul]]
          (html/do->
           (html/set-attr :class "sub-menu")
           (html/remove-attr :id)))
         [:a]
         (html/set-attr :class "menu-main-item"))
        [[:ul] [:ul] [:a]]
        (html/remove-attr :class))
       [:#menu]))))

(defn frontpage-view
  [language
   timezone
   segments]
  (layout
   (keyword language)
   language
   {:menu (transform-menu (:content (:data (:menu segments))))
    :main
    (frontpage (keyword language)
               timezone
               language
               segments)}))

(deftemplates content-page-model
  {:nl "templates/nl/content-page.html"
   :en "templates/en/content-page.html"}
  [{:keys [content description subtitle] :as document}]
  [:h1#page-title]
  (if (not-empty subtitle)
    (html/content subtitle)
    (html/substitute nil))
  [:div#leader-text]
  (when (not-empty description)
    (html/html-content description))
  [:div#content]
  (when (not-empty content)
    (html/html-content content)))

(defn page-view
  [language
   timezone
   document
   segments]
  (layout
   (keyword language)
   language
   {:title (:title document)
    :menu (transform-menu (:content (:data (:menu segments))))
    :main
    (content-page-model (keyword language)
                        document)}))

(defn make-pagination-uri
  [language
   query
   last-doc
   pp-after-doc-id
   pp-after-score
   after-doc-id
   after-score
   first-page?
   direction]
  (let [after-str (str "&after-doc-id="
                       (if (= direction :next)
                         (:doc-id
                          (:index
                           last-doc))
                         (last pp-after-doc-id))
                       "&after-score="
                       (if (= direction :next)
                         (:score
                          (:index
                           last-doc))
                         (last pp-after-score)))
        base-uri (str "/"
                      language
                      "/search?q="
                      query
                      (when-not (and (= direction :previous)
                                     (not (pos? (count pp-after-doc-id)))
                                     (not (pos? (count pp-after-score))))
                        after-str))
        pp-after-doc-id-str (when-not first-page?
                              (if (= direction :next)
                                (apply str
                                       (concat
                                        (when pp-after-doc-id
                                          (interleave
                                           (repeat "&pp-aid[]=")
                                           pp-after-doc-id))
                                        ["&pp-aid[]=" after-doc-id]))
                                (apply str
                                       (concat
                                        (when (> (count pp-after-doc-id) 1)
                                          (interleave
                                           (repeat "&pp-aid[]=")
                                           (butlast pp-after-doc-id)))))))
        pp-after-score-str (when-not first-page?
                             (if (=  direction :next)
                               (apply str
                                      (concat
                                       (when pp-after-score
                                         (interleave
                                          (repeat "&pp-as[]=")
                                          pp-after-score))
                                       ["&pp-as[]=" after-score]))
                               (apply str
                                      (concat
                                       (when (> (count pp-after-score) 1)
                                         (interleave
                                          (repeat "&pp-as[]=")
                                          (butlast pp-after-score)))))))]
    (when-not (and (= direction :previous)
                   (nil? after-doc-id)
                   (nil? after-score))
      (str base-uri pp-after-doc-id-str pp-after-score-str))))

(defsnippets search-result-model
  {:nl "templates/nl/search-results.html"
   :en "templates/en/search-results.html"}
  [[:ol#search-results] [:li]]
  [document]
  [:a] (html/do->
        (html/content (:title document))
        (html/set-attr :href (:slug document))))

(deftemplates search-results
  {:nl "templates/nl/search-results.html"
   :en "templates/en/search-results.html"}
  [language
   per-page
   result
   query
   pp-after-doc-id
   pp-after-score
   after-doc-id
   after-score
   has-next-page?
   first-page?
   segments]
  [:span#search-stats] (html/content
                        (cond           ; FIXME: i18n here!
                         (= (:total-hits result) 0)
                         (if (= language "nl")
                           "Geen resultaten voor zoekopdracht"
                           "No results for query")
                         (= (:total-hits result) 1)
                         (if (= language "nl")
                           "1 resultaat voor zoekopdracht"
                           "1 result for query")
                         :else
                         (str (:total-hits result)
                              (if (= language "nl")
                                " resultaten voor zoekopdracht"
                                " results for query"))))
  [:strong#search-stats-query] (html/content query)
  [:ol#search-results] (when (not (empty? (:docs result)))
                         (html/content
                          (map (partial search-result-model
                                        (keyword language))
                               (:docs result))))
  [:a#next-search-results-page] (when has-next-page?
                                  (html/set-attr
                                   :href
                                   (make-pagination-uri language
                                                        query
                                                        (last
                                                         (:docs result))
                                                        pp-after-doc-id
                                                        pp-after-score
                                                        after-doc-id
                                                        after-score
                                                        first-page?
                                                        :next)))
  [:a#previous-search-results-page] (when-not first-page?
                                      (html/set-attr
                                       :href
                                       (make-pagination-uri language
                                                            query
                                                            (last
                                                             (:docs result))
                                                            pp-after-doc-id
                                                            pp-after-score
                                                            after-doc-id
                                                            after-score
                                                            false
                                                            :previous)))
  [:ul#search-results-navigate] (when (or has-next-page?
                                          (not first-page?))
                                  identity))

(defn search-results-view
  [language
   per-page
   result
   query
   pp-after-doc-id
   pp-after-score
   after-doc-id
   after-score
   first-page?
   segments]
  (let [has-next-page? (if (= (count (:docs result)) (inc per-page))
                         true
                         false)]
    (layout (keyword language)
            language
            {:menu (transform-menu (:content (:data (:menu segments))))
             :title (if (= language "nl")
                      (str "Zoekresultaten voor '" query "'")
                      (str "Search Results for '" query "'"))
             :main (search-results (keyword language)
                                   language
                                   per-page
                                   (if has-next-page?
                                     (assoc result :docs
                                            (butlast (:docs result)))
                                     result)
                                   query
                                   pp-after-doc-id
                                   pp-after-score
                                   after-doc-id
                                   after-score
                                   has-next-page?
                                   first-page?
                                   segments)})))

(deftemplates newsletter-subscribed-template
  {:en "templates/en/newsletter-subscribed.html"
   :nl "templates/nl/newsletter-subscribed.html"}
  [language news-items]
  )

(defn newsletter-subscribed-view
  [language segments]
  (layout
   language
   (keyword language) ;;FIXME: add title
   {:menu (transform-menu (:content (:data (:menu segments))))
    :main (newsletter-subscribed-template (keyword language)
                                          language
                                          (:recent-news segments))}))

(deftemplates newsletter-already-subscribed-template
  {:en "templates/en/newsletter-already-subscribed.html"
   :nl "templates/nl/newsletter-already-subscribed.html"}
  [language news-items]
  )


(defn newsletter-already-subscribed-view
  [language segments]
  (layout
   (keyword language) ;;FIXME: add title
   language
   {:menu (transform-menu (:content (:data (:menu segments))))
    :main (newsletter-already-subscribed-template (keyword language)
                                                  language
                                                  (:recent-news segments))}))

(deftemplates newsletter-confirmed-template
  {:en "templates/en/newsletter-confirmed.html"
   :nl "templates/nl/newsletter-confirmed.html"}
  [language news-items])

(defn newsletter-confirmed-view
  [language segments]
  (layout
   (keyword language) ;;FIXME: add title
   language
   {:menu (transform-menu (:content (:data (:menu segments))))
    :main (newsletter-confirmed-template (keyword language)
                                         language
                                         (:recent-news segments))}))

(deftemplates newsletter-confirmation-failed-template
  {:en "templates/en/newsletter-confirmation-failed.html"
   :nl "templates/nl/newsletter-confirmation-failed.html"}
  [language news-items])


(defn newsletter-confirmation-failed-view
  [language segments]
  (layout
   (keyword language) ;;FIXME: add title
   language
   {:menu (transform-menu (:content (:data (:menu segments))))
    :main (newsletter-confirmation-failed-template (keyword language)
                                                   language
                                                   (:recent-news segments))}))