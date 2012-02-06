;; src/vix/views.clj template views
;; Copyright 2011-2012, F.M. (Filip) de Waard <fmw@vix.io>.
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
  (:require [net.cgrand.enlive-html :as html]
            [clj-time.core :as time-core]
            [vix.util :as util]))

(html/deftemplate login-page-template "templates/login.html"
  [message]
  [:div#status-message] (html/content message))

(html/deftemplate admin-template "templates/admin.html"
  [ctxt]
  [:div#status-message] (html/content (:message ctxt)))

(html/deftemplate layout "templates/layout.html"
  [{:keys [title main]}]
  [:title] (if title
             (html/content title) identity)
  [:#page-title] (when title
                   (html/content title))
  [:div#main-content] (when main
                        (html/substitute main)))

(html/defsnippet article-model "templates/blog-article.html"
  [[:article (html/nth-of-type 1)]]
  [{:keys [slug title content]} published include-title-link?]
  [:h4 :a] (when include-title-link?
             (html/do->
              (html/content title) (html/set-attr :href slug)))
  [:.month] (html/content (str (time-core/month published)))
  [:.day] (html/content (str (time-core/day published)))
  [:.year] (html/content (str (time-core/year published)))
  [:.hour] (html/content (str (time-core/hour published)))
  [:.minute] (html/content (str (time-core/minute published)))
  [:div.content] (html/html-content content))

(defn blog-frontpage-view [documents timezone]
  (layout {:main (map #(article-model %
                                      (util/rfc3339-to-jodatime
                                       (:published %) timezone)
                                      true)
                      (:documents documents))}))

(defn blog-article-view [document timezone]
  (layout {:main (article-model document
                                (util/rfc3339-to-jodatime
                                 (:published document) timezone)
                                false)
           :title (:title document)}))

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

(html/defsnippet search-result-model "templates/search-results.html"
  [[:ol#search-results] [:li]]
  [document]
  [:a] (html/do->
        (html/content (:title document))
        (html/set-attr :href (:slug document))))

(html/defsnippet search-results-snippet "templates/search-results.html"
  [:div#main-content]
  [language
   per-page
   result
   query
   pp-after-doc-id
   pp-after-score
   after-doc-id
   after-score
   has-next-page?
   first-page?]
  [:span#search-stats] (html/content (cond ;; FIXME: i18n here!
                                      (= (:total-hits result) 0)
                                      "No results for query"
                                      (= (:total-hits result) 1)
                                      "1 result for query"
                                      :else
                                      (str (:total-hits result)
                                           " results for query")))
  [:strong#search-stats-query] (html/content query)
  [:ol#search-results] (when (not (empty? (:docs result)))
                         (html/content
                          (map search-result-model (:docs result))))
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
   first-page?]
  (let [has-next-page? (if (= (count (:docs result)) (inc per-page))
                     true
                     false)]
    (layout {:title "Search Results"
             :main (search-results-snippet language
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
                                           first-page?)})))