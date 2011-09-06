; src/vix/views.clj template views
; Copyright 2011, F.M. (Filip) de Waard <fmw@vix.io>.
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.

(ns vix.views
  (:require [net.cgrand.enlive-html :as html]
            [clj-time.core]
            [clj-time.format]))

(defn parse-date [date-string]
  (clj-time.core/to-time-zone
    (clj-time.format/parse
      (clj-time.format/formatters :date-time) date-string)
    (clj-time.core/time-zone-for-id "Europe/Amsterdam")))

(def *article-sel* [[:article (html/nth-of-type 1)]])

(html/deftemplate login-page-template "templates/login.html"
  [message]
  [:div#status-message] (html/content message))

(html/deftemplate admin-template "templates/admin.html"
  [ctxt]
  [:div#status-message] (html/content (:message ctxt)))

(html/defsnippet article-model "templates/blog.html" *article-sel*
  [{:keys [slug title content published-]} published]
  [:h3 :a] (html/do->
             (html/content title) (html/set-attr :href slug))
  [:.month] (html/content (str (clj-time.core/month published)))
  [:.day] (html/content (str (clj-time.core/day published)))
  [:.year] (html/content (str (clj-time.core/year published)))
  [:.hour] (html/content (str (clj-time.core/hour published)))
  [:.minute] (html/content (str (clj-time.core/minute published)))
  [:div.content] (html/html-content content))

(html/deftemplate blog-frontpage-template "templates/blog.html"
  [documents]
  [:#main-page] (html/content
                  (map #(article-model % (parse-date (:published %)))
                       (:documents documents))))

(html/deftemplate blog-article-template "templates/blog.html"
  [document]
  [:#main-page] (html/content (article-model
                                document (parse-date (:published document)))))
