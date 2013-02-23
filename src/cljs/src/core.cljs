;; cljs/src/core.cljs: core application and routing for client-side code.
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

(ns vix.core
  (:use-macros [vix.crossover.macros :only [routes]])
  (:require [vix.util :as util]
            [vix.ui :as ui]
            [vix.views.editor :as editor]
            [vix.views.feed :as feed]
            [clojure.browser.repl :as repl]
            [clojure.string :as string]
            [goog.userAgent :as useragent]))

(defn chop-path [path]
  (rest (string/split path #"/")))

(comment
  (deftest test-route-matches
    (are [segments path expected] (= (route-matches segments path) expected)
         ["admin" "edit" :x :y] "/admin/edit/foo/bar" {:x "foo" :y "bar"}
         [:feed :*] "/blog/hello/kitty" {:feed "blog" :* "hello/kitty"}
         :fallback "/foo/bar" true)

    (testing "these calls are expected to fail"
      (are [segments path] (= (route-matches segments path) false)
           [] "/admin/edit"
           ["admin" "edit" :foo] "/admin/edit/foo/bar"
           ["admin" "editor" :foo :bar] "/admin/edit/foo/bar"))))

(defn route-matches [route-segments uri-path]
  (if-not (= route-segments :fallback)
    (let [values (chop-path uri-path)]
      (if (and (>= (count values) (count route-segments))
               (or (= (count values) (count route-segments))
                   (= (last route-segments) :*)))
        (loop [segments route-segments
               values values
               result {}]
          (if (pos? (count segments))
            (let [segment (first segments)
                  value (first values)]
              (cond
               (keyword? segment) (if (= segment :*) ; catch-all
                                    (recur []
                                           []
                                           (assoc result
                                             :* (string/join "/" values)))
                                    (recur (rest segments)
                                           (rest values)
                                           (assoc result segment value)))
               (string? segment) (if (= segment value)
                                   (recur (rest segments)
                                          (rest values)
                                          result)
                                   (recur [] [] false))))
            result))
        false))
    true))

(defn execute-routes!
  [uri-path]
  (routes uri-path
          ["admin" ""]
          (feed/list-feeds!)
          ["admin" "new-feed"]
          (feed/display-feed-form!)
          ["admin" "edit-feed" :language :feed-name]
          (feed/display-edit-feed-form! (:language params)
                                        (:feed-name params))
          ["admin" :language :feed-name "new"]
          (editor/display-editor! (:language params)
                                  (:feed-name params))
          ["admin" :language :feed-name "edit" :*]
          (editor/display-editor! (:language params)
                                  (:feed-name params)
                                  (str "/" (:* params)))
          ["admin" :language :feed-name "overview"]
          (feed/list-documents! (:language params) (:feed-name params))
          :fallback
          (util/navigate-replace-state "" "Vix overview")))

(defn ^:export start-app
  [uri-path]
  ;; in Chrome this triggers an event, leading to a (routes) call
  (util/start-history! (fn [e]
                         (execute-routes! js/document.location.pathname)))
  ;; trigger routes manually for non-webkit browsers
  (when-not useragent/WEBKIT
    (execute-routes! uri-path)))

(defn ^:export repl-connect []
  (repl/connect "http://localhost:9000/repl"))