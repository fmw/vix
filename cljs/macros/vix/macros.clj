;; cljs/macros/vix/macros.clj: Useful macros for ClojureScript code.
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

(ns vix.macros)

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