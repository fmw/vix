; src/vix/core.clj utility functions
;
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

(ns vix.core
  (:require [couchdb [client :as couchdb]]
            [clj-time.core]
            [clj-time.format]))

(defn force-initial-slash [slug]
  (if (= (first slug) \/)
    slug
    (str "/" slug)))

(defn increment-slug [slug]
  (if-let [slug-matches (re-matches #"(.*?)-([\d]+)$" slug)]
    (str (nth slug-matches 1) "-" (inc (Integer/parseInt (last slug-matches))))
    (str slug "-2")))

(defn now-rfc3339 []
  (clj-time.format/unparse
    (clj-time.format/formatters :date-time)
    (clj-time.core/now)))

(defn log-stream []
  (java.io.PrintStream. (java.io.ByteArrayOutputStream.) false))

(let [orig (atom nil)
      monitor (Object.)]
  
  (defn log-hide! []
    "Adapted from clojure.tools.logging/log-capture! (this version
     works around issues when switching namespaces)."
    (locking monitor
      (compare-and-set! orig nil [System/out System/err])
      (System/setErr (log-stream))
      (System/setOut (log-stream))))

    (defn log-restore!
    "Restores System.out and System.err to their original values.
     Copied from clojure.tools.logging/log-uncapture!"
    []
    (locking monitor
      (when-let [[out err :as v] @orig]
        (swap! orig (constantly nil))
        (System/setOut out)
        (System/setErr err)))))