;; src/vix/util.clj utility functions
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
  (:require [couchdb [client :as couchdb]]
            [clj-time.core :as time-core]
            [clj-time.coerce :as time-coerce]
            [clj-time.format :as time-format]))

(defn read-int [possible-integer]
  "Converts a numeric string to an integer or returns nil when
   the number isn't formatted as expected."
  (when possible-integer
    (try
      (Integer/parseInt possible-integer)
      (catch java.lang.NumberFormatException e
        nil))))

(defn read-float [possible-float]
  "Converts a numeric string to a float or returns nil when the
   number format isn't recognized."
  (when possible-float
    (try
      (Float/parseFloat possible-float)
      (catch java.lang.NumberFormatException e
        nil))))

(defn force-initial-slash [slug]
  (if (= (first slug) \/)
    slug
    (str "/" slug)))

(defn increment-slug [slug]
  (if-let [slug-matches (re-matches #"(.*?)-([\d]+)$" slug)]
    (str (nth slug-matches 1)
         "-"
         (inc (Integer/parseInt (last slug-matches))))
    (str slug "-2")))

(defn now-rfc3339 []
  (time-format/unparse
    (time-format/formatters :date-time)
    (time-core/now)))

(defn rfc3339-to-long
  [s]
  "Converts RFC3339 formatted date string to microseconds since UNIX epoch.
   Throws NullPointerException on incorrect input."
  (when (string? s)
    (time-coerce/to-long (time-format/parse s))))

(defn rfc3339-to-jodatime [date-string timezone]
  "Converts a RFC3339 formatted date string into an org.joda.time.DateTime
   object for the provided timezone."
  (time-core/to-time-zone
    (time-format/parse
      (time-format/formatters :date-time) date-string)
    (time-core/time-zone-for-id timezone)))

(defn editor-datetime-to-rfc3339 [date-string timezone]
  (when (not (= date-string ""))
    (time-format/unparse
     (time-format/formatters :date-time)
     (time-core/from-time-zone
      (time-format/parse 
       (time-format/formatter "yyyy-MM-dd HH:mm") date-string)
      (time-core/time-zone-for-id timezone)))))

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