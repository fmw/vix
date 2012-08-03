;; src/vix/util.clj utility functions
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

(ns vix.util
  (:require [clj-time.core :as time-core]
            [clj-time.coerce :as time-coerce]
            [clj-time.format :as time-format]
            [clojure.string :as string]))

(defn read-int
  "Converts a numeric string to an integer or returns nil when the
   number isn't formatted as expected."
  [possible-integer]
  (when possible-integer
    (try
      (Integer/parseInt possible-integer)
      (catch java.lang.NumberFormatException e
        nil))))

(defn read-float
  "Converts a numeric string to a float or returns nil when the number
   format isn't recognized."
  [possible-float]
  (when possible-float
    (try
      (Float/parseFloat possible-float)
      (catch java.lang.NumberFormatException e
        nil))))

(defn force-initial-slash
  "Returns the slug string with a / prepended to it."
  [slug]
  (if (= (first slug) \/)
    slug
    (str "/" slug)))

(defn increment-slug
  "Takes the provided slug and returns it with either -2 appended to
   it if it doesn't end in a dash followed by a number (e.g. -3) or
   increments that number if it does."
  [slug]
  (if-let [slug-matches (re-matches #"(.*?)-([\d]+)$" slug)]
    (str (nth slug-matches 1)
         "-"
         (inc (Integer/parseInt (last slug-matches))))
    (str slug "-2")))

(defn now-rfc3339
  "Returns the RFC3339 current time (e.g. 2012-07-09T09:31:01.579Z)."
  []
  (time-format/unparse
    (time-format/formatters :date-time)
    (time-core/now)))

(defn rfc3339-to-long
  "Converts RFC3339 formatted date string to microseconds since UNIX epoch.
   Throws NullPointerException on incorrect input."
  [date-string]
  (when (and (string? date-string) (not (= date-string "")))
    (time-coerce/to-long (time-format/parse date-string))))

(defn rfc3339-to-jodatime
  "Converts a RFC3339 formatted date string into an org.joda.time.DateTime
   object for the provided timezone."
  [date-string timezone]
  (when (and (string? date-string) (not (= date-string "")))
    (time-core/to-time-zone
     (time-format/parse
      (time-format/formatters :date-time) date-string)
     (time-core/time-zone-for-id timezone))))

(defn editor-datetime-to-rfc3339
  "Accepts a date-string (in yyyy-MM-dd HH:mm format) and returns a
   RFC3339 timestamp converted from the given timezone."
  [date-string timezone]
  (when (and (string? date-string) (not (= date-string "")))
    (time-format/unparse
     (time-format/formatters :date-time)
     (time-core/from-time-zone
      (time-format/parse 
       (time-format/formatter "yyyy-MM-dd HH:mm") date-string)
      (time-core/time-zone-for-id timezone)))))

(defn parse-accept-language-header
  "Returns a lowercase sequence of language tags for the provided
   accept-language string value, sorted by quality."
  [header]
  (when header
    (map first
         (sort-by second
                  >
                  (map (fn [[a tag c weight]]
                         [tag
                          (if weight
                            (java.lang.Double/parseDouble weight)
                            1.0)])
                       (re-seq #"([a-z\-]+)(\;q=(\d+\.\d+)){0,1}"
                               (string/lower-case header)))))))

(defn get-preferred-language
  "Returns the preferred language from the given sorted
   preferred-languages sequence and the available-languages, or nil if
   there is no match."
  [preferred-languages available-languages]
  (first (filter #(some #{%} available-languages) preferred-languages)))