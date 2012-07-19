;; test/vix/test/util.clj tests for misc utility functions.
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

(ns vix.test.util
  (:use [vix.util] :reload)
  (:use [clojure.test])
  (:require [clj-time.core :as time-core]))

(deftest test-read-int
  (is (= (read-int "42") 42))
  (is (= (read-int "i42") nil)))

(deftest test-read-float
  (is (= (read-float "3.14") (float 3.14)))
  (is (= (read-float "f3.14") nil)))

(deftest test-force-initial-slash
  (is (= (force-initial-slash "/slug") "/slug")
      "don't change a slug that already starts with a slash")
  (is (= (force-initial-slash "slug") "/slug")
      "add a slash to a slug that doesn't start with one"))

(deftest test-increment-slug
  (is (= (increment-slug "/blog/foo") "/blog/foo-2"))
  (is (= (increment-slug "/blog/foo-2") "/blog/foo-3"))
  (is (= (increment-slug "/blog/foo-123456789") "/blog/foo-123456790")))

(deftest test-now-rfc3339 []
  (is (re-matches
        #"^[\d]{4}-[\d]{2}-[\d]{2}T[\d]{2}:[\d]{2}:[\d]{2}\.[\d]{1,4}Z"
        (now-rfc3339))))

(deftest test-rfc3339-to-long
  (is (= (rfc3339-to-long "2011-11-04T09:16:52.253Z")
         1320398212253))
  (is (= (rfc3339-to-long "2012-01-12T15:40:07+01:00")
         1326379207000))
  (is (= (rfc3339-to-long nil) nil))
  (is (= (rfc3339-to-long 10) nil)))

(deftest test-rfc3339-to-jodatime
  (let [datetime-obj (rfc3339-to-jodatime "2011-11-04T09:16:52.253Z"
                                          "Europe/Amsterdam")]
    (is (= (time-core/month datetime-obj) 11))
    (is (= (time-core/day datetime-obj) 4))
    (is (= (time-core/year datetime-obj) 2011))
    (is (= (time-core/hour datetime-obj) 10))
    (is (= (time-core/minute datetime-obj) 16))
    (is (= (time-core/sec datetime-obj) 52))
    (is (= (time-core/milli datetime-obj) 253))))

(deftest test-editor-datetime-to-rfc3339
  (is (= (editor-datetime-to-rfc3339 "2012-02-20 14:30" "Europe/Amsterdam")
         "2012-02-20T13:30:00.000Z"))

  (is (= (editor-datetime-to-rfc3339 "2012-01-01 00:00" "Europe/Amsterdam")
         "2011-12-31T23:00:00.000Z"))

  (is (= (editor-datetime-to-rfc3339 "2012-08-04 14:30" "Europe/Amsterdam")
         "2012-08-04T12:30:00.000Z"))

  (is (= (editor-datetime-to-rfc3339 "" "Europe/Amsterdam")
         nil)))

(deftest test-parse-accept-language-header
  (is (= (parse-accept-language-header "en-US,en;q=0.8")
         ["en-us" "en"]))

  (is (= (parse-accept-language-header "da, en-gb;q=0.8, en;q=0.7")
         ["da" "en-gb" "en"]))

  (is (= (parse-accept-language-header "da, en-gb;q=0.8, en;q=0.7, nl;q=0.9")
         ["da" "nl" "en-gb" "en"]))

  (is (= (parse-accept-language-header
          "da;q=0.9, en-gb;q=0.8, en;q=0.7, nl")
         ["nl" "da" "en-gb" "en"]))

  (is (= (parse-accept-language-header nil))
      nil))

(deftest test-get-preferred-language
  (is (= (get-preferred-language ["nl" "da" "en-gb" "en"]
                                 ["de" "nl" "da" "en-gb"]))
      "nl")
  
  (is (= (get-preferred-language ["nl"]
                                 ["de" "da" "en-gb"]))
      nil))