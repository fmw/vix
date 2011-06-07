; test/vix/test/core.clj tests for core namespace.
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

(ns vix.test.core
  (:use [vix.core] :reload)
  (:use [clojure.test]))

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
