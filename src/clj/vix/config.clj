;; src/vix/config.clj: configuraton namespace for Vix
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

(ns vix.config)

;; database
(def database "vix")

(def lucene-index-path "/var/lucene/vix")

;; host
(def server-name "localhost")
(def default-host "localhost:3000")
(def base-uri "http://localhost:3000/")
(def cdn-hostname "http://localhost:3000")

;; redirects, e.g. {"/vixu" "http://www.vixu.com"}
(def redirects {})

;; i18n
(def default-timezone "Europe/Amsterdam")
(def default-language "en")

;; search
(def search-results-per-page 10)

;; newsletter
(def from-email-address "fmw@vixu.com")

;; presentation
(def title-prefix "")

(defn get-menu-slug [language]
  (str "/" language "/menu/menu"))

(defn get-frontpage-slug [language]
  (str "/" language "/frontpage"))

(def page-segments
  {:frontpage
   {:menu
    {:type :document
     :nodes :#menu
     :slug get-menu-slug}
    :frontpage
    {:type :document
     :nodes :#content
     :slug get-frontpage-slug}}
   :search-page
   {:menu
    {:type :document
     :nodes :#menu
     :slug get-menu-slug}}
   :default-page
   {:menu
    {:type :document
     :nodes :#menu
     :slug get-menu-slug}}})