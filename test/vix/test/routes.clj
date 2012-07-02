;; test/vix/test/routes.clj tests for routes namespace.
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

(ns vix.test.routes
  (:use [clojure.test]
        [vix.routes] :reload
        [vix.db :only [create-document create-feed get-document list-feeds]]
        [vix.auth :only [add-user]]
        [clojure.contrib.json :only [json-str read-json]]
        [vix.test.db :only [database-fixture +test-server+ +test-db+]])
  (:require [clj-time.format :as time-format]
            [clj-time.core :as time-core]
            [clojure.contrib [error-kit :as kit]]
            [net.cgrand.enlive-html :as html]
            [vix.auth :as auth]
            [vix.config :as config]
            [vix.util :as util]
            [vix.lucene :as lucene])
  (:import [org.apache.commons.codec.binary Base64]))

(def last-modified-pattern
  #"[A-Z]{1}[a-z]{2}, \d{1,2} [A-Z]{1}[a-z]{2} \d{4} \d{2}:\d{2}:\d{2} \+0000")

; Copied from Clojure 1.3.
; Also discussed in Joy of Clojure by Fogus & Houser, page 299.
(defn with-redefs-fn
  "Temporarily redefines Vars during a call to func.  Each val of
  binding-map will replace the root value of its key which must be
  a Var.  After func is called with no args, the root values of all
  the Vars will be set back to their old values.  These temporary
  changes will be visible in all threads.  Useful for mocking out
  functions during testing."
  {:added "1.3"}
  [binding-map func]
  (let [root-bind (fn [m]
                    (doseq [[a-var a-val] m]
                      (.bindRoot ^clojure.lang.Var a-var a-val)))
        old-vals (zipmap (keys binding-map)
                         (map deref (keys binding-map)))]
    (try
      (root-bind binding-map)
      (func)
      (finally
        (root-bind old-vals)))))

; Copied from Clojure 1.3.
; Also discussed in Joy of Clojure by Fogus & Houser, page 299.
(defmacro with-redefs
  "binding => var-symbol temp-value-expr

  Temporarily redefines Vars while executing the body.  The
  temp-value-exprs will be evaluated and each resulting value will
  replace in parallel the root value of its Var.  After the body is
  executed, the root values of all the Vars will be set back to their
  old values.  These temporary changes will be visible in all threads.
  Useful for mocking out functions during testing."
  {:added "1.3"}
  [bindings & body]
  `(with-redefs-fn ~(zipmap (map #(list `var %) (take-nth 2 bindings))
                            (take-nth 2 (next bindings)))
                    (fn [] ~@body)))


(defn request-map [method resource body params]
  {:request-method method
   :uri resource
   :body (when body (java.io.StringReader. body))
   :params params
   :server-name "localhost"})

; FIXME: refactor these four request functions to get rid of duplication
(defn request
  ([method resource my-routes]
     (request method resource "" my-routes {}))
  ([method resource body my-routes]
     (request method resource body my-routes {}))
  ([method resource body my-routes params]
   (my-routes (assoc (request-map method resource body params)
                     :session
                     {:username "someone"
                      :permissions {:* ["GET" "POST" "PUT" "DELETE"]}}))))

(defn unauthorized-request
  ([method resource my-routes]
   (unauthorized-request method resource "" my-routes))
  ([method resource body my-routes & params]
   (app (assoc (request-map method resource body params)
                     :session
                     {:username "nemo"
                      :permissions {:* ["GET" "POST" "PUT"]
                                    :blog []}}))))

(defn unauthenticated-request
  ([method resource my-routes]
   (unauthorized-request method resource "" my-routes))
  ([method resource body my-routes & params]
     (app (request-map method resource body params))))

(defn form-request [method resource my-routes form-params]
  (app (dissoc (assoc (request-map method resource nil nil)
                            :form-params
                            form-params)
               :body)))

(deftest test-reset-search-allowed-feeds!
  (do
    (create-feed +test-server+
                 +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :default-slug-format "/{document-title}"
                  :default-document-type "with-description"
                  :language "en"
                  :searchable true})
    (create-feed +test-server+
                 +test-db+
                 {:title "Images"
                  :subtitle "Vix Weblog!"
                  :name "images"
                  :default-slug-format "/{document-title}"
                  :default-document-type "with-description"
                  :language "en"
                  :searchable false}))

  (reset! *search-allowed-feeds* {})
  (reset-search-allowed-feeds! +test-server+ +test-db+)
  (is (= @*search-allowed-feeds* {"en" ["blog"]}))

  (create-feed +test-server+
               +test-db+
               {:title "News"
                :subtitle "Vix News!"
                :name "news"
                :default-slug-format "/{document-title}"
                :default-document-type "with-description"
                :language "en"
                :searchable true})

  (reset-search-allowed-feeds! +test-server+ +test-db+)
  (is (= @*search-allowed-feeds* {"en" ["news" "blog"]})))

(deftest test-reset-available-languages!
  (do
    (create-feed +test-server+
                 +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :default-slug-format "/{document-title}"
                  :default-document-type "with-description"
                  :language "en"
                  :searchable true}))

  (reset! *available-languages* {})
  (reset-available-languages! +test-server+ +test-db+)
  (is (= @*available-languages* ["en"]))

  (create-feed +test-server+
               +test-db+
               {:title "News"
                :subtitle "Vix News!"
                :name "news"
                :default-slug-format "/{document-title}"
                :default-document-type "with-description"
                :language "nl"
                :searchable true})

  (reset-available-languages! +test-server+ +test-db+)
  (is (= @*available-languages* ["en" "nl"])))

(deftest test-reset-index-reader!
  (let [ir @*index-reader*]
    (do
      (reset-index-reader!))

    (is (not (= ir @*index-reader*))))

  (do
    (compare-and-set! *index-reader* @*index-reader* nil)
    (is (= @*index-reader* nil))
    (reset-index-reader!))

  (is (= (class @*index-reader*)
         org.apache.lucene.index.ReadOnlyDirectoryReader)))

(deftest test-json-response
  (is (= (:status (json-response nil)) 404))

  ; FIXME: fix charset issues
  (is (= (json-response {:foo "bar"})
         {:status 200
          :headers {"Content-Type" "application/json; charset=UTF-8"}
          :body "{\"foo\":\"bar\"}"}))

  (is (= (json-response {:foo "bar"} :status 201)
         {:status 201
          :headers {"Content-Type" "application/json; charset=UTF-8"}
          :body "{\"foo\":\"bar\"}"})))

(deftest test-response
  (is (= (response "foo") {:status 200
                         :headers {"Content-Type"
                                   "text/html; charset=UTF-8"}
                         :body "foo"}))

  (is (= (:status (response nil) 404)))
  (is (= (:status (response "foo" :status 201) 201)))

  (is (= (get (:headers (response "foo" :content-type "image/png"))
              "Content-Type")
         "image/png")))

(deftest test-page-not-found-response
  (is (= (page-not-found-response)
         {:status 404
          :headers {"Content-Type" "text/html; charset=UTF-8"}
          :body "<h1>Page not found</h1>"})))

(deftest test-image-response
  (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                 "vQAsAAAAAAEAAQAAAgJEAQA7")
        white-pixel-doc (create-document +test-server+
                                         +test-db+
                                         "en"
                                         "images"
                                         "Europe/Amsterdam"
                                         {:attachment {:type "image/gif"
                                                       :data gif}
                                          :title "a single black pixel!"
                                          :slug "/images/white-pixel.gif"
                                          :content ""
                                          :draft false})
        no-image-doc (create-document +test-server+
                                      +test-db+
                                      "en"
                                      "images"
                                      "Europe/Amsterdam"
                                      {:title "not a single black pixel!"
                                       :slug "/images/not-a-white-pixel.gif"
                                       :content ""
                                       :draft false})
        wp-response (image-response +test-server+
                                    +test-db+
                                    white-pixel-doc)]
    
    (testing "test if a simple gif is returned correctly"
      (is (= (get wp-response "Last-Modified")
             (time-format/unparse (time-format/formatters :rfc822)
                                  (util/rfc3339-to-jodatime
                                   (:published white-pixel-doc)
                                   "UTC"))))

      (is (re-matches last-modified-pattern
                      (get wp-response "Last-Modified")))

      (is (= (:status wp-response) 200))

      (is (= (:headers wp-response)
             {"ETag" (:_rev white-pixel-doc)
              "Content-Type" "image/gif"}))
    
      (is (= (class (:body wp-response)) java.io.ByteArrayInputStream)))

    (testing "test if a non-image document is handled correctly"
      (is (= (image-response +test-server+
                             +test-db+
                             no-image-doc)
             (page-not-found-response))))))

(deftest test-get-segment-and-get-segments
  (let [slug-fn
        (fn [language]
          (str "/" language "/menu/menu"))
        menu-doc
        (create-document +test-server+
                         +test-db+
                         "en"
                         "menu"
                         "Europe/Amsterdam"
                         {:title "menu"
                          :slug "/en/menu/menu"
                          :content (str "<ul><li><a href=\""
                                        "/\">home</a></li></ul>")
                          :draft false})
        grote-zaal-doc
        (create-document +test-server+
                         +test-db+
                         "en"
                         "grote-zaal"
                         "Europe/Amsterdam"
                         {:title
                          "Tomasz Stańko Middelburg"
                          :slug
                          "/en/grote-zaal/stanko-middelburg"
                          :content
                          (str "The legendary Polish "
                               "trumpet player Stańko "
                               "will be playing in "
                               "Middelburg.")
                          :start-time
                          "2012-04-25 20:30"
                          :end-time
                          "2012-04-25 23:59"
                          :draft false})

        kleine-zaal-doc
        (create-document +test-server+
                         +test-db+
                         "en"
                         "kleine-zaal"
                         "Europe/Amsterdam"
                         {:title
                          "The Impossible Gentlemen"
                          :slug
                          (str "/en/kleine-zaal/"
                               "impossible-gentlemen")
                          :content
                          (str "Gwilym Simcock, "
                               "Mike Walker, "
                               "Adam Nussbaum, "
                               "Steve Swallow "
                               "will be playing "
                               "at the Bimhuis "
                               "in Amsterdam.")
                          :start-time
                          "2012-07-06 20:30"
                          :end-time
                          "2012-07-06 23:59"
                          :draft false})

        kabinet-doc
        (create-document +test-server+
                         +test-db+
                         "en"
                         "kabinet"
                         "Europe/Amsterdam"
                         {:title
                          "Yuri Honing"
                          :slug
                          "/en/kabinet/yuri-honing-tilburg"
                          :content
                          (str "VPRO/Boy Edgar prize winner "
                               "Yuri Honing will be playing at "
                               "the Paradox venue in Tilburg.")
                          :start-time
                          "2013-02-01 20:30"
                          :end-time
                          "2013-02-01 23:59"
                          :draft false})
        kabinet-doc-2 ;; dummy doc that should not be retrieved
        (create-document +test-server+
                         +test-db+
                         "en"
                         "kabinet"
                         "Europe/Amsterdam"
                         {:title
                          "Yuri Honing"
                          :slug
                          "/en/kabinet/yuri-honing-tilburg"
                          :content
                          (str "VPRO/Boy Edgar prize winner "
                               "Yuri Honing at "
                               "the Paradox venue in Tilburg.")
                          :start-time
                          "2012-02-01 20:30"
                          :end-time
                          "2012-02-01 23:59"
                          :draft false})
        news-1
        (create-document +test-server+
                         +test-db+
                         "en"
                         "news"
                         "Europe/Amsterdam"
                         {:title
                          "hey!"
                          :slug
                          "/en/news/hey"
                          :content
                          ""
                          :draft false})
        news-2
        (create-document +test-server+
                         +test-db+
                         "en"
                         "news"
                         "Europe/Amsterdam"
                         {:title
                          "you!"
                          :slug
                          "/en/news/you"
                          :content
                          ""
                          :draft false})
        frontpage-segments
        {:menu
         {:type :document
          :nodes :#menu
          :slug slug-fn}
         :primary-exposition
         {:type :most-recent-events
          :nodes :div#primary-exposition-block
          :feed "grote-zaal"
          :limit 1}
         :secondary-exposition
         {:type :most-recent-events
          :nodes :div#secondary-exposition-block
          :feed "kleine-zaal"
          :limit 1}
         :tertiary-exposition
         {:type :most-recent-events
          :nodes :div#tertiary-exposition-block
          :feed "kabinet"
          :limit 1}
         :news
         {:type :feed
          :nodes [:div#news-block-first :div#news-block-second]
          :feed "news"
          :limit 2}
         :background-image
         {:type :string
          :data (str "http://cdn0.baz.vixu.com/"
                     "/static/images/baz-content-bg.jpg")}}]
    
    (testing "test (get-segment ...)"
      (is (= (:content (:data (get-segment (:menu frontpage-segments)
                                           +test-server+
                                           +test-db+
                                           "en"
                                           "Europe/Amsterdam")))
             "<ul><li><a href=\"/\">home</a></li></ul>"))

      (is (= (get-segment (:primary-exposition frontpage-segments)
                          +test-server+
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             (assoc (:primary-exposition frontpage-segments)
               :data
               grote-zaal-doc)))

      (is (= (get-segment (:secondary-exposition frontpage-segments)
                          +test-server+
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             (assoc (:secondary-exposition frontpage-segments)
               :data
               kleine-zaal-doc)))

      (is (= (get-segment (:tertiary-exposition frontpage-segments)
                          +test-server+
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             (assoc (:tertiary-exposition frontpage-segments)
               :data
               kabinet-doc)))

      (is (= (get-segment (:news frontpage-segments)
                          +test-server+
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             (assoc (:news frontpage-segments)
               :data
               {:next nil
                :documents [news-2 news-1]})))

      (is (= (get-segment {:type :string
                           :data (str "http://cdn0.baz.vixu.com/"
                                      "/static/images/baz-content-bg.jpg")}
                          +test-server+
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             {:type :string
              :data (str "http://cdn0.baz.vixu.com/"
                         "/static/images/baz-content-bg.jpg")})))

    (testing "test (get-segments...)"
      (is (= (get-segments frontpage-segments
                           +test-server+
                           +test-db+
                           "en"
                           "Europe/Amsterdam")
             {:menu
              {:type :document
               :nodes :#menu
               :slug slug-fn
               :data menu-doc}
              :primary-exposition
              {:type :most-recent-events
               :nodes :div#primary-exposition-block
               :feed "grote-zaal"
               :limit 1
               :data grote-zaal-doc}
              :secondary-exposition
              {:type :most-recent-events
               :nodes :div#secondary-exposition-block
               :feed "kleine-zaal"
               :limit 1
               :data kleine-zaal-doc}
              :tertiary-exposition
              {:type :most-recent-events
               :nodes :div#tertiary-exposition-block
               :feed "kabinet"
               :limit 1
               :data kabinet-doc}
              :news
              {:type :feed
               :nodes [:div#news-block-first :div#news-block-second]
               :feed "news"
               :limit 2
               :data {:next nil
                      :documents [news-2 news-1]}}
              :background-image
              {:type :string
               :data (str "http://cdn0.baz.vixu.com/"
                          "/static/images/baz-content-bg.jpg")}})))))

(deftest test-get-frontpage-for-language!
  ;; this is tested in the tests for the view

  ;; if it returns a string the view is executed successfully
  (is (string? (first (get-frontpage-for-language! +test-server+
                                                +test-db+
                                                "nl"
                                                "Europe/Amsterdam")))))

(deftest test-get-cached-frontpage!
  (is (= @*frontpage-cache* {}))
  (let [empty-cache-fp (get-cached-frontpage! +test-server+
                                              +test-db+
                                              "nl"
                                              "Europe/Amsterdam")]
    (is (= @*frontpage-cache* {"nl" empty-cache-fp}))

    (do  ; insert fake string to make sure pages are served from cache
      (swap! *frontpage-cache* assoc "nl" "foo!"))

    (is (= (get-cached-frontpage! +test-server+
                                 +test-db+
                                 "nl"
                                 "Europe/Amsterdam")
           "foo!")))

  (reset-frontpage-cache! "nl"))

(deftest test-reset-frontpage-cache!
  (testing "test reset-frontpage-cache! on a single language cache"
    (swap! *frontpage-cache* assoc "nl" "foo!")
    (is (= @*frontpage-cache* {"nl" "foo!"}))
    (is (= (reset-frontpage-cache! "nl") {})))
  
  (testing "test reset-frontpage-cache! on a multiple language cache"
    (swap! *frontpage-cache* assoc "nl" "foo!")
    (swap! *frontpage-cache* assoc "en" "bar!")
    (is (= @*frontpage-cache* {"nl" "foo!" "en" "bar!"}))
    (is (= (reset-frontpage-cache! "nl") {"en" "bar!"}))))

(deftest test-reset-page-cache!
  (is (= @*page-cache* {}))
  (swap! *page-cache* assoc "/events/clojure-meetup.html" "hey!")
  (is (not (= @*page-cache* {})))
  (is (= (reset-page-cache!) {}))
  (is (= @*page-cache* {})))

(deftest test-get-cached-page!
  (is (= @*page-cache* {}))
  
  (testing "make sure images skip the cache"
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")]
      (do
        (create-document +test-server+
                         +test-db+
                         "en"
                         "images"
                         "Europe/Amsterdam"
                         {:attachment {:type "image/gif"
                                       :data gif}
                          :title "a single black pixel!"
                          :slug "/images/white-pixel.gif"
                          :content ""
                          :draft false}))

      (is (= (keys
              (get-cached-page! +test-server+
                                +test-db+
                                "/images/white-pixel.gif"
                                "Europe/Amsterdam"))
             ["Last-Modified" :status :headers :body]))

      ;; the cache should still be empty:
      (is (= @*page-cache* {}))))

  (testing "test with a regular page"
    (do
      (create-document +test-server+
                       +test-db+
                       "en"
                       "pages"
                       "Europe/Amsterdam"
                       {:title "hic sunt dracones!"
                        :slug "/pages/hic-sunt-dracones.html"
                        :content "<h3>Here be dragons!</h3>"
                        :draft false}))
    (is (= (keys
            (get-cached-page! +test-server+
                              +test-db+
                              "/pages/hic-sunt-dracones.html"
                              "Europe/Amsterdam"))
           [:status :headers :body]))

    (is (= (keys (get @*page-cache* "/pages/hic-sunt-dracones.html"))
           [:status :headers :body]))

    ;; make sure the page is really served from the cache
    (swap! *page-cache* assoc "/pages/hic-sunt-dracones.html" "hi!")
    (is (= (get-cached-page! +test-server+
                             +test-db+
                             "/pages/hic-sunt-dracones.html"
                             "Europe/Amsterdam")
           "hi!")))

  ;; TODO: make this test actually meaningful
  (testing "test with event page"
    (do
      (create-document +test-server+
                       +test-db+
                       "en"
                       "calendar"
                       "Europe/Amsterdam"
                       {:title "Clojure Meetup!"
                        :slug "/events/clojure-meetup.html"
                        :content "<h3>Here be dragons!</h3>"
                        :start-time "2012-05-16 18:45"
                        :start-time-rfc3339 "2012-05-16T18:45:00.000Z"
                        :end-time "2012-05-16 23:00"
                        :end-time-rfc3339 "2012-05-16T23:00:00.000Z"
                        :draft false}))
    (is (= (keys
            (get-cached-page! +test-server+
                              +test-db+
                              "/events/clojure-meetup.html"
                              "Europe/Amsterdam"))
           [:status :headers :body]))

    (is (= (keys (get @*page-cache* "/events/clojure-meetup.html"))
           [:status :headers :body]))

    ;; make sure the page is really served from the cache
    (swap! *page-cache* assoc "/events/clojure-meetup.html" "hello!")
    (is (= (get-cached-page! +test-server+
                             +test-db+
                             "/events/clojure-meetup.html"
                             "Europe/Amsterdam")
           "hello!")))

  ;; clean up
  (reset-page-cache!))

(deftest test-catch-all
  (is (= (:status (catch-all +test-server+
                             +test-db+
                             "/blog/bar"
                             "Europe/Amsterdam"))
         404))
  (is (= (:body (catch-all +test-server+
                           +test-db+
                           "/blog/bar"
                           "Europe/Amsterdam"))
         "<h1>Page not found</h1>"))
  (do
    (create-document +test-server+
                     +test-db+
                     "en"
                     "blog"
                     "Europe/Amsterdam"
                     {:title "foo"
                      :slug "/blog/bar"
                      :content "bar"
                      :draft false}))

  (is (= (:status (catch-all +test-server+
                             +test-db+
                             "/blog/bar"
                             "Europe/Amsterdam"))
         200))

  (testing "Test if attachments are handled correctly."
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")
          document (create-document
                    +test-server+
                    +test-db+
                    "en"
                    "images"
                    "Europe/Amsterdam"
                    {:attachment {:type "image/gif" :data gif}
                     :title "a single black pixel!"
                     :slug "/pixel.gif"
                     :content ""
                     :draft false})
          catch-all (catch-all +test-server+
                               +test-db+
                               "/pixel.gif"
                               "Europe/Amsterdam")]

      (is (= (get (:headers catch-all) "Content-Type") "image/gif"))
      (is (= (class (:body catch-all)) java.io.ByteArrayInputStream)))))

(deftest ^{:integration true} test-routes
  (do
    (create-feed +test-server+
                 +test-db+
                 {:title "Pages"
                  :subtitle "Test Pages"
                  :name "pages"
                  :default-slug-format "/{feed-name}/{document-title}"
                  :default-document-type "standard"
                  :language "en"
                  :searchable true})
    
    (create-document +test-server+
                     +test-db+
                     "en"
                     "blog"
                     "Europe/Amsterdam"
                     {:title "foo"
                      :slug "/blog/bar"
                      :language "en"
                      :content "bar"
                      :draft false}))

  (let [directory (lucene/create-directory :RAM)]
    (with-redefs [*search-allowed-feeds* (atom {"en" ["pages"]})
                  config/search-results-per-page 10
                  config/database +test-db+
                  lucene/directory directory]
      (dotimes [n 21]
        (lucene/add-documents-to-index!
         lucene/directory
         [(create-document +test-server+
                           +test-db+
                           "en"
                           "pages"
                           "Europe/Amsterdam"
                           {:title (str "doc " n)
                            :slug (str "/pages/doc-" n)
                            :language "en"
                            :content "bar"
                            :draft false})]))
      
      (with-redefs [*index-reader* (atom
                                    (lucene/create-index-reader directory))]
        (testing "test document pagination"
          (let [first-five (read-json
                            (:body (request :get
                                            "/json/en/pages/list-documents"
                                            nil
                                            main-routes
                                            {:limit "5"})))]
            (is (= (count (:documents first-five)) 5))

            (let [next-five (read-json
                             (:body (request :get
                                             "/json/en/pages/list-documents"
                                             nil
                                             main-routes
                                             {:limit "5"
                                              :startkey-published
                                              (:published
                                               (:next first-five))})))]
              (is (= (count (:documents next-five)) 5)))))

        (testing "test search page and search pagination"
          (let [first-page (html/html-resource
                            (java.io.StringReader.
                             (apply str
                                    (:body
                                     (request :get
                                              "/en/search"
                                              ""
                                              main-routes
                                              {:q "bar"})))))]

            (is (= (html/text
                    (first
                     (html/select
                      first-page
                      [:span#search-stats])))
                   "21 results for query"))

            (are [n expected-href]
                 (= (first
                     (html/attr-values
                      (nth (html/select first-page
                                        [[:ol#search-results] [:li] [:a]])
                           n)
                      :href))
                    expected-href)
                 0 "/pages/doc-0"
                 1 "/pages/doc-1"
                 2 "/pages/doc-2"
                 3 "/pages/doc-3"
                 4 "/pages/doc-4"
                 5 "/pages/doc-5"
                 6 "/pages/doc-6"
                 7 "/pages/doc-7"
                 8 "/pages/doc-8"
                 9 "/pages/doc-9")

            (is (= (html/select first-page
                                [:a#previous-search-results-page])
                   []))
          
            (is (= (first
                    (html/attr-values
                     (first
                      (html/select first-page
                                   [:a#next-search-results-page]))
                     :href))
                   "/en/search?q=bar&after-doc-id=9&after-score=0.47674")))
        
          (let [second-page (html/html-resource
                             (java.io.StringReader.
                              (apply str
                                     (:body
                                      (request :get
                                               "/en/search"
                                               ""
                                               main-routes
                                               {:q "bar"
                                                :after-doc-id "9"
                                                :after-score "0.47674"})))))]

            (is (= (html/text
                    (first
                     (html/select
                      second-page
                      [:span#search-stats])))
                   "21 results for query"))

            (are [n expected-href]
                 (= (first
                     (html/attr-values
                      (nth (html/select second-page
                                        [[:ol#search-results] [:li] [:a]])
                           n)
                      :href))
                    expected-href)
                 0 "/pages/doc-17"
                 1 "/pages/doc-18"
                 2 "/pages/doc-10"
                 3 "/pages/doc-11"
                 4 "/pages/doc-12"
                 5 "/pages/doc-13"
                 6 "/pages/doc-14"
                 7 "/pages/doc-15"
                 8 "/pages/doc-16"
                 9 "/pages/doc-19")

            (is (= (html/attr-values
                    (first
                     (html/select second-page
                                  [:a#previous-search-results-page]))
                    :href)
                                   
                   #{"/en/search?q=bar"}))
          
            (is (= (first
                    (html/attr-values
                     (first
                      (html/select second-page
                                   [:a#next-search-results-page]))
                     :href))
                   (str "/en/search?q=bar&after-doc-id=19&after-score=0.47674"
                        "&pp-aid[]=9&pp-as[]=0.47674"))))
        
          (let [third-page (html/html-resource
                            (java.io.StringReader.
                             (apply str
                                    (:body
                                     (request :get
                                              "/en/search"
                                              ""
                                              main-routes
                                              {:q "bar"
                                               :after-doc-id "19"
                                               :after-score "0.47674"
                                               :pp-aid ["9"]
                                               :pp-as ["0.47674"]})))))]

            (is (= (html/text
                    (first
                     (html/select
                      third-page
                      [:span#search-stats])))
                   "21 results for query"))

            (is (= (first
                    (html/attr-values
                     (first (html/select third-page
                                         [[:ol#search-results] [:li] [:a]]))
                     :href))
                   "/pages/doc-20"))

            (is (= (html/attr-values
                    (first
                     (html/select third-page
                                  [:a#previous-search-results-page]))
                    :href)
                                   
                   #{"/en/search?q=bar&after-doc-id=9&after-score=0.47674"}))
          
            (is (=  (html/select third-page [:a#next-search-results-page])
                    []))))
    
                                        ;(is (= (:status (request :get "/" main-routes)) 200))
        (is (= (:status (request :get "/login" main-routes)) 200))
        (is (= (:status (request :get "/logout" main-routes)) 302))
        (is (= (:status (request :get "/admin" main-routes)) 200))
        (is (= (:status (request :get
                                 "/json/en/blog/list-documents"
                                 main-routes))
               200))

        (is (= (:status (request
                         :post
                         "/json/en/blog/new"
                         (json-str {:title "test-create"
                                    :slug "/blog/test"
                                    :content "hic sunt dracones"})
                         main-routes))
               201))

        (testing "test if the document is added to the database"
          (let [document (get-document +test-server+ +test-db+ "/blog/test")]
            (is (= (:title document)) "test-create")))

        (testing "test if the document is added to the lucene index"
          (let [reader (lucene/create-index-reader directory)]
            (is (= (.get (lucene/get-doc reader 21) "title") "test-create"))))

        (is (= (:status (request :get "/json/document/blog/bar" main-routes))
               200))
        (is (= (:status (request :get "/json/document/blog/t3" main-routes))
               404))
    
        ;; FIXME: should add a test-case for a 409 conflict
        (testing "test if documents are updated correctly"
          (let [document (get-document +test-server+ +test-db+ "/blog/bar")]
            (is (= (:status (request
                             :put
                             "/json/document/blog/bar"
                             (json-str (assoc document :title "hi!"))
                             main-routes))
                   200))

            (is (= (:title (get-document +test-server+ +test-db+ "/blog/bar"))
                   "hi!"))

            (is (= (:status (request
                             :put
                             "/json/document/blog/doesnt-exist"
                             (json-str (assoc document :title "hi!"))
                             main-routes))
                   404))))

        (testing "test if documents are also updated in the lucene index"
          (let [reader (lucene/create-index-reader directory)]
            (is (= (.get (lucene/get-doc reader 22) "title") "hi!"))))

        (testing "test if document is deleted from the database correctly"
          (is (= (:status
                  (request :delete "/json/document/blog/bar" main-routes))
                 200))
          (is (= (get-document +test-server+ +test-db+ "/blog/bar") nil)))

        (testing "test if document is also deleted from the lucene index."
          (let [reader (lucene/create-index-reader directory)
                analyzer (lucene/create-analyzer)
                filter (lucene/create-filter {:slug "/blog/bar"})
                result (lucene/search "hi" filter 15 reader analyzer)
                docs (lucene/get-docs reader (:docs result))]
            (is (= (:total-hits result) 0)))))

      (is (= (:status (request :get "/static/none" main-routes)) 404))
      (is (= (:body (request :get "/static/none" main-routes))
             "<h1>Page not found</h1>"))
    
      (is (= (:status (request :get "/blog/bar" main-routes)) 404))
      (is (= (:body (request :get "/blog/bar" main-routes))
             "<h1>Page not found</h1>"))

      (is (= (:status (request :get "/blog/test" main-routes)) 200))
   
      (let [post-feed-request (request
                               :post
                               "/json/new-feed"
                               (json-str {:name "blog"
                                          :title "Vix Weblog"
                                          :language "en"
                                          :subtitle "Vix Weblog..."
                                          :default-slug-format
                                          "/{document-title}"
                                          :default-document-type "standard"
                                          :searchable true})
                               main-routes)]
        (is (= (:status post-feed-request) 201))

        (is (= @*search-allowed-feeds* {"en" ["pages" "blog"]})
            "Test if search-allowed-feeds is updated when feed is added")
        
        (let [image-feed (read-json
                          (:body
                           (request
                            :post
                            "/json/new-feed"
                            (json-str {:name "image"
                                       :title "Images"
                                       :language "en"
                                       :subtitle "Pictures."
                                       :default-slug-format
                                       "/static/{document-title}.{ext}"
                                       :default-document-type "image"})
                            main-routes)))
              all-feeds (read-json
                         (:body
                          (request :get "/json/list-feeds" main-routes)))
              image-feed-nl (read-json
                             (:body
                              (request
                               :post
                               "/json/new-feed"
                               (json-str {:name "image"
                                          :title "Images"
                                          :language "nl"
                                          :subtitle "Pictures."
                                          :default-slug-format
                                          "/static/{document-title}.{ext}"
                                          :default-document-type "image"})
                               main-routes)))]

          (is (= (count all-feeds) 3))

          (testing "test language argument for /json/list-feeds"
            (is (= (sort-by :name all-feeds)
                   (sort-by :name
                            (read-json
                             (:body
                              (request :get
                                       "/json/list-feeds"
                                       nil
                                       main-routes
                                       {:language "en"}))))))

            (is (= [image-feed-nl]
                   (read-json
                    (:body
                     (request
                      :get
                      "/json/list-feeds"
                      nil
                      main-routes
                      {:default-document-type "image"
                       :language "nl"}))))))
          
          (is (= (count (read-json
                         (:body (request :get
                                         "/json/list-feeds"
                                         main-routes))))
                 4))
          
          (is (= [image-feed-nl image-feed]
                 (read-json
                  (:body (request
                          :get
                          "/json/list-feeds"
                          nil
                          main-routes
                          {:default-document-type
                           "image"})))))))

      (let [get-feed-request (request :get "/json/feed/en/blog" main-routes)
            json-body (read-json (:body get-feed-request))]
        (is (= (:status get-feed-request) 200))
        (is (= (:name json-body) "blog"))
        (is (= (:title json-body) "Vix Weblog"))

        (let [put-feed-request (request :put
                                        "/json/feed/en/blog"
                                        (json-str (assoc json-body
                                                    :title "Vix!"
                                                    :searchable false))
                                        main-routes)
              json-put-body (read-json (:body put-feed-request))]
          (is (= (:status put-feed-request) 200))
          (is (= (:name json-put-body) "blog"))
          (is (= (:title json-put-body) "Vix!"))
          
          (is (= @*search-allowed-feeds* {"nl" [] "en" ["pages"]})
              "Make sure search-allowed-feeds is updated when feeds are")))
    
      (is (:status (request :get "/json/feed/en/blog" main-routes)) 200)
      (is (:status (request :delete "/json/feed/en/blog" main-routes)) 200)
      (is (:status (request :get "/json/feed/en/blog" main-routes)) 404))))

(deftest ^{:integration true} test-routes-authorization
  (do
    (create-document +test-server+
                     +test-db+
                     "en"
                     "blog"
                     "Europe/Amsterdam"
                     {:title "foo"
                      :slug "/blog/test"
                      :content "bar"
                      :draft false})
    
    (create-feed +test-server+
                 +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :default-slug-format "/{feed-name}/{document-title}"
                  :default-document-type "with-description"
                  :language "en"}))

  (with-redefs [config/database +test-db+
                lucene/directory (lucene/create-directory :RAM)]
    (testing "Test if authorization is enforced correctly."
      (is (= (:status (unauthorized-request :get "/admin/" main-routes))
             302))
      (is (= (:status (unauthorized-request :get
                                            "/json/en/blog/list-documents"
                                            main-routes))
             302))
      (is (= (:status (unauthorized-request
                       :post
                       "/json/en/blog/new"
                       (json-str {:title "test-create"
                                  :slug "/blog/test"
                                  :content "hic sunt dracones"})

                       main-routes))
             302))
      (is (= (:status (unauthorized-request
                       :get
                       "/json/document/blog/test"
                       main-routes))
             302))
      (is (= (:status (unauthorized-request
                       :put
                       "/json/document/blog/test"
                       (json-str {:title "test-create"
                                  :slug "/blog/test"
                                  :content "hic sunt dracones"})

                       main-routes))
             302))
      (is (= (:status (unauthorized-request
                       :delete
                       "/json/document/blog/test"
                       main-routes))
             302))

      ;; feed
      (is (= (:status (unauthorized-request
                       :post
                       "/json/new-feed"
                       main-routes))
             302))
      
      (is (= (:status (unauthorized-request
                       :get
                       "/json/feed/en/blog"
                       main-routes))
             302))
      
      (is (= (:status (unauthorized-request
                       :put
                       "/json/feed/en/blog"
                       main-routes))
             302))

      (is (= (:status (unauthorized-request
                       :delete
                       "/json/feed/en/blog"
                       main-routes))
             302)))))

(deftest ^{:integration true} test-routes-authentication
  (do
    (create-document +test-server+
                     +test-db+
                     "en"
                     "blog"
                     "Europe/Amsterdam"
                     {:title "foo"
                      :slug "/blog/test"
                      :content "bar"
                      :draft false}))

  (testing "Test if authentication is enforced correctly."
    (with-redefs [config/database +test-db+
                  lucene/directory (lucene/create-directory :RAM)]
      (is (= (:status (unauthenticated-request :get "/admin" main-routes))
             302))
      (is (= (:status (unauthenticated-request :get
                                               "/json/en/blog/list-documents"
                                               main-routes))
             302))
      (is (= (:status (unauthenticated-request
                       :post
                       "/json/en/blog/new"
                       (json-str {:title "test-create"
                                  :slug "/blog/test"
                                  :content "hic sunt dracones"})

                       main-routes))
             302))
      (is (= (:status (unauthenticated-request
                       :get
                       "/json/document/blog/test"
                       main-routes))
             302))
      (is (= (:status (unauthenticated-request
                       :put
                       "/json/document/blog/test"
                       (json-str {:title "test-create"
                                  :slug "/blog/test"
                                  :content "hic sunt dracones"})

                       main-routes))
             302))
      (is (= (:status (unauthenticated-request
                       :delete
                       "/json/document/blog/test"
                       main-routes))
             302)))))

(deftest test-logout
  (is (= (logout {:username "johndoe"})
         {:session {}
          :status 302
          :headers {"Location" "/"}})
      "should empty the session and redirect to /"))

(deftest test-login
  (do
    (add-user +test-db+
              "fmw"
              "oops"
              {:* ["GET" "POST" "PUT" "DELETE"]}))

  (with-redefs [config/database +test-db+]
    (is (= (form-request :post "/login" main-routes {"username" "fmw"
                                                     "password" "foo"})
           {:status 302
            :headers {"Expires" "Mon, 26 Mar 2012 09:00:00 GMT"
                      "Location" "/login"}
            :body ""}))

    (let [r (form-request :post "/login" main-routes {"username" "fmw"
                                                      "password" "oops"})]
      (is (= ((:headers r) "Location") "/admin/"))
      (is (= (:status r) 302)))))

(deftest test-wrap-caching-headers
  (testing "test if regular pages are pre-expired"
    (is (= ((wrap-caching-headers identity) {})
           {:headers {"Expires" "Mon, 26 Mar 2012 09:00:00 GMT"}})))

  (with-redefs [time-core/now
                (fn []
                  (time-core/date-time 2012 4 22 16 04 57 525))]
    (testing "test if files are expired correclty"
      (are [content-type]
           (= ((wrap-caching-headers identity)
               {:headers {"Content-Type" content-type}})
              {:headers {"Expires" "Wed, 22 Apr 2015 16:04:57 +0000"
                         "Cache-Control" "public"
                         "Content-Type" content-type}})
           "image/png"
           "image/jpeg"
           "image/gif"
           "text/css"
           "text/javascript"))))

(deftest test-redirect-301
  (is (= (redirect-301 "/foo")
         {:status 301
          :body "Moved Permanently"
          :headers {"Location" "/foo"}})))

(deftest test-redirection-handler
  (with-redefs [config/default-host "localhost"
                config/base-uri "http://localhost:3000"
                config/cdn-hostname "http://localhost:3000/"]
    (is (= ((redirection-handler identity) {:server-name "localhost"})
           {:server-name "localhost"}))

    (testing "test if visitors are redirected to the default-host"
      (with-redefs [config/default-host "www.vixu.com"]
        (is (= ((redirection-handler identity) {:server-name "vixu.com"})
               {:status 301
                :body "Moved Permanently"
                :headers {"Location" "http://www.vixu.com"}}))))

    (testing "test if administrators are redirected to https://"
      (with-redefs [config/server-name "www.vixu.com"
                    config/default-host "www.vixu.com"]
        (is (= ((redirection-handler identity) {:server-name "www.vixu.com"
                                                :uri "/admin/"
                                                :scheme :http})
               {:status 301
                :body "Moved Permanently"
                :headers {"Location" "https://www.vixu.com/admin/"}}))

        ;; don't redirect if the scheme is already https
        (is (= ((redirection-handler identity) {:server-name "www.vixu.com"
                                                :uri "/admin/"
                                                :scheme :https})
               {:server-name "www.vixu.com" :uri "/admin/" :scheme :https}))))

    (testing "on localhost, /admin shouldn't redirect to https://"
      (with-redefs [config/default-host "localhost"]
        (is (= ((redirection-handler identity)
                {:server-name "localhost"
                 :uri "/admin/"
                 :scheme :http})
               {:server-name "localhost" :uri "/admin/" :scheme :http}))))

    (testing "test if custom redirects are correctly executed"
      (with-redefs [config/redirects {"/foo" "/bar"
                                      "/vixu" "http://www.vixu.com/"}]

        (is (= ((redirection-handler identity)
                {:server-name "localhost"
                 :uri "/foo"})
               {:status 301
                :body "Moved Permanently"
                :headers {"Location" "/bar"}}))

        (is (= ((redirection-handler identity)
                {:server-name "localhost"
                 :uri "/vixu"})
               {:status 301
                :body "Moved Permanently"
                :headers {"Location" "http://www.vixu.com/"}}))
      
        ;; do nothing unless the uri is a listed redirect
        (is (= ((redirection-handler identity)
                {:server-name "localhost"
                 :uri "/test"})
               {:server-name "localhost" :uri "/test"}))))))

(deftest test-handle-authentication-errors
  (is (= ((handle-authentication-errors identity) :works) :works))

  (is (= ((handle-authentication-errors
           (fn [handler]
             (kit/raise auth/InsufficientPrivileges)))
          :should-not-work)
         {:status 302
          :headers {"Location" "/permission-denied"}
          :body ""}))

  (is (= ((handle-authentication-errors
           (fn [handler]
             (kit/raise auth/AuthenticationRequired)))
          :should-not-work)
         {:status 302
          :headers {"Location" "/login"}
          :body ""})))

(defn test-ns-hook []
  (database-fixture test-reset-search-allowed-feeds!)
  (database-fixture test-reset-available-languages!)
  (test-reset-index-reader!)
  (test-json-response)
  (test-response)
  (test-page-not-found-response)
  (database-fixture test-image-response)
  (database-fixture test-get-segment-and-get-segments)
  (database-fixture test-get-frontpage-for-language!)
  (database-fixture test-get-cached-frontpage!)
  (database-fixture test-get-cached-page!)
  (test-reset-frontpage-cache!)
  (test-reset-page-cache!)
  (database-fixture test-catch-all)
  (database-fixture test-routes)
  (database-fixture test-routes-authorization)
  (database-fixture test-routes-authentication)
  (test-logout)
  (database-fixture test-login)
  (test-wrap-caching-headers)
  (test-redirect-301)
  (test-redirection-handler)
  (test-handle-authentication-errors))