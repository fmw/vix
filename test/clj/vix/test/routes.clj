;; test/vix/test/routes.clj tests for routes namespace.
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

(ns vix.test.routes
  (:use [clojure.test]
        [ring.middleware params
                         keyword-params
                         nested-params
                         multipart-params
                         cookies
                         flash]
        [slingshot.slingshot :only [throw+]]
        [slingshot.test]
        [vix.routes] :reload
        [clojure.data.json :only [json-str read-json]]
        [vix.test.db :only [database-fixture +test-db+]]
        [vix.test.test])
  (:require [clj-time.format :as time-format]
            [clj-time.core :as time-core]
            [net.cgrand.enlive-html :as html]
            [com.ashafa.clutch :as clutch]
            [vix.db :as db]
            [vix.auth :as auth]
            [vix.config :as config]
            [vix.util :as util]
            [vix.lucene :as lucene])
  (:import [org.apache.commons.codec.binary Base64]))

(def last-modified-pattern
  #"[A-Z]{1}[a-z]{2}, \d{1,2} [A-Z]{1}[a-z]{2} \d{4} \d{2}:\d{2}:\d{2} \+0000")

(def mock-app
  "Mock app without session middleware, so session is preserved."
  (-> main-routes
      wrap-keyword-params
      wrap-nested-params
      wrap-params
      (wrap-multipart-params)
      (wrap-flash)
      (redirection-handler)
      (wrap-caching-headers)
      (handle-exceptions)))

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
     (mock-app (assoc (request-map method resource body params)
                 :session
                 {:username "someone"
                  :permissions {:* ["GET" "POST" "PUT" "DELETE"]}}))))

(defn unauthorized-request
  ([method resource my-routes]
     (unauthorized-request method resource "" my-routes))
  ([method resource body my-routes & params]
     (mock-app (assoc (request-map method resource body params)
                 :session
                 {:username "nemo"
                  :permissions {:blog []}}))))

(defn unauthenticated-request
  ([method resource my-routes]
     (unauthorized-request method resource "" my-routes))
  ([method resource body my-routes & params]
     (mock-app (request-map method resource body params))))

(defn form-request [method resource my-routes form-params]
  (app (dissoc (assoc (request-map method resource nil nil)
                            :form-params
                            form-params)
               :body)))

(deftest test-reset-search-allowed-feeds!
  (do
    (db/append-to-feed +test-db+
                       {:action :create
                        :title "Weblog"
                        :subtitle "Vix Weblog!"
                        :name "blog"
                        :default-slug-format "/{document-title}"
                        :default-document-type "with-description"
                        :language "en"
                        :searchable true})
    (db/append-to-feed +test-db+
                       {:action :create
                        :title "Images"
                        :subtitle "Vix Weblog!"
                        :name "images"
                        :default-slug-format "/{document-title}"
                        :default-document-type "with-description"
                        :language "en"
                        :searchable false}))

  (reset! search-allowed-feeds {})
  (reset-search-allowed-feeds! +test-db+)
  (is (= @search-allowed-feeds {"en" ["blog"]}))

  (db/append-to-feed +test-db+
                     {:action :create
                      :title "News"
                      :subtitle "Vix News!"
                      :name "news"
                      :default-slug-format "/{document-title}"
                      :default-document-type "with-description"
                      :language "en"
                      :searchable true})

  (reset-search-allowed-feeds! +test-db+)
  (is (= @search-allowed-feeds {"en" ["news" "blog"]})))

(deftest test-reset-available-languages!
  (do
    (db/append-to-feed +test-db+
                       {:action :create
                        :title "Weblog"
                        :subtitle "Vix Weblog!"
                        :name "blog"
                        :default-slug-format "/{document-title}"
                        :default-document-type "with-description"
                        :language "en"
                        :searchable true}))

  (reset! available-languages {})
  (reset-available-languages! +test-db+)
  (is (= @available-languages ["en"]))

  (db/append-to-feed +test-db+
                     {:action :create
                      :title "News"
                      :subtitle "Vix News!"
                      :name "news"
                      :default-slug-format "/{document-title}"
                      :default-document-type "with-description"
                      :language "nl"
                      :searchable true})

  (reset-available-languages! +test-db+)
  (is (= @available-languages ["en" "nl"])))

(deftest test-reset-index-reader!
  (let [ir @index-reader]
    (do
      (reset-index-reader!))

    (is (not (= ir @index-reader))))

  (do
    (compare-and-set! index-reader @index-reader nil)
    (is (= @index-reader nil))
    (reset-index-reader!))

  (is (= (class @index-reader)
         org.apache.lucene.index.ReadOnlyDirectoryReader)))

(deftest test-data-response
  (is (= (:status (data-response nil))
         (:status (data-response nil :type :json))
         (:status (data-response nil :type :clj)) 404))

  (is (= (data-response {:foo "bar"} :type :json)
         {:status 200
          :headers {"Content-Type" "application/json; charset=UTF-8"}
          :body "{\"foo\":\"bar\"}"}))

  (is (= (data-response {:foo "bar"})
         (data-response {:foo "bar"} :type :clj)
         {:status 200
          :headers {"Content-Type" "text/plain; charset=UTF-8"}
          :body "{:foo \"bar\"}"}))

  (is (= (data-response {:foo "bar"} :status 201 :type :json)
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
        white-pixel-doc (db/append-to-document
                         +test-db+
                         "Europe/Amsterdam"
                         {:action :create
                          :language "en"
                          :feed "images"
                          :attachment {:type "image/gif"
                                       :data gif}
                          :title "a single black pixel!"
                          :slug "/images/white-pixel.gif"
                          :content ""
                          :draft false})
        no-image-doc (db/append-to-document
                      +test-db+
                      "Europe/Amsterdam"
                      {:action :create
                       :language "en"
                       :feed "images"
                       :title "not a single black pixel!"
                       :slug "/images/not-a-white-pixel.gif"
                       :content ""
                       :draft false})
        wp-response (image-response +test-db+ white-pixel-doc)]

    (testing "test if a simple gif is returned correctly"
      (is (= (get-in wp-response [:headers "Last-Modified"])
             (time-format/unparse (time-format/formatters :rfc822)
                                  (util/rfc3339-to-jodatime
                                   (:published (first white-pixel-doc))
                                   "UTC"))))

      (is (re-matches last-modified-pattern
                      (get-in wp-response [:headers "Last-Modified"])))

      (is (= (:status wp-response) 200))

      (is (= (dissoc (:headers wp-response) "Last-Modified")
             {"ETag" (:_rev (first white-pixel-doc))
              "Content-Type" "image/gif"}))
    
      (is (= (class (:body wp-response))
             clj_http.core.proxy$java.io.FilterInputStream$0)))

    (testing "test if a non-image document is handled correctly"
      (is (= (image-response +test-db+ no-image-doc)
             (page-not-found-response))))))

(deftest test-get-segment-and-get-segments
  (let [slug-fn
        (fn [language]
          (str "/" language "/menu/menu"))
        menu-doc
        (db/append-to-document +test-db+
                               "Europe/Amsterdam"
                               {:action :create
                                :language "en"
                                :feed "menu"
                                :title "menu"
                                :slug "/en/menu/menu"
                                :content (str "<ul><li><a href=\""
                                              "/\">home</a></li></ul>")
                                :draft false})
        grote-zaal-doc
        (db/append-to-document +test-db+
                               "Europe/Amsterdam"
                               {:action
                                :create
                                :language
                                "en"
                                :feed
                                "grote-zaal"
                                :title
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
        (db/append-to-document +test-db+
                               "Europe/Amsterdam"
                               {:action
                                :create
                                :language
                                "en"
                                :feed
                                "kleine-zaal"
                                :title
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
        (db/append-to-document +test-db+
                               "Europe/Amsterdam"
                               {:action
                                :create
                                :language
                                "en"
                                :feed
                                "kabinet"
                                :title
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
        (db/append-to-document +test-db+
                               "Europe/Amsterdam"
                               {:action
                                :create
                                :language
                                "en"
                                :feed
                                "kabinet"
                                :title
                                "Yuri Honing"
                                :slug
                                "/en/kabinet/yuri-honing-tilburg-dummy"
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
        (db/append-to-document +test-db+
                               "Europe/Amsterdam"
                               {:action
                                :create
                                :language
                                "en"
                                :feed
                                "news"
                                :title
                                "hey!"
                                :slug
                                "/en/news/hey"
                                :content
                                ""
                                :draft false})
        news-2
        (db/append-to-document +test-db+
                               "Europe/Amsterdam"
                               {:action
                                :create
                                :language
                                "en"
                                :feed
                                "news"
                                :title
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
                                           +test-db+
                                           "en"
                                           "Europe/Amsterdam")))
             "<ul><li><a href=\"/\">home</a></li></ul>"))

      (is (= (get-segment (:primary-exposition frontpage-segments)
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             (assoc (:primary-exposition frontpage-segments)
               :data
               (first grote-zaal-doc))))

      (is (= (get-segment (:secondary-exposition frontpage-segments)
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             (assoc (:secondary-exposition frontpage-segments)
               :data
               (first kleine-zaal-doc))))

      (is (= (get-segment (:tertiary-exposition frontpage-segments)
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             (assoc (:tertiary-exposition frontpage-segments)
               :data
               (first kabinet-doc))))

      (is (= (get-segment (:news frontpage-segments)
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             (assoc (:news frontpage-segments)
               :data
               {:next nil
                :documents [(first news-2) (first news-1)]})))

      (is (= (get-segment {:type :string
                           :data (str "http://cdn0.baz.vixu.com/"
                                      "/static/images/baz-content-bg.jpg")}
                          +test-db+
                          "en"
                          "Europe/Amsterdam")
             {:type :string
              :data (str "http://cdn0.baz.vixu.com/"
                         "/static/images/baz-content-bg.jpg")})))

    (testing "test (get-segments...)"
      (is (= (get-segments frontpage-segments
                           +test-db+
                           "en"
                           "Europe/Amsterdam")
             {:menu
              {:type :document
               :nodes :#menu
               :slug slug-fn
               :data (first menu-doc)}
              :primary-exposition
              {:type :most-recent-events
               :nodes :div#primary-exposition-block
               :feed "grote-zaal"
               :limit 1
               :data (first grote-zaal-doc)}
              :secondary-exposition
              {:type :most-recent-events
               :nodes :div#secondary-exposition-block
               :feed "kleine-zaal"
               :limit 1
               :data (first kleine-zaal-doc)}
              :tertiary-exposition
              {:type :most-recent-events
               :nodes :div#tertiary-exposition-block
               :feed "kabinet"
               :limit 1
               :data (first kabinet-doc)}
              :news
              {:type :feed
               :nodes [:div#news-block-first :div#news-block-second]
               :feed "news"
               :limit 2
               :data {:next nil
                      :documents [(first news-2) (first news-1)]}}
              :background-image
              {:type :string
               :data (str "http://cdn0.baz.vixu.com/"
                          "/static/images/baz-content-bg.jpg")}})))))

(deftest test-get-frontpage-for-language!
  ;; this is tested in the tests for the view

  ;; if it returns a string the view is executed successfully
  (is (string? (first (get-frontpage-for-language! +test-db+
                                                   "nl"
                                                   "Europe/Amsterdam")))))

(deftest test-get-cached-frontpage!
  (is (= @frontpage-cache {}))
  (let [empty-cache-fp (get-cached-frontpage! +test-db+
                                              "nl"
                                              "Europe/Amsterdam")]
    (is (= @frontpage-cache {"nl" empty-cache-fp}))

    (do  ; insert fake string to make sure pages are served from cache
      (swap! frontpage-cache assoc "nl" "foo!"))

    (is (= (get-cached-frontpage! +test-db+
                                  "nl"
                                  "Europe/Amsterdam")
           "foo!")))

  (reset-frontpage-cache! "nl"))

(deftest test-reset-frontpage-cache!
  (testing "test reset-frontpage-cache! on a single language cache"
    (swap! frontpage-cache assoc "nl" "foo!")
    (is (= @frontpage-cache {"nl" "foo!"}))
    (is (= (reset-frontpage-cache! "nl") {})))
  
  (testing "test reset-frontpage-cache! on a multiple language cache"
    (swap! frontpage-cache assoc "nl" "foo!")
    (swap! frontpage-cache assoc "en" "bar!")
    (is (= @frontpage-cache {"nl" "foo!" "en" "bar!"}))
    (is (= (reset-frontpage-cache! "nl") {"en" "bar!"}))))

(deftest test-reset-page-cache!
  (is (= @page-cache {}))
  (swap! page-cache assoc "/events/clojure-meetup.html" "hey!")
  (is (not (= @page-cache {})))
  (is (= (reset-page-cache!) {}))
  (is (= @page-cache {})))

(deftest test-get-cached-page!
  (is (= @page-cache {}))
  
  (testing "make sure images skip the cache"
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")]
      (do
        (db/append-to-document +test-db+
                               "Europe/Amsterdam"
                               {:action :create
                                :language "en"
                                :feed "images"
                                :attachment {:type "image/gif"
                                             :data gif}
                                :title "a single black pixel!"
                                :slug "/images/white-pixel.gif"
                                :content ""
                                :draft false}))

      (is (= (keys
              (get-cached-page! +test-db+
                                "/images/white-pixel.gif"
                                "Europe/Amsterdam"))
             [:status :headers :body]))

      ;; the cache should still be empty:
      (is (= @page-cache {}))))

  (testing "test with a regular page"
    (do
      (db/append-to-document +test-db+
                             "Europe/Amsterdam"
                             {:action :create
                              :language "en"
                              :feed "pages"
                              :title "hic sunt dracones!"
                              :slug "/pages/hic-sunt-dracones.html"
                              :content "<h3>Here be dragons!</h3>"
                              :draft false}))
    (is (= (keys
            (get-cached-page! +test-db+
                              "/pages/hic-sunt-dracones.html"
                              "Europe/Amsterdam"))
           [:status :headers :body]))

    (is (= (keys (get @page-cache "/pages/hic-sunt-dracones.html"))
           [:status :headers :body]))

    ;; make sure the page is really served from the cache
    (swap! page-cache assoc "/pages/hic-sunt-dracones.html" "hi!")
    (is (= (get-cached-page! +test-db+
                             "/pages/hic-sunt-dracones.html"
                             "Europe/Amsterdam")
           "hi!")))

  ;; TODO: make this test actually meaningful
  (testing "test with event page"
    (do
      (db/append-to-document +test-db+
                             "Europe/Amsterdam"
                             {:action :create
                              :language "en"
                              :feed "calendar"
                              :title "Clojure Meetup!"
                              :slug "/events/clojure-meetup.html"
                              :content "<h3>Here be dragons!</h3>"
                              :start-time "2012-05-16 18:45"
                              :start-time-rfc3339 "2012-05-16T18:45:00.000Z"
                              :end-time "2012-05-16 23:00"
                              :end-time-rfc3339 "2012-05-16T23:00:00.000Z"
                              :draft false}))
    (is (= (keys
            (get-cached-page! +test-db+
                              "/events/clojure-meetup.html"
                              "Europe/Amsterdam"))
           [:status :headers :body]))

    (is (= (keys (get @page-cache "/events/clojure-meetup.html"))
           [:status :headers :body]))

    ;; make sure the page is really served from the cache
    (swap! page-cache assoc "/events/clojure-meetup.html" "hello!")
    (is (= (get-cached-page! +test-db+
                             "/events/clojure-meetup.html"
                             "Europe/Amsterdam")
           "hello!")))

  ;; clean up
  (reset-page-cache!)

  (is (= (:status (get-cached-page! +test-db+
                                    "/blog/bar"
                                    "Europe/Amsterdam"))
         404))
  
  (is (= (:body (get-cached-page! +test-db+
                                  "/blog/bar"
                                  "Europe/Amsterdam"))
         "<h1>Page not found</h1>"))
  
  (do
    (db/append-to-document +test-db+
                           "Europe/Amsterdam"
                           {:action :create
                            :language "en"
                            :feed "blog"
                            :title "foo"
                            :slug "/blog/bar"
                            :content "bar"
                            :draft false}))

  (is (= (:status (get-cached-page! +test-db+
                                    "/blog/bar"
                                    "Europe/Amsterdam"))
         200))

  (testing "Test if attachments are handled correctly."
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")
          document (db/append-to-document
                    +test-db+
                    "Europe/Amsterdam"
                    {:action :create
                     :language "en"
                     :feed "images"
                     :attachment {:type "image/gif" :data gif}
                     :title "a single black pixel!"
                     :slug "/pixel.gif"
                     :content ""
                     :draft false})
          image-response (get-cached-page! +test-db+
                                           "/pixel.gif"
                                           "Europe/Amsterdam")]

      (is (= (get (:headers image-response) "Content-Type") "image/gif"))
      (is (= (class (:body image-response))
             clj_http.core.proxy$java.io.FilterInputStream$0))))

  ;; clean up
  (reset-page-cache!))

(defn- remove-ids
  "Removes internal database identifiers from the provided state map."
  [{:keys [action] :as state}]
  (assoc (dissoc state :_rev :_id :previous-id)
    :action (keyword action)))

(defn- clean-body
  "Returns a new version of the response map, with database internals
   (i.e. :_id, :_rev and :previous-id) removed from the state sequence
   in the response body. The body is read through read-json if :json
   is passed as the value of the type argument and otherwise through
   read-string. If the response type is json, the :action value is
   turned into a keyword. If the response status is 400, the original
   response is returned."
  [type response]
  (if (= (:status response) 400)
    response
    (update-in response
               [:body]
               (fn [body]
                 (map remove-ids
                      (if (= type :json)
                        (read-json body)
                        (read-string body)))))))

(deftest test-feed-request
  (with-redefs [util/now-rfc3339 #(str "2012-07-19T15:09:16.253Z")
                config/database +test-db+]
    (let [blog-feed {:action :create
                     :created "2012-07-19T15:09:16.253Z"
                     :datestamp "2012-07-19T15:09:16.253Z"
                     :default-document-type "standard"
                     :default-slug-format (str "/{language}/{feed-name}"
                                               "/{document-title}")
                     :language "en"
                     :language-full "English"
                     :name "blog"
                     :searchable true
                     :subtitle ""
                     :title "Weblog"
                     :type "feed"}
          image-feed {:action :create
                      :created "2012-07-19T15:09:16.253Z"
                      :datestamp "2012-07-19T15:09:16.253Z"
                      :default-document-type "image"
                      :default-slug-format (str "/{language}/{feed-name}"
                                                "/{document-title}.{ext}")
                      :language "en"
                      :language-full "English"
                      :name "images"
                      :searchable false
                      :subtitle ""
                      :title "Images"
                      :type "feed"}]
      (testing "test if feeds are created correctly."
        ;; test json request
        (is (= (clean-body
                :json
                (feed-request +test-db+ :POST :json blog-feed "en" "blog"))
               {:status 201
                :headers {"Content-Type" "application/json; charset=UTF-8"}
                :body [blog-feed]}))


        ;; test clojure request
        (is (= (clean-body
                :clj
                (feed-request +test-db+ :POST :clj image-feed "en" "blog"))
               {:status 201
                :headers {"Content-Type" "text/plain; charset=UTF-8"}
                :body [image-feed]})))

      (testing "test if feeds are updated correctly."
        ;; test json request
        (is (= (clean-body
                :json
                (feed-request +test-db+
                              :PUT
                              :json
                              (assoc blog-feed
                                :action :update
                                :previous-id (:_id
                                              (first
                                               (db/get-feed +test-db+
                                                            "en"
                                                            "blog")))
                                :title "foo")
                              "en"
                              "blog"))
               {:status 200
                :headers {"Content-Type" "application/json; charset=UTF-8"}
                :body [(assoc blog-feed
                         :action :update
                         :title "foo"
                         :datestamp "2012-07-19T15:09:16.253Z")
                       blog-feed]}))

        ;; test clojure request
        (is (= (clean-body
                :clj
                (feed-request +test-db+
                              :PUT
                              :clj
                              (assoc image-feed
                                :action :update
                                :previous-id (:_id
                                              (first
                                               (db/get-feed +test-db+
                                                            "en"
                                                            "images")))
                                :title "foo")
                              "en"
                              "images"))
               {:status 200
                :headers {"Content-Type" "text/plain; charset=UTF-8"}
                :body [(assoc image-feed
                         :action :update
                         :title "foo"
                         :datestamp "2012-07-19T15:09:16.253Z")
                       image-feed]})))

      (testing "test if feeds are loaded correctly."
        ;; test json request
        (is (= (clean-body
                :json
                (feed-request +test-db+ :GET :json nil "en" "blog"))
               {:status 200
                :headers {"Content-Type" "application/json; charset=UTF-8"}
                :body [(assoc blog-feed
                         :action :update
                         :title "foo"
                         :datestamp "2012-07-19T15:09:16.253Z")
                       blog-feed]}))

        ;; test clojure request
        (is (= (clean-body
                :clj
                (feed-request +test-db+ :GET :clj nil "en" "images"))
               {:status 200
                :headers {"Content-Type" "text/plain; charset=UTF-8"}
                :body [(assoc image-feed
                         :action :update
                         :title "foo"
                         :datestamp "2012-07-19T15:09:16.253Z")
                       image-feed]})))

      (testing "test if feeds are deleted correctly."
        ;; test json request
        (let [current-blog-feed (db/get-feed +test-db+ "en" "blog")]
          (is (= (clean-body
                  :json
                  (feed-request +test-db+
                                :DELETE
                                :json
                                (assoc (first current-blog-feed)
                                  :action :delete
                                  :previous-id (:_id
                                                (first current-blog-feed)))
                                "en"
                                "blog"))
                 {:status 200
                  :headers {"Content-Type" "application/json; charset=UTF-8"}
                  :body (map remove-ids
                             (cons (assoc (first current-blog-feed)
                                     :action :delete
                                     :previous-id (:_id
                                                   (first
                                                    current-blog-feed)))
                                   current-blog-feed))})))

        ;; test clojure request
        (let [current-images-feed (db/get-feed +test-db+ "en" "images")]
          (is (= (clean-body
                  :clj
                  (feed-request +test-db+
                                :DELETE
                                :clj
                                (assoc (first current-images-feed)
                                  :action :delete
                                  :previous-id (:_id
                                                (first
                                                 current-images-feed)))
                                "en"
                                "images"))
                 {:status 200
                  :headers {"Content-Type" "text/plain; charset=UTF-8"}
                  :body (map remove-ids
                             (cons (assoc (first current-images-feed)
                                     :action :delete
                                     :previous-id (:_id
                                                   (first
                                                    current-images-feed)))
                                   current-images-feed))})))))))

(defn- clean-req
  "Parses the response :body and removes :_id and :_rev from states.
   Also turns each state :action value into a keyword if it concerns a
   JSON request. Accepts a response argument, as well as a type
   (i.e. :json or :clj)."
  [response type]
  (update-in response
             [:body]
             (fn [states]
               (map (fn [state]
                      (if (= type :clj)
                        (dissoc state :_rev :_id :previous-id)
                        (update-in (dissoc state :_rev :_id :previous-id)
                                   [:action] keyword)))
                    (if (= type :clj)
                      (read-string states)
                      (read-json states))))))

(deftest test-document-request
  (with-redefs [util/now-rfc3339 #(str "2012-07-19T15:09:16.253Z")
                config/database +test-db+]
    (let [json-doc {:action :create
                    :content "Hic sunt dracones."
                    :description "A nice map."
                    :draft false
                    :start-time ""
                    :start-time-rfc3339 nil
                    :end-time ""
                    :end-time-rfc3339 nil
                    :feed "blog"
                    :icon ""
                    :language "en"
                    :published "2012-07-19T15:09:16.253Z"
                    :datestamp "2012-07-19T15:09:16.253Z"
                    :created "2012-07-19T15:09:16.253Z"
                    :related-pages []
                    :related-images []
                    :slug "/en/blog/hsd"
                    :subtitle "Here be dragons"
                    :title "Hello, world!"
                    :type "document"}
          clj-doc {:action :create
                   :content "Hic sunt dracones."
                   :description "A nice map."
                   :draft false
                   :start-time ""
                   :start-time-rfc3339 nil
                   :end-time ""
                   :end-time-rfc3339 nil
                   :feed "blog"
                   :icon ""
                   :language "en"
                   :published "2012-07-19T15:09:16.253Z"
                   :datestamp "2012-07-19T15:09:16.253Z"
                   :created "2012-07-19T15:09:16.253Z"
                   :related-pages []
                   :related-images []
                   :slug "/en/blog/hsd-clj"
                   :subtitle "Here be dragons"
                   :title "Hello, world!"
                   :type "document"}]
      (testing "test if feeds are created correctly."
        ;; test json request
        (is (= (clean-req (document-request :POST :json json-doc) :json)
               {:status 201
                :headers {"Content-Type" "application/json; charset=UTF-8"}
                :body [json-doc]}))

        ;; test clojure request
        (is (= (clean-req (document-request :POST :clj clj-doc) :clj)
               {:status 201
                :headers {"Content-Type" "text/plain; charset=UTF-8"}
                :body [clj-doc]})))

      (testing "test if documents are updated correctly."
        ;; test json request
        (let [json-doc-fresh (db/get-document +test-db+ "/en/blog/hsd")]
          (is (= (clean-req
                  (document-request :PUT
                                    :json
                                    (assoc (first json-doc-fresh)
                                      :action
                                      :update
                                      :previous-id
                                      (:_id (first json-doc-fresh))
                                      :title
                                      "foo"))
                  :json)
                 {:status 200
                  :headers {"Content-Type" "application/json; charset=UTF-8"}
                  :body (map #(dissoc % :_rev :_id)
                             [(assoc (first json-doc-fresh)
                                :action :update
                                :title "foo")
                              (second
                               (db/get-document +test-db+
                                                "/en/blog/hsd"))])})))
        
        ;; test clojure request
        (let [clj-doc-fresh (db/get-document +test-db+ "/en/blog/hsd-clj")]
          (is (= (clean-req
                  (document-request :PUT
                                    :clj
                                    (assoc (first clj-doc-fresh)
                                      :action
                                      :update
                                      :previous-id
                                      (:_id (first clj-doc-fresh))
                                      :title
                                      "foo"))
                  :clj)
                 {:status 200
                  :headers {"Content-Type" "text/plain; charset=UTF-8"}
                  :body (map #(dissoc % :_rev :_id)
                             [(assoc (first clj-doc-fresh)
                                :action :update
                                :title "foo")
                              (second
                               (db/get-document +test-db+
                                                "/en/blog/hsd-clj"))])}))))

      (testing "test if feeds are loaded correctly."
        ;; test json request
        (let [existing-doc (db/get-document +test-db+ "/en/blog/hsd")]
          (is (= (document-request :GET :json existing-doc)
                 {:status 200
                  :headers {"Content-Type" "application/json; charset=UTF-8"}
                  :body (json-str existing-doc)})))

        ;; test clojure request
        (let [existing-doc (db/get-document +test-db+ "/en/blog/hsd-clj")]
          (is (= (document-request :GET :clj existing-doc)
                 {:status 200
                  :headers {"Content-Type" "text/plain; charset=UTF-8"}
                  :body (pr-str existing-doc)}))))

      (testing "test if feeds are deleted correctly."
        ;; test json request
        (let [existing-doc
              (first (db/get-document +test-db+ "/en/blog/hsd"))]
          (is (= (clean-req
                  (document-request :DELETE
                                    :json
                                    (assoc existing-doc
                                      :action :delete
                                      :previous-id (:_id existing-doc)))
                  :json)
                 {:status 200
                  :headers
                  {"Content-Type" "application/json; charset=UTF-8"}
                  :body
                  [{:subtitle "Here be dragons"
                    :slug "/en/blog/hsd"
                    :icon ""
                    :content "Hic sunt dracones."
                    :action :delete,
                    :related-images []
                    :language "en"
                    :title "foo"
                    :start-time-rfc3339 nil
                    :published "2012-07-19T15:09:16.253Z"
                    :datestamp "2012-07-19T15:09:16.253Z"
                    :created "2012-07-19T15:09:16.253Z"
                    :type "document"
                    :feed "blog"
                    :draft false,
                    :related-pages [],
                    :description "A nice map."
                    :end-time-rfc3339 nil
                    :start-time ""
                    :end-time ""}
                   {:subtitle "Here be dragons"
                    :slug "/en/blog/hsd"
                    :icon ""
                    :content "Hic sunt dracones."
                    :action :update,
                    :related-images []
                    :language "en"
                    :title "foo"
                    :start-time-rfc3339 nil,
                    :published "2012-07-19T15:09:16.253Z"
                    :datestamp "2012-07-19T15:09:16.253Z"
                    :created "2012-07-19T15:09:16.253Z"
                    :type "document"
                    :feed "blog"
                    :draft false,
                    :related-pages [],
                    :description "A nice map."
                    :end-time-rfc3339 nil
                    :start-time ""
                    :end-time ""}
                   {:subtitle "Here be dragons"
                    :slug "/en/blog/hsd"
                    :icon ""
                    :content "Hic sunt dracones."
                    :action :create,
                    :related-images []
                    :language "en"
                    :title "Hello, world!"
                    :start-time-rfc3339 nil
                    :published "2012-07-19T15:09:16.253Z"
                    :datestamp "2012-07-19T15:09:16.253Z"
                    :created "2012-07-19T15:09:16.253Z"
                    :type "document"
                    :feed "blog"
                    :draft false
                    :related-pages []
                    :description "A nice map."
                    :end-time-rfc3339 nil
                    :start-time ""
                    :end-time ""}]})))

        ;; test clojure request
        (let [existing-doc
              (first (db/get-document +test-db+ "/en/blog/hsd-clj"))]
          (is (= (clean-req
                  (document-request :DELETE
                                    :clj
                                    (assoc existing-doc
                                      :action :delete
                                      :previous-id (:_id existing-doc)))
                  :clj)
                 {:status 200
                  :headers {"Content-Type" "text/plain; charset=UTF-8"}
                  :body
                  [{:subtitle "Here be dragons"
                    :slug "/en/blog/hsd-clj"
                    :icon ""
                    :content "Hic sunt dracones."
                    :action :delete
                    :related-images []
                    :language "en"
                    :title "foo"
                    :start-time-rfc3339 nil
                    :published "2012-07-19T15:09:16.253Z"
                    :datestamp "2012-07-19T15:09:16.253Z"
                    :created "2012-07-19T15:09:16.253Z"
                    :type "document"
                    :feed "blog"
                    :draft false
                    :related-pages []
                    :description "A nice map."
                    :end-time-rfc3339 nil
                    :start-time ""
                    :end-time ""}
                   {:subtitle "Here be dragons"
                    :slug "/en/blog/hsd-clj"
                    :icon ""
                    :content "Hic sunt dracones."
                    :action :update
                    :related-images []
                    :language "en"
                    :title "foo"
                    :start-time-rfc3339 nil
                    :published "2012-07-19T15:09:16.253Z"
                    :datestamp "2012-07-19T15:09:16.253Z"
                    :created "2012-07-19T15:09:16.253Z"
                    :type "document"
                    :feed "blog"
                    :draft false
                    :related-pages []
                    :description "A nice map."
                    :end-time-rfc3339 nil
                    :start-time ""
                    :end-time ""}
                   {:subtitle "Here be dragons"
                    :slug "/en/blog/hsd-clj"
                    :icon ""
                    :content "Hic sunt dracones."
                    :action :create
                    :related-images []
                    :language "en"
                    :title "Hello, world!"
                    :start-time-rfc3339 nil
                    :published "2012-07-19T15:09:16.253Z"
                    :datestamp "2012-07-19T15:09:16.253Z"
                    :created "2012-07-19T15:09:16.253Z"
                    :type "document"
                    :feed "blog"
                    :draft false
                    :related-pages []
                    :description "A nice map."
                    :end-time-rfc3339 nil
                    :start-time ""
                    :end-time ""}]})))))))

(deftest ^{:integration true} test-routes
  (with-redefs [util/now-rfc3339 #(str "2012-09-22T04:07:05.756Z")]
    (do
      (db/append-to-feed +test-db+
                         {:action :create
                          :title "Pages"
                          :subtitle "Test Pages"
                          :name "pages"
                          :default-slug-format
                          "/{feed-name}/{document-title}"
                          :default-document-type "standard"
                          :language "en"
                          :searchable true})
    
      (db/append-to-document +test-db+
                             "Europe/Amsterdam"
                             {:action :create
                              :language "en"
                              :feed "blog"
                              :title "foo"
                              :slug "/blog/bar"
                              :content "bar"
                              :draft false}))

    (let [directory (lucene/create-directory :RAM)]
      (with-redefs [search-allowed-feeds (atom {"en" ["pages"]})
                    config/search-results-per-page 10
                    config/database +test-db+
                    lucene/directory directory]
        (with-redefs [util/now-rfc3339
                      (fn []
                        (time-format/unparse
                         (time-format/formatters :date-time)
                         (time-core/now)))]
          (dotimes [n 21]
            (lucene/add-documents-to-index!
             lucene/directory
             [(first
               (db/append-to-document +test-db+
                                      "Europe/Amsterdam"
                                      {:action :create
                                       :language "en"
                                       :feed "pages"
                                       :title (str "doc " n)
                                       :slug (str "/pages/doc-" n)
                                       :content "bar"
                                       :draft false}))])))
      
        (with-redefs [index-reader (atom
                                    (lucene/create-index-reader directory))]
          (testing "test document pagination"
            ;; test json request
            (let [first-five (read-json
                              (:body
                               (request :get
                                        "/_api/json/en/pages/_list-documents"
                                        nil
                                        main-routes
                                        {:limit "5"})))]
              (is (= (count (:documents first-five)) 5))

              (let [next-five (read-json
                               (:body
                                (request
                                 :get
                                 "/_api/json/en/pages/_list-documents"
                                 nil
                                 main-routes
                                 {:limit "5"
                                  :startkey-published
                                  (:published
                                   (:next first-five))})))]
                (is (= (count (:documents next-five)) 5))))

            ;; test clojure request
            (let [first-five (read-string
                              (:body
                               (request :get
                                        "/_api/clj/en/pages/_list-documents"
                                        nil
                                        main-routes
                                        {:limit "5"})))]
              (is (= (count (:documents first-five)) 5))

              (let [next-five (read-string
                               (:body
                                (request :get
                                         "/_api/clj/en/pages/_list-documents"
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
                                        (request
                                         :get
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
                     (str "/en/search?q=bar&after-doc-id=19"
                          "&after-score=0.47674"
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
                                           [[:ol#search-results]
                                            [:li]
                                            [:a]]))
                       :href))
                     "/pages/doc-20"))

              (is
               (= (html/attr-values 
                   (first
                    (html/select third-page
                                 [:a#previous-search-results-page]))
                   :href)
                                   
                  #{"/en/search?q=bar&after-doc-id=9&after-score=0.47674"}))

              (is (=  (html/select third-page [:a#next-search-results-page])
                      []))))
    
          (is (= (:status (request :get "/" main-routes)) 200))
          (is (= (:status (request :get "/login" main-routes)) 200))
          (is (= (:status (request :get "/logout" main-routes)) 302))
          (is (= (:status (request :get "/admin" main-routes)) 200))
          (is (= (:status (request :get
                                   "/_api/json/en/blog/_list-documents"
                                   main-routes))
                 (:status (request :get
                                   "/_api/clj/en/blog/_list-documents"
                                   main-routes))
                 200))

          ;; test json request
          (is (= (:status (request
                           :post
                           "/_api/json/_document/blog/test"
                           (json-str {:action :create
                                      :title "test-create"
                                      :slug "/blog/test"
                                      :language "en"
                                      :feed "blog"
                                      :content "hic sunt dracones"})
                           main-routes))
                 201))

          ;; test clojure request
          (is (= (:status (request
                           :post
                           "/_api/clj/_document/blog/test-clj"
                           (pr-str {:action :create
                                    :title "test-create"
                                    :slug "/blog/test-clj"
                                    :language "en"
                                    :feed "blog"
                                    :content "hic sunt dracones"})
                           main-routes))
                 201))

          ;; test json POST request with HTTP method/action mismatch
          (is (= (request :post
                          "/_api/json/_document/blog/test"
                          (json-str {:action :delete
                                     :title "test-create"
                                     :slug "/blog/test-unique"
                                     :language "en"
                                     :feed "blog"
                                     :content "hic sunt dracones"})
                          main-routes)
                 {:status 400
                  :headers
                  {"Content-Type" "text/plain; charset=UTF-8"}
                  :body
                  (str "The :action value in the provided map must "
                       "correspond to the right HTTP method "
                       "(i.e. POST & :create, PUT & :update "
                       "and DELETE and :delete).")}))

          ;; test duplicate clojure request with HTTP method/action mismatch
          (is (= (request :post
                          "/_api/clj/_document/blog/test-clj"
                          (pr-str {:action :delete
                                   :title "test-create"
                                   :slug "/blog/test-unique-snowflake"
                                   :language "en"
                                   :feed "blog"
                                   :content "hic sunt dracones"})
                          main-routes)
                 {:status 400
                  :headers
                  {"Content-Type" "text/plain; charset=UTF-8"}
                  :body
                  (str "The :action value in the provided map must "
                       "correspond to the right HTTP method "
                       "(i.e. POST & :create, PUT & :update "
                       "and DELETE and :delete).")}))

          ;; test duplicate json POST request
          (is (= (request :post
                          "/_api/json/_document/blog/test"
                          (json-str {:action :create
                                     :title "test-create"
                                     :slug "/blog/test"
                                     :language "en"
                                     :feed "blog"
                                     :content "hic sunt dracones"})
                          main-routes)
                 {:status 400
                  :headers
                  {"Content-Type" "text/plain; charset=UTF-8"}
                  :body
                  "There is an existing document with the provided slug."}))

          ;; test duplicate clojure POST request
          (is (= (request :post
                          "/_api/clj/_document/blog/test-clj"
                          (pr-str {:action :create
                                   :title "test-create"
                                   :slug "/blog/test-clj"
                                   :language "en"
                                   :feed "blog"
                                   :content "hic sunt dracones"})
                          main-routes)
                 {:status 400
                  :headers
                  {"Content-Type" "text/plain; charset=UTF-8"}
                  :body
                  "There is an existing document with the provided slug."}))

          ;; test json POST request with missing keys
          (is (= (request :post
                          "/_api/json/_document/blog/test"
                          (json-str {:action :create
                                     :title "test-create"
                                     :slug "/blog/test-missing-keys"
                                     :language "en"
                                     :content "hic sunt dracones"})
                          main-routes)
                 {:status 400
                  :headers
                  {"Content-Type" "text/plain; charset=UTF-8"}
                  :body
                  (str "This document is missing required keys. "
                       "The keys :slug, :language, :feed, :title "
                       "are required.")}))

          ;; test clojure POST request with missing keys
          (is (= (request :post
                          "/_api/clj/_document/blog/test-clj"
                          (pr-str {:action :create
                                   :title "test-create"
                                   :slug "/blog/test-missing-keys"
                                   :language "en"
                                   :content "hic sunt dracones"})
                          main-routes)
                 {:status 400
                  :headers
                  {"Content-Type" "text/plain; charset=UTF-8"}
                  :body
                  (str "This document is missing required keys. "
                       "The keys :slug, :language, :feed, :title "
                       "are required.")}))

          (testing "test if the document is added to the database"
            ;; test for json request
            (let [document (db/get-document +test-db+ "/blog/test")]
              (is (= (:title document)) "test-create"))

            ;; test for clojure request
            (let [document (db/get-document +test-db+ "/blog/test-clj")]
              (is (= (:title document)) "test-create")))

          (testing "test if the document is added to the lucene index"
            (let [reader (lucene/create-index-reader directory)]
              (is (= (.get (lucene/get-doc reader 21) "title")
                     (.get (lucene/get-doc reader 22) "title")
                     "test-create"))))

          (is (= (:status (request :get
                                   "/_api/json/_document/blog/bar"
                                   main-routes))
                 (:status (request :get
                                   "/_api/clj/_document/blog/bar"
                                   main-routes))
                 200))
        
          (is (= (:status (request :get
                                   "/_api/json/_document/blog/t3"
                                   main-routes))
                 (:status (request :get
                                   "/_api/clj/_document/blog/t3"
                                   main-routes))
                 404))
    
          (testing "test if documents are updated correctly"
            ;; test json request
            (let [document (first (db/get-document +test-db+ "/blog/bar"))]
              (is (= (clean-req (request :put
                                         "/_api/json/_document/blog/bar"
                                         (json-str
                                          (assoc document
                                            :action :update
                                            :previous-id (:_id document)
                                            :title "hi!"))
                                         main-routes)
                                :json)
                     {:status 200
                      :headers
                      {"Content-Type" "application/json; charset=UTF-8"
                       "Expires" "Mon, 26 Mar 2012 09:00:00 GMT"}
                      :body
                      [{:slug "/blog/bar"
                        :content "bar"
                        :action :update
                        :language "en"
                        :title "hi!"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"
                        :draft false}
                       {:slug "/blog/bar"
                        :content "bar"
                        :action :create
                        :language "en"
                        :title "foo"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"
                        :draft false}]}))

              (is (= (request
                      :put
                      "/_api/json/_document/blog/doesnt-exist"
                      (json-str (assoc document
                                  :action :update
                                  :title "hi!"))
                      main-routes)
                     {:status 400
                      :headers {"Content-Type" "text/plain; charset=UTF-8"}
                      :body (str "This document map doesn't contain "
                                 "the most recent :previous-id.")})))

            ;; test clojure request
            (let [document
                  (first (db/get-document +test-db+ "/blog/test-clj"))]
              (is (= (clean-req (request
                                 :put
                                 "/_api/clj/_document/blog/test-clj"
                                 (pr-str (assoc document
                                           :action :update
                                           :previous-id (:_id document)
                                           :title "hi!"))
                                 main-routes)
                                :clj)
                     {:status 200
                      :headers
                      {"Content-Type" "text/plain; charset=UTF-8"
                       "Expires" "Mon, 26 Mar 2012 09:00:00 GMT"}
                      :body
                      [{:slug "/blog/test-clj"
                        :content "hic sunt dracones"
                        :action :update
                        :language "en"
                        :title "hi!"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"}
                       {:slug "/blog/test-clj"
                        :content "hic sunt dracones"
                        :action :create
                        :language "en"
                        :title "test-create"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"}]}))

              (is (= (request
                      :put
                      "/_api/clj/_document/blog/doesnt-exist"
                      (pr-str (assoc document
                                :action :update
                                :title "hi!"))
                      main-routes)
                     {:status 400
                      :headers {"Content-Type" "text/plain; charset=UTF-8"}
                      :body (str "This document map doesn't contain "
                                 "the most recent :previous-id.")}))))

          (testing "test if documents are also updated in the lucene index"
            (let [reader (lucene/create-index-reader directory)]
              (is (= (.get (lucene/get-doc reader 22) "title") "hi!"))))

          (testing "test if document is deleted from the database correctly"
            ;; test json request
            (let [document (first (db/get-document +test-db+ "/blog/bar"))]
              (is (= (clean-req (request :delete
                                         "/_api/json/_document/blog/bar"
                                         (json-str
                                          (assoc document
                                            :action :delete
                                            :previous-id (:_id document)))
                                         main-routes)
                                :json)
                     {:status 200
                      :headers
                      {"Content-Type" "application/json; charset=UTF-8"
                       "Expires" "Mon, 26 Mar 2012 09:00:00 GMT"}
                      :body
                      [{:slug "/blog/bar"
                        :content "bar"
                        :action :delete
                        :language "en"
                        :title "hi!"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"
                        :draft false}
                       {:slug "/blog/bar"
                        :content "bar"
                        :action :update
                        :language "en"
                        :title "hi!"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"
                        :draft false}
                       {:slug "/blog/bar"
                        :content "bar"
                        :action :create
                        :language "en"
                        :title "foo"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"
                        :draft false}]}))

              ;; test error message for double deletion
              (is (= (request :delete
                              "/_api/json/_document/blog/bar"
                              (json-str
                               (assoc document
                                 :action
                                 :delete
                                 :previous-id
                                 (:_id
                                  (first
                                   (db/get-document +test-db+
                                                    "/blog/bar")))))
                              main-routes)
                     {:status 400
                      :headers {"Content-Type" "text/plain; charset=UTF-8"}
                      :body "This document has already been deleted."})))
            
            ;; test clojure request
            (let [document
                  (first (db/get-document +test-db+ "/blog/test-clj"))]
              (is (= (clean-req (request :delete
                                         "/_api/clj/_document/blog/test-clj"
                                         (pr-str
                                          (assoc document
                                            :action :delete
                                            :previous-id (:_id document)))
                                         main-routes)
                                :clj)
                     {:status 200
                      :headers
                      {"Content-Type" "text/plain; charset=UTF-8"
                       "Expires" "Mon, 26 Mar 2012 09:00:00 GMT"}
                      :body
                      [{:slug "/blog/test-clj"
                        :content "hic sunt dracones"
                        :action :delete
                        :language "en"
                        :title "hi!"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"}
                       {:slug "/blog/test-clj"
                        :content "hic sunt dracones"
                        :action :update
                        :language "en"
                        :title "hi!"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"}
                       {:slug "/blog/test-clj"
                        :content "hic sunt dracones"
                        :action :create
                        :language "en"
                        :title "test-create"
                        :published "2012-09-22T04:07:05.756Z"
                        :datestamp "2012-09-22T04:07:05.756Z"
                        :created "2012-09-22T04:07:05.756Z"
                        :type "document"
                        :feed "blog"}]}))

              ;; test error message for double deletion
              (is (= (request :delete
                              "/_api/clj/_document/blog/test-clj"
                              (pr-str
                               (assoc document
                                 :action
                                 :delete
                                 :previous-id
                                 (:_id
                                  (first
                                   (db/get-document +test-db+
                                                    "/blog/test-clj")))))
                              main-routes)
                     {:status 400
                      :headers {"Content-Type" "text/plain; charset=UTF-8"}
                      :body "This document has already been deleted."}))))

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
                                 "/_api/json/_feed/en/blog"
                                 (json-str {:action :create
                                            :name "blog"
                                            :title "Vix Weblog"
                                            :language "en"
                                            :subtitle "Vix Weblog..."
                                            :default-slug-format
                                            "/{document-title}"
                                            :default-document-type "standard"
                                            :searchable true})
                                 main-routes)]
          (is (= (:status post-feed-request) 201))

          (is (= @search-allowed-feeds
                 (db/get-searchable-feeds (db/list-feeds +test-db+))
                 {"en" ["pages" "blog"]})
              "Test if search-allowed-feeds is updated when feed is added")
        
          (let [image-feed (map #(update-in % [:action] keyword)
                                (read-json
                                 (:body
                                  (request
                                   :post
                                   "/_api/json/_feed/en/image"
                                   (json-str
                                    {:action :create
                                     :name "image"
                                     :title "Images"
                                     :language "en"
                                     :subtitle "Pictures."
                                     :default-slug-format
                                     "/static/{document-title}.{ext}"
                                     :default-document-type "image"})
                                   main-routes))))
                all-feeds (map #(update-in % [:action] keyword)
                               (read-json
                                (:body
                                 (request :get
                                          "/_api/json/_list-feeds"
                                          main-routes))))
                image-feed-nl (read-string
                               (:body
                                (request
                                 :post
                                 "/_api/clj/_feed/nl/image"
                                 (pr-str {:action :create
                                          :name "image"
                                          :title "Images"
                                          :language "nl"
                                          :subtitle "Pictures."
                                          :default-slug-format
                                          "/static/{document-title}.{ext}"
                                          :default-document-type "image"})
                                 main-routes)))]

            (is (= (count all-feeds) 3))

            ;; test HTTP method/action mismatch
            (is (= (request
                    :post
                    "/_api/json/_feed/en/image"
                    (json-str {:action :delete
                               :name "image"
                               :title "Images"
                               :language "en"
                               :subtitle "Pictures."
                               :default-slug-format
                               "/static/{document-title}.{ext}"
                               :default-document-type "image"})
                    main-routes)
                   (request
                    :post
                    "/_api/clj/_feed/nl/image"
                    (pr-str {:action :delete
                             :name "image"
                             :title "Images"
                             :language "nl"
                             :subtitle "Pictures."
                             :default-slug-format
                             "/static/{document-title}.{ext}"
                             :default-document-type "image"})
                    main-routes)
                   {:status 400
                    :headers {"Content-Type" "text/plain; charset=UTF-8"}
                    :body
                    (str "The :action value in the provided map must "
                         "correspond to the right HTTP method "
                         "(i.e. POST & :create, PUT & :update "
                         "and DELETE and :delete).")}))
            
            ;; make sure it isn't possible to recreate existing feeds
            (is (= (request
                    :post
                    "/_api/json/_feed/en/image"
                    (json-str {:action :create
                               :name "image"
                               :title "Images"
                               :language "en"
                               :subtitle "Pictures."
                               :default-slug-format
                               "/static/{document-title}.{ext}"
                               :default-document-type "image"})
                    main-routes)
                   (request
                    :post
                    "/_api/clj/_feed/nl/image"
                    (pr-str {:action :create
                             :name "image"
                             :title "Images"
                             :language "nl"
                             :subtitle "Pictures."
                             :default-slug-format
                             "/static/{document-title}.{ext}"
                             :default-document-type "image"})
                    main-routes)
                   {:status 400
                    :headers {"Content-Type" "text/plain; charset=UTF-8"}
                    :body "The provided feed already exists."}))

            (testing "test language argument for /_api/x/_list-feeds"
              (is (= (sort-by :name all-feeds)
                     (sort-by :name
                              (map #(update-in % [:action] keyword)
                                   (read-json
                                    (:body
                                     (request :get
                                              "/_api/json/_list-feeds"
                                              nil
                                              main-routes
                                              {:language "en"})))))
                     (sort-by :name
                              (read-string
                               (:body
                                (request :get
                                         "/_api/clj/_list-feeds"
                                         nil
                                         main-routes
                                         {:language "en"}))))))

              (is (= image-feed-nl
                     (read-string
                      (:body
                       (request
                        :get
                        "/_api/clj/_list-feeds"
                        nil
                        main-routes
                        {:default-document-type "image"
                         :language "nl"}))))))
          
            (is (= (count (read-json
                           (:body (request :get
                                           "/_api/json/_list-feeds"
                                           main-routes))))
                   4))

            (is (= (map #(update-in % [:action] keyword)
                        (read-json
                         (:body (request :get
                                         "/_api/json/_list-feeds"
                                         main-routes))))
                   (read-string
                    (:body (request :get
                                    "/_api/clj/_list-feeds"
                                    main-routes)))))
          
            (is (= (flatten [image-feed-nl image-feed])
                   (map #(update-in % [:action] keyword)
                        (read-json
                         (:body (request
                                 :get
                                 "/_api/json/_list-feeds"
                                 nil
                                 main-routes
                                 {:default-document-type
                                  "image"}))))
                   (read-string
                    (:body (request
                            :get
                            "/_api/clj/_list-feeds"
                            nil
                            main-routes
                            {:default-document-type
                             "image"})))))))

        (let [{:keys [status body] :as get-feed-json}
              (update-in (request :get
                                  "/_api/json/_feed/en/blog"
                                  main-routes)
                         [:body]
                         read-json)
              get-feed-clj
              (update-in (request :get
                                  "/_api/clj/_feed/en/blog"
                                  main-routes)
                         [:body]
                         read-string)]
          (is (= (update-in (dissoc get-feed-json :headers)
                            [:body]
                            (fn [states]
                              (map #(update-in % [:action] keyword) states)))
                 (dissoc get-feed-clj :headers)))
          (is (= status 200))
          (is (= (:name (first body)) "blog"))
          (is (= (:title (first body)) "Vix Weblog"))

          ;; make sure that update requests aren't allowed without
          ;; :previous-id
          (let [prev-body (first
                           (read-json
                            (:body
                             (request :get
                                      "/_api/json/_feed/en/blog"
                                      main-routes))))]
            (is (= (request :put
                            "/_api/json/_feed/en/blog"
                            (json-str
                             (assoc prev-body
                               :action :update
                               :previous-id :foo
                               :title "Vix!"
                               :searchable false))
                            main-routes)
                   (request :put
                            "/_api/json/_feed/en/blog"
                            (json-str
                             (assoc prev-body
                               :action :update
                               :title "Vix!"
                               :searchable false))
                            main-routes)
                   (request :put
                            "/_api/clj/_feed/en/blog"
                            (pr-str
                             (assoc prev-body
                               :action :update
                               :previous-id :foo
                               :title "Vix!"
                               :searchable false))
                            main-routes)
                   (request :put
                            "/_api/clj/_feed/en/blog"
                            (pr-str
                             (assoc prev-body
                               :action :update
                               :title "Vix!"
                               :searchable false))
                            main-routes)
                   {:status 400
                    :headers {"Content-Type" "text/plain; charset=UTF-8"}
                    :body (str "This feed map doesn't contain the "
                               "most recent :previous-id.")})))
        
          ;; test json put request
          (let [prev-body (first
                           (read-json
                            (:body
                             (request :get
                                      "/_api/json/_feed/en/blog"
                                      main-routes))))
                {:keys [body status] :as put-request-json}
                (update-in (request :put
                                    "/_api/json/_feed/en/blog"
                                    (json-str
                                     (assoc prev-body
                                       :action :update
                                       :previous-id (:_id prev-body)
                                       :title "Vix!"
                                       :searchable false))
                                    main-routes)
                           [:body]
                           read-json)]
            (is (= status 200))

            (is (= (:name (first body)) "blog"))
            (is (= (:title (first body)) "Vix!"))
          
            (is (= @search-allowed-feeds {"nl" [] "en" ["pages"]})
                "Make sure search-allowed-feeds is updated when feeds are")))

        ;; test clojure put request
        (let [prev-body (first
                         (read-string
                          (:body
                           (request :get
                                    "/_api/clj/_feed/en/blog"
                                    main-routes))))
              {:keys [body status] :as put-request-clj}
              (update-in (request :put
                                  "/_api/clj/_feed/en/blog"
                                  (pr-str
                                   (assoc prev-body
                                     :action :update
                                     :previous-id (:_id prev-body)
                                     :title "Fix!"
                                     :searchable false))
                                  main-routes)
                         [:body]
                         read-string)]

          (is (= status 200))
          (is (= (:name (first body)) "blog"))
          (is (= (:title (first body)) "Fix!")))

        (testing "test delete requests"
          ;; make sure delete requests aren't allowed without :previous-id
          (is (= (request :delete
                          "/_api/json/_feed/en/blog"
                          (json-str
                           {:action :delete
                            :previous-id :foo
                            :name "blog"
                            :language "en"})
                          main-routes)
                 (request :delete
                          "/_api/json/_feed/en/blog"
                          (json-str
                           {:action :delete
                            :name "blog"
                            :language "en"})
                          main-routes)
                 (request :delete
                          "/_api/clj/_feed/en/blog"
                          (pr-str
                           {:action :delete
                            :previous-id :foo
                            :name "blog"
                            :language "en"})
                          main-routes)
                 (request :delete
                          "/_api/clj/_feed/en/blog"
                          (pr-str
                           {:action :delete
                            :name "blog"
                            :language "en"})
                          main-routes)
                 {:status 400
                  :headers {"Content-Type" "text/plain; charset=UTF-8"}
                  :body (str "This feed map doesn't contain the most recent "
                             ":previous-id.")}))
        
          ;; test json delete request
          (let [prev-body (first
                           (read-json
                            (:body
                             (request :get
                                      "/_api/json/_feed/en/blog"
                                      main-routes))))
                {:keys [body status] :as delete-request-json}
                (update-in (request :delete
                                    "/_api/json/_feed/en/blog"
                                    (json-str
                                     (assoc prev-body
                                       :action :delete
                                       :previous-id (:_id prev-body)))
                                    main-routes)
                           [:body]
                           read-json)]
            (is (= status 200))
            (is (= (keyword (:action (first body)))
                   (:action (first (db/get-feed +test-db+ "en" "blog")))
                   :delete)))

          ;; make sure the right error is returned when feed is already gone
          (let [prev-body (first
                           (read-json
                            (:body
                             (request :get
                                      "/_api/json/_feed/en/blog"
                                      main-routes))))]
            (is (= (request :delete
                            "/_api/json/_feed/en/blog"
                            (json-str
                             (assoc prev-body
                               :action :delete
                               :previous-id (:_id prev-body)))
                            main-routes)
                   (request :delete
                            "/_api/clj/_feed/en/blog"
                            (pr-str
                             (assoc prev-body
                               :action :delete
                               :previous-id (:_id prev-body)))
                            main-routes)
                   {:status 400
                    :headers {"Content-Type" "text/plain; charset=UTF-8"}
                    :body "This feed has already been deleted."})))

        
          ;; test clojure delete request
          (let [prev-body (first
                           (read-json
                            (:body
                             (request
                              :post
                              "/_api/json/_feed/en/blog"
                              (json-str {:action :create
                                         :name "blog"
                                         :title "Weblog"
                                         :language "en"
                                         :subtitle "Weblog..."
                                         :default-slug-format
                                         "/static/{document-title}.{ext}"
                                         :default-document-type "standard"})
                              main-routes))))
                {:keys [body status] :as delete-request-clj}
                (update-in (request :delete
                                    "/_api/clj/_feed/en/blog"
                                    (pr-str
                                     (assoc prev-body
                                       :action :delete
                                       :previous-id (:_id prev-body)))
                                    main-routes)
                           [:body]
                           read-string)]
            (is (= status 200))
            (is (= (:action (first body))
                   (:action (first (db/get-feed +test-db+ "en" "blog")))
                   :delete))))))

    (testing "test invalid api requests"
      (are [uri]
           (= (request :post
                       uri
                       "[}"
                       main-routes)
              {:status 400
               :headers {"Content-Type" "text/plain; charset=UTF-8"}
               :body "The JSON or Clojure request body is invalid."})
           "/_api/clj/_feed/nl/invalid-image"
           "/_api/json/_feed/nl/invalid-image"
           "/_api/clj/_document/blog/invalid-test"
           "/_api/json/_document/blog/invalid-test"))))

(deftest test-read-body
  (is (= (read-body "json" (java.io.StringReader. ""))
         (read-body "clj" (java.io.StringReader. ""))
         nil))

  (is (= (read-body "json" (java.io.StringReader. "{\"foo\":\"bar\"}"))
         {:foo "bar"}))

  (is (= (read-body "clj" (java.io.StringReader. "{:foo :bar}"))
         {:foo :bar}))

  (is (thrown+? (partial check-exc :vix.routes/invalid-request-body)
                (read-body "json" (java.io.StringReader. "{]"))))

  (is (thrown+? (partial check-exc :vix.routes/invalid-request-body)
                (read-body "clj" (java.io.StringReader. "{]")))))

(deftest ^{:integration true} test-routes-authorization
  (do
    (db/append-to-document +test-db+
                           "Europe/Amsterdam"
                           {:action :create
                            :language "en"
                            :feed "blog"
                            :title "foo"
                            :slug "/blog/test"
                            :content "bar"
                            :draft false})
    
    (db/append-to-feed +test-db+
                       {:action :create
                        :title "Weblog"
                        :subtitle "Vix Weblog!"
                        :name "blog"
                        :default-slug-format "/{feed-name}/{document-title}"
                        :default-document-type "with-description"
                        :language "en"}))

  (with-redefs [config/database +test-db+
                lucene/directory (lucene/create-directory :RAM)]
    (testing "Test if authorization is enforced correctly."
      (is (= (:status (unauthorized-request :get "/admin/" main-routes))
             ;; test json requests
             (:status (unauthorized-request
                       :get
                       "/_api/json/en/blog/_list-documents"
                       main-routes))
             (:status (unauthorized-request
                       :post
                       "/_api/json/_document/blog/test"
                       (json-str {:title "test-create"
                                  :slug "/blog/test"
                                  :content "hic sunt dracones"})
                       main-routes))
             (:status (unauthorized-request
                       :get
                       "/_api/json/_document/blog/test"
                       main-routes))
             (:status (unauthorized-request
                       :put
                       "/_api/json/_document/blog/test"
                       (json-str {:title "test-create"
                                  :slug "/blog/test"
                                  :content "hic sunt dracones"})
                       main-routes))
             (:status (unauthorized-request
                       :delete
                       "/_api/json/_document/blog/test"
                       main-routes))
             (:status (unauthorized-request
                       :post
                       "/_api/clj/_feed/foo/bar"
                       main-routes))
             (:status (unauthorized-request
                       :get
                       "/_api/clj/_feed/en/blog"
                       main-routes))
             (:status (unauthorized-request
                       :put
                       "/_api/clj/_feed/en/blog"
                       main-routes))
             (:status (unauthorized-request
                       :delete
                       "/_api/clj/_feed/en/blog"
                       main-routes))

             ;; test clojure requests
             (:status (unauthorized-request
                       :get
                       "/_api/clj/en/blog/_list-documents"
                       main-routes))
             (:status (unauthorized-request
                       :post
                       "/_api/clj/_document/blog/test"
                       (pr-str {:title "test-create"
                                :slug "/blog/test"
                                :content "hic sunt dracones"})
                       main-routes))
             (:status (unauthorized-request
                       :get
                       "/_api/clj/_document/blog/test"
                       main-routes))
             (:status (unauthorized-request
                       :put
                       "/_api/clj/_document/blog/test"
                       (pr-str {:title "test-create"
                                :slug "/blog/test"
                                :content "hic sunt dracones"})
                       main-routes))
             (:status (unauthorized-request
                       :delete
                       "/_api/clj/_document/blog/test"
                       main-routes))
             (:status (unauthorized-request
                       :post
                       "/_api/clj/_feed/foo/bar"
                       main-routes))
             (:status (unauthorized-request
                       :get
                       "/_api/clj/_feed/en/blog"
                       main-routes))
             (:status (unauthorized-request
                       :put
                       "/_api/clj/_feed/en/blog"
                       main-routes))
             (:status (unauthorized-request
                       :delete
                       "/_api/clj/_feed/en/blog"
                       main-routes))
             302)))))

(deftest ^{:integration true} test-routes-authentication
  (do
    (db/append-to-document +test-db+
                           "Europe/Amsterdam"
                           {:action :create
                            :language "en"
                            :feed "blog"
                            :title "foo"
                            :slug "/blog/test"
                            :content "bar"
                            :draft false}))

  (testing "Test if authentication is enforced correctly."
    (with-redefs [config/database +test-db+
                  lucene/directory (lucene/create-directory :RAM)]
      (is (= (:status (unauthenticated-request :get "/admin" main-routes))
             
             ;; test json requests
             (:status (unauthenticated-request
                       :get
                       "/_api/json/en/blog/_list-documents"
                       main-routes))
             (:status (unauthenticated-request
                       :post
                       "/_api/json/_document/blog/test"
                       (json-str {:title "test-create"
                                  :slug "/blog/test"
                                  :content "hic sunt dracones"})
                       main-routes))
             (:status (unauthenticated-request
                       :get
                       "/_api/json/_document/blog/test"
                       main-routes))
             (:status (unauthenticated-request
                       :put
                       "/_api/json/_document/blog/test"
                       (json-str {:title "test-create"
                                  :slug "/blog/test"
                                  :content "hic sunt dracones"})

                       main-routes))
             (:status (unauthenticated-request
                       :delete
                       "/_api/json/_document/blog/test"
                       main-routes))

             ;; test clojure requests
             (:status (unauthenticated-request
                       :get
                       "/_api/clj/en/blog/_list-documents"
                       main-routes))
             (:status (unauthenticated-request
                       :post
                       "/_api/clj/_document/blog/test"
                       (pr-str {:title "test-create"
                                :slug "/blog/test"
                                :content "hic sunt dracones"})
                       main-routes))
             (:status (unauthenticated-request
                       :get
                       "/_api/clj/_document/blog/test"
                       main-routes))
             (:status (unauthenticated-request
                       :put
                       "/_api/clj/_document/blog/test"
                       (pr-str {:title "test-create"
                                :slug "/blog/test"
                                :content "hic sunt dracones"})

                       main-routes))
             (:status (unauthenticated-request
                       :delete
                       "/_api/clj/_document/blog/test"
                       main-routes))
             302)))))

(deftest test-logout
  (is (= (logout {:username "johndoe" :permissions {:* [:DELETE]}})
         {:session {}
          :status 302
          :headers {"Location" "/"}})
      "should empty the session and redirect to /"))

(deftest test-login
  (do
    (auth/add-user +test-db+
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

(deftest test-handle-exceptions
  (is (= ((handle-exceptions identity) :works) :works))

  (is (= ((handle-exceptions
           (fn [handler]
             (throw+ auth/insufficient-privileges-error)))
          :should-not-work)
         {:status 302
          :headers {"Location" "/permission-denied"}
          :body ""}))

  (is (= ((handle-exceptions
           (fn [handler]
             (throw+ auth/authentication-required-error)))
          :should-not-work)
         {:status 302
          :headers {"Location" "/login"}
          :body ""}))

  (is (= ((handle-exceptions
           (fn [handler]
             (throw+ invalid-request-body-error)))
          :should-not-work)
         {:status 400
          :headers {"Content-Type" "text/plain; charset=UTF-8"}
          :body "The JSON or Clojure request body is invalid."})))

(defn test-ns-hook []
  (database-fixture test-reset-search-allowed-feeds!)
  (database-fixture test-reset-available-languages!)
  (test-reset-index-reader!)
  (test-data-response)
  (test-response)
  (test-page-not-found-response)
  (database-fixture test-image-response)
  (database-fixture test-get-segment-and-get-segments)
  (database-fixture test-get-frontpage-for-language!)
  (database-fixture test-get-cached-frontpage!)
  (database-fixture test-get-cached-page!)
  (test-reset-frontpage-cache!)
  (test-reset-page-cache!)
  (database-fixture test-feed-request)
  (database-fixture test-document-request)
  (database-fixture test-routes)
  (test-read-body)
  (database-fixture test-routes-authorization)
  (database-fixture test-routes-authentication)
  (test-logout)
  (database-fixture test-login)
  (test-wrap-caching-headers)
  (test-redirect-301)
  (test-redirection-handler)
  (test-handle-exceptions))