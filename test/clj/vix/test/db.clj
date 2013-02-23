;; test/vix/test/db.clj tests for db namespace.
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

(ns vix.test.db
  (:use [vix.db] :reload
        [vix.test.test]
        [slingshot.test]
        [clojure.test]
        [clojure.data.json :only [read-json]])
  (:require [com.ashafa.clutch :as clutch]
            [clj-http.client :as http]
            [vix.util :as util])
  (:import [org.apache.commons.codec.binary Base64]))

(defn couchdb-id? [s]
  (re-matches #"^[a-z0-9]{32}$" s))

(defn couchdb-rev?
  ([s]
     (couchdb-rev? 1 s))
  ([rev-num s]
     (re-matches (re-pattern (str "^" rev-num "-[a-z0-9]{32}$")) s)))

(defn iso-date? [s]
  (re-matches
   #"^[\d]{4}-[\d]{2}-[\d]{2}T[\d]{2}:[\d]{2}:[\d]{2}\.[\d]{1,4}Z"
   s))

(defn random-lower-case-string [length]
  ;; to include uppercase
  ;; (let [ascii-codes (concat (range 48 58) (range 66 91) (range 97 123))])
  (let [ascii-codes (concat (range 48 58) (range 97 123))]
    (apply str (repeatedly length #(char (rand-nth ascii-codes))))))

(def +test-db+ (str "vix-test-" (random-lower-case-string 20)))
(def +test-server+ "http://localhost:5984/")

(defn database-fixture [f]
  (clutch/get-database +test-db+)
  (f)
  (clutch/delete-database +test-db+))

(use-fixtures :each database-fixture)

(deftest test-load-view
  (is (= (load-view "database-views/map_newsletter_subscribers.js")
         (str "function(doc) {\n"
              "    if(doc.type === \"newsletter-subscriber\") {\n"
              "        emit([doc.language, doc.email], doc);\n"
              "    }\n}\n"))))

(deftest test-create-views
  (create-views +test-db+ "views" views)
  (let [view-doc (read-json (:body (http/get
                                    (str +test-server+
                                         +test-db+
                                         "/_design/views"))))]

    (is (= (count (:views view-doc)) 9))
    
    (is (= (:map (:feeds (:views view-doc)))
           (str "function(feed) {\n"
                "    if(feed.type === \"feed\") {\n"
                "        emit([feed.language, feed.name, feed.datestamp]"
                ", feed);\n    }\n}\n")))

    (is (= (:map (:feeds_by_default_document_type (:views view-doc)))
           (str "function(feed) {\n"
                "    if(feed.type === \"feed\" &&\n"
                "       feed[\"current-state\"] === true &&\n"
                "       feed.action !== \"delete\") {\n"
                "        emit([feed[\"default-document-type\"],\n"
                "              feed[\"language\"],\n"
                "              feed[\"name\"]],\n"
                "             feed);\n"
                "    }\n}\n")))

    (is (= (:map (:feeds_overview (:views view-doc)))
           (str "function(feed) {\n"
                "    if(feed.type === \"feed\" &&\n"
                "       feed[\"current-state\"] === true &&\n"
                "       feed.action !== \"delete\") {\n"
                "        emit([feed.language, feed.name, feed.datestamp],"
                " feed);\n"
                "    }\n}\n")))
    
    (is (= (:map (:by_slug (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"document\") {\n"
                "        emit([doc.slug, doc.datestamp], doc);\n"
                "    }\n}\n")))
    
    (is (= (:map (:by_feed (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"document\" &&\n"
                "       doc[\"current-state\"] === true &&\n"
                "       doc.action !== \"delete\") {\n"
                "        emit([[doc.language, doc.feed], doc.published], "
                "doc);\n"
                "    }\n}\n")))

    (is (= (:map (:by_username (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"user\") {\n"
                "        emit(doc.username, doc);\n"
                "    }\n"
                "}\n")))

    (is (= (:map (:events_by_feed (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"document\" &&\n"
                "       doc[\"end-time-rfc3339\"] &&\n"
                "       doc[\"current-state\"] === true) {\n"
                "        emit([[doc.language, doc.feed], "
                "doc[\"end-time-rfc3339\"]], doc);\n    }\n}\n")))

    (is (= (:map (:subscribers (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"newsletter-subscriber\") {\n"
                "        emit([doc.language, doc.email], doc);\n"
                "    }\n}\n")))

    (is (= (:map (:languages (:views view-doc)))
           (str "function(feed) {\n"
                "    if(feed.type === \"feed\" &&\n"
                "       feed[\"current-state\"] === true &&\n"
                "       feed.action !== \"delete\") {\n"
                "        emit(feed.language, null);\n"
                "    }\n}\n")))

    (is (= (:reduce (:languages (:views view-doc)))
           "function(k,v) {\n    return null;\n}\n")))

  (is (thrown+? (partial check-exc :vix.db/database-socket-error)
                (create-views "http://localhost:9999/foo" "bar" views))))

(deftest test-get-attachment-as-base64-string
  (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                 "vQAsAAAAAAEAAQAAAgJEAQA7")
        document (append-to-document +test-db+
                                     "Europe/Amsterdam"
                                     {:action :create
                                      :attachment {:type "image/gif"
                                                   :data gif}
                                      :feed "images"
                                      :language "en"
                                      :title "a single black pixel!"
                                      :slug "pixel.gif"
                                      :content ""
                                      :draft false})]
    (is (= (get-attachment-as-base64-string +test-db+
                                            (:_id (first document))
                                            :original)
           gif))))

(deftest test-get-document
  (is (nil? (get-document +test-db+ "/blog/foo")))
  (is (nil? (get-document +test-db+ "/blog/foo" {:limit 100000})))
  
  (with-redefs [util/now-rfc3339 #(str "2012-09-15T23:48:58.050Z")]
    (append-to-document +test-db+
                        "Europe/Amsterdam"
                        {:action :create
                         :title "foo"
                         :slug "/blog/foo"
                         :language "en"
                         :feed "blog"
                         :content "bar"
                         :draft true})
    (append-to-document +test-db+
                        "Europe/Amsterdam"
                        {:action :update
                         :previous-id (:_id
                                       (first
                                        (get-document
                                         +test-db+
                                         "/blog/foo")))
                         :title "foo"
                         :slug "/blog/foo"
                         :language "en"
                         :feed "blog"
                         :content "bar"
                         :draft false}))

  ;; The view isn't created manually; successful execution of
  ;; this test also implies that it is created automatically.

  (is (couchdb-id? (:_id (first (get-document +test-db+ "/blog/foo")))))
  (is (couchdb-rev? (:_rev (first (get-document +test-db+ "/blog/foo")))))
  
  (is (= (vec (map #(dissoc % :_id :_rev :previous-id)
                   (get-document +test-db+ "/blog/foo")))
         [{:slug "/blog/foo"
           :content "bar"
           :action :update
           :language "en"
           :title "foo"
           :published "2012-09-15T23:48:58.050Z"
           :datestamp "2012-09-15T23:48:58.050Z"
           :created "2012-09-15T23:48:58.050Z"
           :type "document"
           :feed "blog"
           :draft false}
          {:slug "/blog/foo"
           :content "bar"
           :action :create
           :language "en"
           :title "foo"
           :published "2012-09-15T23:48:58.050Z"
           :datestamp "2012-09-15T23:48:58.050Z"
           :created "2012-09-15T23:48:58.050Z"
           :type "document"
           :feed "blog"
           :draft true}]))

  (is (= (vec (map #(dissoc % :_id :_rev :previous-id)
                   (get-document +test-db+ "/blog/foo" {:limit 1})))
         [{:slug "/blog/foo"
           :content "bar"
           :action :update
           :language "en"
           :title "foo"
           :published "2012-09-15T23:48:58.050Z"
           :datestamp "2012-09-15T23:48:58.050Z"
           :created "2012-09-15T23:48:58.050Z"
           :type "document"
           :feed "blog"
           :draft false}]))

  (testing "Test if attachments are handled correctly."
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")]
      (do
        (append-to-document +test-db+
                            "Europe/Amsterdam"
                            {:action :create
                             :attachment {:type "image/gif" :data gif}
                             :title "a single black pixel!"
                             :slug "/images/white-pixel.gif"
                             :language "en"
                             :feed "images"
                             :content ""
                             :draft false})
        (append-to-document +test-db+
                            "Europe/Amsterdam"
                            {:action :update
                             :previous-id (:_id
                                           (first
                                            (get-document
                                             +test-db+
                                             "/images/white-pixel.gif")))
                             :attachment {:type "image/gif" :data gif}
                             :title "a single black pixel!"
                             :slug "/images/white-pixel.gif"
                             :language "en"
                             :feed "images"
                             :content ""
                             :draft false})
        (append-to-document +test-db+
                            "Europe/Amsterdam"
                            {:action :create
                             :title "not a single black pixel!"
                             :slug "/images/not-a-white-pixel.gif"
                             :language "en"
                             :feed "images"
                             :content ""
                             :draft false}))

      (is (= (get-in (get-document +test-db+ "/images/white-pixel.gif")
                     [0 :attachments :original])
             (get-in (get-document +test-db+ "/images/white-pixel.gif")
                     [1 :attachments :original])
             {:type "image/gif" :data gif :length 57})))))

(deftest test-get-feed
  (with-redefs [util/now-rfc3339 #(str "2012-09-04T03:46:52.096Z")]
    (let [en-feed (append-to-feed
                   +test-db+
                   {:action :create
                    :title "Weblog"
                    :subtitle "Vix Weblog!"
                    :name "blog"
                    :default-slug-format "/{document-title}"
                    :default-document-type "with-description"
                    :language "en"})
          nl-feed (append-to-feed
                   +test-db+
                   {:action :create
                    :title "Weblog"
                    :subtitle "Vix Weblog!"
                    :name "blog"
                    :default-slug-format "/{document-title}"
                    :default-document-type "with-description"
                    :language "nl"})
          nl-feed-update-0 (with-redefs
                             [util/now-rfc3339
                              #(str "2012-09-04T03:50:52.096Z")]
                             (append-to-feed
                              +test-db+
                              {:action :update
                               :previous-id (:_id (first nl-feed))
                               :title "B1"
                               :subtitle "b1!"
                               :name "blog"
                               :default-slug-format "/{document-title}"
                               :default-document-type "with-description"
                               :language "nl"}))
          nl-feed-update-1 (with-redefs
                             [util/now-rfc3339
                              #(str "2012-09-04T03:55:52.096Z")]
                             (append-to-feed
                              +test-db+
                              {:action :update
                               :previous-id (:_id (first nl-feed-update-0))
                               :title "B2"
                               :subtitle "b2!"
                               :name "blog"
                               :default-slug-format "/{document-title}"
                               :default-document-type "with-description"
                               :language "nl"}))]

      (is (= (map #(dissoc % :_id :_rev) (get-feed +test-db+ "en" "blog"))
             [{:subtitle "Vix Weblog!"
               :action :create
               :name "blog"
               :language "en"
               :title "Weblog"
               :datestamp "2012-09-04T03:46:52.096Z"
               :created "2012-09-04T03:46:52.096Z"
               :type "feed"
               :default-document-type "with-description"
               :default-slug-format "/{document-title}"}]))

      (is (couchdb-id? (:_id (first (get-feed +test-db+ "en" "blog")))))
      (is (couchdb-rev? (:_rev (first (get-feed +test-db+ "en" "blog")))))

      (is (= (map #(dissoc % :_id :_rev :previous-id)
                  (get-feed +test-db+ "nl" "blog"))
             [{:subtitle "b2!"
               :action :update
               :name "blog"
               :language "nl"
               :title "B2"
               :datestamp "2012-09-04T03:55:52.096Z"
               :created "2012-09-04T03:46:52.096Z"
               :type "feed"
               :default-document-type "with-description"
               :default-slug-format "/{document-title}"}
              {:subtitle "b1!"
               :action :update
               :name "blog"
               :language "nl"
               :title "B1"
               :datestamp "2012-09-04T03:50:52.096Z"
               :created "2012-09-04T03:46:52.096Z"
               :type "feed"
               :default-document-type "with-description"
               :default-slug-format "/{document-title}"}
              {:subtitle "Vix Weblog!"
               :action :create
               :name "blog"
               :language "nl"
               :title "Weblog"
               :datestamp "2012-09-04T03:46:52.096Z"
               :created "2012-09-04T03:46:52.096Z"
               :type "feed"
               :default-document-type "with-description"
               :default-slug-format "/{document-title}"}]))

            (is (= (map #(dissoc % :_id :_rev :previous-id)
                  (get-feed +test-db+ "nl" "blog" 1))
             [{:subtitle "b2!"
               :action :update
               :name "blog"
               :language "nl"
               :title "B2"
               :datestamp "2012-09-04T03:55:52.096Z"
               :created "2012-09-04T03:46:52.096Z"
               :type "feed"
               :default-document-type "with-description"
               :default-slug-format "/{document-title}"}]))

        
      (let [blog-states (get-feed +test-db+ "nl" "blog")]
        (are [n]
             (couchdb-id? (:_id (nth blog-states n)))
             0 1 2)

        (are [rev-number n]
             (couchdb-rev? rev-number (:_rev (nth blog-states n)))
             1 0
             2 1
             2 2)))))

(deftest test-append-to-feed
  (let [blog-feed (with-redefs [util/now-rfc3339
                                #(str "2012-09-04T04:30:17.872Z")]
                    (first
                     (append-to-feed
                      +test-db+
                      {:action :create
                       :title "Weblog"
                       :subtitle "Vix Weblog!"
                       :name "blog"
                       :language "en"
                       :default-slug-format "/{document-title}"
                       :default-document-type "with-description"})))]
    
    (testing "test create action"
      (is (= (:type blog-feed) "feed"))
      (is (couchdb-id? (:_id blog-feed)))
      (is (couchdb-rev? (:_rev blog-feed)))
      (is (iso-date? (:created blog-feed)))
      (is (= (:title blog-feed) "Weblog"))
      (is (= (:subtitle blog-feed) "Vix Weblog!"))
      (is (= (:name blog-feed) "blog"))
      (is (= (:language blog-feed) "en"))
      (is (= (:default-slug-format blog-feed) "/{document-title}"))
      (is (= (:default-document-type blog-feed) "with-description")))

    ;; make sure that invalid actions aren't allowed
    (is (thrown+? (partial check-exc :vix.db/invalid-action)
                  (append-to-feed
                   +test-db+
                   (assoc blog-feed
                     :action :invalid
                     :previous-id (:_id blog-feed)
                     :title "Updated Weblog Feed"
                     :default-document-type "standard"
                     :searchable true))))

    (is (thrown+? (partial check-exc :vix.db/invalid-action)
                  (append-to-feed
                   +test-db+
                   (dissoc (assoc blog-feed
                             :previous-id (:_id blog-feed)
                             :title "Updated Weblog Feed"
                             :default-document-type "standard"
                             :searchable true)
                           :action))))


    ;; a non-keyword action should work:
    (append-to-feed
     +test-db+
     {:action :create
      :title "Weblog"
      :subtitle "Another Weblog!"
      :name "another-blog"
      :language "en"
      :default-slug-format "/{document-title}"
      :default-document-type "with-description"})

    (let [blog-feed-updated (with-redefs [util/now-rfc3339
                                          #(str "2012-09-04T04:30:17.930Z")]
                              (first
                               (append-to-feed
                                +test-db+
                                (assoc blog-feed
                                  :action :update
                                  :previous-id (:_id blog-feed)
                                  :title "Updated Weblog Feed"
                                  :default-document-type "standard"
                                  :searchable true))))]
      
      (testing "test update action"
        (is (= (first (get-feed +test-db+ "en" "blog")) blog-feed-updated))
        (is (couchdb-rev? (:_rev blog-feed-updated)))
        (is (iso-date? (:datestamp blog-feed-updated)))
        (is (= (:created blog-feed) (:created blog-feed-updated)))
        (is (= (:title blog-feed-updated) "Updated Weblog Feed"))
        (is (= (:subtitle blog-feed-updated) "Vix Weblog!"))
        (is (= (:language blog-feed-updated) "en"))
        (is (= (:name blog-feed-updated) "blog")) ; NB: not updated!
        (is (= (:default-slug-format blog-feed-updated) "/{document-title}"))
        (is (= (:default-document-type blog-feed-updated) "standard"))
        (is (= (:searchable blog-feed-updated) true))

        (is (thrown+? (partial check-exc :vix.db/feed-update-conflict)
                      (append-to-feed
                       +test-db+
                       (assoc blog-feed
                         :action :update))))

        (is (thrown+? (partial check-exc :vix.db/feed-update-conflict)
                      (append-to-feed
                       +test-db+
                       (assoc blog-feed
                         :action :update
                         :previous-id (:previous-id blog-feed))))))

      (testing "test delete action"
        (is (not (nil? (get-feed +test-db+ "en" "blog")))
            "Assure the feed exists before it is deleted.")

        (is (thrown+? (partial check-exc :vix.db/feed-update-conflict)
                      (append-to-feed
                       +test-db+
                       (assoc (dissoc blog-feed-updated :previous-id)
                         :action :delete))))

        (is (thrown+? (partial check-exc :vix.db/feed-update-conflict)
                      (append-to-feed
                       +test-db+
                       (assoc blog-feed-updated
                         :action :delete
                         :previous-id (:previous-id blog-feed)))))

        (let [deleted-feed (with-redefs [util/now-rfc3339
                                         #(str "2012-09-04T04:30:18.010Z")]
                             (append-to-feed
                              +test-db+
                              (assoc blog-feed-updated
                                :action
                                :delete
                                :previous-id
                                (:_id blog-feed-updated))))]

          (is (= (map #(dissoc % :_id :_rev :previous-id) deleted-feed)
                 [{:subtitle "Vix Weblog!"
                   :action :delete
                   :name "blog"
                   :language "en"
                   :title "Updated Weblog Feed"
                   :datestamp "2012-09-04T04:30:18.010Z"
                   :searchable true
                   :created "2012-09-04T04:30:17.872Z"
                   :type "feed"
                   :default-document-type "standard"
                   :default-slug-format "/{document-title}"}
                  {:subtitle "Vix Weblog!"
                   :action :update
                   :name "blog"
                   :language "en"
                   :title "Updated Weblog Feed"
                   :datestamp "2012-09-04T04:30:17.930Z"
                   :searchable true
                   :created "2012-09-04T04:30:17.872Z"
                   :type "feed"
                   :default-document-type "standard"
                   :default-slug-format "/{document-title}"}
                  {:subtitle "Vix Weblog!"
                   :action :create
                   :name "blog"
                   :language "en"
                   :title "Weblog"
                   :datestamp "2012-09-04T04:30:17.872Z"
                   :created "2012-09-04T04:30:17.872Z"
                   :type "feed"
                   :default-document-type "with-description"
                   :default-slug-format "/{document-title}"}]))

          (is (thrown+? (partial check-exc :vix.db/feed-already-deleted)
                        (append-to-feed
                         +test-db+
                         (assoc (first deleted-feed)
                           :action :delete
                           :previous-id (:_id (first deleted-feed))))))))))

  (testing "test feed-already-exists-conflict"
    (do
      (append-to-feed +test-db+
                      {:subtitle "bar"
                       :action :create
                       :name "foobar"
                       :language "en"
                       :title "Foobar"
                       :type "feed"
                       :default-document-type "with-description"
                       :default-slug-format "/{document-title}"}))
    
    (is (thrown+? (partial check-exc :vix.db/feed-already-exists-conflict)
                  (append-to-feed
                   +test-db+
                   {:subtitle "bar"
                    :action :create
                    :name "foobar"
                    :language "en"
                    :title "Foobar"
                    :type "feed"
                    :default-document-type "with-description"
                    :default-slug-format "/{document-title}"})))

    (do
      (append-to-feed +test-db+
                      {:subtitle "bar"
                       :action :delete
                       :previous-id (:_id
                                     (first
                                      (get-feed +test-db+ "en" "foobar")))
                       :name "foobar"
                       :language "en"
                       :title "Foobar"
                       :type "feed"
                       :default-document-type "with-description"
                       :default-slug-format "/{document-title}"})

      ;; once the feed is deleted, it should be possible to recreate
      (append-to-feed +test-db+
                      {:subtitle "bar"
                       :action :create
                       :name "foobar"
                       :language "en"
                       :title "Foobar"
                       :type "feed"
                       :default-document-type "with-description"
                       :default-slug-format "/{document-title}"}))))

(deftest test-get-documents-for-feed
  (let [doc-1 (append-to-document +test-db+
                                  "Europe/Amsterdam"
                                  {:action :create
                                   :language "en"
                                   :feed "blog"
                                   :title "foo"
                                   :slug "/blog/foo-0"
                                   :content "bar"
                                   :draft true})

        doc-2 (append-to-document +test-db+
                                  "Europe/Amsterdam"
                                  {:action :create
                                   :language "en"
                                   :feed "blog"
                                   :title "foo"
                                   :slug "/blog/foo-1"
                                   :content "bar"
                                   :draft true})

        doc-3 (append-to-document +test-db+
                                  "Europe/Amsterdam"
                                  {:action :create
                                   :language "nl"
                                   :feed "blog"
                                   :title "foo"
                                   :slug "/blog/foo-nl"
                                   :content "bar"
                                   :draft true})
        
        feed (get-documents-for-feed +test-db+ "en" "blog")]

    ;; FIXME: should also test other possible argument combinations!
    (is (= (count (:documents feed)) 2))

    (is (= (:next feed) nil))
    (is (some #{(first doc-1)} (:documents feed)))
    (is (some #{(first doc-2)} (:documents feed))))

  (testing "test pagination"
    (let [now "2011-09-06T12:56:16.322Z"]
      (dotimes [n 21]
        (let [my-now (if (< n 7) ;; mix identical and unique datestamps
                       now
                       (util/now-rfc3339))]
          (clutch/put-document +test-db+
                               {:action :create
                                :current-state true
                                :type "document"
                                :title (str "doc " n)
                                :slug (str "/pages/doc-" n)
                                :content ""
                                :draft false
                                :language "en"
                                :feed "pages"
                                :published my-now
                                :created my-now
                                :datestamp my-now}))))
    
    (is (= (count (:documents (get-documents-for-feed +test-db+
                                                      "en"
                                                      "pages")))
           21))

    (let [first-five (get-documents-for-feed +test-db+ "en" "pages" 5)]
      (is (= (count (:documents first-five)) 5))
      
      (is (= (:title (nth (:documents first-five) 0)) "doc 20"))
      (is (= (:title (nth (:documents first-five) 1)) "doc 19"))
      (is (= (:title (nth (:documents first-five) 2)) "doc 18"))
      (is (= (:title (nth (:documents first-five) 3)) "doc 17"))
      (is (= (:title (nth (:documents first-five) 4)) "doc 16"))

      (let [next-five (get-documents-for-feed +test-db+
                                              "en"
                                              "pages"
                                              5
                                              (:published (:next first-five))
                                              (:startkey_docid
                                               (:next first-five)))]
        
        (is (= (count (:documents next-five)) 5))

        (is (= (:title (nth (:documents next-five) 0)) "doc 15"))
        (is (= (:title (nth (:documents next-five) 1)) "doc 14"))
        (is (= (:title (nth (:documents next-five) 2)) "doc 13"))
        (is (= (:title (nth (:documents next-five) 3)) "doc 12"))
        (is (= (:title (nth (:documents next-five) 4)) "doc 11"))

        (let [next-ten (get-documents-for-feed +test-db+
                                               "en"
                                               "pages"
                                               10
                                               (:published (:next next-five))
                                               (:startkey_docid
                                                (:next next-five)))]

          (is (= (count (:documents next-ten)) 10))

          (is (= (:title (nth (:documents next-ten) 0)) "doc 10"))
          (is (= (:title (nth (:documents next-ten) 1)) "doc 9"))
          (is (= (:title (nth (:documents next-ten) 2)) "doc 8"))
          (is (= (:title (nth (:documents next-ten) 3)) "doc 7"))
          (is (= (:title (nth (:documents next-ten) 4)) "doc 6"))
          (is (= (:title (nth (:documents next-ten) 5)) "doc 5"))
          (is (= (:title (nth (:documents next-ten) 6)) "doc 4"))
          (is (= (:title (nth (:documents next-ten) 7)) "doc 3"))
          (is (= (:title (nth (:documents next-ten) 8)) "doc 2"))
          (is (= (:title (nth (:documents next-ten) 9)) "doc 1"))

          (is (= (:published (nth (:documents next-ten) 4))
                 (:published (nth (:documents next-ten) 5))
                 (:published (nth (:documents next-ten) 6))
                 (:published (nth (:documents next-ten) 7))
                 (:published (nth (:documents next-ten) 7))
                 (:published (nth (:documents next-ten) 9))
                 "2011-09-06T12:56:16.322Z"))

          (let [last-doc (get-documents-for-feed +test-db+
                                                 "en"
                                                 "pages"
                                                 1
                                                 (:published
                                                  (:next next-ten))
                                                 (:startkey_docid
                                                  (:next next-ten)))
                x2 (get-documents-for-feed +test-db+
                                           "en"
                                           "pages"
                                           10
                                           (:published
                                            (:next next-ten))
                                           (:startkey_docid
                                            (:next next-ten)))]
            (is (= last-doc x2))
            (is (nil? (:next last-doc)))
            (is (= (:published (nth (:documents last-doc) 0))
                   "2011-09-06T12:56:16.322Z"))
            (is (= (:title (nth (:documents last-doc) 0)) "doc 0"))))))))

(deftest test-list-feeds-and-list-feeds-by-default-document-type
  (let [blog-feed (first
                   (append-to-feed
                    +test-db+
                    {:action :create
                     :title "Weblog"
                     :subtitle "Vix Weblog!"
                     :language "en"
                     :name "blog"
                     :default-slug-format
                     "/{feed-name}/{document-title}"
                     :default-document-type "with-description"}))
        pages-feed (first
                    (append-to-feed
                     +test-db+
                     {:action :create
                      :title "Pages"
                      :subtitle "Web Pages"
                      :language "en"
                      :name "pages"
                      :default-slug-format "/{document-title}"
                      :default-document-type "standard"}))
        images-feed (first
                     (append-to-feed
                      +test-db+
                      {:action :create
                       :title "Images"
                       :subtitle "Internal feed with images"
                       :language "en"
                       :name "images"
                       :default-slug-format
                       "/media/{document-title}"
                       :default-document-type "image"}))
        blog-feed-nl (first
                      (append-to-feed
                       +test-db+
                       {:action :create
                        :title "Weblog"
                        :subtitle "Vix Weblog!"
                        :language "nl"
                        :name "blog"
                        :default-slug-format
                        "/{feed-name}/{document-title}"
                        :default-document-type
                        "with-description"}))
        pages-feed-nl (first
                       (append-to-feed
                        +test-db+
                        {:action :create
                         :title "Pages"
                         :subtitle "Web Pages"
                         :language "nl"
                         :name "pages"
                         :default-slug-format "/{document-title}"
                         :default-document-type "standard"}))
        ;; do an update, to make sure only the most recent version is used
        images-feed-nl (first
                        (append-to-feed
                         +test-db+
                         (let [images-feed-nl-0
                               (first
                                (append-to-feed
                                 +test-db+
                                 {:action :create
                                  :title "Images"
                                  :subtitle "Internal feed with images"
                                  :language "nl"
                                  :name "images"
                                  :default-slug-format
                                  "/media/{document-title}"
                                  :default-document-type "image"}))]
                           (assoc images-feed-nl-0
                             :action :update
                             :previous-id (:_id images-feed-nl-0)))))]

    (do
      ;; create and remove a feed, to make sure it isn't included
      (append-to-feed
       +test-db+
       (let [feed-0
             (first
              (append-to-feed
               +test-db+
               {:action :create
                :title "Images (deleted)"
                :subtitle "Internal feed with images"
                :language "en"
                :name "images-delete"
                :default-slug-format
                "/media/{document-title}"
                :default-document-type "image"}))]
         (assoc feed-0
           :action :delete
           :previous-id (:_id feed-0)))))

    (testing "test without providing a language"
      (is (= [pages-feed-nl
              images-feed-nl
              blog-feed-nl
              pages-feed
              images-feed
              blog-feed]
             (list-feeds +test-db+))))

    (testing "test with a language argument"
      (is (= [pages-feed
              images-feed
              blog-feed]
             (list-feeds +test-db+ "en")))

      (is (= [pages-feed-nl
              images-feed-nl
              blog-feed-nl]
             (list-feeds +test-db+ "nl"))))


    (testing "test with default-document-type without a language argument"
      (is (= (list-feeds-by-default-document-type +test-db+
                                                  "image")
             [images-feed-nl images-feed])))

    (testing "test with default-document-type and  a language argument"
      (is (= (list-feeds-by-default-document-type +test-db+
                                                  "image"
                                                  "en")
             [images-feed]))

      (is (= (list-feeds-by-default-document-type +test-db+
                                                  "image"
                                                  "nl")
             [images-feed-nl])))))

(deftest test-append-to-document-create
  (with-redefs [util/now-rfc3339 #(str "2012-09-16T00:17:30.722Z")]
    (let [document (append-to-document +test-db+
                                       "Europe/Amsterdam"
                                       {:action :create
                                        :language "en"
                                        :feed "blog"
                                        :title "foo"
                                        :slug "/blog/foo"
                                        :content "bar"
                                        :start-time "2012-02-21 01:19"
                                        :end-time "2012-02-21 10:00"
                                        :draft false})]

      (is (couchdb-id? (:_id (first document))))
      (is (couchdb-rev? (:_rev (first document))))

      (is (= (vec (map #(dissoc % :_id :_rev) document))
             [{:slug "/blog/foo"
               :content "bar"
               :action :create
               :language "en"
               :title "foo"
               :start-time-rfc3339 "2012-02-21T00:19:00.000Z"
               :published "2012-09-16T00:17:30.722Z"
               :datestamp "2012-09-16T00:17:30.722Z"
               :created "2012-09-16T00:17:30.722Z"
               :start-time "2012-02-21 01:19"
               :end-time "2012-02-21 10:00"
               :type "document"
               :feed "blog"
               :draft false
               :end-time-rfc3339 "2012-02-21T09:00:00.000Z"}]))))

  ;; make sure valid actions are enforced
  (is (thrown+? (partial check-exc :vix.db/invalid-action)
                (append-to-document
                 +test-db+
                 "Europe/Amsterdam"
                 {:action :invalid
                  :language "en"
                  :feed "blog"
                  :title "foo"
                  :slug "/blog/foo-action"
                  :content "bar"
                  :start-time "2012-02-21 01:19"
                  :end-time "2012-02-21 10:00"
                  :draft false})))

  (is (thrown+? (partial check-exc :vix.db/invalid-action)
                (append-to-document
                 +test-db+
                 "Europe/Amsterdam"
                 {:language "en"
                  :feed "blog"
                  :title "foo"
                  :slug "/blog/foo-action"
                  :content "bar"
                  :start-time "2012-02-21 01:19"
                  :end-time "2012-02-21 10:00"
                  :draft false})))

  ;; non-keyword actions should work:
  (append-to-document +test-db+
                      "Europe/Amsterdam"
                      {:action "create"
                       :language "en"
                       :feed "blog"
                       :title "foo"
                       :slug "/blog/foo-action"
                       :content "bar"
                       :start-time "2012-02-21 01:19"
                       :end-time "2012-02-21 10:00"
                       :draft false})
  
  ;; make sure existing documents don't get overwritten
  (is (thrown+? (partial check-exc :vix.db/document-already-exists-conflict)
                (append-to-document
                 +test-db+
                 "Europe/Amsterdam"
                 {:action :create
                  :language "en"
                  :feed "blog"
                  :title "foo"
                  :slug "/blog/foo"
                  :content "bar"
                  :start-time "2012-02-21 01:19"
                  :end-time "2012-02-21 10:00"
                  :draft false})))

  ;; test without a :language key
  (is (thrown+? (partial check-exc :vix.db/document-missing-required-keys)
                (append-to-document
                 +test-db+
                 "Europe/Amsterdam"
                 {:action :create
                  :feed "blog"
                  :title "foo"
                  :slug "/blog/foobar"
                  :content "bar"
                  :start-time "2012-02-21 01:19"
                  :end-time "2012-02-21 10:00"
                  :draft false})))

  ;; test without a :feed key
  (is (thrown+? (partial check-exc :vix.db/document-missing-required-keys)
                (append-to-document
                 +test-db+
                 "Europe/Amsterdam"
                 {:action :create
                  :language "en"
                  :title "foo"
                  :slug "/blog/foobar"
                  :content "bar"
                  :start-time "2012-02-21 01:19"
                  :end-time "2012-02-21 10:00"
                  :draft false})))

  ;; test without a :slug key
  (is (thrown+? (partial check-exc :vix.db/document-missing-required-keys)
                (append-to-document
                 +test-db+
                 "Europe/Amsterdam"
                 {:action :create
                  :language "en"
                  :feed "blog"
                  :title "foo"
                  :content "bar"
                  :start-time "2012-02-21 01:19"
                  :end-time "2012-02-21 10:00"
                  :draft false})))

  ;; test without a :title key
  (is (thrown+? (partial check-exc :vix.db/document-missing-required-keys)
                (append-to-document
                 +test-db+
                 "Europe/Amsterdam"
                 {:action :create
                  :language "en"
                  :feed "blog"
                  :slug "/blog/foobar"
                  :content "bar"
                  :start-time "2012-02-21 01:19"
                  :end-time "2012-02-21 10:00"
                  :draft false})))

  (testing "Test if attachments are handled correctly."
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")
          document (first
                    (append-to-document +test-db+
                                        "Europe/Amsterdam"
                                        {:action :create
                                         :attachment {:type "image/gif"
                                                      :data gif}
                                         :title "a single black pixel!"
                                         :language "en"
                                         :feed "images"
                                         :slug "pixel.gif"
                                         :content ""
                                         :draft false}))]
      (is (= (:original (:attachments document))
             {:type "image/gif"
              :length 57
              :data (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+"
                         "9BAQUA++/vQAsAAAAAAEAAQAAAgJEAQA7")}))

      (is (= (get-attachment-as-base64-string +test-db+
                                              (:_id document)
                                              :original)
             gif)))))

(deftest test-append-to-document-update
  (with-redefs [util/now-rfc3339 #(str "2012-09-16T02:51:47.588Z")]
    (let [new-doc (append-to-document
                   +test-db+
                   "Europe/Amsterdam"
                   {:action :create
                    :language "en"
                    :feed "blog"
                    :title "foo"
                    :subtitle ""
                    :slug "/blog/bar"
                    :content "bar"
                    :description ""
                    :draft false
                    :icon nil
                    :related-pages []
                    :related-images []})
          updated-doc (append-to-document
                       +test-db+
                       "Europe/Amsterdam"
                       {:action :update
                        :previous-id (:_id (first new-doc))
                        :language "en"
                        :feed "blog"
                        :slug "/blog/bar"
                        :title "hic sunt dracones"
                        :subtitle "old maps are cool!"
                        :description "hey!"
                        :draft true
                        :start-time "2012-02-21 01:19"
                        :end-time "2012-02-21 10:00"
                        :icon {:title "cat" :slug "/cat.png"}
                        :related-pages [{:title "foo" :slug "bar"}]
                        :related-images [{:title "cat" :slug "cat.png"}]})]
      (is (= (get-document +test-db+ "/blog/bar") updated-doc))
      (is (couchdb-rev? 1 (:_rev (first updated-doc))))
      (is (couchdb-id? (:previous-id (first updated-doc))))
      
      (is (= (map #(dissoc % :_id :_rev :previous-id) updated-doc)
             [{:subtitle "old maps are cool!"
               :slug "/blog/bar"
               :icon {:slug "/cat.png"
                      :title "cat"}
               :action :update
               :related-images [{:slug "cat.png"
                                 :title "cat"}]
               :language "en"
               :title "hic sunt dracones"
               :start-time-rfc3339 "2012-02-21T00:19:00.000Z"
               :published "2012-09-16T02:51:47.588Z"
               :datestamp "2012-09-16T02:51:47.588Z"
               :created "2012-09-16T02:51:47.588Z"
               :start-time "2012-02-21 01:19"
               :end-time "2012-02-21 10:00"
               :type "document"
               :feed "blog"
               :draft true
               :related-pages [{:slug "bar"
                                :title "foo"}]
               :description "hey!"
               :end-time-rfc3339 "2012-02-21T09:00:00.000Z"}
              {:subtitle ""
               :slug "/blog/bar"
               :icon nil
               :content "bar"
               :action :create
               :related-images []
               :language "en"
               :title "foo"
               :published "2012-09-16T02:51:47.588Z"
               :datestamp "2012-09-16T02:51:47.588Z"
               :created "2012-09-16T02:51:47.588Z"
               :type "document"
               :feed "blog"
               :draft false
               :related-pages []
               :description ""}]))

      ;; make sure that the internal current-state flag is removed
      ;; from non-current document states (this non-public flag is
      ;; used for overview views that show e.g. the most recent states
      ;; for documents in a specific feed).
      (is (= (:current-state
              (clutch/get-document +test-db+ (get-in updated-doc [0 :_id])))
             true))

      (is (= (:current-state
              (clutch/get-document +test-db+ (get-in updated-doc [1 :_id])))
             false))

      ;; test with expired :previous-id
      (is (thrown+? (partial check-exc :vix.db/document-update-conflict)
                    (append-to-document
                     +test-db+
                     "Europe/Amsterdam"
                     {:action :update
                      :previous-id (:_id (first new-doc))
                      :language "en"
                      :feed "blog"
                      :slug "/blog/bar"
                      :title "hic sunt dracones"
                      :subtitle "old maps are cool!"
                      :description "hey!"
                      :draft true
                      :start-time "2012-02-21 01:19"
                      :end-time "2012-02-21 10:00"
                      :icon {:title "cat" :slug "/cat.png"}
                      :related-pages [{:title "foo" :slug "bar"}]
                      :related-images [{:title "cat" :slug "cat.png"}]})))

      ;; test without :previous-id
      (is (thrown+? (partial check-exc :vix.db/document-update-conflict)
                    (append-to-document
                     +test-db+
                     "Europe/Amsterdam"
                     {:action :update
                      :language "en"
                      :feed "blog"
                      :slug "/blog/bar"
                      :title "hic sunt dracones"
                      :subtitle "old maps are cool!"
                      :description "hey!"
                      :draft true
                      :start-time "2012-02-21 01:19"
                      :end-time "2012-02-21 10:00"
                      :icon {:title "cat" :slug "/cat.png"}
                      :related-pages [{:title "foo" :slug "bar"}]
                      :related-images [{:title "cat" :slug "cat.png"}]})))

      ;; make sure that passing couchdb-options works (useful for
      ;; limiting returned states on frequently updated image
      ;; documents).
      (is (= (map #(dissoc % :_rev :_id :previous-id)
                  (append-to-document
                   +test-db+
                   "Europe/Amsterdam"
                   {:action :update
                    :previous-id (:_id (first (get-document +test-db+
                                                            "/blog/bar")))
                    :language "en"
                    :feed "blog"
                    :slug "/blog/bar"
                    :title "here be dragons"
                    :subtitle "old maps are cool!"
                    :description "hey!"
                    :draft true
                    :start-time "2012-02-21 01:19"
                    :end-time "2012-02-21 10:00"
                    :icon {:title "cat" :slug "/cat.png"}
                    :related-pages [{:title "foo" :slug "bar"}]
                    :related-images [{:title "cat" :slug "cat.png"}]}
                   {:limit 1}))
             [{:subtitle "old maps are cool!"
               :slug "/blog/bar"
               :icon {:slug "/cat.png"
                      :title "cat"}
               :action :update
               :related-images [{:slug "cat.png"
                                 :title "cat"}]
               :language "en"
               :title "here be dragons"
               :start-time-rfc3339 "2012-02-21T00:19:00.000Z"
               :datestamp "2012-09-16T02:51:47.588Z"
               :created "2012-09-16T02:51:47.588Z"
               :published "2012-09-16T02:51:47.588Z"
               :start-time "2012-02-21 01:19"
               :end-time "2012-02-21 10:00"
               :type "document"
               :feed "blog"
               :draft true
               :related-pages [{:slug "bar"
                                :title "foo"}]
               :description "hey!"
               :end-time-rfc3339 "2012-02-21T09:00:00.000Z"}]))))

  (testing "Test if attachments are handled correctly."
    (let [black-pixel
          (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQU"
               "A++/vQAsAAAAAAEAAQAAAgJEAQA7")
          white-pixel
          (str "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJ"
               "CQgKDBQNDAsLDBkSEw8UHRofHh0aHBwgJC4nICIsIxwcKDcp"
               "LDAxNDQ0Hyc5PTgyPC4zNDL/2wBDAQkJCQwLDBgNDRgyIRwh"
               "MjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIyMjIy"
               "MjIyMjIyMjIyMjIyMjL/wAARCAABAAEDASIAAhEBAxEB/8QA"
               "HwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAA"
               "AgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKB"
               "kaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6"
               "Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWG"
               "h4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXG"
               "x8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QA"
               "HwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREA"
               "AgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEI"
               "FEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5"
               "OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOE"
               "hYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPE"
               "xcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oA"
               "DAMBAAIRAxEAPwD3+iiigD//2Q==")
          new-doc
          (append-to-document
           +test-db+
           "Europe/Amsterdam"
           {:action :create
            :attachment {:type "image/jpeg" :data white-pixel}
            :language "en"
            :feed "images"
            :slug "/pixel.jpeg"
            :title "a single white pixel!"
            :content ""
            :draft false})
          updated-doc
          (append-to-document
           +test-db+
           "Europe/Amsterdam"
           {:action :update
            :attachment {:type "image/gif" :data black-pixel}
            :previous-id (:_id (first new-doc))
            :language "en"
            :feed "images"
            :slug "/pixel.jpeg"
            :title "a single black pixel!"
            :content ""
            :draft false})]

      ;; check if the image has actually been updated:
      (is (= (get-in updated-doc [0 :attachments])
             {:original {:type "image/gif"
                         :length 57
                         :data black-pixel}}))

      ;; The attachment for the previous state should also be included
      ;; in the result of the update action.
      (is (= (get-in updated-doc [1 :attachments])
             {:original {:type "image/jpeg"
                         :length 631
                         :data white-pixel}}))

      (is (= (get-attachment-as-base64-string +test-db+
                                              (:_id (first updated-doc))
                                              :original)
             black-pixel)))))

(deftest test-append-to-document-delete
  (with-redefs [util/now-rfc3339 #(str "2012-09-16T03:43:20.953Z")]
    (let [doc (append-to-document +test-db+
                                  "Europe/Amsterdam"
                                  {:action :create
                                   :language "en"
                                   :feed "blog"
                                   :slug "/blog/bar"
                                   :title "foo"
                                   :content "bar"
                                   :draft false})]

      (is (not (nil? (first (get-document +test-db+ "/blog/bar"))))
          "Assure the document exists before it is deleted.")

      (is (= (map #(dissoc % :_id :_rev :previous-id)
                  (append-to-document +test-db+
                                      "Europe/Amsterdam"
                                      {:action :delete
                                       :previous-id (:_id (first doc))
                                       :language "en"
                                       :feed "blog"
                                       :slug "/blog/bar"
                                       :title "foo"
                                       :content "bar"
                                       :draft false}))
             [{:slug "/blog/bar"
               :content "bar"
               :action :delete
               :language "en"
               :title "foo"
               :published "2012-09-16T03:43:20.953Z"
               :datestamp "2012-09-16T03:43:20.953Z"
               :created "2012-09-16T03:43:20.953Z"
               :type "document"
               :feed "blog"
               :draft false}
              {:slug "/blog/bar"
               :content "bar"
               :action :create
               :language "en"
               :title "foo"
               :published "2012-09-16T03:43:20.953Z"
               :datestamp "2012-09-16T03:43:20.953Z"
               :created "2012-09-16T03:43:20.953Z"
               :type "document"
               :feed "blog"
               :draft false}]))

      ;; make sure documents can't be deleted twice
      (is (thrown+? (partial check-exc :vix.db/document-already-deleted)
                    (append-to-document
                     +test-db+
                     "Europe/Amsterdam"
                     {:action :delete
                      :previous-id (:_id
                                    (first
                                     (get-document +test-db+
                                                   "/blog/bar")))
                      :language "en"
                      :feed "blog"
                      :slug "/blog/bar"
                      :title "foo"
                      :content "bar"
                      :draft false}))))))

(deftest test-get-available-languages
  (do
    (append-to-feed +test-db+
                    {:action :create
                     :title "Weblog"
                     :subtitle "Vix Weblog!"
                     :name "blog"
                     :language "en"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Images"
                     :subtitle "Images"
                     :name "images"
                     :language "en"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Menu"
                     :subtitle "Menu"
                     :name "menu"
                     :language "en"
                     :searchable false})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Weblog"
                     :name "blog"
                     :language "nl"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Weblog"
                     :name "blog"
                     :language "de"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :delete
                     :previous-id (:_id
                                   (first
                                    (get-feed +test-db+ "de" "blog")))
                     :title "Weblog"
                     :name "blog"
                     :language "de"
                     :searchable true}))

  (is (= (get-available-languages +test-db+) ["en" "nl"])))

(deftest test-get-languages
  (do
    (append-to-feed +test-db+
                    {:action :create
                     :title "Weblog"
                     :subtitle "Vix Weblog!"
                     :name "blog"
                     :language "en"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Images"
                     :subtitle "Images"
                     :name "images"
                     :language "en"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Menu"
                     :subtitle "Menu"
                     :name "menu"
                     :language "en"
                     :searchable false})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Weblog"
                     :name "blog"
                     :language "nl"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Weblog"
                     :name "blog"
                     :language "de"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :delete
                     :previous-id (:_id
                                   (first
                                    (get-feed +test-db+ "de" "blog")))
                     :title "Weblog"
                     :name "blog"
                     :language "de"
                     :searchable true}))

  (is (= (get-languages (list-feeds +test-db+)) #{"en" "nl"})))

(deftest test-get-searchable-feeds
  (do
    (append-to-feed +test-db+
                    {:action :create
                     :title "Weblog"
                     :subtitle "Vix Weblog!"
                     :name "blog"
                     :language "en"
                     :searchable true})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Images"
                     :subtitle "Images"
                     :name "images"
                     :language "en"
                     :searchable true})

    (append-to-feed +test-db+ 
                    {:action :create
                     :title "Menu"
                     :subtitle "Menu"
                     :name "menu"
                     :language "en"
                     :searchable false})

    (append-to-feed +test-db+
                    {:action :create
                     :title "Weblog"
                     :name "blog"
                     :language "nl"
                     :searchable true}))

  (is (= (get-searchable-feeds (list-feeds +test-db+))
         {"nl" ["blog"]
          "en" ["images" "blog"]})))

(deftest test-get-most-recent-event-documents
  (let [doc-1 (append-to-document +test-db+
                                  "Europe/Amsterdam"
                                  {:action
                                   :create
                                   :language
                                   "en"
                                   :feed
                                   "events"
                                   :title
                                   "Tomasz Stańko Middelburg"
                                   :slug
                                   "/en/events/stanko-middelburg"
                                   :content
                                   (str "The legendary Polish trumpet "
                                        "player Stańko will be playing "
                                        "in Middelburg.")
                                   :start-time
                                   "2012-04-25 20:30"
                                   :end-time
                                   "2012-04-25 23:59"
                                   :draft false})

        doc-2 (append-to-document +test-db+
                                  "Europe/Amsterdam"
                                  {:action
                                   :create
                                   :language
                                   "en"
                                   :feed
                                   "events"
                                   :title
                                   "The Impossible Gentlemen"
                                   :slug
                                   (str "/en/events/impossible-gentlemen"
                                        "-amsterdam")
                                   :content
                                   (str "Gwilym Simcock, Mike Walker, "
                                        "Adam Nussbaum, Steve Swallow "
                                        "will be playing at the Bimhuis "
                                        "in Amsterdam.")
                                   :start-time
                                   "2012-07-06 20:30"
                                   :end-time
                                   "2012-07-06 23:59"
                                   :draft false})

        doc-3 (append-to-document +test-db+
                                  "Europe/Amsterdam"
                                  {:action
                                   :create
                                   :language
                                   "en"
                                   :feed
                                   "events"
                                   :title
                                   "Yuri Honing"
                                   :slug
                                   "/en/events/yuri-honing-tilburg"
                                   :content
                                   (str "VPRO/Boy Edgar prize winner "
                                        "Yuri Honing will be playing at "
                                        "the Paradox venue in Tilburg.")
                                   :start-time
                                   "2013-02-01 20:30"
                                   :end-time
                                   "2013-02-01 23:59"
                                   :draft false})]

    (is (= (get-most-recent-event-documents +test-db+
                                            "en"
                                            "events")
           (get-most-recent-event-documents +test-db+
                                            "en"
                                            "events"
                                            nil)
           (map first [doc-3 doc-2 doc-1])))

    ;; when limited, the fn retrieves (inc limit)
    (is (= (get-most-recent-event-documents +test-db+
                                            "en"
                                            "events"
                                            1)
           [(first doc-3)]))))