;; test/vix/test/db.clj tests for db namespace.
;;
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

(ns vix.test.db
  (:use [vix.db] :reload)
  (:use [clojure.test]
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

    (is (= (count (:views view-doc)) 8))
    
    (is (= (:map (:feeds (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"feed\") {\n"
                "        emit([doc.language, doc.name], doc);\n"
                "    }\n"
                "}\n")))

    (is (= (:map (:feeds_by_default_document_type (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"feed\") {\n"
                "        emit([doc[\"default-document-type\"],\n"
                "              doc[\"language\"],\n"
                "              doc[\"name\"]],\n"
                "             doc);\n"
                "    }\n"
                "}\n")))
    
    (is (= (:map (:by_slug (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"document\") {\n"
                "        emit(doc.slug, doc);\n"
                "    }\n"
                "}\n")))
    
    (is (= (:map (:by_feed (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"document\") {\n"
                "        emit([[doc.language, doc.feed], doc.published]"
                ", doc);\n"
                "    }\n"
                "}\n")))

    (is (= (:map (:by_username (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"user\") {\n"
                "        emit(doc.username, doc);\n"
                "    }\n"
                "}\n")))

    (is (= (:map (:events_by_feed (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"document\" "
                "&& doc[\"end-time-rfc3339\"]) {\n"
                "        emit([[doc.language, doc.feed], "
                "doc[\"end-time-rfc3339\"]], doc);\n    }\n}\n")))

    (is (= (:map (:subscribers (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"newsletter-subscriber\") {\n"
                "        emit([doc.language, doc.email], doc);\n"
                "    }\n}\n")))

    (is (= (:map (:languages (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"feed\") {\n"
                "        emit(doc.language, null);\n"
                "    }\n}\n")))

    (is (= (:reduce (:languages (:views view-doc)))
           "function(k,v) {\n    return null;\n}\n"))))

(deftest test-get-attachment-as-base64-string
  (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                 "vQAsAAAAAAEAAQAAAgJEAQA7")
        document (create-document +test-db+
                                  "en"
                                  "images"
                                  "Europe/Amsterdam"
                                  {:attachment {:type "image/gif"
                                                :data gif}
                                   :title "a single black pixel!"
                                   :slug "pixel.gif"
                                   :content ""
                                   :draft false})]
    (is (= (get-attachment-as-base64-string +test-db+ document :original)
           gif))))

(deftest test-get-document
  (do
    (create-document +test-db+
                     "en"
                     "blog"
                     "Europe/Amsterdam"
                     {:title "foo"
                      :slug "/blog/foo"
                      :content "bar"
                      :draft true}))

                                        ; as we didn't create the view manually here, this test also implies
                                        ; views are created automatically by get-document
  (let [document (get-document +test-db+ "/blog/foo")]
    (is (couchdb-id? (:_id document)))
    (is (couchdb-rev? (:_rev document)))
    (is (iso-date? (:published document)))
    (is (= (:language document) "en"))
    (is (= (:feed document) "blog"))
    (is (= (:title document) "foo"))
    (is (= (:slug document) (str "/blog/foo")))
    (is (= (:content document) "bar"))
    (is (true? (:draft document))))

  (testing "Test if attachments are handled correctly."
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")]
      (do
       (create-document +test-db+
                        "en"
                        "images"
                        "Europe/Amsterdam"
                        {:attachment {:type "image/gif" :data gif}
                         :title "a single black pixel!"
                         :slug "/images/white-pixel.gif"
                         :content ""
                         :draft false})
       (create-document +test-db+
                        "en"
                        "images"
                        "Europe/Amsterdam"
                        {:title "not a single black pixel!"
                         :slug "/images/not-a-white-pixel.gif"
                         :content ""
                         :draft false}))

      (is (= (:attachment (get-document +test-db+
                                        "/images/white-pixel.gif"
                                        true))
             {:type "image/gif" :data gif}))

      (is (nil? (:attachment (get-document +test-db+
                                           "/images/not-a-white-pixel.gif"
                                           true)))))))

(deftest test-get-feed
  (do
    (create-feed +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :default-slug-format "/{document-title}"
                  :default-document-type "with-description"
                  :language "en"})
    (create-feed +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :default-slug-format "/{document-title}"
                  :default-document-type "with-description"
                  :language "nl"}))

  (is (nil? (get-feed +test-db+ "de" "blog")))

  (is (= (:title (get-feed +test-db+ "en" "blog"))
         (:title (get-feed +test-db+ "nl" "blog"))))
  
  (let [feed (get-feed +test-db+ "nl" "blog")]
    (is (= (:type feed) "feed"))
    (is (couchdb-id? (:_id feed)))
    (is (couchdb-rev? (:_rev feed)))
    (is (iso-date? (:created feed)))
    (is (= (:title feed) "Weblog"))
    (is (= (:subtitle feed) "Vix Weblog!"))
    (is (= (:name feed) "blog"))
    (is (= (:default-slug-format feed) "/{document-title}"))
    (is (= (:default-document-type feed) "with-description"))))

(deftest test-get-unique-slug
  (is (= (get-unique-slug +test-db+ "/blog/foo") "/blog/foo"))

  (do
    (create-document +test-db+
                     "en"
                     "blog"
                     "Europe/Amsterdam"
                     {:title "foo"
                      :slug "/blog/foo-1234567890"
                      :content "bar"
                      :draft true})

    (create-document +test-db+
                     "en"
                     "blog"
                     "Europe/Amsterdam"
                     {:title "foo"
                      :slug "/blog/foo-1234567891"
                      :content "bar"
                      :draft true}))

  ; this should retrieve the next available slug:
  (is (= (get-unique-slug +test-db+ "/blog/foo-1234567890")
         "/blog/foo-1234567892")))

(deftest test-create-document
  (testing "Test document creation"
    (let [document (create-document +test-db+
                                    "en"
                                    "blog"
                                    "Europe/Amsterdam"
                                    {:title "foo"
                                     :slug "/blog/foo"
                                     :content "bar"
                                     :start-time "2012-02-21 01:19"
                                     :end-time "2012-02-21 10:00"
                                     :draft false})]

      (is (couchdb-id? (:_id document)))
      (is (couchdb-rev? (:_rev document)))
      (is (iso-date? (:published document)))
      (is (= (:type document) "document"))
      (is (= (:language document) "en"))
      (is (= (:feed document) "blog"))
      (is (= (:title document) "foo"))
      (is (= (:slug document) "/blog/foo"))
      (is (= (:content document) "bar"))
      (is (= (:start-time document) "2012-02-21 01:19"))
      (is (= (:end-time document) "2012-02-21 10:00"))
      (is (= (:start-time-rfc3339 document) "2012-02-21T00:19:00.000Z"))
      (is (= (:end-time-rfc3339 document) "2012-02-21T09:00:00.000Z"))
      (is (false? (:draft document)))))

  (testing "Test if slugs are correctly autoincremented"
    (dotimes [n 10]
      (let [document (create-document +test-db+
                                      "en"
                                      "blog"
                                      "Europe/Amsterdam"
                                      {:title "foo"
                                       :slug "/blog/foo"
                                       :content "bar"
                                       :draft true})]
        (is (couchdb-id? (:_id document)))
        (is (couchdb-rev? (:_rev document)))
        (is (iso-date? (:published document)))
        (is (= (:language document) "en"))
        (is (= (:feed document) "blog"))
        (is (= (:title document) "foo"))
        (is (= (:slug document) (str "/blog/foo-" (+ n 2))))
        (is (= (:content document) "bar"))
        (is (true? (:draft document))))))


  (testing "Test if attachments are handled correctly."
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")
          document (create-document +test-db+
                                    "en"
                                    "images"
                                    "Europe/Amsterdam"
                                    {:attachment {:type "image/gif"
                                                  :data gif}
                                     :title "a single black pixel!"
                                     :slug "pixel.gif"
                                     :content ""
                                     :draft false})]
      (is (= (:attachment document)
             {:type "image/gif"
              :data (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+"
                         "9BAQUA++/vQAsAAAAAAEAAQAAAgJEAQA7")}))
      (is (= (:_attachments document)
             {:original {:content_type "image/gif"
                         :revpos 2
                         :length 57
                         :stub true}}))

      (is (= (get-attachment-as-base64-string +test-db+ document :original)
             gif)))))

(deftest test-create-feed
  (let [feed (create-feed +test-db+
                          {:title "Weblog"
                           :subtitle "Vix Weblog!"
                           :name "blog"
                           :language "en"
                           :default-slug-format "/{document-title}"
                           :default-document-type "with-description"})]
    
    (is (= (:type feed) "feed"))
    (is (couchdb-id? (:_id feed)))
    (is (couchdb-rev? (:_rev feed)))
    (is (iso-date? (:created feed)))
    (is (= (:title feed) "Weblog"))
    (is (= (:subtitle feed) "Vix Weblog!"))
    (is (= (:name feed) "blog"))
    (is (= (:language feed) "en"))
    (is (= (:default-slug-format feed) "/{document-title}"))
    (is (= (:default-document-type feed) "with-description"))))

(deftest test-get-documents-for-feed
  (let [doc-1 (create-document +test-db+
                               "en"
                               "blog"
                               "Europe/Amsterdam"
                               {:title "foo"
                                :slug "/blog/foo"
                                :content "bar"
                                :draft true})

        doc-2 (create-document +test-db+
                               "en"
                               "blog"
                               "Europe/Amsterdam"
                               {:title "foo"
                                :slug "/blog/foo"
                                :content "bar"
                                :draft true})

        doc-3 (create-document +test-db+
                               "nl"
                               "blog"
                               "Europe/Amsterdam"
                               {:title "foo"
                                :slug "/blog/foo"
                                :content "bar"
                                :draft true})
        
        feed (get-documents-for-feed +test-db+ "en" "blog")]

    ;; FIXME: should also test other possible argument combinations!
    (is (= (count (:documents feed)) 2))

    (is (= (:next feed) nil))
    (is (some #{doc-1} (:documents feed)))
    (is (some #{doc-2} (:documents feed))))

  (testing "test pagination"
    (let [now "2011-09-06T12:56:16.322Z"]
      (dotimes [n 21]
        (clutch/put-document +test-db+
                             {:type "document"
                              :title (str "doc " n)
                              :slug (str "/pages/doc-" n)
                              :content ""
                              :draft false
                              :language "en"
                              :feed "pages"
                                        ; mix identical and unique dates
                              :published (if (< n 7)
                                           now
                                           (util/now-rfc3339))})))
    
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
  (let [blog-feed (create-feed +test-db+
                               {:title "Weblog"
                                :subtitle "Vix Weblog!"
                                :language "en"
                                :name "blog"
                                :default-slug-format
                                "/{feed-name}/{document-title}"
                                :default-document-type "with-description"})
        pages-feed (create-feed +test-db+
                                {:title "Pages"
                                 :subtitle "Web Pages"
                                 :language "en"
                                 :name "pages"
                                 :default-slug-format "/{document-title}"
                                 :default-document-type "standard"})
        images-feed (create-feed +test-db+
                                 {:title "Images"
                                  :subtitle "Internal feed with images"
                                  :language "en"
                                  :name "images"
                                  :default-slug-format
                                  "/media/{document-title}"
                                  :default-document-type "image"})
        blog-feed-nl (create-feed +test-db+
                                  {:title "Weblog"
                                   :subtitle "Vix Weblog!"
                                   :language "nl"
                                   :name "blog"
                                   :default-slug-format
                                   "/{feed-name}/{document-title}"
                                   :default-document-type
                                   "with-description"})
        pages-feed-nl (create-feed +test-db+
                                   {:title "Pages"
                                    :subtitle "Web Pages"
                                    :language "nl"
                                    :name "pages"
                                    :default-slug-format "/{document-title}"
                                    :default-document-type "standard"})
        images-feed-nl (create-feed +test-db+
                                    {:title "Images"
                                     :subtitle "Internal feed with images"
                                     :language "nl"
                                     :name "images"
                                     :default-slug-format
                                     "/media/{document-title}"
                                     :default-document-type "image"})]

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

(deftest test-update-document
  (let [new-doc (create-document
                  +test-db+
                  "en"
                  "blog"
                  "Europe/Amsterdam"
                  {:title "foo"
                   :subtitle ""
                   :slug "/blog/bar"
                   :content "bar"
                   :description ""
                   :draft false
                   :icon nil
                   :related-pages []
                   :related-images []})
        updated-doc (update-document
                      +test-db+
                      "Europe/Amsterdam"
                      "/blog/bar"
                      (assoc new-doc
                        :title "hic sunt dracones"
                        :subtitle "old maps are cool!"
                        :description "hey!"
                        :draft true
                        :start-time "2012-02-21 01:19"
                        :end-time "2012-02-21 10:00"
                        :icon {:title "cat" :slug "/cat.png"}
                        :related-pages [{:title "foo" :slug "bar"}]
                        :related-images [{:title "cat" :slug "cat.png"}]))]
    (is (= (get-document +test-db+ "/blog/bar") updated-doc))
    (is (couchdb-rev? 2 (:_rev updated-doc)))
    (is (iso-date? (:updated updated-doc)))

    (is (= (:published new-doc) (:published updated-doc)))

    (is (= (:subtitle new-doc) ""))
    (is (= (:description new-doc) ""))
    (is (= (:start-time new-doc) nil))
    (is (= (:end-time new-doc) nil))
    (is (= (:start-time-rfc3339 new-doc) nil))
    (is (= (:end-time-rfc3339 new-doc) nil))
    (is (not (:draft new-doc)))
    (is (nil? (:icon new-doc)))
    (is (= (:related-pages new-doc) []))
    (is (= (:related-images new-doc) []))
    
    (is (= (:title updated-doc) "hic sunt dracones"))
    (is (= (:subtitle updated-doc) "old maps are cool!"))
    (is (= (:description updated-doc) "hey!"))
    (is (= (:start-time updated-doc) "2012-02-21 01:19"))
    (is (= (:end-time updated-doc) "2012-02-21 10:00"))
    (is (= (:start-time-rfc3339 updated-doc) "2012-02-21T00:19:00.000Z"))
    (is (= (:end-time-rfc3339 updated-doc) "2012-02-21T09:00:00.000Z"))
    (is (true? (:draft updated-doc)))
    (is (= (:icon updated-doc) {:title "cat" :slug "/cat.png"}))
    (is (= (:related-pages updated-doc) [{:title "foo" :slug "bar"}]))
    (is (= (:related-images updated-doc) [{:title "cat" :slug "cat.png"}])))
  
  (testing "Test if attachments are handled correctly."
    (let [black-pixel (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQU"
                           "A++/vQAsAAAAAAEAAQAAAgJEAQA7")
          white-pixel (str "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDAAgGBgcGBQgHBwcJ"
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
          new-doc (create-document
                   +test-db+
                   "en"
                   "images"
                   "Europe/Amsterdam"
                   {:attachment {:type "image/jpeg" :data white-pixel}
                    :title "a single black pixel!"
                    :slug "/pixel.jpeg"
                    :content ""
                    :draft false})
          updated-doc (update-document
                       +test-db+
                       "Europe/Amsterdam"
                       "/pixel.jpeg"
                       {:attachment {:type "image/gif" :data black-pixel}
                        :title "a single black pixel!"
                        :content ""
                        :draft false})
          attachment (clutch/get-attachment +test-db+ updated-doc :original)]

      (is (= (:_attachments updated-doc)
             {:original {:content_type "image/gif"
                         :revpos 4
                         :length 57
                         :stub true}}))

      (is (= (get-attachment-as-base64-string +test-db+
                                              updated-doc
                                              :original)
             black-pixel)))))

(deftest test-update-feed
  (let [blog-feed (create-feed +test-db+
                               {:title "Weblog"
                                :subtitle "Vix Weblog!"
                                :name "blog"
                                :language "en"
                                :default-slug-format
                                "/{feed-name}/{document-title}"
                                :default-document-type "with-description"
                                :searchable false})
        blog-feed-updated (update-feed +test-db+
                                       "en"
                                       "blog"
                                       {:title "Weblog Feed"
                                        :subtitle "Vix Weblog"
                                        :name "weblog"
                                        :language "nl"
                                        :default-slug-format
                                        "/{document-title}"
                                        :default-document-type "standard"
                                        :searchable true})]

    (is (nil? (update-feed +test-db+
                           "de"
                           "blog"
                           {:title "foo"
                            :subtitle "bar"
                            :name "weblog"
                            :language "de"
                            :default-slug-format "/foo"
                            :default-document-type "none"})))

    (is (= (get-feed +test-db+ "nl" "blog") blog-feed-updated))
    (is (couchdb-rev? 2 (:_rev blog-feed-updated)))
    (is (iso-date? (:feed-updated blog-feed-updated)))
    (is (= (:created blog-feed) (:created blog-feed-updated)))
    (is (= (:title blog-feed-updated) "Weblog Feed"))
    (is (= (:subtitle blog-feed-updated) "Vix Weblog"))
    (is (= (:language blog-feed-updated) "nl"))
    (is (= (:name blog-feed-updated) "blog")) ; NB: not updated!
    (is (= (:default-slug-format blog-feed-updated) "/{document-title}"))
    (is (= (:default-document-type blog-feed-updated) "standard"))
    (is (= (:searchable blog-feed-updated) true))))

(deftest test-delete-document
  (do
    (create-document +test-db+
                     "en"
                     "blog"
                     "Europe/Amsterdam"
                     {:title "foo"
                      :slug "/blog/bar"
                      :content "bar"
                      :draft false}))

  (is (not (nil? (get-document +test-db+ "/blog/bar")))
      "Assure the document exists before it is deleted.")

  (do
    (delete-document +test-db+ "/blog/bar"))
  
  (is (nil? (delete-document +test-db+ "/blog/bar"))
      "Expect nil value if document is deleted twice.")

  (is (nil? (get-document +test-db+ "/blog/bar"))
      "Assure the document is truly removed."))

(deftest test-delete-feed
  (do
    (create-feed +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :language "en"
                  :name "blog"
                  :default-slug-format "/{feed-name}/{document-title}"
                  :default-document-type "with-description"}))
  
  (is (nil? (delete-feed +test-db+ "nl" "blog")))
  
  (is (not (nil? (get-feed +test-db+ "en" "blog")))
      "Assure the feed exists before it is deleted.")

  (do
    (delete-feed +test-db+ "en" "blog"))
  
  (is (nil? (delete-feed +test-db+ "en" "blog"))
      "Expect nil value if feed is deleted twice.")

  (is (nil? (get-feed +test-db+ "en" "blog"))
      "Assure the feed is truly removed."))

(deftest test-get-available-languages
  (do
    (create-feed +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :language "en"
                  :searchable true})

    (create-feed +test-db+
                 {:title "Images"
                  :subtitle "Images"
                  :name "images"
                  :language "en"
                  :searchable true})

    (create-feed +test-db+
                 {:title "Menu"
                  :subtitle "Menu"
                  :name "menu"
                  :language "en"
                  :searchable false})

    (create-feed +test-db+
                 {:title "Weblog"
                  :name "blog"
                  :language "nl"
                  :searchable true}))

  (is (= (get-available-languages +test-db+) ["en" "nl"])))

(deftest test-get-languages
  (do
    (create-feed +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :language "en"
                  :searchable true})

    (create-feed +test-db+
                 {:title "Images"
                  :subtitle "Images"
                  :name "images"
                  :language "en"
                  :searchable true})

    (create-feed +test-db+
                 {:title "Menu"
                  :subtitle "Menu"
                  :name "menu"
                  :language "en"
                  :searchable false})

    (create-feed +test-db+
                 {:title "Weblog"
                  :name "blog"
                  :language "nl"
                  :searchable true}))

  (is (= (get-languages (list-feeds +test-db+)) #{"en" "nl"})))

(deftest test-get-searchable-feeds
  (do
    (create-feed +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :language "en"
                  :searchable true})

    (create-feed +test-db+
                 {:title "Images"
                  :subtitle "Images"
                  :name "images"
                  :language "en"
                  :searchable true})

    (create-feed +test-db+ 
                 {:title "Menu"
                  :subtitle "Menu"
                  :name "menu"
                  :language "en"
                  :searchable false})

    (create-feed +test-db+
                 {:title "Weblog"
                  :name "blog"
                  :language "nl"
                  :searchable true}))

  (is (= (get-searchable-feeds (list-feeds +test-db+))
         {"nl" ["blog"]
          "en" ["images" "blog"]})))

(deftest test-get-most-recent-event-documents
  (let [doc-1 (create-document +test-db+
                               "en"
                               "events"
                               "Europe/Amsterdam"
                               {:title
                                "Tomasz Stańko Middelburg"
                                :slug
                                "/en/events/stanko-middelburg"
                                :content
                                (str "The legendary Polish trumpet player "
                                     "Stańko will be playing in Middelburg.")
                                :start-time
                                "2012-04-25 20:30"
                                :end-time
                                "2012-04-25 23:59"
                                :draft false})

        doc-2 (create-document +test-db+
                               "en"
                               "events"
                               "Europe/Amsterdam"
                               {:title
                                "The Impossible Gentlemen"
                                :slug
                                "/en/events/impossible-gentlemen-amsterdam"
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

        doc-3 (create-document +test-db+
                               "en"
                               "events"
                               "Europe/Amsterdam"
                               {:title
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
           [doc-3 doc-2 doc-1]))

    ;; when limited, the fn retrieves (inc limit)
    (is (= (get-most-recent-event-documents +test-db+
                                            "en"
                                            "events"
                                            1)
           [doc-3]))))