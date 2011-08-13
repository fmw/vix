; test/vix/test/db.clj tests for db namespace.
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

(ns vix.test.db
  (:use [vix.db] :reload)
  (:use [clojure.test]
        [clojure.contrib.json :only [read-json]])
  (:require [couchdb [client :as couchdb]]
            [clj-http.client :as http]))

(def date-re #"^[\d]{4}-[\d]{2}-[\d]{2}T[\d]{2}:[\d]{2}:[\d]{2}\.[\d]{1,4}Z")

(defn random-lower-case-string [length]
  ; to include uppercase
  ; (let [ascii-codes (concat (range 48 58) (range 66 91) (range 97 123))]
  (let [ascii-codes (concat (range 48 58) (range 97 123))]
    (apply str (repeatedly length #(char (rand-nth ascii-codes))))))

(def +test-db+ (str "vix-test-" (random-lower-case-string 20)))
(def +test-server+ "http://localhost:5984/")

(defn database-fixture [f]
  (couchdb/database-create
   +test-server+ +test-db+)
  (f)
  (couchdb/database-delete +test-server+ +test-db+))

(use-fixtures :each database-fixture)

(deftest test-view-sync
  (testing "Create new view"
    (do
      (view-sync +test-server+
                 +test-db+
                 "test"
                 "by_slug"
                 {:map (str "function(doc) {\n"
                            "    emit(doc.slug, doc);\n"
                            "}\n")}))
           
    (let [view-doc (read-json (:body (http/get
                                       (str +test-server+
                                            +test-db+
                                            "/_design/test"))))]
      (is (= (:map (:by_slug (:views view-doc)))
             (str "function(doc) {\n"
                  "    emit(doc.slug, doc);\n"
                  "}\n")))))

  (testing "Update existing view"
    (do
      (view-sync +test-server+
                 +test-db+
                 "test"
                 "by_slug"
                 {:map (str "function(d) {\n"
                            "    emit(d.slug, d);\n"
                            "}\n")
                  :reduce (str "function(key, v, rereduce) {\n"
                               "    return sum(v);\n"
                               "}\n")}))
           
    (let [view-doc (read-json (:body (http/get
                                       (str +test-server+
                                            +test-db+
                                            "/_design/test"))))]
      (is (= (:map (:by_slug (:views view-doc)))
             (str "function(d) {\n"
                  "    emit(d.slug, d);\n"
                  "}\n")))
      (is (= (:reduce (:by_slug (:views view-doc)))
             (str "function(key, v, rereduce) {\n"
                  "    return sum(v);\n"
                  "}\n"))))))


(deftest test-create-views
  (create-views +test-server+ +test-db+ "views" views)
  (let [view-doc (read-json (:body (http/get
                     (str +test-server+
                          +test-db+
                          "/_design/views"))))]
    
    (is (= (:map (:by_slug (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"document\") {\n"
                "        emit(doc.slug, doc);\n"
                "    }\n"
                "}\n")))
    
    (is (= (:map (:by_feed (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"document\") {\n"
                "        emit([doc.feed, doc.published], doc);\n"
                "    }\n"
                "}\n")))


    
    (is (= (:map (:by_username (:views view-doc)))
           (str "function(doc) {\n"
                "    if(doc.type === \"user\") {\n"
                "        emit(doc.username, doc);\n"
                "    }\n"
                "}\n")))))


(deftest test-get-document
  (do
    (create-document
      +test-server+
      +test-db+
      "blog"
      {:title "foo"
      :slug "/blog/foo"
      :content "bar"
      :draft true}))

  ; as we didn't create the view manually here, this test also implies
  ; views are created automatically by get-document
  (let [document (get-document +test-server+ +test-db+ "/blog/foo")]
        (is (re-matches #"^[a-z0-9]{32}$" (:_id document)))
        (is (re-matches #"^1-[a-z0-9]{32}$" (:_rev document)))
        (is (re-matches date-re (:published document)))
        (is (= (:feed document) "blog"))
        (is (= (:title document) "foo"))
        (is (= (:slug document) (str "/blog/foo")))
        (is (= (:content document) "bar"))
        (is (true? (:draft document)))))

(deftest test-get-unique-slug
  (is (= (get-unique-slug +test-server+ +test-db+ "/blog/foo") "/blog/foo"))

  (do
    (create-document
      +test-server+
      +test-db+
      "blog"
      {:title "foo"
      :slug "/blog/foo-1234567890"
      :content "bar"
      :draft true})

    (create-document
      +test-server+
      +test-db+
      "blog"
      {:title "foo"
      :slug "/blog/foo-1234567891"
      :content "bar"
      :draft true}))

  ; this should retrieve the next available slug:
  (is (= (get-unique-slug +test-server+ +test-db+ "/blog/foo-1234567890")
         "/blog/foo-1234567892")))

(deftest test-create-document
  (testing "Test document creation"
    (let [document (create-document
                     +test-server+
                     +test-db+
                     "blog"
                     {:title "foo"
                      :slug "/blog/foo"
                      :content "bar"
                      :draft false})]

      (is (re-matches #"^[a-z0-9]{32}$" (:_id document)))
      (is (re-matches #"^1-[a-z0-9]{32}$" (:_rev document)))
      (is (re-matches date-re (:published document)))
      (is (= (:type document) "document"))
      (is (= (:feed document) "blog"))
      (is (= (:title document) "foo"))
      (is (= (:slug document) "/blog/foo"))
      (is (= (:content document) "bar"))
      (is (false? (:draft document)))))

  (testing "Test if slugs are correctly autoincremented"
    (dotimes [n 10]
      (let [document (create-document
                       +test-server+
                       +test-db+
                       "blog"
                       {:title "foo"
                        :slug "/blog/foo"
                        :content "bar"
                        :draft true})]

        (is (re-matches #"^[a-z0-9]{32}$" (:_id document)))
        (is (re-matches #"^1-[a-z0-9]{32}$" (:_rev document)))
        (is (re-matches date-re (:published document)))
        (is (= (:feed document) "blog"))
        (is (= (:title document) "foo"))
        (is (= (:slug document) (str "/blog/foo-" (+ n 2))))
        (is (= (:content document) "bar"))
        (is (true? (:draft document)))))))

(deftest test-get-feed
  (let [doc-1 (create-document +test-server+ +test-db+ "blog" {:title "foo"
                                                              :slug "/blog/foo"
                                                              :content "bar"
                                                              :draft true})

        doc-2 (create-document +test-server+ +test-db+ "blog" {:title "foo"
                                                    :slug "/blog/foo"
                                                    :content "bar"
                                                    :draft true})
        feed (get-feed +test-server+ +test-db+ "blog")]

    (is (= (count feed) 2))

    (is (some #{doc-1} feed))
    (is (some #{doc-2} feed))))

(deftest test-update-document
  (let [new-doc (create-document
                  +test-server+
                  +test-db+
                  "blog"
                  {:title "foo"
                   :slug "/blog/bar"
                   :content "bar"
                   :draft false})
        updated-doc (update-document
                      +test-server+
                      +test-db+
                      "/blog/bar"
                      (assoc new-doc :title "hic sunt dracones"))]
    (is (= (get-document +test-server+ +test-db+ "/blog/bar") updated-doc))
    (is (re-matches #"^2-[a-z0-9]{32}$" (:_rev updated-doc)))
    (is (re-matches date-re (:updated updated-doc)))
    (is (= (:published new-doc) (:published updated-doc)))
    (is (= (:title updated-doc) "hic sunt dracones"))))

(deftest test-delete-document
  (do
    (create-document
      +test-server+
      +test-db+
      "blog"
      {:title "foo"
       :slug "/blog/bar"
       :content "bar"
       :draft false}))

  (is (not (nil? (get-document +test-server+ +test-db+ "/blog/bar")))
      "Assure the document exists before it is deleted.")

  (do
    (delete-document +test-server+ +test-db+ "/blog/bar"))
  
  (is (nil? (delete-document +test-server+ +test-db+ "/blog/bar"))
      "Expect nil value if document is deleted twice.")

  (is (nil? (get-document +test-server+ +test-db+ "/blog/bar"))
      "Assure the document is truly removed."))

