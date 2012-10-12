;; test/vix/test/views.clj tests for views namespace.
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

(ns vix.test.views
  (:require [net.cgrand.enlive-html :as html])
  (:use [vix.views] :reload)
  (:use [clojure.test]))

(deftemplates dummy-tpl
  {:en (java.io.StringReader. "<p id=\"msg\">message goes here</p>")
   :nl (java.io.StringReader. "<div id=\"msg\">message goes here</div>")}
  [msg]
  [:#msg] (html/content msg))

(deftest test-deftemplates 
  (is (= (apply str (dummy-tpl :en "Hello, world!"))
         "<html><body><p id=\"msg\">Hello, world!</p></body></html>"))
  (is (= (apply str (dummy-tpl :nl "Hello, world!"))
         "<html><body><div id=\"msg\">Hello, world!</div></body></html>"))
  (is (= (dummy-tpl :qq "Hello, world!")
         nil)))

(deftest test-make-pagination-uri
  (testing "test :next links"
    (are [last-doc
          pp-after-doc-id
          pp-after-score
          after-doc-id
          after-score
          first-page?
          expected-uri]
         (= (make-pagination-uri "en"
                                 "whisky"
                                 last-doc
                                 pp-after-doc-id
                                 pp-after-score
                                 after-doc-id
                                 after-score
                                 first-page?
                                 :next)
            expected-uri)
         
         ;; get a :next link on page 0 to page 1
         ;; first-doc: {:index {:doc-id 0 :score 1.0}}
         {:index {:doc-id 4 :score 0.81}}
         nil
         nil
         nil
         nil
         true
         (str "/en/search?q=whisky&after-doc-id=4&after-score=0.81")

         ;; get a :next link on page 1 to page 2
         ;; first-doc: {:index {:doc-id 5 :score 0.8}}
         {:index {:doc-id 9 :score 0.61}}
         []
         []
         4
         0.81
         false
         (str "/en/search?q=whisky&after-doc-id=9&after-score=0.61&"
              "pp-aid[]=4&"
              "pp-as[]=0.81")

         ;; get a :next link on page 2 to page 3
         ;; first-doc: {:index {:doc-id 10 :score 0.6}}
         {:index {:doc-id 14 :score 0.41}}
         [4]
         [0.81]
         9
         0.61
         false
         (str "/en/search?q=whisky&after-doc-id=14&after-score=0.41&"
              "pp-aid[]=4&pp-aid[]=9&"
              "pp-as[]=0.81&pp-as[]=0.61")

         ;; get a :next link on page 3 to page 4
         ;; first-doc: {:index {:doc-id 15 :score 0.4}}
         {:index {:doc-id 19 :score 0.21}}
         [4 9]
         [0.81 0.61]
         14
         0.41
         false
         (str "/en/search?q=whisky&after-doc-id=19&after-score=0.21&"
              "pp-aid[]=4&pp-aid[]=9&pp-aid[]=14&"
              "pp-as[]=0.81&pp-as[]=0.61&pp-as[]=0.41")))

  (testing "test :previous links"
    (are [last-doc
          pp-after-doc-id
          pp-after-score
          after-doc-id
          after-score
          first-page?
          expected-uri]
         (= (make-pagination-uri "en"
                                 "whisky"
                                 last-doc
                                 pp-after-doc-id
                                 pp-after-score
                                 after-doc-id
                                 after-score
                                 first-page?
                                 :previous)
            expected-uri)
         
         ;; get a :previous link on page 0 to page -1
         ;; first-doc: {:index {:doc-id 0 :score 1.0}}
         {:index {:doc-id 4 :score 0.81}}
         nil
         nil
         nil
         nil
         true
         nil ;; shouldn't return a link

         ;; get a :next link on page 1 to page 0
         ;; first-doc: {:index {:doc-id 5 :score 0.8}}
         {:index {:doc-id 9 :score 0.61}}
         []
         []
         4
         0.81
         false
         "/en/search?q=whisky"

         ;; get a :next link on page 2 to page 1
         ;; first-doc: {:index {:doc-id 10 :score 0.6}}
         {:index {:doc-id 14 :score 0.41}}
         [4]
         [0.81]
         9
         0.61
         false
         "/en/search?q=whisky&after-doc-id=4&after-score=0.81"

         ;; get a :next link on page 3 to page 2
         ;; first-doc: {:index {:doc-id 15 :score 0.4}}
         {:index {:doc-id 19 :score 0.21}}
         [4 9]
         [0.81 0.61]
         14
         0.41
         false
         (str "/en/search?q=whisky&after-doc-id=9&after-score=0.61&"
              "pp-aid[]=4&"
              "pp-as[]=0.81")

         ;; get a :next link on page 4 to page 3
         ;; first-doc: {:index {:doc-id 20 :score 0.2}}
         {:index {:doc-id 24 :score 0.01}}
         [4 9 14]
         [0.81 0.61 0.41]
         19
         0.21
         false
         (str "/en/search?q=whisky&after-doc-id=14&after-score=0.41&"
              "pp-aid[]=4&pp-aid[]=9&"
              "pp-as[]=0.81&pp-as[]=0.61"))))

(deftest test-login-page-template
  (let [resource (html/html-resource
                  (java.io.StringReader.
                   (apply str (login-page-template "You need to log in!"))))]

    (is (= (html/text (first (html/select resource [:#status-message])))
           "You need to log in!"))))

(deftest test-admin-template
  (is (= (html/html-resource (java.io.StringReader.
                              (slurp "src/templates/en/admin.html")))
         (html/html-resource (java.io.StringReader.
                              (apply str (admin-template {})))))))

(comment
  (deftest test-layout
    (let [dummy-main (html/html-resource
                      (java.io.StringReader.
                       "<p id=\"dummy-resource\">Added</p>"))
          default-resource  (html/html-resource
                             (java.io.StringReader.
                              (slurp "src/templates/layout.html")))
          resource (html/html-resource
                    (java.io.StringReader.
                     (apply str (layout {:title "foo"
                                         :main dummy-main}))))]

      (is (= (html/text (first (html/select resource [:title])))
             "foo"))
      (is (= (html/text (first (html/select resource [:#page-title])))
             "foo"))
      (is (= (html/text (first (html/select resource [:#dummy-resource])))
             "Added"))

      (testing "test without a :title"
        (let [resource (html/html-resource
                        (java.io.StringReader.
                         (apply str (layout {:main dummy-main}))))
              default-title (html/text
                             (first (html/select default-resource [:title])))]

          ;; make sure the original template title is unchanged
          (is (= (html/text (first (html/select resource [:title])))
                 default-title))

          ;; the #page-title div should be empty
          (is (= (and (list? (html/select resource [:#page-title]))
                      (= (count (html/select resource [:#page-title])) 0))))

          ;; but the :main resource should still be added to the template
          (is (= (html/text (first (html/select resource [:#dummy-resource])))
                 "Added"))))

      (testing "test without any arguments"
        (let [resource (html/html-resource
                        (java.io.StringReader.
                         (apply str (layout {}))))]
          (is (= (first
                  (:content
                   (first (html/select resource [:#main-page]))))
                 "\n          \n          \n        ")))))))
(comment
  (deftest test-article-model
    (testing "test with title link as true"
      (let [am (article-model {:slug "/en/blog/first"
                               :title "first"
                               :content "Welcome!"}
                              (clj-time.core/date-time 2012 2 3 16 39 07 232)
                              true)]
        (is (= (first
                (html/attr-values
                 (first (html/select am [:h4 :a])) :href))
               "/en/blog/first"))

        (are [selector value]
             (= (html/text (first (html/select am selector))) value)
             [:.month] "2"
             [:.day] "3"
             [:.year] "2012"
             [:.hour] "16"
             [:.minute] "39"
             [:div.content] "Welcome!"))))

  (testing "test with title link as false"
    (let [am (article-model {:slug "/en/blog/first"
                             :title "first"
                             :content "Welcome!"}
                            (clj-time.core/date-time 2012 2 3 16 39 07 232)
                            false)]
      (is (= (count (html/select am [:h4 :a])) 0)))))

(comment
  (deftest test-blog-frontpage-view
    (let [default-title (html/text
                         (first
                          (html/select
                           (html/html-resource
                            (java.io.StringReader.
                             (slurp "src/templates/layout.html")))
                           [:title])))
          resource (html/html-resource
                    (java.io.StringReader.
                     (apply str
                            (blog-frontpage-view
                             {:documents
                              [{:title "0"
                                :slug "/blog/0"
                                :content "zero"
                                :published "2012-02-03T16:39:07.232Z"}
                               {:title "1"
                                :slug "/blog/1"
                                :content "one"
                                :published "2012-02-03T16:39:07.232Z"}
                               {:title "2"
                                :slug "/blog/2"
                                :content "two"
                                :published "2012-02-03T16:39:07.232Z"}]}
                             "Europe/Amsterdam"))))]


      (testing "test if the title is correctly omitted"
        (is (= (html/text (first (html/select resource [:title])))
               default-title))
        (is (= (and (list? (html/select resource [:#page-title]))
                    (= (count (html/select resource [:#page-title])) 0)))))

      (testing "test if the title links are added correctly"
        (are [n v]
             (is (= (first (html/attr-values
                            (nth (html/select resource [:h4 :a]) n) :href))
                    v))
             0 "/blog/0"
             1 "/blog/1"
             2 "/blog/2")

        (are [n v]
             (is (= (html/text (nth (html/select resource [:h4 :a]) n))
                    v))
             0 "0"
             1 "1"
             2 "2"))

      (testing "test if the content is added correctly"
        (are [n v]
             (is (= (html/text (nth (html/select resource [:div.content]) n))
                    v))
             0 "zero"
             1 "one"
             2 "two"))
    
      (testing "test if the datetime is added correctly"
        (are [n v]
             (is (= (html/text (nth (html/select resource [:.month]) n))
                    v))
             0 "2"
             1 "2"
             2 "2")

        (are [n v]
             (is (= (html/text (nth (html/select resource [:.day]) n))
                    v))
             0 "3"
             1 "3"
             2 "3")

        (are [n v]
             (is (= (html/text (nth (html/select resource [:.year]) n))
                    v))
             0 "2012"
             1 "2012"
             2 "2012")

        (are [n v]
             (is (= (html/text (nth (html/select resource [:.hour]) n))
                    v))
             0 "17"
             1 "17"
             2 "17")

        (are [n v]
             (is (= (html/text (nth (html/select resource [:.minute]) n))
                    v))
             0 "39"
             1 "39"
             2 "39")))))

(comment
  (deftest test-blog-article-view
    (let [resource (html/html-resource
                    (java.io.StringReader.
                     (apply str
                            (blog-article-view
                             {:title "0"
                              :slug "/blog/0"
                              :content "zero"
                              :published "2012-02-03T16:39:07.232Z"}
                             "Europe/Amsterdam"))))]

      (testing "test if the title is added correctly"
        (is (= (html/text (first (html/select resource [:title])))
               "0"))
        (is (= (html/text (first (html/select resource [:#page-title])))
               "0")))

      (testing "test if the title link is omitted correctly"
        (is (= (count (html/select resource [:h4 :a])) 0)))

      (testing "test if the article values are set correctly"
        (is (= (html/text (first (html/select resource [:div.content])))
               "zero"))
        (is (= (html/text (first (html/select resource [:.month]))) "2"))
        (is (= (html/text (first (html/select resource [:.day]))) "3"))
        (is (= (html/text (first (html/select resource [:.year]))) "2012"))
        (is (= (html/text (first (html/select resource [:.hour]))) "17"))
        (is (= (html/text (first (html/select resource [:.minute])))
               "39"))))))


(deftest test-search-result-model
  (is (= (:href
          (:attrs
           (first
            (:content
             (first
              (search-result-model :en {:title "foo" :slug "/foo"}))))))
         "/foo"))

  (is (= (first
          (:content
           (first
            (:content
             (first
              (search-result-model :en {:title "foo" :slug "/foo"}))))))
         "foo")))

(deftest test-search-results-view
  ;; this is tested thoroughly elsewhere, so just
  ;; want to make sure there are no exceptions
  (is (not (nil? (search-results-view "en"
                                      5
                                      {}
                                      "whisky"
                                      nil
                                      nil
                                      nil
                                      nil
                                      true
                                      {})))))