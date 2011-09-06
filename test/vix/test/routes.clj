; test/vix/test/routes.clj tests for routes namespace.
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

(ns vix.test.routes
  (:use [vix.routes] :reload
        [vix.db :only [create-document create-feed get-document]]
        [vix.auth :only [add-user]]
        [clojure.contrib.json :only [json-str read-json]]
        [vix.test.db :only [database-fixture +test-server+ +test-db+]])
  (:use [clojure.test])
  (:import [org.apache.commons.codec.binary Base64]))

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
   :params params})

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

(deftest test-json-response
  (is (= (:status (json-response nil)) 404))

  ; FIXME: fix charset issues
  (is (= (json-response {:foo "bar"})
         {:status 200
          :headers {"Content-type" "application/json; charset=UTF-8"}
          :body "{\"foo\":\"bar\"}"}))

  (is (= (json-response {:foo "bar"} :status 201)
         {:status 201
          :headers {"Content-type" "application/json; charset=UTF-8"}
          :body "{\"foo\":\"bar\"}"})))

(deftest test-response
  (is (= (response "foo") {:status 200
                         :headers {"Content-type"
                                   "text/html; charset=UTF-8"}
                         :body "foo"}))

  (is (= (:status (response nil) 404)))
  (is (= (:status (response "foo" :status 201) 201)))

  (is (= (get (:headers (response "foo" :content-type "image/png")) "Content-type")
         "image/png")))

(deftest test-catch-all
  (is (= (:status (catch-all +test-server+ +test-db+ "/blog/bar")) 404))
  (is (= (:body (catch-all +test-server+ +test-db+ "/blog/bar"))
         "<h1>Page not found</h1>"))

  (do
    (create-document
      +test-server+
      +test-db+
      "blog"
      {:title "foo"
       :slug "/blog/bar"
       :content "bar"
       :draft false}))

  (is (= (:status (catch-all +test-server+ +test-db+ "/blog/bar")) 200))

  (testing "Test if attachments are handled correctly."
    (let [gif (str "R0lGODlhAQABA++/vQAAAAAAAAAA77+9AQIAAAAh77+9BAQUA++/"
                   "vQAsAAAAAAEAAQAAAgJEAQA7")
          document (create-document
                    +test-server+
                    +test-db+
                    "images"
                    {:attachment {:type "image/gif" :data gif}
                     :title "a single black pixel!"
                     :slug "/pixel.gif"
                     :content ""
                     :draft false})
          catch-all (catch-all +test-server+ +test-db+ "/pixel.gif")]

      (is (= (get (:headers catch-all) "Content-type") "image/gif"))
      (is (= (class (:body catch-all)) java.io.ByteArrayInputStream)))))

(deftest ^{:integration true} test-routes
  (do
    (create-document
      +test-server+
      +test-db+
      "blog"
      {:title "foo"
       :feed "blog"
       :slug "/blog/bar"
       :content "bar"
       :draft false}))

  (with-redefs [database +test-db+]
    (testing "test pagination"
      (dotimes [n 21]
        (create-document +test-server+
                         +test-db+
                         "pages"
                         {:title (str "doc " n)
                          :slug (str "/pages/doc-" n)
                          :content "bar"
                          :draft false}))

      (let [first-five (read-json (:body (request :get
                                                  "/json/pages/list-documents"
                                                  nil
                                                  main-routes
                                                  {:limit "5"})))]
        (is (= (count (:documents first-five)) 5))

        (let [next-five (read-json (:body (request :get
                                                   "/json/pages/list-documents"
                                                   nil
                                                   main-routes
                                                   {:limit "5"
                                                    :startkey-published
                                                    (:published
                                                     (:next first-five))})))]
          (is (= (count (:documents next-five)) 5)))))
    
    (is (= (:status (request :get "/" main-routes)) 200))
    (is (= (:status (request :get "/login" main-routes)) 200))
    (is (= (:status (request :get "/logout" main-routes)) 302))
    (is (= (:status (request :get "/admin" main-routes)) 200))
    (is (= (:status (request :get "/json/blog/list-documents" main-routes)) 200))

    (is (= (:status (request
                          :post
                          "/json/blog/new"
                          (json-str {:title "test-create"
                                     :slug "/blog/test"
                                     :content "hic sunt dracones"})
                          main-routes))
           201))

    (let [document (get-document +test-server+ +test-db+ "/blog/test")]
      (is (= (:title document)) "test-create"))

    (is (= (:status (request :get "/json/document/blog/bar" main-routes)) 200))
    (is (= (:status (request :get "/json/document/blog/t3" main-routes)) 404))
    
    ; FIXME: should add a test-case for a 409 conflict
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
             404)))
     

    (is (= (:status (request :delete "/json/document/blog/bar" main-routes))
           200))
    (is (= (get-document +test-server+ +test-db+ "/blog/bar") nil))
                        
    (is (= (:status (request :get "/static/css/style.css" main-routes)) 200))
    (is (= (:status (request :get "/static/none" main-routes)) 404))
    (is (= (:body (request :get "/static/none" main-routes))
           "<h1>Page not found</h1>"))
    
    (is (= (:status (request :get "/blog/bar" main-routes)) 404))
    (is (= (:body (request :get "/blog/bar" main-routes))
           "<h1>Page not found</h1>"))

    (is (= (:status (request :get "/blog/test" main-routes)) 200))
   
    (let [post-feed-request (request :post
                                     "/json/new-feed"
                                     (json-str {:name "blog"
                                                :title "Vix Weblog"
                                                :subtitle "Vix Weblog..."
                                                :default-slug-format
                                                  "/{document-title}"
                                                :default-document-type "standard"})
                                     main-routes)]
      (is (= (:status post-feed-request) 201)))
    

    (let [get-feed-request (request :get "/json/feed/blog" main-routes)
          json-body (read-json (:body get-feed-request))]
      (is (= (:status get-feed-request) 200))
      (is (= (:name json-body) "blog"))
      (is (= (:title json-body) "Vix Weblog"))

      (let [put-feed-request (request :put
                                      "/json/feed/blog"
                                      (json-str (assoc json-body :title "Vix!"))
                                      main-routes)
            json-put-body (read-json (:body put-feed-request))]
        (is (= (:status put-feed-request) 200))
        (is (= (:name json-put-body) "blog"))
        (is (= (:title json-put-body) "Vix!"))))
    
    (is (:status (request :get "/json/feed/blog" main-routes)) 200)
    (is (:status (request :delete "/json/feed/blog" main-routes)) 200)
    (is (:status (request :get "/json/feed/blog" main-routes)) 404)))

(deftest ^{:integration true} test-routes-authorization
  (do
    (create-document
      +test-server+
      +test-db+
      "blog"
      {:title "foo"
       :feed "blog"
       :slug "/blog/test"
       :content "bar"
       :draft false})
    
    (create-feed +test-server+
                 +test-db+
                 {:title "Weblog"
                  :subtitle "Vix Weblog!"
                  :name "blog"
                  :default-slug-format "/{feed-name}/{document-title}"
                  :default-document-type "with-description"}))

  (with-redefs [database +test-db+]
    (testing "Test if authorization is enforced correctly."
      (is (= (:status (unauthorized-request :get "/admin/" main-routes))
             302))
      (is (= (:status (unauthorized-request :get
                                            "/json/blog/list-documents"
                                            main-routes))
             302))
      (is (= (:status (unauthorized-request
                        :post
                        "/json/blog/new"
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
                        "/json/feed/blog"
                        main-routes))
             302))
      
      (is (= (:status (unauthorized-request
                        :put
                        "/json/feed/blog"
                        main-routes))
             302))

      (is (= (:status (unauthorized-request
                        :delete
                        "/json/feed/blog"
                        main-routes))
             302)))))

(deftest ^{:integration true} test-routes-authentication
  (do
    (create-document
      +test-server+
      +test-db+
      "blog"
      {:title "foo"
       :feed "blog"
       :slug "/blog/test"
       :content "bar"
       :draft false}))

  (testing "Test if authentication is enforced correctly."
    (with-redefs [database +test-db+]
      (is (= (:status (unauthenticated-request :get "/admin" main-routes))
             302))
      (is (= (:status (unauthenticated-request :get
                                               "/json/blog/list-documents"
                                               main-routes))
             302))
      (is (= (:status (unauthenticated-request
                        :post
                        "/json/blog/new"
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
    (add-user
      +test-server+
      +test-db+
      "fmw"
      "oops"
      {:* ["GET" "POST" "PUT" "DELETE"]}))

  (with-redefs [database +test-db+]
    (is (= (form-request :post "/login" main-routes {"username" "fmw"
                                                     "password" "foo"})
           {:status 302
            :headers {"Location" "/login"}
            :body ""}))

    (let [r (form-request :post "/login" main-routes {"username" "fmw"
                                                     "password" "oops"})]
      (is (= ((:headers r) "Location") "/admin/"))
      (is (= (:status r) 302)))
   ))


(defn test-ns-hook []
  (test-json-response)
  (test-response)
  (database-fixture test-catch-all)
  (database-fixture test-routes)
  (database-fixture test-routes-authorization)
  (database-fixture test-routes-authentication)
  (test-logout)
  (database-fixture test-login))