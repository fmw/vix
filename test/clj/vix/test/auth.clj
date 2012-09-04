;; test/vix/test/auth.clj tests for auth namespace.
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

(ns vix.test.auth
  (:use [vix.auth] :reload
        [clojure.test]
        [slingshot.test]
        [vix.test.test]
        [vix.test.db :only [database-fixture +test-server+ +test-db+]]))

(deftest test-fix-complex-keys
  (is (= (fix-complex-keys {:* "a" :b "b" (keyword "[:en :blog]") "c"})
         {:* "a" :b "b" [:en :blog] "c"}))
  (is (= (fix-complex-keys {:foo :bar})
         {:foo :bar}))
  (is (= (fix-complex-keys {(keyword "[\"foo\", \"bar\"]") :foobar})
         {["foo" "bar"] :foobar}))
  (is (= (fix-complex-keys {(keyword "{:a :b :c :d}") :map})
         {{:a :b :c :d} :map})))

(deftest test-authenticate
  (do
    (add-user +test-db+
              "fmw"
              "oops"
              {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
               ["en" "pages"] ["GET"]}))

  (are [type username password]
       (thrown+? (partial check-exc type)
                 (authenticate +test-db+
                               {}
                               username
                               password))
       :vix.auth/user-does-not-exist "foo" "bar"
       :vix.auth/username-password-mismatch "fmw" "ps"
       :vix.auth/user-does-not-exist "not-fmw" "oops")
  
  (is (= (authenticate +test-db+ {} "fmw" "oops")
         {:username "fmw"
          :permissions {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
                        ["en" "pages"] ["GET"]}})))

(deftest test-authorize-for-feed
  (is (#'vix.auth/authorize-for-feed
       {["en" "blog"] ["GET" "POST" "PUT" "DELETE"]}
       :GET
       "en"
       "blog"))

  (are [permissions-map language feed-name method]
       (not (#'vix.auth/authorize-for-feed permissions-map
                                           method
                                           language
                                           feed-name))
       {["en" "blog"] ["GET" "POST" "PUT" "DELETE"]} :GET "en" "pages"
       {["en" "blog"] ["GET"]} :DELETE "en" "blog"))

(deftest test-authorize
  (are [feed method] (authorize {:username "fmw"
                                 :permissions {["en" "blog"]
                                               ["GET" "PUT" "POST" "DELETE"]
                                               ["en" "pages"] ["GET"]}}
                                method
                                "en"
                                feed)
       "blog" :GET
       "blog" :PUT
       "blog" :POST
       "blog" :DELETE
       "pages" :GET)

  (testing "Expect exception if no :username key is provided in session map"
    (is (thrown+? (partial check-exc :vix.auth/authentication-required)
          (authorize
           {:permissions
            {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
             ["en" "pages"] ["GET"]}}
           :GET
           "en"
           "pages")))

    (is (thrown+? (partial check-exc :vix.auth/authentication-required)
          (authorize
           {:username :not-a-string
            :permissions
            {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
             ["en" "pages"] ["GET"]}}
           :GET
           "en"
           "pages"))))

  (testing "Expect exception if unlisted method is requested for feed."
    (are [method]
         (thrown+? (partial check-exc :vix.auth/insufficient-privileges)
           (authorize {:username "fmw"
                       :permissions
                       {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
                        ["en" "pages"] ["GET"]}}
                      method
                      "en"
                      "pages"))
         :PUT
         :POST
         :DELETE))

  (testing "Requests on unlisted feeds should fail without global privileges"
    (are [method]
         (thrown+? (partial check-exc :vix.auth/insufficient-privileges)
           (authorize {:username "fmw"
                       :permissions
                       {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
                        ["en" "pages"] ["GET"]}}
                      "en"
                      "foo"
                      method))
         :GET
         :PUT
         :POST
         :DELETE))

  (testing "Specific privileges take preference over global privileges."
    (is (thrown+? (partial check-exc :vix.auth/insufficient-privileges)
          (authorize {:username "fmw"
                      :permissions
                      {:* ["GET" "PUT" "POST" "DELETE"]
                       ["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
                       ["en" "pages"] ["GET"]}}
                     :DELETE
                     "en"
                     "pages"))))

  (testing "Feed permission handling should be the same with a global map."
    (is (authorize {:username "fmw"
                    :permissions {:* ["GET" "PUT" "POST" "DELETE"]
                                  ["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
                                  ["en" "pages"] ["GET"]}}
                   :DELETE
                   "en"
                   "blog"))

    (is (authorize {:username "fmw"
                    :permissions {:* ["GET" "PUT" "POST" "DELETE"]
                                  ["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
                                  ["en" "pages"] ["GET"]}}
                   :GET
                   "en"
                   "pages")))


  (testing "Fall back to general privileges if no specific privilege found."
    (is (authorize {:username "fmw"
                    :permissions {:* ["GET"]
                                  ["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
                                  ["en" "pages"] ["GET"]}}
                   :GET
                   "en"
                   "photos"))

    (is (thrown+? (partial check-exc :vix.auth/insufficient-privileges)
          (authorize {:username "fmw"
                      :permissions
                      {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
                       ["en" "pages"] ["GET"]}}
                     :GET
                     "en"
                     "photos")))))

(deftest test-add-user
  (let [user (add-user +test-db+
                       "username"
                       "password"
                       {["en" "blog"] ["GET" "POST" "PUT" "DELETE"]
                        ["en" "pages"] ["GET"]})]
    (is (= (:type user) "user"))
    (is (= (:username user) "username"))
    (is (re-matches #"^\$2a\$[\d]{2}\$[A-z\d./]{53}$" (:password user)))
    (is (= (:permissions user)
           {["en" "blog"] ["GET" "POST" "PUT" "DELETE"]
            ["en" "pages"] ["GET"]})))

  (are [username]
       (thrown+? (partial check-exc :vix.auth/invalid-username)
         (add-user +test-db+ username "password" {}))
       "  foo"
       "space "
       "u"
       "user$"
       "user#"
       "user!"
       "user%")
  
  (is (thrown+? (partial check-exc :vix.auth/user-exists)
        (add-user +test-db+
                  "username"
                  "password"
                  {["en" "blog"] [:GET :POST :PUT :DELETE]
                   ["en" "pages"] [:GET]}))
      "Expecting exception when user already exists."))

(deftest test-get-user
  (do
    (add-user +test-db+ "fmw" "oops" {})
    (add-user +test-db+ "fmw1" "oops" {:* ["GET" "PUT" "POST" "DELETE"]})
    (add-user +test-db+
              "fmw2"
              "oops"
              {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
               ["en" "pages"] ["GET" "PUT" "POST" "DELETE"]}))

  (let [user (get-user +test-db+ "fmw")]
    (is (re-matches #"^[a-z0-9]{32}$" (:_id user)))
    (is (re-matches #"^1-[a-z0-9]{32}$" (:_rev user)))
    (is (= (:type user) "user"))
    (is (re-matches #"^\$2a\$[\d]{2}\$[A-z\d./]{53}$" (:password user)))
    (is (= (:permissions user) {})))

  (let [user (get-user +test-db+ "fmw1")]
    (is (= (:permissions user) {:* ["GET" "PUT" "POST" "DELETE"]})))

  (let [user (get-user +test-db+ "fmw1")]
    (is (= (:permissions user) {:* ["GET" "PUT" "POST" "DELETE"]})))

  (let [user (get-user +test-db+ "fmw2")]
    (is (= (:permissions user)
           {["en" "blog"] ["GET" "PUT" "POST" "DELETE"]
            ["en" "pages"] ["GET" "PUT" "POST" "DELETE"]})))

  (is (nil? (get-user +test-db+ "john.doe"))))

(defn test-ns-hook []
  (database-fixture test-authenticate)
  (test-authorize-for-feed)
  (test-authorize)
  (database-fixture test-add-user)
  (database-fixture test-get-user)
  (test-fix-complex-keys))