; test/vix/test/auth.clj tests for auth namespace.
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

(ns vix.test.auth
  (:use [vix.auth] :reload)
  (:use [clojure.test]
        [vix.test.db :only [database-fixture +test-server+ +test-db+]]))

(deftest test-fix-complex-keys
  (is (= (fix-complex-keys {:* "a" :b "b" (keyword "[:en :blog]") "c"})
         {:* "a" :b "b" [:en :blog] "c"})))

(deftest test-authenticate
  (do
    (add-user +test-server+
              +test-db+
              "fmw"
              "oops"
              {[:en :blog] ["GET" "PUT" "POST" "DELETE"]
               [:en :pages] ["GET"]}))

  (are [exc-re username password]
       (thrown-with-msg? Exception exc-re (authenticate +test-server+
                                                        +test-db+
                                                        {}
                                                        username
                                                        password))
       #"doesn't exist" "foo" "bar"
       #"username and password do not match" "fmw" "ps"
       #"doesn't exist" "not-fmw" "oops")
  
  (is (= (authenticate +test-server+ +test-db+ {} "fmw" "oops")
         {:username "fmw"
          :permissions {[:en :blog] ["GET" "PUT" "POST" "DELETE"]
                        [:en :pages] ["GET"]}})))

(deftest test-authorize-for-feed
  (is (authorize-for-feed {[:en :blog] ["GET" "POST" "PUT" "DELETE"]}
                          "en"
                          "blog"
                          :GET))

  (are [permissions-map language feed-name method]
       (not (authorize-for-feed permissions-map language feed-name method))
       {[:en :blog] ["GET" "POST" "PUT" "DELETE"]} "en" "pages" :GET
       {[:en :blog] ["GET"]} "en" "blog" :DELETE))

(deftest test-authorize
  (are [feed method] (authorize {:username "fmw"
                                 :permissions {[:en :blog]
                                               ["GET" "PUT" "POST" "DELETE"]
                                               [:en :pages] ["GET"]}}
                                "en"
                                feed
                                method)
       "blog" :GET
       "blog" :PUT
       "blog" :POST
       "blog" :DELETE
       "pages" :GET)

  (testing "Expect exception if no :username key is provided in session map"
    (is (thrown-with-msg? Exception #"You need to authenticate"
          (authorize
           {:permissions
            {[:en :blog] ["GET" "PUT" "POST" "DELETE"]
             [:en :pages] ["GET"]}}
           "en"
           "pages"
           :GET)
          true)))

  (testing "Expect exception if unlisted method is requested for feed."
    (are [method]
         (thrown-with-msg? Exception #"insufficient privileges"
           (authorize {:username "fmw"
                       :permissions
                       {[:en :blog] ["GET" "PUT" "POST" "DELETE"]
                        [:en :pages] ["GET"]}}
                      "en"
                      "pages"
                      method))
         :PUT
         :POST
         :DELETE))

  (testing "Requests on unlisted feeds should fail without global privileges"
    (are [method]
         (thrown-with-msg? Exception #"insufficient privileges"
          (authorize {:username "fmw"
                      :permissions
                      {[:en :blog] ["GET" "PUT" "POST" "DELETE"]
                       [:en :pages] ["GET"]}}
                     "en"
                     "foo"
                     method))
         :GET
         :PUT
         :POST
         :DELETE))

  (testing "Specific privileges take preference over global privileges."
    (is (thrown-with-msg? Exception #"insufficient privileges"
          (authorize {:username "fmw"
                      :permissions
                      {:* ["GET" "PUT" "POST" "DELETE"]
                       [:en :blog] ["GET" "PUT" "POST" "DELETE"]
                       [:en :pages] ["GET"]}}
                     "en"
                     "pages"
                     :DELETE))))

  (testing "Feed permission handling should be the same with a global map."
    (is (authorize {:username "fmw"
                    :permissions {:* ["GET" "PUT" "POST" "DELETE"]
                                  [:en :blog] ["GET" "PUT" "POST" "DELETE"]
                                  [:en :pages] ["GET"]}}
                   "en"
                   "blog"
                   :DELETE))

    (is (authorize {:username "fmw"
                   :permissions {:* ["GET" "PUT" "POST" "DELETE"]
                                 [:en :blog] ["GET" "PUT" "POST" "DELETE"]
                                 [:en :pages] ["GET"]}}
                   "en"
                   "pages"
                   :GET)))


  (testing "Fall back to general privileges if no specific privilege found."
    (is (authorize {:username "fmw"
                    :permissions {:* ["GET"]
                                  [:en :blog] ["GET" "PUT" "POST" "DELETE"]
                                  [:en :pages] ["GET"]}}
                   "en"
                   "photos"
                   :GET))

    (is (thrown-with-msg? Exception #"insufficient privileges"
          (authorize {:username "fmw"
                      :permissions
                      {[:en :blog] ["GET" "PUT" "POST" "DELETE"]
                       [:en :pages] ["GET"]}}
                     "en"
                     "photos"
                     :GET)))))

(deftest test-add-user
  (let [user (add-user
               +test-server+
               +test-db+
               "username"
               "password"
               {[:en :blog] [:GET :POST :PUT :DELETE]
                [:en :pages] [:GET]})]
    (is (= (:type user) "user"))
    (is (= (:username user) "username"))
    (is (re-matches #"^\$2a\$[\d]{2}\$[A-z\d./]{53}$" (:password user)))
    (is (= (:permissions user)
           {[:en :blog] [:GET :POST :PUT :DELETE]
            [:en :pages] [:GET]})))

  (are [username]
       (thrown-with-msg? Exception #"Username's can only contain"
         (add-user +test-server+ +test-db+ username "password" {}))
       "space "
       "u"
       "user$"
       "user#"
       "user!"
       "user%")
  
  (is (thrown-with-msg? Exception #"already exists"
        (add-user
         +test-server+
         +test-db+
         "username"
         "password"
         {[:en :blog] [:GET :POST :PUT :DELETE]
          [:en :pages] [:GET]}))
      "Expecting exception when user already exists."))

(deftest test-get-user
  (do
    (add-user +test-server+ +test-db+ "fmw" "oops" {}))

  (let [user (get-user +test-server+ +test-db+ "fmw")]
    (is (re-matches #"^[a-z0-9]{32}$" (:_id user)))
    (is (re-matches #"^1-[a-z0-9]{32}$" (:_rev user)))
    (is (:type user) "user")
    (is (re-matches #"^\$2a\$[\d]{2}\$[A-z\d./]{53}$" (:password user)))
    (is (:permissions user) {}))

  (is (nil? (get-user +test-server+ +test-db+ "john.doe"))))

(defn test-ns-hook []
  (database-fixture test-authenticate)
  (test-authorize-for-feed)
  (test-authorize)
  (database-fixture test-add-user)
  (database-fixture test-get-user)
  (test-fix-complex-keys))