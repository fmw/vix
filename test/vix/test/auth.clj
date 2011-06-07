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

(deftest test-authenticate
  (do
    (add-user +test-server+
              +test-db+
              "fmw"
              "oops"
              {:blog ["GET" "PUT" "POST" "DELETE"]
               :pages ["GET"]}))

  (is (thrown-with-msg? Exception #"doesn't exist"
                        (authenticate +test-server+ +test-db+ {} "foo" "bar")))
  (is (thrown-with-msg? Exception #"username and password do not match"
                        (authenticate +test-server+ +test-db+ {} "fmw" "ps")))
  (is (thrown-with-msg? Exception #"doesn't exist"
                        (authenticate
                          +test-server+ +test-db+ {} "not-fmw" "oops")))

  (is (= (authenticate +test-server+ +test-db+ {} "fmw" "oops")
         {:username "fmw"
          :permissions {:blog ["GET" "PUT" "POST" "DELETE"]
                        :pages ["GET"]}})))

(deftest test-authorize-for-feed
  (is (authorize-for-feed {:blog ["GET" "POST" "PUT" "DELETE"]} "blog" :GET))

  (is
    (not (authorize-for-feed
           {:blog ["GET" "POST" "PUT" "DELETE"]} "pages" :GET)))
  
  (is (not (authorize-for-feed {:blog ["GET"]} "blog" :DELETE))))

(deftest test-authorize
  (is (authorize {:username "fmw"
                 :permissions {:blog ["GET" "PUT" "POST" "DELETE"]
                               :pages ["GET"]}}
                 "blog" :GET))

  (is (authorize {:username "fmw"
                  :permissions {:blog ["GET" "PUT" "POST" "DELETE"]
                                :pages ["GET"]}}
                 "blog" :PUT))

  (is (authorize {:username "fmw"
                  :permissions {:blog ["GET" "PUT" "POST" "DELETE"]
                                :pages ["GET"]}}
                 "blog" :POST))

  (is (authorize {:username "fmw"
                  :permissions {:blog ["GET" "PUT" "POST" "DELETE"]
                                :pages ["GET"]}}
                 "blog" :DELETE))

  (is (authorize {:username "fmw"
                  :permissions {:blog ["GET" "PUT" "POST" "DELETE"]
                                :pages ["GET"]}}
                 "pages" :GET))

  (testing "Expect exception if no :username key is provided in session map"
    (is (thrown-with-msg? Exception #"You need to authenticate"
                          (authorize {:permissions
                                      {:blog ["GET" "PUT" "POST" "DELETE"]
                                       :pages ["GET"]}}
                                     "pages" :GET) true)))

  (testing "Expect exception if unlisted method is requested for feed."
    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions
                                        {:blog ["GET" "PUT" "POST" "DELETE"]
                                         :pages ["GET"]}}
                                     "pages" :PUT)))

    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions
                                        {:blog ["GET" "PUT" "POST" "DELETE"]
                                         :pages ["GET"]}}
                                     "pages" :POST)))

    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions
                                      {:blog ["GET" "PUT" "POST" "DELETE"]
                                       :pages ["GET"]}}
                                     "pages" :DELETE))))

  (testing "Requests on unlisted feeds should fail without global privileges"
    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions
                                      {:blog ["GET" "PUT" "POST" "DELETE"]
                                       :pages ["GET"]}}
                                     "foo" :GET)))
    
    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions 
                                      {:blog ["GET" "PUT" "POST" "DELETE"]
                                       :pages ["GET"]}}
                                     "foo" :PUT)))

    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions
                                      {:blog ["GET" "PUT" "POST" "DELETE"]
                                       :pages ["GET"]}}
                                     "foo" :POST)))

    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions
                                      {:blog ["GET" "PUT" "POST" "DELETE"]
                                       :pages ["GET"]}}
                                     "foo" :DELETE))))

  (testing "Specific privileges take preference over global privileges."
    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions
                                      {:* ["GET" "PUT" "POST" "DELETE"]
                                      :blog ["GET" "PUT" "POST" "DELETE"]
                                       :pages ["GET"]}}
                                     "pages" :DELETE))))

  (testing "Feed permission handling should be the same with a global map."
    (is (authorize {:username "fmw"
                    :permissions {:* ["GET" "PUT" "POST" "DELETE"]
                                  :blog ["GET" "PUT" "POST" "DELETE"]
                                  :pages ["GET"]}}
                   "blog" :DELETE))

    (is (authorize {:username "fmw"
                   :permissions {:* ["GET" "PUT" "POST" "DELETE"]
                                 :blog ["GET" "PUT" "POST" "DELETE"]
                                 :pages ["GET"]}}
                   "pages" :GET)))


  (testing "Fall back to general privileges if no specific privilege is found."
    (is (authorize {:username "fmw"
                    :permissions {:* ["GET"]
                                  :blog ["GET" "PUT" "POST" "DELETE"]
                                  :pages ["GET"]}}
                   "photos" :GET))

    (is (thrown-with-msg? Exception #"insufficient privileges"
                          (authorize {:username "fmw"
                                      :permissions
                                      {:blog ["GET" "PUT" "POST" "DELETE"]
                                       :pages ["GET"]}}
                                     "photos" :GET)))))

(deftest test-add-user
  (let [user (add-user
               +test-server+
               +test-db+
               "username"
               "password"
               {"blog" [:GET :POST :PUT :DELETE] "pages" [:GET]})]
    (is (= (:type user) "user"))
    (is (= (:username user) "username"))
    (is (re-matches #"^\$2a\$[\d]{2}\$[A-z\d./]{53}$" (:password user)))
    (is (= (:permissions user)
           {"blog" [:GET :POST :PUT :DELETE] "pages" [:GET]})))

  (is (thrown-with-msg? Exception #"Username's can only contain"
                        (add-user +test-server+ +test-db+ "space " "p" {})))

  (is (thrown-with-msg? Exception #"Username's can only contain"
                        (add-user +test-server+ +test-db+ "u" "p" {})))

  (is (thrown-with-msg? Exception #"Username's can only contain"
                        (add-user +test-server+ +test-db+  "user!" "p" {})))

  (is (thrown-with-msg? Exception #"Username's can only contain"
                        (add-user +test-server+ +test-db+ "user$" "p" {})))

  (is (thrown-with-msg? Exception #"Username's can only contain"
                        (add-user +test-server+ +test-db+ "user#" "p" {})))

  (is (thrown-with-msg? Exception #"Username's can only contain"
                        (add-user +test-server+ +test-db+ "user%" "p" {})))

  (is (thrown-with-msg? Exception #"already exists"
                        (add-user
                          +test-server+
                          +test-db+
                          "username"
                           "password"
                           {"blog" [:GET :POST :PUT :DELETE] "pages" [:GET]}))
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
  (database-fixture test-get-user))
