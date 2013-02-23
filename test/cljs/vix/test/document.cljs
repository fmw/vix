(ns vix.test.document
  (:use [vix.document :only [request]]))

(defn run []
  (comment (assert (= (add-some-numbers 2 2) 4))
           (assert (= (add-some-numbers 1 2 3) 6))
           (assert (= (add-some-numbers 4 5 6) 15))))
