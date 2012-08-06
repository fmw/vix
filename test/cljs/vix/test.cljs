(ns vix.test
  (:require [vix.test.document :as document]))

(def success 0)

(defn ^:export run []
  (.log js/console "Tests started.")
  (document/run)
  success)