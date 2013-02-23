(ns vix.test
  (:require [vix.test.document :as document]
            [vix.test.views.feed :as feed]))

(def success 0)

(defn ^:export run []
  (.log js/console "Tests started.")
  (feed/run)
  success)