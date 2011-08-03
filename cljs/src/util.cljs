(ns vix.util
  (:require [clojure.string :as string]))

(defn map-to-obj [m]
  (let [o (js-obj)]
    ;; FIXME: remove ugly string/split hack once the name function is fixed
    (doall (map #(aset o (second (string/split (first %) #"'")) (second %)) m)) o))
