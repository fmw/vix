(ns vix.util
  (:require [goog.global :as global]
            [clojure.string :as string]))

(defn map-to-obj [m]
  (let [o (js-obj)]
    ;; FIXME: remove ugly string/split hack once the name function is fixed
    (doall (map #(aset o (second (string/split (first %) #"'")) (second %)) m)) o))

(defn set-page-title! [title]
  (set! global/document.title title))