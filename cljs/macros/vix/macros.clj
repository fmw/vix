(ns vix.macros)

(comment
  (deftest test-routes
    (is (= (routes "/admin/blog/edit"
                   ["admin" "foo"] :unmatched
                   ["admin" :feed "edit"] (identity (:feed params))
                   ["admin" "bar"] :unmatched)
           "blog"))

    (is (= (routes "/admin/blog/edit/foo"
                   ["admin" "foo"] :unmatched
                   ["admin" :feed "edit"] (identity (:feed params))
                   ["admin" "bar"] :unmatched
                   :fallback :fallen-back)
           :fallen-back))))

(defmacro routes [uri-path & clauses]
  (when-let [[route-segments body & clauses] clauses]
    `(if-let [~'params (vix.core/route-matches ~route-segments ~uri-path)]
      ~body
      (routes ~uri-path ~@clauses))))