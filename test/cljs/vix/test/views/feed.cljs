(ns vix.test.views.feed
  (:use [vix.views.feed :only [get-invalid-tokens
                               has-unbalanced-braces?
                               validate-default-slug-format
                               validate-feed-name
                               group-feeds-by-name
                               list-feeds-for-language
                               list-feeds
                               get-feed-data-from-dom!
                               display-feed-form
                               display-edit-feed-form
                               list-documents]]))

(defn run []
  (assert (= :foo :bar)))