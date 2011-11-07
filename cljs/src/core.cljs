(ns vix.core
  (:require-macros [vix.macros :as macros])
  (:require [vix.views.editor :as editor]
            [vix.views.feed :as feed]
            [goog.global :as global]
            [goog.events :as events]
            [goog.history.EventType :as event-type]
            [goog.history.Html5History :as Html5History]
            [goog.Uri :as Uri]
            [goog.dom :as dom]
            [clojure.string :as string]))

(def *h* (atom nil))

(defn get-path [uri]
  (. (new goog.Uri uri false) (getPath)))

(defn get-internal-links! []
  (filter #(= (.substr (get-path (.href %)) 0 7) "/admin/")
          (cljs.core.Vector/fromArray
           (. global/document (getElementsByTagName "a")))))

; FIXME: figure out while only nested calls (e.g. in
; create-document-list-events and render-editor-template)
; work and replace with a centralized call
(defn xhrify-internal-links! [link-elements]
  (doseq [element link-elements]
    (events/listen element
                   "click"
                   (fn [e]
                     (. e (preventDefault))
                     (navigate (.substr (get-path (.href (.target e))) 7)
                               (.title (.target e)))))))

(defn start-history! [uri-path]
  (compare-and-set! *h* nil (new goog.history.Html5History))
  (.setUseFragment @*h* false)
  (.setPathPrefix @*h* "/admin/")
  (.setEnabled @*h* true)
  (events/listen @*h*
                 event-type/NAVIGATE
                 (fn [e]
                   (routes global/document.location.pathname))))

; FIXME: get rid!
(defn get-feed-from-uri []
  (let [parts (re-find #"/admin/([^/]+)/(.*?)"
                       global/document.location.pathname)]
    ;; TODO: throw error if feed isn't found
    (when (= 3 (count parts))
      (nth parts 1))))

(defn chop-path [path]
  (rest (string/split path #"/")))

(comment
  (deftest test-route-matches
    (are [segments path expected] (= (route-matches segments path) expected)
         ["admin" "edit" :x :y] "/admin/edit/foo/bar" {:x "foo" :y "bar"}
         [:feed :*] "/blog/hello/kitty" {:feed "blog" :* "hello/kitty"}
         :fallback "/foo/bar" true)

    (testing "these calls are expected to fail"
      (are [segments path] (= (route-matches segments path) false)
           [] "/admin/edit"
           ["admin" "edit" :foo] "/admin/edit/foo/bar"
           ["admin" "editor" :foo :bar] "/admin/edit/foo/bar"))))

(defn route-matches [route-segments uri-path]
  (if-not (= route-segments :fallback)
    (let [values (chop-path uri-path)]
      (if (and (>= (count values) (count route-segments))
               (or (= (count values) (count route-segments))
                   (= (last route-segments) :*)))
        (loop [segments route-segments
               values values
               result {}]
          (if (pos? (count segments))
            (let [segment (first segments)
                  value (first values)]
              (cond
               (keyword? segment) (if (= segment :*) ; catch-all
                                    (recur []
                                           []
                                           (assoc result
                                             :* (string/join "/" values)))
                                    (recur (rest segments)
                                           (rest values)
                                           (assoc result segment value)))
               (string? segment) (if (= segment value)
                                   (recur (rest segments)
                                          (rest values)
                                          result)
                                   (recur [] [] false))))
            result))
        false))
    true))

(defn routes [uri-path]
  (macros/routes uri-path
                 ["admin" ""]
                 (feed/list-feeds)
                 ["admin" "new-feed"]
                 (feed/display-new-feed-form)
                 ["admin" "edit-feed" :language :feed-name]
                 (feed/display-edit-feed-form (:language params)
                                              (:feed-name params))
                 ["admin" :language :feed-name "new"]
                 (editor/start (:language params)
                               (:feed-name params)
                               nil
                               :new)
                 ["admin" :language :feed-name "edit" :*]
                 (editor/start (:language params)
                               (:feed-name params)
                               (str "/" (:* params))
                               :edit)
                 ["admin" :language :feed-name "overview"]
                 (feed/list-documents (:language params) (:feed-name params))
                 :fallback
                 (navigate-replace-state "" "Vix overview")
                 ))

(defn navigate [token title]
  (. @*h* (setToken token title)))

(defn navigate-replace-state [token title]
  (. @*h* (replaceToken token title)))

(defn ^:export start-app [uri-path]
  (start-history!) ; in Chrome this triggers an event,
                   ; leading to a (routes) call
  (routes uri-path))