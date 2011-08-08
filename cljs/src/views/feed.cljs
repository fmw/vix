(ns vix.views.feed
  (:require [vix.core :as core]
            [vix.document :as document]
            [vix.util :as util]
            [soy :as soy]
            [vix.templates.feed :as tpl]
            [goog.events :as events]
            [goog.dom :as dom]
            [goog.Uri :as Uri]
            [goog.string :as string]))

(defn display-document-list [main-el xhr e]
  (soy/renderElement main-el
                     tpl/list-documents
                     (util/map-to-obj
                      {:json (.getResponseJson xhr e)})))

(defn delete-doc-callback [e]
  (list-documents (js* "document.location.pathname")))

(defn create-document-list-events [feed]
  (core/xhrify-internal-links! (core/get-internal-links!))
  (events/listen (dom/getElement "add-document")
                 "click"
                 (fn [e]
                   (core/navigate (str feed "/new") "New Document")))
  (doseq [delete-link (cljs.core.Vector/fromArray
                       (dom/getElementsByTagNameAndClass "a" "delete-document"))]
    (events/listen delete-link
                   "click"
                   (fn [e]
                     (js* "e.preventDefault()")
                     (document/delete-doc (.substr (.id (.target e)) 12)
                                          delete-doc-callback)))))

(defn list-documents [uri-path]
  (let [feed (last (re-find #"^/admin/(.*?)/overview$" uri-path))]
    (util/set-page-title! (str "List of documents for feed \"" feed "\""))
    (document/get-feed
     feed
     (fn [e]
       (let [main-el (dom/getElement "main-page")
             xhr (.target e)
             status (.getStatus xhr e)]
         (if (= status 200)
           (do
             (display-document-list main-el xhr e)
             (create-document-list-events feed))
           (soy/renderElement main-el tpl/list-documents-error)))))))