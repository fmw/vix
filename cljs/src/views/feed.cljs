(ns vix.views.feed
  (:require [vix.core :as core]
            [vix.document :as document]
            [vix.util :as util]
            [vix.ui :as ui]
            [clojure.set :as set]
            [clojure.string :as string]
            [soy :as soy]
            [vix.templates.feed :as tpl]
            [goog.global :as global]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [goog.Uri :as Uri]))

(def default-slug-has-invalid-chars-err
  (str "Slugs can only contain '/', '-', '.', alphanumeric characters "
       "and tokens (e.g. {day} and {document-title})."))

(def default-slug-has-consecutive-dashes-or-slashes-err
  "Slugs shouldn't contain any consecutive '-' or '/' characters.")

(def default-slug-initial-slash-required-err
  "The default slug needs to start with a '/'.")

(def default-slug-requires-document-title-err
  "The default slug needs to include a {document-title} token.")

(def default-slug-has-invalid-tokens-err
  "The following slug tokens aren't recognized: ")

(def default-slug-has-unbalanced-braces-err
  "The default slug contains unbalanced '{' and '}' characters.")

(def feed-name-required-err
  "The feed name value is required.")

(def feed-title-required-err
  "The feed title value is required.")

(def feed-name-has-invalid-characters-err
  "The feed name value can only contain '-' and alphanumeric characters.")

(def feed-name-only-allows-dashes-in-body-err
  "The feed name needs to start and end with an alphanumeric character.")

(def feed-name-has-consecutive-dashes-err
  "The feed name shouldn't contain any consecutive '-' characters.")

(def could-not-save-feed-err
  "Something went wrong while trying to save this feed.")

(defn display-document-list [main-el xhr]
  (soy/renderElement main-el
                     tpl/list-documents
                     (util/map-to-obj
                      {:json (. xhr (getResponseJson))})))

(defn display-feed-list [main-el xhr]
  (soy/renderElement main-el
                     tpl/list-feeds
                     (util/map-to-obj
                      {:json (. xhr (getResponseJson))})))

(defn list-documents [uri-path]
  (let [feed (last (re-find #"^/admin/(.*?)/overview$" uri-path))]
    (util/set-page-title! (str "List of documents for feed \"" feed "\""))
    (document/get-documents-for-feed
     feed
     (fn [e]
       (let [main-el (dom/getElement "main-page")
             xhr (.target e)
             status (. xhr (getStatus))]
         (if (= status 200)
           (do
             (display-document-list main-el xhr)
             (create-document-list-events feed))
           (soy/renderElement main-el tpl/list-documents-error)))))))

(defn list-feeds-callback [e]
  (let [main-el (dom/getElement "main-page")
        xhr (.target e)
        status (. xhr (getStatus))]
    (if (= status 200)
      (do
        (display-feed-list main-el xhr)
        (create-feed-list-events))
      (soy/renderElement main-el tpl/list-feeds-error))))

(defn list-feeds []
  (util/set-page-title! "Feeds overview")
  (document/get-feeds-list list-feeds-callback))

(defn delete-doc-callback [e]
  (list-documents global/document.location.pathname))

(defn create-document-list-events [feed]
  (core/xhrify-internal-links! (core/get-internal-links!))
  (events/listen (dom/getElement "add-document")
                 "click"
                 (fn [e]
                   (core/navigate (str feed "/new") "New Document")))
  
  ; converting to vector to avoid issues with doseq and arrays
  (doseq [delete-link (cljs.core.Vector/fromArray
                       (dom/getElementsByTagNameAndClass "a" "delete-document"))]
    (events/listen delete-link
                   "click"
                   (fn [e]
                     (. e (preventDefault))
                     (document/delete-doc (.substr (.id (.target e)) 12)
                                          delete-doc-callback)))))
(defn create-feed-list-events [feed]
  (core/xhrify-internal-links! (core/get-internal-links!))
  (events/listen (dom/getElement "add-feed")
                 "click"
                 (fn [e]
                   (core/navigate "new-feed") "New Feed"))
  
  ; converting to vector to avoid issues with doseq and arrays
  (doseq [delete-link (cljs.core.Vector/fromArray
                       (dom/getElementsByTagNameAndClass "a" "delete-feed"))]
    (events/listen delete-link
                   "click"
                   (fn [e]
                     (. e (preventDefault))
                     (document/delete-feed (.substr (.id (.target e)) 17)
                                           list-feeds)))))
(defn get-invalid-tokens [slug]
  (set/difference (set (re-seq #"\{[^\}]{0,}}" slug))
                  #{"{day}" "{month}" "{year}" "{document-title}" "{feed-name}"}))

(defn has-unbalanced-braces? [slug]
  (let [braces (re-seq #"[\{\}]" slug)]
    (if (odd? (count braces))
      true
      (pos? (count (filter #(not (= ["{" "}"] %)) (partition 2 braces)))))))

(defn validate-default-slug [e]
  (let [status-el (dom/getElement "status-message")
        slug-el (.target e)
        slug (.value slug-el)
        slug-label-el (dom/getElement "default-slug-format-select-label")
        err #(ui/display-error status-el % slug-el slug-label-el)
        invalid-tokens (get-invalid-tokens slug)]
    (cond
     (not (= (first slug) "/"))
       (err default-slug-initial-slash-required-err)
     (re-find #"[^/\-a-zA-Z0-9\{\}\.]" slug)
       (err default-slug-has-invalid-chars-err)
     (not (re-find #"\{document-title\}" slug))
       (err default-slug-requires-document-title-err)
     (has-unbalanced-braces? slug)
       (err default-slug-has-unbalanced-braces-err)
     (pos? (count invalid-tokens))
       (err (str default-slug-has-invalid-tokens-err
                 (apply str (interpose ", " invalid-tokens))))
     (util/has-consecutive-dashes-or-slashes? slug)
       (err default-slug-has-consecutive-dashes-or-slashes-err)
       :else (ui/remove-error status-el slug-el slug-label-el))))


(defn validate-feed-name-and-preview-in-slug [e]
  (let [name-el (.target e)
        name-val (.value name-el)
        name-label-el (dom/getElement "name-label")
        status-el (dom/getElement "status-message")
        dsfs-el (dom/getElement "default-slug-format-select")
        select-opts (cljs.core.Vector/fromArray
                     (dom/getChildren dsfs-el))
        err #(ui/display-error status-el % name-el name-label-el)]

    (cond
     (string/blank? name-val)
      (err feed-name-required-err)
     (not (re-matches #"[\-a-zA-Z0-9]+" name-val))
      (err feed-name-has-invalid-characters-err)
     (or (= (first name-val) "-") (= (last name-val) "-"))
      (err feed-name-only-allows-dashes-in-body-err)
     (util/has-consecutive-dashes-or-slashes? name-val)
      (err feed-name-has-consecutive-dashes-err)
     :else
      (ui/remove-error status-el name-el name-label-el))

    ; preview the feed name in the slug format select
    (when-not (classes/has name-el "error")
      (dom/setTextContent (first select-opts)
                          (str "/"
                               name-val
                               "/document-title")))))

(defn validate-feed! []
  (let [name-el (dom/getElement "name")
        title-el (dom/getElement "title")
        subtitle-el (dom/getElement "subtitle")
        dsf-el (dom/getElement "default-slug-format")
        ddt-el (dom/getElement "default-document-type")
        status-el (dom/getElement "status-message")
        err (partial ui/display-error status-el)]
    (cond
     (string/blank? (.value name-el))
       (err feed-name-required-err
            name-el
            (dom/getElement "name-label"))
     (string/blank? (.value title-el))
       (err feed-title-required-err
            (dom/getElement "title")
            (dom/getElement "title-label"))
     :else
       (if (classes/has status-el "error")
         false
         true))))

(defn get-feed-value-map! []
  {:name (.value (dom/getElement "name"))
   :title (.value (dom/getElement "title"))
   :subtitle (.value (dom/getElement "subtitle"))
   :default-slug-format (.value (dom/getElement "default-slug-format"))
   :default-document-type (.value (dom/getElement "default-document-type"))})

; FIXME: avoid duplication between this and the other 3 xhr callback fns
(defn save-new-feed-xhr-callback [e]
  (let [xhr (.target e)]
    (if (= (. xhr (getStatus)) 201)
      (let [json (js->clj (. xhr (getResponseJson)))]
        (core/navigate-replace-state (str "edit-feed/" ("name" json))
                                     (str "Edit feed \"" ("name" json) "\"")))
      (ui/display-error (dom/getElement "status-message")
                        could-not-save-feed-err))))

; FIXME: avoid duplication between this and the other 3 xhr callback fns
(defn save-existing-feed-xhr-callback [e]
  (let [xhr (.target e)]
    (if (= (. xhr (getStatus)) 200)
      nil ; TODO: display status-message
      (ui/display-error (dom/getElement "status-message")
                        could-not-save-feed-err))))

(defn create-feed-form-events [feed-name]  
  (let [dsf-el (dom/getElement "default-slug-format")
        title-el (dom/getElement "title")]
    (events/listen (dom/getElement "default-slug-format-select")
                   event-type/CHANGE
                   (fn [e]
                     (let [val (.value (.target e))]
                       (if (= val "custom")
                         (do
                           (ui/enable-element dsf-el))
                         (do
                           (ui/disable-element dsf-el)
                           (ui/set-form-value dsf-el val))))))

    (events/listen dsf-el event-type/INPUT validate-default-slug)

    (events/listen (dom/getElement "name")
                   event-type/INPUT
                   validate-feed-name-and-preview-in-slug)

    ; remove outdated errors left by save event validation
    (events/listen title-el
                   event-type/INPUT
                   (fn [e]
                     (when-not (string/blank? (.value title-el))
                       (ui/remove-error (dom/getElement "status-message")
                                        (dom/getElement "title-label")
                                        title-el))))
    
    (events/listen (dom/getElement "save-feed")
                   event-type/CLICK
                   (fn [e]
                     (when (validate-feed!)
                       (if (= :new feed-name)
                         (document/create-feed save-new-feed-xhr-callback
                                               (get-feed-value-map!))
                         (document/update-feed feed-name
                                               save-existing-feed-xhr-callback
                                               (get-feed-value-map!))))))))

(defn render-feed-form [feed-data]
  (soy/renderElement (dom/getElement "main-page")
                     tpl/manage-feed
                     (util/map-to-obj feed-data))
  (core/xhrify-internal-links! (core/get-internal-links!)))

(defn display-new-feed-form []
  (render-feed-form {:status "new"
                     :name ""
                     :title ""
                     :subtitle ""
                     :default_slug_format
                     "/{feed-name}/{document-title}"
                     :default_document_type "standard"})
  (create-feed-form-events :new))

(defn display-edit-feed-xhr-callback [feed-name e]
  (let [xhr (.target e)
        status (. xhr (getStatus))]
    (if (= status 200)
      (let [json (js->clj (. xhr (getResponseJson)))]
        (util/set-page-title!
         (str "Edit feed \"" ("title" json) "\""))
        (render-feed-form {:status "edit"
                           :name ("name" json)
                           :title ("title" json)
                           :subtitle (or ("subtitle" json) "")
                           :default_slug_format ("default-slug-format" json)
                           :default_document_type ("default-document-type" json)})
        
        (let [dsf ("default-slug-format" json)
              select-option (partial ui/set-form-value
                                     (dom/getElement "default-slug-format-select"))]
          (cond
           (= dsf "/{feed-name}/{document-title}")
             (select-option "/{feed-name}/{document-title}")
           (= dsf "/{document-title}")
             (select-option "/{document-title}")
           (= dsf "/{year}/{month}/{day}/{document-title}")
             (select-option "/{year}/{month}/{day}/{document-title}")
           :else
             (do
               (select-option "custom")
               (ui/enable-element "default-slug-format-select")
               (ui/enable-element "default-slug-format"))))
          
        (ui/disable-element "name")
        (create-feed-form-events feed-name))
      ; else clause
      (soy/renderElement (dom/getElement "main-page") tpl/feed-not-found))))

(defn display-edit-feed-form [feed-name]
  (document/get-feed feed-name #(display-edit-feed-xhr-callback feed-name %)))
                     