(ns vix.views.editor
  (:require [vix.core :as core]
            [vix.document :as document]
            [vix.ui :as ui]
            [vix.util :as util]
            [vix.templates.editor :as tpl]
            [soy :as soy]
            [clojure.string :as string]
            [goog.date :as date]
            [goog.global :as global]
            [goog.editor.Field :as Field]
            [goog.editor.plugins.BasicTextFormatter :as BasicTextFormatter]
            [goog.editor.plugins.RemoveFormatting :as RemoveFormatting]
            [goog.editor.plugins.UndoRedo :as UndoRedo]
            [goog.editor.plugins.ListTabHandler :as ListTabHandler]
            [goog.editor.plugins.SpacesTabHandler :as SpacesTabHandler]
            [goog.editor.plugins.EnterHandler :as EnterHandler]
            [goog.editor.plugins.HeaderFormatter :as HeaderFormatter]
            [goog.editor.plugins.LoremIpsum :as LoremIpsum]
            [goog.editor.plugins.LinkDialogPlugin :as LinkDialogPlugin]
            [goog.editor.plugins.LinkBubble :as LinkBubble]
            [goog.editor.Command :as buttons]
            [goog.ui.editor.DefaultToolbar :as DefaultToolbar]
            [goog.ui.editor.ToolbarController :as ToolbarController]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]))

(def slug-has-invalid-chars-err
  "Slugs can only contain '/', '-', '.' and alphanumeric characters.")

(def slug-has-consecutive-dashes-or-slashes-err
  "Slugs shouldn't contain any consecutive '-' or '/' characters.")

(def slug-required-err
  "A valid slug is required for every document.")

(def slug-initial-slash-required-err
  "The slug needs to start with a '/'.")

(def slug-not-unique-err
  "This slug is not unique (document already exists).")

(def could-not-create-document-err
  "Something when wrong while creating the document.")

(def could-not-save-document-err
  "Something went wrong while saving the document.")

(defn slug-has-invalid-chars? [slug]
  (if (re-matches #"[/\-a-zA-Z0-9\.]+" slug) false true))

(defn create-editor-field [element-id]
  (goog.editor.Field. element-id))

(defn register-editor-plugins [editor]
  (doto editor
    (.registerPlugin (goog.editor.plugins.BasicTextFormatter.))
    (.registerPlugin (goog.editor.plugins.RemoveFormatting.))
    (.registerPlugin (goog.editor.plugins.UndoRedo.))
    (.registerPlugin (goog.editor.plugins.ListTabHandler.))
    (.registerPlugin (goog.editor.plugins.SpacesTabHandler.))
    (.registerPlugin (goog.editor.plugins.EnterHandler.))
    (.registerPlugin (goog.editor.plugins.HeaderFormatter.))
    (.registerPlugin (goog.editor.plugins.LoremIpsum. "Click here to edit."))
    (.registerPlugin (goog.editor.plugins.LinkDialogPlugin.))
    (.registerPlugin (goog.editor.plugins.LinkBubble.))))

(defn create-editor-toolbar [element-id]
  (let [buttons (to-array [buttons/BOLD
                           buttons/ITALIC
                           buttons/UNDERLINE
                           buttons/FONT_COLOR
                           buttons/BACKGROUND_COLOR
                           buttons/FONT_FACE
                           buttons/FONT_SIZE
                           buttons/LINK
                           buttons/UNDO
                           buttons/REDO
                           buttons/UNORDERED_LIST
                           buttons/ORDERED_LIST
                           buttons/INDENT
                           buttons/OUTDENT
                           buttons/JUSTIFY_LEFT
                           buttons/JUSTIFY_CENTER
                           buttons/JUSTIFY_RIGHT
                           buttons/SUBSCRIPT
                           buttons/SUPERSCRIPT
                           buttons/STRIKE_THROUGH
                           buttons/REMOVE_FORMAT])]
    (DefaultToolbar/makeToolbar buttons (dom/getElement element-id))))

(defn increment-slug [slug]
  (if-let [slug-matches (re-matches #"(.*?)-([0-9]+)$" slug)]
    (str (nth slug-matches 1) "-" (inc (js/parseInt (last slug-matches))))
    (str slug "-2")))

(defn handle-duplicate-slug-callback [e]
  (let [status (. (.target e) (getStatus))
        slug-el (dom/getElement "slug")]
    (when (= status 200) 
      (ui/set-form-value slug-el
                         (increment-slug
                          (document/add-initial-slash (.value slug-el))))
      (document/get-doc (.value slug-el) handle-duplicate-slug-callback))))

(defn handle-duplicate-custom-slug-callback [e]
  (let [status (. (.target e) (getStatus))
        slug-el (dom/getElement "slug")
        slug-label-el (dom/getElement "slug-label")
        status-el (dom/getElement "status-message")]
    (cond
     (= status 200) (ui/display-error status-el
                                      slug-not-unique-err
                                      slug-el
                                      slug-label-el)
     :else (when (= (. status-el textContent) slug-not-unique-err)
             (ui/remove-error status-el slug-el slug-label-el)))))

(defn create-document-title-token [title]
  (string/join "-" (filter #(not (string/blank? %)) (.split title #"[^a-zA-Z0-9]"))))

(defn create-slug [feed title]
  (let [today (util/date-now)]
    (loop [slug (:default-slug-format feed)
           substitutions [["{document-title}" (create-document-title-token title)]
                          ["{feed-name}" (:name feed)]
                          ["{year}" (:year today)]
                          ["{month}" (:month today)]
                          ["{day}" (:day today)]
                          ["{ext}" "html"]]]
      (if (pos? (count substitutions))
        (recur (string/replace slug
                               (first (first substitutions))
                               (last (first substitutions)))
               (rest substitutions))
        slug))))

(defn sync-slug-with-title [feed]
  (when-not (.checked (dom/getElement "custom-slug"))
    (let [title (.value (dom/getElement "title"))
          slug-el (dom/getElement "slug")]
      (ui/set-form-value slug-el (create-slug feed title))
      (document/get-doc (.value slug-el) handle-duplicate-slug-callback))))

(defn toggle-custom-slug [feed]
  (let [slug-el (dom/getElement "slug")]
    (if (.checked (dom/getElement "custom-slug"))
      (ui/enable-element slug-el)
      (do
        (ui/disable-element slug-el)
        (ui/remove-error (dom/getElement "status-message")
                           slug-el
                           (dom/getElement "slug-label"))
        (sync-slug-with-title feed)))))

(defn validate-slug []
  (if (.checked (dom/getElement "custom-slug"))
    (let [status-el (dom/getElement "status-message")
          slug-el (dom/getElement "slug")
          slug-label-el (dom/getElement "slug-label")
          slug (.value slug-el)
          err #(ui/display-error status-el % slug-el slug-label-el)
          dash-slash-err slug-has-consecutive-dashes-or-slashes-err]
      (cond
       (string/blank? slug) (err slug-required-err)
       (not (= (first slug) "/")) (err slug-initial-slash-required-err)
       (slug-has-invalid-chars? slug) (err slug-has-invalid-chars-err)
       (util/has-consecutive-dashes-or-slashes? slug) (err dash-slash-err)
       :else (ui/remove-error status-el slug-el slug-label-el))

      (when (and (not (string/blank? slug)) (= (first slug) "/"))
        (document/get-doc (.value slug-el) handle-duplicate-custom-slug-callback)))))

(def editor-field (atom nil))

(defn get-document-value-map! [feed-name]
  {:feed feed-name
   :title (.value (dom/getElement "title"))
   :slug (.value (dom/getElement "slug"))
   :draft (.checked (dom/getElement "draft"))
   :content (.getCleanContents @editor-field @editor-field)})

(defn save-new-document-xhr-callback [e]
  (let [xhr (.target e)]
    (if (= (.getStatus xhr e) 201)
      (let [json (js->clj (.getResponseJson xhr e))]
        (core/navigate-replace-state (str ("feed" json) "/edit" ("slug" json))
                                     (str "Edit \"" ("title" json) "\"")))
      (ui/display-error (dom/getElement "status-message")
                        could-not-create-document-err))))

(defn save-new-document-click-callback [feed-name e]
  (document/create-doc save-new-document-xhr-callback
                       feed-name
                       (get-document-value-map! feed-name)))

(defn save-existing-document-xhr-callback [e]
  (let [xhr (.target e)]
    (if (= (. xhr (getStatus)) 200)
      nil ; TODO: display status message
      (ui/display-error (dom/getElement "status-message")
                        could-not-save-document-err))))

(defn save-existing-document-click-callback [feed-name e]
  (document/update-doc (.value (dom/getElement "slug"))
                       save-existing-document-xhr-callback
                       (get-document-value-map! feed-name)))

(defn render-editor-template [data]
  (do
    (soy/renderElement
     (dom/getElement "main-page") tpl/editor (util/map-to-obj data))
    (core/xhrify-internal-links! (core/get-internal-links!))))

(defn render-document-not-found-template []
  (soy/renderElement (dom/getElement "main-page") tpl/document-not-found))

(defn render-editor
  ([feed tpl-map] (render-editor feed tpl-map nil))
  ([feed tpl-map content]
     (if (= "new" (:status tpl-map))
       (do
         (render-editor-template tpl-map)
         (events/listen (dom/getElement "title")
                        event-type/INPUT
                        (partial sync-slug-with-title feed))
         (events/listen (dom/getElement "slug")
                        event-type/INPUT
                        validate-slug)
         (events/listen (dom/getElement "custom-slug")
                        event-type/CHANGE
                        (partial toggle-custom-slug feed))
         (events/listen (dom/getElement "save-document")
                        "click"
                        (partial save-new-document-click-callback (:name feed))))
       (do
         (render-editor-template tpl-map)
         (events/listen (dom/getElement "save-document")
                        "click"
                        (partial save-existing-document-click-callback
                                 (:name feed)))))

       (let [editor (create-editor-field "content")
             toolbar (create-editor-toolbar "toolbar")]
         (reset! editor-field editor)
         (when content
           (. editor (setHtml false content true false)))

         (do
           (register-editor-plugins editor)
           (goog.ui.editor.ToolbarController. editor toolbar)
           (. editor (makeEditable))))))

(defn start-default-mode! [feed slug status]
  (if (= :new status)
    (do
      (util/set-page-title! "New document")
      (render-editor feed
                     {:status "new"
                      :feed (:name feed)
                      :title ""
                      :slug ""
                      :draft false}))
    (document/get-doc slug
                      (fn [e]
                        (let [xhr (.target e)]
                          (if (= (. xhr (getStatus)) 200)
                            (let [json (js->clj (. xhr (getResponseJson)))]
                              (util/set-page-title!
                               (str "Edit \"" ("title" json) "\""))
                              (render-editor feed
                                             {:status "edit"
                                              :feed ("feed" json)
                                              :title ("title" json)
                                              :slug ("slug" json)
                                              :draft ("draft" json)}
                                             ("content" json)))
                            (render-document-not-found-template)))))))

(defn start-image-mode! [feed slug status]
  (if (= :new status)
    (do
      (util/set-page-title! "New document")
      (render-editor feed
                     {:status "new"
                      :feed (:name feed)
                      :title ""
                      :slug ""
                      :draft false}))
    (document/get-doc slug
                      (fn [e]
                        (let [xhr (.target e)]
                          (if (= (. xhr (getStatus)) 200)
                            (let [json (js->clj (. xhr (getResponseJson)))]
                              (util/set-page-title!
                               (str "Edit \"" ("title" json) "\""))
                              (render-editor feed
                                             {:status "edit"
                                              :feed ("feed" json)
                                              :title ("title" json)
                                              :slug ("slug" json)
                                              :draft ("draft" json)}
                                             ("content" json)))
                            (render-document-not-found-template)))))))

(defn start-mode-callback! [slug status e]
  (let [xhr (.target e)]
    (when (= (. xhr (getStatus)) 200)
      (let [json (js->clj (. xhr (getResponseJson)))
            feed {:name ("name" json)
                  :default-slug-format ("default-slug-format" json)
                  :default-document-type ("default-document-type" json)}]
        (cond
         (= (:default-document-type feed) "image")
           (start-image-mode! feed slug status)
         :default
           (start-default-mode! feed slug status))))))

(defn start [feed-name status uri]
  (let [slug (when (= status :edit)
               (str "/" (last (re-find #"^/admin/[^/]+/edit/(.*?)$" uri))))]
    (document/get-feed feed-name (partial start-mode-callback! slug status))))