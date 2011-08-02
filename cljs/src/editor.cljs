(ns vix.editor
  (:require [vix.document :as document]
            [clojure.string :as string]
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
            [goog.net.XhrIo :as xhr]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]))

(def slug-has-invalid-chars-err
  "Slugs can only contain '/', '-' and alphanumeric characters.")

(def slug-has-consecutive-dashes-or-slashes-err
  "Slugs shouldn't contain any consecutive '-' or '/' characters.")

(def slug-required-err
  "A valid slug is required for every document.")

(def slug-initial-slash-required-err
  "The slug needs to start with a '/'.")

(def slug-not-unique-err
  "This slug is not unique (document already exists).")

; FIXME: replace with string/blank? once implemented
(defn blank? [string]
  (if (= 0 (count string)) true false))

(defn get-feed-from-uri []
  (let [parts (re-find #"[^/]+/admin/([^/]+)/(.*?)" (js* "location.href"))]
    ;; TODO: throw error if feed isn't found
    (when (= 3 (count parts))
      (nth parts 1))))

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
  (let [status (.getStatus (.target e) e)
        slug-el (dom/getElement "slug")]
    (when (= status 200) 
      (set! (.value slug-el)
            (increment-slug (document/add-initial-slash (.value slug-el))))
      (document/get-doc (.value slug-el) handle-duplicate-slug-callback))))

(defn handle-duplicate-custom-slug-callback [e]
  (let [status (.getStatus (.target e) e)
        slug-el (dom/getElement "slug")
        status-el (dom/getElement "status-message")]
    (cond
     (= status 200) (display-slug-error status-el slug-el slug-not-unique-err)
     :else (when (= (.getTextContent status-el) slug-not-unique-err)
             remove-slug-error status-el slug-el))))

(defn slug-has-invalid-chars? [slug]
  (if (re-matches #"[/\-a-zA-Z0-9]+" slug) false true))

(defn slug-has-consecutive-dashes-or-slashes? [slug]
  (if (re-find #"[\-/]{2,}" slug) true false))

(defn create-slug [prefix title]
  (str prefix
       (string/join "-" (filter #(not (blank? %)) (.split title #"[^a-zA-Z0-9]")))))

(defn sync-slug-with-title []
  (when-not (.checked (dom/getElement "custom-slug"))
    (let [title (.value (dom/getElement "title"))
          slug-el (dom/getElement "slug")]
      (set! (.value slug-el)
            (create-slug (str "/" (get-feed-from-uri) "/") title))
      (document/get-doc (.value slug-el) handle-duplicate-slug-callback))))

(defn toggle-custom-slug []
  (let [slug-el (dom/getElement "slug")]
    (if (.checked (dom/getElement "custom-slug"))
      (doto slug-el
        (classes/remove "disabled")
        (.removeAttribute "disabled" "-1"))
      (do
        (doto slug-el
          (classes/add "disabled")
          (.setAttribute "disabled" "disabled"))
        (remove-slug-error (dom/getElement "status-message") slug-el)
        (sync-slug-with-title)))))

(defn display-slug-error [status-el slug-el message]
  (do
    (dom/setTextContent status-el message)
    (classes/remove status-el "status-ok")
    (classes/add slug-el "error")
    (classes/add status-el "status-error")))

(defn remove-slug-error [status-el slug-el]
  (when (classes/has slug-el "error")
    (classes/remove status-el "status-error")
    (classes/remove status-el "error")
    (dom/setTextContent status-el " ")
    (classes/remove slug-el "error")))

(defn validate-slug []
  (if (.checked (dom/getElement "custom-slug"))
    (let [status-el (dom/getElement "status-message")
          slug-el (dom/getElement "slug")
          slug (.value slug-el)
          err (partial display-slug-error status-el slug-el)
          dash-slash-err slug-has-consecutive-dashes-or-slashes-err]
      (cond
       (blank? slug) (err slug-required-err)
       (not (= (first slug) "/")) (err slug-initial-slash-required-err)
       (slug-has-invalid-chars? slug) (err slug-has-invalid-chars-err)
       (slug-has-consecutive-dashes-or-slashes? slug) (err dash-slash-err)
       :else (remove-slug-error status-el slug-el))
      (do
        (document/get-doc (.value slug-el) handle-duplicate-custom-slug-callback)))))

(defn enable-editor []
  (let [editor (create-editor-field "content")
        toolbar (create-editor-toolbar "toolbar")]
    (do
      (register-editor-plugins editor)
      (goog.ui.editor.ToolbarController. editor toolbar)
      (.makeEditable editor editor)))
  
    ;; these events should only exist for new docs (existing slugs are immutable)
    (when (re-matches #"(.*?)/admin/[a-z]+/new" (js* "location.href"))
      (events/listen (dom/getElement "title")
                     event-type/INPUT
                     sync-slug-with-title)
      (events/listen (dom/getElement "slug")
                     event-type/INPUT
                     validate-slug)
      (events/listen (dom/getElement "custom-slug")
                     event-type/CHANGE
                     toggle-custom-slug))
    nil)

(enable-editor)