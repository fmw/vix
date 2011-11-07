(ns vix.views.editor
  (:require [vix.core :as core]
            [vix.document :as document]
            [vix.ui :as ui]
            [vix.util :as util]
            [vix.templates.editor :as tpl]
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
            [goog.editor.plugins.LinkDialogPlugin :as LinkDialogPlugin]
            [goog.editor.plugins.LinkBubble :as LinkBubble]
            [goog.editor.Command :as buttons]
            [goog.ui.editor.DefaultToolbar :as DefaultToolbar]
            [goog.ui.editor.ToolbarController :as ToolbarController]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.dom :as dom]
            [goog.dom.classes :as classes]
            [goog.crypt.base64 :as base64]))

(def document-title-required-err
  "The document title value is required.")

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

(def invalid-filetype-err
  "The filetype for the file you are trying to upload isn't supported.")

(def file-required-err
  "This editing mode requires a file to be added.")

(defn slug-has-invalid-chars? [slug]
  (if (re-matches #"[/\-a-zA-Z0-9\.]+" slug) false true))


(defn html-with-clean-image-uris [html]
  (let [get-num-sub-paths (fn [s] (when (string? s)
                                    (count (re-seq #"../" s))))
        unsorted-pairs (map (fn [re-pair]
                              (let [orig-src (nth re-pair 1)]
                                [orig-src
                                 (str
                                  (when-not (re-matches
                                             #"^http[s]{0,1}(.*)"
                                             orig-src)
                                    "/")
                                  (string/replace orig-src "../" ""))]))
                            (re-seq #"<img src=\"(.*?)\"" html))]
    ;; need to sort, otherwise shorter links to the same image mess
    ;; up longer ones
    (loop [modified-html html
           img-uri-pairs (sort-by get-num-sub-paths unsorted-pairs)]
    (if (pos? (count img-uri-pairs))
      (recur (string/replace modified-html
                             (first (last img-uri-pairs))
                             (last (last img-uri-pairs)))
             (butlast img-uri-pairs))
      modified-html))))

(defn display-image-feeds []
  (document/get-feeds-list
   (fn [e]
     (let [xhr (.target e)
           json (. xhr (getResponseJson))
           first-feed (first json)]
       (if (pos? (count json))
         (display-images (.language first-feed) (.name first-feed) json)
         (ui/render-template (dom/getElement "editor-images")
                             tpl/no-image-feeds-found))))
   "image"))

(defn display-images
  ([language feed-name all-feeds]
     (display-images language feed-name all-feeds nil []))
  ([language feed-name all-feeds current-page previous-pages]
     (let [display-images-xhr-callback
           (fn [e]
             (let [xhr (.target e)
                   json (. xhr (getResponseJson))
                   docs (when (pos? (count (.documents json)))
                          (.documents json))
                   first-doc (when docs
                               (first docs))
                   current-page (when docs
                                  (or current-page
                                      {:startkey-published
                                       (.published first-doc)
                                       :startkey_docid (._id first-doc)}))
                   next-page (when docs
                               {:startkey-published (when (.next json)
                                                      (.published
                                                       (.next json)))
                                :startkey_docid (when (.next json)
                                                  (.startkey_docid
                                                   (.next json)))})]

               (ui/render-template (dom/getElement "editor-images")
                                   tpl/editor-images
                                   {:hasPrev (pos? (count previous-pages))
                                    :feeds all-feeds
                                    :feed feed-name
                                    :language language
                                    :json json})
               (let [image-feeds-select-el (dom/getElement "image-feeds")]
                 (events/listen image-feeds-select-el
                                event-type/CHANGE
                                (fn [evt]
                                  (let [chunks (string/split
                                                (.value
                                                 image-feeds-select-el)
                                                ":")
                                        language (nth chunks 0)
                                        feed-name (nth chunks 1)]
                                    (display-images language
                                                    feed-name
                                                    all-feeds)))))

               (when (pos? (count previous-pages))
                 (events/listen (dom/getElement
                                 "editor-images-pagination-prev-link")
                                event-type/CLICK
                                (fn [evt]
                                  (. evt (preventDefault))
                                  ;; conj will mess up the order of
                                  ;; the elements when called on nil
                                  ;; (because only vectors have the
                                  ;; expected order), so make sure an
                                  ;; empty vector is passed instead of nil.
                                           (display-images feed
                                                           all-feeds
                                                           (last
                                                            previous-pages)
                                                           (or
                                                            (butlast
                                                             previous-pages)
                                                            [])))))
               (when (.next json)
                 (events/listen
                  (dom/getElement "editor-images-pagination-next-link")
                  event-type/CLICK
                  (fn [evt]
                    (. evt (preventDefault))
                    (display-images language
                                    feed-name
                                    all-feeds
                                    next-page
                                    (conj previous-pages
                                          current-page)))))))]
       (document/get-documents-for-feed language
                                        feed-name
                                        display-images-xhr-callback
                                        6
                                        (:startkey-published current-page)
                                        (:startkey_docid current-page)))))

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
                           buttons/IMAGE
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

(defn create-slug! [title feed]
  (util/create-slug (:default-slug-format feed)
                    title
                    feed
                    (util/date-now!)
                    (if (and (= (:default-document-type feed) "image")
                             (:extension (:data @*file*)))
                      (:extension (:data @*file*))
                      "html")))

(defn sync-slug-with-title [feed]
  (when-not (.checked (dom/getElement "custom-slug"))
    (let [title (.value (dom/getElement "title"))
          slug-el (dom/getElement "slug")]
      (ui/set-form-value slug-el (create-slug! title feed))
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

(defn validate-title []
  (let [status-el (dom/getElement "status-message")
        title-el (dom/getElement "title")
        title-label-el (dom/getElement "title-label")]
    (if (string/blank? (.value title-el))
      (ui/display-error status-el
                        document-title-required-err
                        title-el
                        title-label-el)
      (ui/remove-error status-el
                       title-el
                       title-label-el))))

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
        (document/get-doc (.value slug-el)
                          handle-duplicate-custom-slug-callback)))))

(def editor-field (atom nil))

(defn get-document-value-map! [language feed-name]
  {:feed [language feed-name]
   :title (.value (dom/getElement "title"))
   :slug (.value (dom/getElement "slug"))
   :draft (.checked (dom/getElement "draft"))
   :content (html-with-clean-image-uris
              (.getCleanContents @editor-field @editor-field))})

(defn save-new-document-xhr-callback [e]
  (let [xhr (.target e)]
    (if (= (.getStatus xhr e) 201)
      (let [json (js->clj (. xhr (getResponseJson)))]
        (core/navigate-replace-state (str ("language" json)
                                          "/"
                                          ("feed" json)
                                          "/edit"
                                          ("slug" json))
                                     (str "Edit \"" ("title" json) "\"")))
      (ui/display-error (dom/getElement "status-message")
                        could-not-create-document-err))))

(defn save-new-document-click-callback [language feed-name e]
  (document/create-doc save-new-document-xhr-callback
                       language
                       feed-name
                       (get-document-value-map! language feed-name)))

(defn save-existing-document-xhr-callback [e]
  (let [xhr (.target
             e)]
    (if (= (. xhr (getStatus)) 200)
      nil ; TODO: display status message
      (ui/display-error (dom/getElement "status-message")
                        could-not-save-document-err))))

(defn save-existing-document-click-callback [language feed-name e]
  (document/update-doc (.value (dom/getElement "slug"))
                       save-existing-document-xhr-callback
                       (get-document-value-map! language feed-name)))

(defn strip-filename-extension [filename]
  (let [pieces (re-find #"^(.*?)\.[a-zA-Z0-9]{1,10}$" filename)]
    (if (= (count pieces) 2)
      (nth pieces 1)
      filename)))

(def *file* (atom {}))

(defn display-image-preview [file title]
  (let [reader (new (js* "FileReader"))]
    (set! (.onload reader)
          (fn [e]
            (ui/render-template (dom/getElement "image-preview")
                                tpl/image
                                {:title title
                                 :src (.result (.target e))})))
    (. reader (readAsDataURL file))))

(defn save-image-document-click-callback [create? language feed-name]
  (let [file (:obj @*file*)]
    (if file
      (let [reader (new (js* "FileReader"))]
        (set! (.onload reader)
              (fn [e]
                ;; FIXME: change when a better map-to-json converter
                ;; is implemented
                (let [image-data (util/map-to-obj
                                  {:type (.type file)
                                   :data (base64/encodeString
                                          (.result (.target e)))})]
                  (if create?
                    (document/create-doc save-new-document-xhr-callback
                                         language
                                         feed-name
                                         (assoc
                                             (get-document-value-map!
                                              language
                                              feed-name)
                                           :attachment
                                           image-data))
                    (document/update-doc (.value (dom/getElement "slug"))
                                         save-existing-document-xhr-callback
                                         (assoc
                                             (get-document-value-map!
                                              language
                                              feed-name)
                                           :attachment
                                           image-data))))))
        (. reader (readAsBinaryString file)))
      (if-not create?
        ;; update without changing image
        (document/update-doc (.value (dom/getElement "slug"))
                             save-existing-document-xhr-callback
                             (get-document-value-map! language feed-name))
        (ui/display-error (dom/getElement "status-message")
                          file-required-err)))))

(defn handle-image-drop-callback [feed status e]
  (do
    (. e (preventDefault))
    (. e (stopPropagation)))
  
  (let [status-el (dom/getElement "status-message")
        image-information-el (dom/getElement "image-information")
        file (aget (.files (.dataTransfer e)) 0)
        title (string/join " "
                           (filter #(not (string/blank? %))
                                   (.split (strip-filename-extension (.name file))
                                           #"[^a-zA-Z0-9]")))
        extension (cond
                   (= (.type file) "image/png") "png"
                   (= (.type file) "image/gif") "gif"
                   (= (.type file) "image/jpeg") "jpg")]
    (if extension
      (do
        (swap! *file* assoc :obj file :data {:extension extension})
        (ui/remove-error status-el)
        (ui/set-form-value (dom/getElement "title") title)
        (display-image-preview file title)
        (classes/remove image-information-el "hide")
        (ui/render-template image-information-el
                            tpl/image-information
                            {:filename (.name file)
                             :filetype (.type file)
                             :size (/ (.size file) 1024)})
        
        (when (= status "new")
          (sync-slug-with-title feed)))
      (do
        (swap! *file* dissoc :obj :data)
        (classes/add image-information-el "hide")
        (ui/display-error status-el invalid-filetype-err)))))

(defn render-editor-template [mode data]
  (ui/render-template (dom/getElement "main-page")
                      (cond
                       (= mode :image) tpl/image-mode
                       (= mode :default) tpl/default-mode)
                      data)
  (core/xhrify-internal-links! (core/get-internal-links!)))

(defn render-document-not-found-template []
  (ui/render-template (dom/getElement "main-page") tpl/document-not-found))

(defn render-editor
  ([feed tpl-map] (render-editor feed tpl-map nil))
  ([feed tpl-map content]
     (let [new? (= "new" (:status tpl-map))
           mode (cond
                 (= (:default-document-type feed) "image") :image
                 :default :default)
           tpl-map (if (and (= mode :image) (not new?))
                     (assoc tpl-map :image (str "data:"
                                                (:type (:attachment tpl-map))
                                                ";base64,"
                                                (:data
                                                 (:attachment tpl-map))))
                     tpl-map)]
       (if new?
         (do
           (render-editor-template mode tpl-map)
           (events/listen (dom/getElement "title")
                          event-type/INPUT
                          (fn [e]
                            (do
                              (validate-title)
                              (sync-slug-with-title feed))))
           (events/listen (dom/getElement "slug")
                          event-type/INPUT
                          validate-slug)
           (events/listen (dom/getElement "custom-slug")
                          event-type/CHANGE
                          (partial toggle-custom-slug feed)))
         (do
           (render-editor-template mode tpl-map)))

       (let [error? #(classes/has (dom/getElement "status-message") "error")
             validate-title-and-get-save-fn
             (fn [e]
               (validate-title)
               (when-not (error?)
                 (if new?
                   (cond
                    (= mode :image)
                    (do
                      (save-image-document-click-callback
                       true
                       (:language feed)
                       (:name feed)))
                    :default
                    (do
                      (save-new-document-click-callback
                       (:language feed)
                       (:name feed))))
                   (cond
                    (= mode :image)
                    (do
                      (save-image-document-click-callback
                       false
                       (:language feed)
                       (:name feed)))
                    :default
                    (do
                      (save-existing-document-click-callback
                       (:language feed)
                       (:name feed)))))))]
         
         (events/listen (dom/getElement "save-document")
                        "click"
                        #(validate-title-and-get-save-fn)))

       (when (= mode :image)
         (let [drop-target-el (dom/getElement "image-drop-target")]
           (. drop-target-el (addEventListener "drop"
                                               (partial
                                                handle-image-drop-callback
                                                feed
                                                (:status tpl-map))
                                               false))
           (. drop-target-el (addEventListener "dragover"
                                               (fn [e]
                                                 (. e (preventDefault))
                                                 (. e (stopPropagation)))
                                               false))))

       (let [editor (create-editor-field "content")
             toolbar (create-editor-toolbar "toolbar")]
         (reset! editor-field editor)
         (when content
           (. editor (setHtml false content true false)))

         (do
           (register-editor-plugins editor)
           (goog.ui.editor.ToolbarController. editor toolbar)
           (. editor (makeEditable))
           
           (events/listen (dom/getElement "image")
                          event-type/CLICK
                          (fn [e]
                            (let [editor-images-el (dom/getElement
                                                    "editor-images")]
                              (if (. editor-images-el (hasChildNodes))
                                (set! (.innerHTML editor-images-el) "")
                                (display-image-feeds))))))))))

(defn start-default-mode! [feed slug status]
  (if (= :new status)
    (do
      (util/set-page-title! "New document")
      (render-editor feed
                     {:status "new"
                      :feed (:name feed)
                      :language (:language feed)
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
                                              :language (:language feed)
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
                      :language (:language feed)
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
                                              :language (:language feed)
                                              :title ("title" json)
                                              :slug ("slug" json)
                                              :draft ("draft" json)
                                              :attachment
                                              {:type ("type"
                                                      ("attachment"
                                                       json))
                                               :data ("data"
                                                      ("attachment"
                                                       json))}}
                                             ("content" json)))
                            (render-document-not-found-template)))))))

(defn start-mode-callback! [slug status e]
  (let [xhr (.target e)]
    (when (= (. xhr (getStatus)) 200)
      (let [json (js->clj (. xhr (getResponseJson)))
            feed {:language ("language" json)
                  :name ("name" json)
                  :default-slug-format ("default-slug-format" json)
                  :default-document-type ("default-document-type" json)}]
        (cond
         (= (:default-document-type feed) "image")
           (start-image-mode! feed slug status)
         :default
           (start-default-mode! feed slug status))))))

(defn start [language feed-name slug status]
  (swap! *file* dissoc :obj :data)
  (document/get-feed language
                     feed-name
                     (partial start-mode-callback! slug status)))