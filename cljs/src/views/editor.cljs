;; cljs/src/views/editor.cljs: UI implementation for document editor.
;; Copyright 2011-2012, Vixu.com, F.M. (Filip) de Waard <fmw@vixu.com>.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;; http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns vix.views.editor
  (:require [vix.core :as core]
            [vix.document :as document]
            [vix.ui :as ui]
            [vix.util :as util]
            [vix.templates.editor :as tpl]
            [clojure.string :as string]
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

(def link-label-required-err
  "The link label is required.")

(def link-label-has-invalid-chars-err
  (str "Labels can only contain '/', '-', '.', '?', '!' "
       "and alphanumeric characters."))

(defn slug-has-invalid-chars? [slug]
  (if (re-matches #"[/\-a-zA-Z0-9\.]+" slug) false true))

(defn link-label-has-invalid-chars? [label]
  (if (re-matches #"[\u0080-\uffffa-zA-Z0-9.?! -]+" label)
    false
    true))

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

(defn display-image-feeds [language]
  (document/get-feeds-list
   (fn [e]
     (let [xhr (.-target e)
           json (. xhr (getResponseJson))
           first-feed (first json)]
       (if (pos? (count json))
         (display-images (.-language first-feed) (.-name first-feed) json)
         (ui/render-template (dom/getElement "editor-images")
                             tpl/no-image-feeds-found))))
   "image"
   language))

(defn display-images
  ([language feed-name all-feeds]
     (display-images language feed-name all-feeds nil []))
  ([language feed-name all-feeds current-page previous-pages]
     (let [display-images-xhr-callback
           (fn [e]
             (let [xhr (.-target e)
                   json (. xhr (getResponseJson))
                   docs (when (pos? (count (.-documents json)))
                          (.-documents json))
                   first-doc (when docs
                               (first docs))
                   current-page (when docs
                                  (or current-page
                                      {:startkey-published
                                       (.-published first-doc)
                                       :startkey_docid (.-_id first-doc)}))
                   next-page (when docs
                               {:startkey-published (when (.-next json)
                                                      (.-published
                                                       (.-next json)))
                                :startkey_docid (when (.-next json)
                                                  (.-startkey_docid
                                                   (.-next json)))})]

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
                                                (.-value
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
                                  (display-images language
                                                  feed-name
                                                  all-feeds
                                                  (last
                                                   previous-pages)
                                                  (or
                                                   (butlast
                                                    previous-pages)
                                                   [])))))
               (when (.-next json)
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

(defn create-editor-toolbar [element-id & included-buttons]
  (let [buttons-map {:bold buttons/BOLD
                     :italic buttons/ITALIC
                     :underline buttons/UNDERLINE
                     :font-color buttons/FONT_COLOR
                     :background-color buttons/BACKGROUND_COLOR
                     :font-face buttons/FONT_FACE
                     :font-size buttons/FONT_SIZE
                     :format-block buttons/FORMAT_BLOCK
                     :link buttons/LINK
                     :image buttons/IMAGE
                     :undo buttons/UNDO
                     :redo buttons/REDO
                     :unordered-list buttons/UNORDERED_LIST
                     :ordered-list buttons/ORDERED_LIST
                     :indent buttons/INDENT
                     :outdent buttons/OUTDENT
                     :justify-left buttons/JUSTIFY_LEFT
                     :justify-center buttons/JUSTIFY_CENTER
                     :justify-right buttons/JUSTIFY_RIGHT
                     :subscript buttons/SUBSCRIPT
                     :superscript buttons/SUPERSCRIPT
                     :strike-through buttons/STRIKE_THROUGH
                     :remove-format buttons/REMOVE_FORMAT
                     :edit-html buttons/EDIT_HTML}
        buttons (to-array
                 (map #(get buttons-map %) (or included-buttons
                                               [:bold
                                                :italic
                                                :underline
                                                :strike-through
                                                :font-color
                                                :background-color
                                                :font-face
                                                :font-size
                                                :link
                                                :image
                                                :undo
                                                :redo
                                                :unordered-list
                                                :ordered-list
                                                :indent
                                                :outdent
                                                :justify-left
                                                :justify-center
                                                :justify-right
                                                :subscript
                                                :superscript
                                                :remove-format])))]
    (DefaultToolbar/makeToolbar buttons (dom/getElement element-id))))

(defn increment-slug [slug]
  (if-let [slug-matches (re-matches #"(.*?)-([0-9]+)$" slug)]
    (str (nth slug-matches 1) "-" (inc (js/parseInt (last slug-matches))))
    (str slug "-2")))

(defn handle-duplicate-slug-callback [e]
  (let [status (. (.-target e) (getStatus))
        slug-el (dom/getElement "slug")]
    (when (= status 200) 
      (ui/set-form-value slug-el
                         (increment-slug
                          (document/add-initial-slash (.-value slug-el))))
      (document/get-doc (.-value slug-el) handle-duplicate-slug-callback))))

(defn handle-duplicate-custom-slug-callback [e]
  (let [status (. (.-target e) (getStatus))
        slug-el (dom/getElement "slug")
        slug-label-el (dom/getElement "slug-label")
        status-el (dom/getElement "status-message")]
    (cond
     (= status 200) (ui/display-error status-el
                                      slug-not-unique-err
                                      slug-el
                                      slug-label-el)
     :else (when (= (. status-el -textContent) slug-not-unique-err)
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
  (when-not (.-checked (dom/getElement "custom-slug"))
    (let [title (.-value (dom/getElement "title"))
          slug-el (dom/getElement "slug")]
      (ui/set-form-value slug-el (create-slug! title feed))
      (document/get-doc (.-value slug-el) handle-duplicate-slug-callback))))

(defn toggle-custom-slug [feed]
  (let [slug-el (dom/getElement "slug")]
    (if (.-checked (dom/getElement "custom-slug"))
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
    (if (string/blank? (.-value title-el))
      (ui/display-error status-el
                        document-title-required-err
                        title-el
                        title-label-el)
      (ui/remove-error status-el
                       title-el
                       title-label-el))))

(defn validate-slug []
  (if (.-checked (dom/getElement "custom-slug"))
    (let [status-el (dom/getElement "status-message")
          slug-el (dom/getElement "slug")
          slug-label-el (dom/getElement "slug-label")
          slug (.-value slug-el)
          err #(ui/display-error status-el % slug-el slug-label-el)
          dash-slash-err slug-has-consecutive-dashes-or-slashes-err]
      (cond
       (string/blank? slug) (err slug-required-err)
       (not (= (first slug) "/")) (err slug-initial-slash-required-err)
       (slug-has-invalid-chars? slug) (err slug-has-invalid-chars-err)
       (util/has-consecutive-dashes-or-slashes? slug) (err dash-slash-err)
       :else (ui/remove-error status-el slug-el slug-label-el))

      (when (and (not (string/blank? slug)) (= (first slug) "/"))
        (document/get-doc (.-value slug-el)
                          handle-duplicate-custom-slug-callback)))))

(def editor-field (atom nil))
(def description-editor-field (atom nil))

(defn get-document-value-map!
  ([language feed-name]
     (get-document-value-map! language feed-name nil))
  ([language feed-name content]
     {:feed feed-name
      :language language
      :title (ui/get-form-value "title")
      :subtitle (when-let [subtitle-el (dom/getElement "subtitle")]
                  (ui/get-form-value subtitle-el))
      :slug (ui/get-form-value "slug")
      :draft (.-checked (dom/getElement "draft"))
      :content (or content (html-with-clean-image-uris
                             (.getCleanContents @editor-field
                                                @editor-field)))
      :description (if @description-editor-field
                     (html-with-clean-image-uris
                       (.getCleanContents @description-editor-field
                                          @description-editor-field)))
      :start-time (when-let [start-time-el (dom/getElement "start-time")]
                    (ui/get-form-value  start-time-el))
      :end-time (when-let [end-time-el (dom/getElement "end-time")]
                  (ui/get-form-value end-time-el))
      :icon (when-let [icon-container-el (dom/getElement
                                          "icon-preview-container")]
              (when-let [icon-slug-el (first
                                       (util/get-children-by-tag
                                        icon-container-el
                                        "input"))]
                {:slug (.getAttribute icon-slug-el "value")
                 :title (.-innerHTML (first
                                      (util/get-children-by-tag
                                       icon-container-el
                                       "span")))}))
      :related-pages (vec
                      (get-related-pages
                       (dom/getElement "related-pages-container")))
      :related-images (vec
                       (get-related-images
                        (dom/getElement "related-images-container")))}))


(defn handle-successful-save []
  (let [save-button-el (dom/getElement "save-document")]
    ;; TODO: display status message
    (set! (.-innerHTML save-button-el) "Save")
    (set! (.-disabled save-button-el) false)))

(defn save-new-document-xhr-callback [e]
  (let [xhr (.-target e)]
    (if (= (.getStatus xhr e) 201)
      (let [json (js->clj (. xhr (getResponseJson)))]
        (core/navigate-replace-state (str ("language" json)
                                          "/"
                                          ("feed" json)
                                          "/edit"
                                          ("slug" json))
                                     (str "Edit \"" ("title" json) "\""))
        (handle-successful-save))
      (ui/display-error (dom/getElement "status-message")
                        could-not-create-document-err))))

(defn save-new-document-click-callback [language feed-name & e]
  (let [doc (get-document-value-map! language feed-name)]
    (document/create-doc (:slug doc)
                         save-new-document-xhr-callback
                         doc)))

(defn save-new-menu-document-click-callback [language feed-name & _]
  (let [doc (get-document-value-map! language
                                     feed-name
                                     (render-menu-content-string!))]
    (document/create-doc (:slug doc)
                         save-new-document-xhr-callback
                         doc)))

(defn save-existing-document-xhr-callback [e]
  (let [xhr (.-target e)]
    (if (= (. xhr (getStatus)) 200)
      (handle-successful-save)
      (ui/display-error (dom/getElement "status-message")
                        could-not-save-document-err))))

(defn save-existing-document-click-callback [language feed-name & _]
  (document/update-doc (.-value (dom/getElement "slug"))
                       save-existing-document-xhr-callback
                       (get-document-value-map! language feed-name)))

(defn save-existing-menu-document-click-callback [language feed-name & _]
  (document/update-doc (.-value (dom/getElement "slug"))
                       save-existing-document-xhr-callback
                       (get-document-value-map!
                        language
                        feed-name
                        (render-menu-content-string!))))

(defn strip-filename-extension [filename]
  (let [pieces (re-find #"^(.*?)\.[a-zA-Z0-9]{1,10}$" filename)]
    (if (= (count pieces) 2)
      (nth pieces 1)
      filename)))

(def *file* (atom {}))

(defn display-image-preview [file title]
  (let [reader (new js/FileReader)]
    (set! (.-onload reader)
          (fn [e]
            (ui/render-template (dom/getElement "image-preview")
                                tpl/image
                                {:title title
                                 :src (.-result (.-target e))})))
    (. reader (readAsDataURL file))))

(defn save-image-document-click-callback [create? language feed-name]
  (let [file (:obj @*file*)]
    (if file
      (let [reader (new js/FileReader)]
        (set! (.-onload reader)
              (fn [e]
                (let [image-data (util/map-to-obj
                                  {:type (.-type file)
                                   :data (base64/encodeString
                                          (.-result (.-target e)))})
                      doc (assoc (get-document-value-map! language feed-name)
                            :attachment image-data)]
                  (if create?
                    (document/create-doc (:slug doc)
                                         save-new-document-xhr-callback
                                         doc)
                    (document/update-doc (:slug doc)
                                         save-existing-document-xhr-callback
                                         doc)))))
        (. reader (readAsBinaryString file)))
      (if-not create?
        ;; update without changing image
        (document/update-doc (.-value (dom/getElement "slug"))
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
        file (aget (.-files (.-dataTransfer e)) 0)
        title (string/join " "
                           (filter #(not (string/blank? %))
                                   (.split (strip-filename-extension
                                            (.-name file))
                                           #"[^a-zA-Z0-9]")))
        extension (cond
                   (= (.-type file) "image/png") "png"
                   (= (.-type file) "image/gif") "gif"
                   (= (.-type file) "image/jpeg") "jpg")]
    (if extension
      (do
        (swap! *file* assoc :obj file :data {:extension extension})
        (ui/remove-error status-el)
        (ui/set-form-value (dom/getElement "title") title)
        (display-image-preview file title)
        (classes/remove image-information-el "hide")
        (ui/render-template image-information-el
                            tpl/image-information
                            {:filename (.-name file)
                             :filetype (.-type file)
                             :size (/ (.-size file) 1024)})
        
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
                       (= mode :menu) tpl/menu-mode
                       :default tpl/default-mode)
                      data)
  (core/xhrify-internal-links! (core/get-internal-links!)))

(defn render-document-not-found-template []
  (ui/render-template (dom/getElement "main-page") tpl/document-not-found))

(defn render-menu-content-string! []
  (let [links (get-links-from-ul (dom/getElement "menu-container"))
        menu-string (ui/render-template-as-string tpl/rendered-menu
                                                  {:links links})]
    (when (not (= menu-string "<ul></ul>"))
      menu-string)))

(defn get-link-data-from-li [el]
  (when-not (classes/has el "add-item-node") ; ignore "Add Item" in nested ul
    ;; remove meta tag (sometimes added due to a bug in Chrome)
    (let [children (remove #(= (.-tagName %) "meta") (util/get-children el))
          item-detail-elements (vec (util/get-children (first children)))
          label-el (nth item-detail-elements 0)
          uri-el (nth item-detail-elements 1)]
      {:label (. label-el -textContent)
       :uri (. uri-el -textContent)
       :children (when (= (.-tagName (last children)) "UL")
                   (get-links-from-ul (last children)))})))

(defn get-links-from-ul [el]
  (filter #(not (nil? %))
          (map get-link-data-from-li (util/get-children el))))

(defn parse-menu-data-li [el]
  (let [children (util/get-children el)
        last-child (last children)]
    {:label (.-textContent (first children))
     :uri (. (first children) (getAttribute "href"))
     :children (when (= (.-tagName last-child) "UL")
                 (parse-menu-data-ul last-child))}))

(defn parse-menu-data-ul [el]
  (map parse-menu-data-li (util/get-children el)))

(defn parse-menu-content-string [s]
  (let [dummy-list-el (dom/createElement "ul")]
    (set! (.-innerHTML dummy-list-el) (subs s 4 (- (count s) 5)))
    (parse-menu-data-ul dummy-list-el)))

(defn validate-link-label! [label]
  (let [error-message (cond
                       (string/blank? label)
                       link-label-required-err
                       (link-label-has-invalid-chars? label)
                       link-label-has-invalid-chars-err)]
    (if error-message
      (ui/display-error (dom/getElement "add-link-status")
                        error-message
                        (dom/getElement "link-label-label")
                        (dom/getElement "link-label"))
      (ui/remove-error (dom/getElement "add-link-status")
                       (dom/getElement "link-label-label")
                       (dom/getElement "link-label")))))

(defn menu-item-link-details-from-dialog! []
  {:label (.-value (dom/getElement "link-label"))
   :uri (if (menu-item-is-internal!)
          (.-value (dom/getElement "internal-link"))
          (.-value (dom/getElement "external-link")))})

(defn menu-item-is-internal! []
  (= (ui/get-form-value-by-name "add-menu-item-dialog-form"
                                "link-type")
     "internal"))

(defn menu-link-type-change-callback [e]
  (if (menu-item-is-internal!)
    (do
      (classes/add (dom/getElement "external-link-row") "hide")
      (classes/remove (dom/getElement "internal-link-row") "hide"))
    (do
      (classes/remove (dom/getElement "external-link-row") "hide")
      (classes/add (dom/getElement "internal-link-row") "hide"))))

(defn menu-dialog-upd-link-mode-xhr-callback [e]
  (let [documents (.-documents (. (.-target e) (getResponseJson)))]
    (ui/render-template (dom/getElement "internal-link")
                        tpl/document-link-options
                        {:documents documents})))

(defn call-with-feeds-and-documents [language f]    
  (document/get-feeds-list
   (fn [e]
     (let [feeds (filter #(not (= "menu"
                                  ("default-document-type" (js->clj %))))
                         (. (.-target e) (getResponseJson)))
           first-feed (first feeds)]
       (document/get-documents-for-feed
        (.-language first-feed)
        (.-name first-feed)
        (fn [e]
          (let [documents (.-documents (. (.-target e) (getResponseJson)))]
            (do
              (f feeds documents)))))))
   nil
   language))

(defn update-menu-builder [el links new nested feeds documents]
  (ui/render-template el
                      tpl/menu-items
                      {:links links
                       :new new
                       :nested nested})
  (create-menu-builder-events feeds documents)
  (make-menu-builder-sortable feeds documents))

(defn display-menu-builder [links feeds documents]
  (update-menu-builder (dom/getElement "menu-container")
                        links
                        nil
                        false
                        feeds
                        documents))

(defn update-menu-builder-from-data [content-string feeds documents]
  (display-menu-builder (parse-menu-content-string content-string)
                        feeds
                        documents))

(defn create-menu-builder-events [feeds documents]                  
  (ui/trigger-on-class
   "menu-item-delete-link"
   "click"
   (fn [e]
     (. e (preventDefault))
     (let [parent-li (util/get-parent (util/get-parent (.-target e)))
           grandparent (util/get-parent parent-li)]
       (dom/removeNode parent-li)
       ;; remove ul and re-render menu-container on empty nested lists
       (when (and grandparent
                  (= (.-tagName grandparent) "UL")
                  (not (= (.-id grandparent) "menu-container"))
                  (= (count (util/get-children grandparent)) 1))
         (dom/removeNode grandparent)
         (display-menu-builder
          (get-links-from-ul (dom/getElement "menu-container"))
          feeds
          documents)))))
  
  (ui/trigger-on-class
   "add-sub-item"
   "click"
   (fn [evt]                     
     (let [new-ul (dom/createElement "ul")]
       (classes/add new-ul "new-node")
       (set! (.-draggable new-ul) false)
       (dom/appendChild (util/get-parent (util/get-parent (.-target evt)))
                        new-ul)
       
       (add-menu-item-callback feeds documents new-ul evt))))

  (ui/trigger-on-class
   "add-item-to-nested-menu"
   "click"
   (fn [evt]
     (add-menu-item-callback feeds
                             documents
                             (util/get-parent
                              (util/get-parent
                               (.-target evt)))
                             evt))))

(defn make-menu-builder-sortable [feeds documents]
  (let [menu-container-el (dom/getElement "menu-container")]
    (ui/to-sortable-tree (dom/getElement menu-container-el)
                         #(display-menu-builder
                           (get-links-from-ul menu-container-el)
                           feeds
                           documents))))

(defn menu-item-dialog-callback [feeds documents parent-el e]
  (. e (preventDefault))
 
  (let [active-el (or parent-el menu-container-el)]
    (if (= "ok" (.-key e))
      (let [nested (classes/has parent-el "new-node")
            new-link-details (menu-item-link-details-from-dialog!)
            menu-container-el (dom/getElement "menu-container")]
        (validate-link-label! (:label new-link-details))
        (when-not (classes/has (dom/getElement "add-link-status") "error")
          (let [dummy-el (dom/createElement "ul")]
            (classes/remove parent-el "new-node")
            (set! (.-innerHTML dummy-el)
                  (ui/render-template-as-string tpl/menu-item-li
                                                new-link-details))

            (dom/appendChild parent-el (.-firstChild dummy-el))
          
            ;; re-render whole menu-container after ul is added
            (display-menu-builder
             (get-links-from-ul menu-container-el)
             feeds
             documents)
            (ui/remove-dialog))))
      (do
        (when (and (not (pos? (count (util/get-children active-el))))
                   (not (= (.-id active-el) "menu-container")))
          (dom/removeNode active-el))
        (ui/remove-dialog)))))

(defn update-related-pages [related-pages]
  (when (not-empty related-pages)
    (let [related-pages-el (dom/getElement "related-pages-container")]
      (ui/render-template related-pages-el
                          tpl/related-page-list-items
                          {:pages related-pages})

      (ui/trigger-on-class
       "related-page-delete-link"
       "click"
       (fn [evt]
         (. evt (preventDefault))
         (let [parent-li (util/get-parent (.-target evt))]
           (dom/removeNode parent-li)
           (update-related-pages (get-related-pages related-pages-el))))))))

(defn update-related-pages [related-pages]
  (when (not-empty related-pages)
    (let [related-pages-el (dom/getElement "related-pages-container")]
      (ui/render-template related-pages-el
                          tpl/related-page-list-items
                          {:pages related-pages})

      (ui/trigger-on-class
       "related-page-delete-link"
       "click"
       (fn [e]
         (. e (preventDefault))
         (let [parent-li (util/get-parent (.-target e))]
           (dom/removeNode parent-li)
           (update-related-pages (get-related-pages related-pages-el))))))))

(defn update-related-images [related-images]
  (when (not-empty related-images)
    (let [related-images-el (dom/getElement "related-images-container")]
      (ui/render-template related-images-el
                          tpl/related-image-list-items
                          {:images related-images})

      (ui/trigger-on-class
       "related-image-preview-link"
       "click"
       (fn [evt]
         (. evt (preventDefault))
         
         (ui/display-dialog "Image Preview"
                            (ui/render-template-as-string
                             tpl/image-preview-dialog
                             {:image (get-related-image
                                      (util/get-parent (.-target evt)))})
                            #(ui/remove-dialog)
                            :ok)))
            
      (ui/trigger-on-class
       "related-image-delete-link"
       "click"
       (fn [e]
         (. e (preventDefault))
         (let [parent-li (util/get-parent (.-target e))]
           (dom/removeNode parent-li)
           (update-related-images
            (get-related-images related-images-el))))))))

(defn update-icon-image! [slug title]
  (when (and slug title)
    (set! (.-text (dom/getElement "add-icon-image-link"))
          "Update icon image")
    
    (let [icon-preview-container-el (dom/getElement
                                     "icon-preview-container")]
      (ui/render-template icon-preview-container-el
                          tpl/related-image-list-item
                          {:slug slug
                           :title title})

      (ui/trigger-on-class
       "related-image-preview-link"
       "click"
       (fn [evt]
         (. evt (preventDefault))
         
         (ui/display-dialog "Image Preview"
                            (ui/render-template-as-string
                             tpl/image-preview-dialog
                             {:image {:slug slug
                                      :title title}})
                            #(ui/remove-dialog)
                            :ok)))
            
      (ui/trigger-on-class
       "related-image-delete-link"
       "click"
       (fn [e]
         (. e (preventDefault))
         (set! (.-innerHTML icon-preview-container-el) ""))))))

(defn remove-already-related-pages-from-documents
  [related-page-el documents]
  (let [existing-slugs (map :slug (get-related-pages related-page-el))]
    (util/col-to-js
     (remove
      #(some #{("slug" %)} existing-slugs)
      (js->clj documents)))))

(defn remove-already-related-images-from-documents
  [related-images-el documents]
  (let [existing-slugs (map :slug (get-related-images related-images-el))]
    (util/col-to-js
     (remove
      #(some #{("slug" %)} existing-slugs)
      (js->clj documents)))))

(defn remove-self-from-documents [self-slug documents]
  (util/col-to-js (remove #(= ("slug" %) self-slug) (js->clj documents))))

(defn related-pages-dialog-change-feed-xhr-callback [self-slug e]
  (let [documents (.-documents (. (.-target e) (getResponseJson)))]
    (ui/render-template (dom/getElement "internal-link")
                        tpl/document-link-options
                        {:documents
                         (remove-already-related-pages-from-documents
                          (dom/getElement "related-pages-container")
                          (remove-self-from-documents self-slug
                                                      documents))})))

(defn related-images-dialog-change-feed-xhr-callback [self-slug e]
  (let [documents (.-documents (. (.-target e) (getResponseJson)))]
    (ui/render-template (dom/getElement "internal-link")
                        tpl/document-link-options
                        {:documents
                         (remove-already-related-images-from-documents
                          (dom/getElement "related-images-container")
                          documents)})

    (update-image-in-dialog (first documents))))

(defn add-menu-item-callback
  ([feeds documents evt]
     (add-menu-item-callback feeds
                             documents
                             (dom/getElement "menu-container")
                             evt))
  ([feeds documents el evt]
     (. evt (preventDefault))
     
     (ui/display-dialog "Add Menu Item"
                        (ui/render-template-as-string
                         tpl/add-menu-item-dialog
                         {:feeds feeds
                          :documents documents})
                        (partial menu-item-dialog-callback
                                 feeds
                                 documents
                                 el))
      
     (let [feed-el (dom/getElement "internal-link-feed")]
       (events/listen feed-el
                      "change"
                      (fn []
                        (let [feed-pair (util/pair-from-string
                                         (.-value feed-el))]
                          (document/get-documents-for-feed
                           (first feed-pair)
                           (last feed-pair)
                           menu-dialog-upd-link-mode-xhr-callback)))))
      
     (events/listen (dom/getElement "link-type-external")
                    "change"
                    menu-link-type-change-callback)
      
     (events/listen (dom/getElement "link-type-internal")
                    "change"
                    menu-link-type-change-callback)))

(defn update-image-in-dialog [image]
  (ui/render-template (dom/getElement "image-preview-in-dialog-container")
                      tpl/related-image {:image image}))

(defn add-related-image-link-callback
  [language dialog-title dialog-callback-fn evt]
  (document/get-feeds-list
   (fn [evt]
     (let [feeds (. (.-target evt) (getResponseJson))
           related-images-el (dom/getElement "related-images-container")]
       (if (not-empty feeds)
         (document/get-documents-for-feed
          language
          ("name" (js->clj (first feeds)))
          (fn [evt]
            (let [documents (.-documents
                             (. (.-target evt) (getResponseJson)))]
              (ui/display-dialog
               dialog-title
               (ui/render-template-as-string
                tpl/add-related-image-dialog
                {:feeds
                 feeds
                 :documents
                 (if (= dialog-title "Add Related Image")
                   (remove-already-related-images-from-documents
                    related-images-el
                    documents)
                   documents)})
               dialog-callback-fn)

              (when-let [feed-el (dom/getElement "internal-link-feed")]
                (events/listen
                 feed-el
                 "change"
                 (fn []
                   (let [feed-pair (util/pair-from-string
                                    (.-value feed-el))]
                     (document/get-documents-for-feed
                      (first feed-pair)
                      (last feed-pair)
                      (partial
                       related-images-dialog-change-feed-xhr-callback
                       self-slug))))))

              (let [link-el (dom/getElement "internal-link")
                    img-el (dom/getElement "preview-image-in-dialog")]
                (events/listen
                 link-el
                 "change"
                 (fn [evt]
                   (let [slug (.-value link-el)
                         title (util/get-select-option-name-by-value link-el
                                                                     slug)]
                     (update-image-in-dialog {:title title
                                              :slug slug}))))))))
         (ui/display-dialog
          "Error"
          (ui/render-template-as-string
           tpl/error-dialog
           {:message "No image feeds for this language."})
          #(ui/remove-dialog)
          :ok))))
   "image"
   language))

(defn add-related-page-link-callback [self-slug feeds evt]
  (let [feeds (filter #(not (= "image"
                               ("default-document-type" (js->clj %))))
                      feeds)
        first-feed (first (js->clj feeds))]
    (document/get-documents-for-feed
     ("language" first-feed)
     ("name" first-feed)
     (fn [evt]
       (let [documents (.-documents (. (.-target evt) (getResponseJson)))]
         (ui/display-dialog "Add Related Page"
                          (ui/render-template-as-string
                           tpl/add-related-page-dialog
                           {:feeds
                            feeds
                            :documents
                            (remove-already-related-pages-from-documents
                             (dom/getElement "related-pages-container")
                             (remove-self-from-documents self-slug
                                                         documents))})
                          (partial add-related-page-dialog-callback
                                   (dom/getElement
                                    "related-pages-container")))
      
       (when-let [feed-el (dom/getElement "internal-link-feed")]
         (events/listen feed-el
                        "change"
                        (fn []
                          (let [feed-pair (util/pair-from-string
                                           (.-value feed-el))]
                            (document/get-documents-for-feed
                             (first feed-pair)
                             (last feed-pair)
                             (partial
                              related-pages-dialog-change-feed-xhr-callback
                              self-slug)))))))))))

(defn get-related-pages [related-page-el]
  (when related-page-el
    (map (fn [li-el]
           (let [slug-el (first (util/get-children-by-tag li-el "input"))
                 title-el (first (util/get-children-by-tag li-el "span"))]
             {:slug (.-value slug-el)
              :title (.-textContent title-el)}))
         (util/get-children related-page-el))))

(defn get-related-image [li-el]
  (let [slug-el (first (util/get-children-by-tag li-el "input"))
        title-el (first (util/get-children-by-tag li-el "span"))]
    {:slug (.-value slug-el)
     :title (.-textContent title-el)}))

(defn get-related-images [related-images-el]
  (when related-images-el
    (map get-related-image (util/get-children related-images-el))))

(defn add-related-page-dialog-callback [related-pages-el evt]
  (when (= "ok" (.-key evt))
    (let [link-el (dom/getElement "internal-link")
          slug (.-value link-el)]
      (if (pos? (count slug))
        (let [title (util/get-select-option-name-by-value link-el slug)]
          (update-related-pages (conj
                                 (get-related-pages related-pages-el)
                                 {:slug slug :title title}))))))
  (ui/remove-dialog))

(defn add-related-image-dialog-callback [related-images-el evt]
  (when (= "ok" (.-key evt))
    (let [link-el (dom/getElement "internal-link")
          slug (.-value link-el)]
      (if (not-empty slug)
        (let [title (util/get-select-option-name-by-value link-el slug)]
          (update-related-images (conj
                                 (get-related-images related-images-el)
                                 {:slug slug :title title}))))))
  (ui/remove-dialog))

(defn add-icon-image-dialog-callback [icon-image-el evt]
  (when (= "ok" (.-key evt))
    (let [link-el (dom/getElement "internal-link")
          slug (.-value link-el)]
      (if (not-empty slug)
        (let [title (util/get-select-option-name-by-value link-el slug)]
          (update-icon-image! slug title)))))
  (ui/remove-dialog))

;; FIX: fully refactor into something more modular and condensed
(defn render-editor
  ([feed feeds documents tpl-map]
     (render-editor feed feeds documents tpl-map nil))
  ([feed feeds documents tpl-map content]
     (let [new? (= "new" (:status tpl-map))
           mode (cond
                 (= (:default-document-type feed) "image")
                 :image
                 (= (:default-document-type feed) "menu")
                 :menu
                 (= (:default-document-type feed) "event")
                 :event
                 (= (:default-document-type feed) "with-description")
                 :with-description
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
                    (= mode :menu)
                    (do
                      (save-new-menu-document-click-callback
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
                    (= mode :menu)
                    (do
                      (save-existing-menu-document-click-callback
                       (:language feed)
                       (:name feed)))
                    :default
                    (do
                      (save-existing-document-click-callback
                       (:language feed)
                       (:name feed)))))))]
         
         (let [save-button-el (dom/getElement "save-document")]
           (events/listen save-button-el
                          "click"
                          (fn [e]
                            (set! (.-innerHTML save-button-el) "Saving...")
                            (set! (.-disabled save-button-el) true)
                            (validate-title-and-get-save-fn)))))

       (cond
        (= mode :image)
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
                                              false)))
        (= mode :menu)
        (do
          (events/listen (dom/getElement "add-menu-item")
                         "click"
                         (partial add-menu-item-callback
                                  feeds
                                  documents))))

       (when-let [add-related-page-link-element (dom/getElement
                                                 "add-related-page-link")]
         (update-related-pages (:related-pages tpl-map))
         (events/listen add-related-page-link-element
                        event-type/CLICK
                        (partial add-related-page-link-callback
                                 (:slug tpl-map)
                                 feeds)))

       (when-let [add-related-image-link-element (dom/getElement
                                                  "add-related-image-link")]
         (update-related-images (:related-images tpl-map))
         (events/listen add-related-image-link-element
                        event-type/CLICK
                        (partial add-related-image-link-callback
                                 (:language feed)
                                 "Add Related Image"
                                 (partial add-related-image-dialog-callback
                                          (dom/getElement
                                           "related-images-container")))))

       (when-let [add-icon-image-link-element (dom/getElement
                                               "add-icon-image-link")]
         (update-icon-image! (:slug (:icon tpl-map))
                             (:title (:icon tpl-map)))
         (events/listen add-icon-image-link-element
                        event-type/CLICK
                        (partial add-related-image-link-callback
                                 (:language feed)
                                 "Add Icon Image"
                                 (partial add-icon-image-dialog-callback
                                          (dom/getElement "icon-preview")))))

       (when (= mode :event)
         (classes/remove (dom/getElement "start-time-row") "hide")
         (classes/remove (dom/getElement "end-time-row") "hide")
           
         (when-let [start-time-el (dom/getElement "start-time")]
           (ui/set-form-value start-time-el (:start-time tpl-map))
           (events/listen start-time-el
                          event-type/CLICK
                          #(ui/display-datepicker
                            (fn [date hour minute]
                              (let [datetime-string
                                    (if date
                                      (str (.toIsoString date
                                                         true)
                                           " "
                                           hour
                                           ":"
                                           minute)
                                      "")]
                                (ui/set-form-value start-time-el
                                                   datetime-string)))
                            true)))
              
         (when-let [end-time-el (dom/getElement "end-time")]
           (ui/set-form-value end-time-el (:end-time tpl-map))
           (events/listen end-time-el
                          event-type/CLICK
                          #(ui/display-datepicker
                            (fn [date hour minute]
                              (let [datetime-string
                                    (if date
                                      (str (.toIsoString date
                                                         true)
                                           " "
                                           hour
                                           ":"
                                           minute)
                                      "")]
                                (ui/set-form-value end-time-el
                                                   datetime-string)))
                            true))))

       (if (= mode :menu)
         (when content
           (update-menu-builder-from-data content feeds documents))
         (let [editor (create-editor-field "content")
               toolbar (create-editor-toolbar "toolbar"
                                              :bold
                                              :italic
                                              :underline
                                              :strike-through
                                              :format-block
                                              :link
                                              :image
                                              :undo
                                              :redo
                                              :unordered-list
                                              :ordered-list
                                              :indent
                                              :outdent
                                              :justify-left
                                              :justify-center
                                              :justify-right
                                              :subscript
                                              :superscript
                                              :remove-format)]
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
                                  (set! (.-innerHTML editor-images-el) "")
                                  (display-image-feeds (:language
                                                        feed)))))))))

       (when (= mode :with-description)
         (classes/remove (dom/getElement "description-container") "hide")
         (let [description-editor
               (create-editor-field "description-content")
               description-toolbar
               (create-editor-toolbar "description-toolbar"
                                      :bold
                                      :italic
                                      :underline
                                      :strike-through
                                      :link
                                      :undo
                                      :redo
                                      :remove-formatting)]
           (reset! description-editor-field description-editor)

           (when (:description tpl-map)
             (. description-editor (setHtml false
                                            (:description tpl-map)
                                            true
                                            false)))

           (do
             (register-editor-plugins description-editor)
             (goog.ui.editor.ToolbarController. description-editor
                                                description-toolbar)
             (. description-editor (makeEditable))))))))

(defn start-default-mode! [feed slug status feeds documents]
  (if (= :new status)
    (do
      (util/set-page-title! "New document")
      (render-editor feed
                     feeds
                     documents
                     {:status "new"
                      :feed (:name feed)
                      :language (:language feed)
                      :title ""
                      :subtitle ""
                      :slug ""
                      :draft false
                      :icon {:title nil :slug nil}
                      :related-pages []
                      :related-images []}))
    (document/get-doc slug
                      (fn [e]
                        (let [xhr (.-target e)]
                          (if (= (. xhr (getStatus)) 200)
                            (let [json (js->clj (. xhr
                                                   (getResponseJson)))]
                              (util/set-page-title!
                               (str "Edit \"" ("title" json) "\""))
                              (render-editor feed
                                             feeds
                                             documents
                                             {:status "edit"
                                              :feed ("feed" json)
                                              :language (:language feed)
                                              :title ("title" json)
                                              :subtitle (str
                                                         ("subtitle" json))
                                              :description
                                              ("description" json)
                                              :slug ("slug" json)
                                              :draft ("draft" json)
                                              :start-time ("start-time" json)
                                              :end-time ("end-time" json)
                                              :icon (let [ico ("icon" json)]
                                                      {:slug ("slug" ico)
                                                       :title ("title" ico)})
                                              :related-pages
                                              ("related-pages" json)
                                              :related-images
                                              ("related-images" json)}
                                             ("content" json)))
                            (render-document-not-found-template)))))))

(defn start-image-mode! [feed slug status feeds documents]
  (if (= :new status)
    (do
      (util/set-page-title! "New document")
      (render-editor feed
                     feeds
                     documents
                     {:status "new"
                      :feed (:name feed)
                      :language (:language feed)
                      :title ""
                      :slug ""
                      :draft false
                      :related-pages []}))
    (document/get-doc slug
                      (fn [e]
                        (let [xhr (.-target e)]
                          (if (= (. xhr (getStatus)) 200)
                            (let [json (js->clj (. xhr (getResponseJson)))]
                              (util/set-page-title!
                               (str "Edit \"" ("title" json) "\""))
                              (render-editor feed
                                             feeds
                                             documents
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
                                                       json))}
                                              :related-pages
                                              ("related-pages" json)
                                              :related-images
                                              ("related-images" json)}
                                             ("content" json)))
                            (render-document-not-found-template)))))))

(defn start-mode-callback! [slug status event]
  (let [xhr (.-target event)]
    (when (= (. xhr (getStatus)) 200)
      (let [json (js->clj (. xhr (getResponseJson)))
            feed {:language ("language" json)
                  :name ("name" json)
                  :default-slug-format ("default-slug-format" json)
                  :default-document-type ("default-document-type" json)}]

        (call-with-feeds-and-documents
          (:language feed)
          (fn [feeds documents]
            (cond
             (= (:default-document-type feed) "image")
             (start-image-mode! feed slug status feeds documents)
             (= (:default-document-type feed) "menu")
             (start-default-mode! feed slug status feeds documents)
             (= (:default-document-type feed) "event")
             (start-default-mode! feed slug status feeds documents)
             :default
             (start-default-mode! feed slug status feeds documents))))))))

(defn start [language feed-name slug status]
  (swap! *file* dissoc :obj :data)
  (document/get-feed language
                     feed-name
                     (partial start-mode-callback! slug status)))