;; cljs/src/views/editor.cljs: UI implementation for document editor.
;; Copyright 2011-2013, Vixu.com, F.M. (Filip) de Waard <fmw@vixu.com>.
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
  (:use-macros [vix.crossover.macros :only [defhandler get-cdn-hostname]])
  (:require [vix.document :as document]
            [vix.ui :as ui]
            [vix.util :as util]
            [clojure.string :as string]
            [domina :as domina]
            [domina.events :as events]
            [goog.dom :as dom]
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
            [goog.crypt.base64 :as base64])
  (:use [domina.css :only [sel]]
        [domina.xpath :only [xpath]]))

(def error-messages
  {:document-title-required-error
   "The document title value is required."
   :slug-has-invalid-chars-error
   "Slugs can only contain '/', '-', '.' and alphanumeric characters."
   :slug-has-consecutive-dashes-or-slashes-error
   "Slugs shouldn't contain any consecutive '-' or '/' characters."
   :slug-required-error
   "A valid slug is required for every document."
   :slug-initial-slash-required-error
   "The slug needs to start with a '/'."
   :slug-not-unique-error
   "This slug is not unique (document already exists)."
   :could-not-save-document-error
   "Something went wrong while saving the document."
   :document-update-conflict-error
   (str "The document has been changed after you loaded it. "
        "Please copy your changes, refresh the page and edit the "
        "most recent version of the document.")
   :document-already-exists-error
   (str "There already is an existing document with the provided slug. "
        "Please use a different slug.")
   :invalid-filetype-error
   "The filetype for the file you are trying to upload isn't supported."
   :file-required-error
   "This editing mode requires a file to be added."
   :link-label-required-error
   "The link label is required."
   :link-label-has-invalid-chars-error
   (str "Labels can only contain '/', '-', '.', '?', '!' "
        "and alphanumeric characters.")})

(defn slug-has-invalid-chars?
  "Checks whether the slug contains any invalid characters
   (i.e. not a forward slash, dash, period or alphanumeric character)."
  [slug]
  (nil? (re-matches #"[/\-a-zA-Z0-9\.]+" slug)))

(defn validate-slug
  "Validates the provided slug string and returns either a map with a
   :message corresponding to a human-readable error message and
   an :error key corresponding to the the internal error keyword. If
   there is no error the function returns they keyword :pass.

   Slugs must:
   - not be blank (:slug-required-error),
   - not have invalid characters (:slug-has-invalid-chars-error;
     see the slug-has-invalid-chars? fn),
   - not have any consecutive dashes or slashes
     (:slug-has-consecutive-dashes-or-slashes-error), and,
   - start with a slash (:slug-initial-slash-required-error)."
  [slug]
  (cond
   (string/blank? slug)
   {:error :slug-required-error
    :message (:slug-required-error error-messages)}
   (slug-has-invalid-chars? slug)
   {:error :slug-has-invalid-chars-error
    :message (:slug-has-invalid-chars-error error-messages)}
   (util/has-consecutive-dashes-or-slashes? slug)
   {:error :slug-has-consecutive-dashes-or-slashes-error
    :message (:slug-has-consecutive-dashes-or-slashes-error error-messages)}
   (not (= (first slug) "/"))
   {:error :slug-initial-slash-required-error
    :message (:slug-initial-slash-required-error error-messages)}
   :default
   :pass))

(defn link-label-has-invalid-chars?
  "Returns true if the provided label contains any invalid characters
   and false if it doesn't (\\u0080 to \\uffff, alphanumeric
   characters, periods, question marks, exclamation marks, spaces and
   dashes are valid)."
  [label]
  (nil? (re-matches #"[\u0080-\uffffa-zA-Z0-9.?! -]+" label)))

(defn html-with-clean-image-uris
  "Accepts a string with an HTML value and converts any relative paths
   to images to absolute paths. Returns the improved HTML string."
  [html]
  (let [get-num-sub-paths
        (fn [s]
          (when (string? s)
            (count (re-seq #"../" s))))
        unsorted-pairs
        (map (fn [re-pair]
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

(defn create-editor-field!
  "Creates a goog.editor.Field instance with provided element-id."
  [element-id]
  (goog.editor.Field. element-id))

(defn register-editor-plugins!
  "Accepts a goog.editor.Field instance (created by the
   create-editor-field! fn) and registers the following
   goog.editor.plugins: BasicTextFormatter, RemoveFormatting,
   UndoRedo, ListTabHandler, SpacesTabHandler, EnterHandler,
   HeaderFormatter, LinkDialogPlugin and LinkBubble."
  [editor]
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

(defn create-editor-toolbar!
  "Creates a toolbar using the node with given element-id as a
   containe and optionally accepts buttons that are to be included as
   optional further elements. If no optional arguments are provided
   the default buttons are used.

   The available buttons are:

   :bold
   :italic
   :underline
   :font-color
   :background-color
   :font-face
   :font-size,
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
   :justify-center,
   :justify-right
   :subscript
   :superscript
   :strike-through,
   :remove-format
   :edit-html

   The default buttons are:

   :bold
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
   :remove-format"
  [element-id & included-buttons]
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
                 (map #(get buttons-map %)
                      (or included-buttons
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
    (DefaultToolbar/makeToolbar buttons (util/get-element element-id))))

(def editor-fields (atom {}))

(defn make-editable!
  "Turns the node with provided element-id into an editable field,
   using the node with the provided toolbar-id as a toolbar and
   swapping the value into the editor-fields map using the provided
   field-key. Sets the content to the provided html-content string."
  [field-key element-id toolbar-id html-content]
  (let [editor (create-editor-field! element-id)]
    (swap! editor-fields assoc field-key editor)
    (when html-content
      (. editor (setHtml false html-content true false)))

    (register-editor-plugins! editor)
    
    (goog.ui.editor.ToolbarController. editor
                                       (create-editor-toolbar!
                                        toolbar-id
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
                                        :remove-format))
    (. editor (makeEditable))))

(def *file* (atom {}))

(defn create-slug
  "Returns a slug using the provided title and using the
   :default-slug-format and :default-document-type keys from the feed
   map provided as the second argument through the
   vix.util/create-slug fn."
  [title {:keys [default-slug-format default-document-type] :as feed}]
  (util/create-slug default-slug-format
                    title
                    feed
                    (util/date-now!)
                    (if (and (= default-document-type "image")
                             (:extension (:data @*file*)))
                      (:extension (:data @*file*))
                      "html")))

(defn strip-filename-extension
  "Returns a the provided filename string, removing the extension if
   it has any."
  [filename]
  (let [pieces (re-find #"^(.*?)\.[a-zA-Z0-9]{1,10}$" filename)]
    (if (= (count pieces) 2)
      (nth pieces 1)
      filename)))

(defn image-filename-to-title
  "Translates the provided image filename to a document title
   suggestion by joining the alphanumeric segments separated by
   spaces."
  [filename]
  (string/join " "
               (filter #(not (string/blank? %))
                       (string/split (strip-filename-extension filename)
                                     #"[^a-zA-Z0-9]"))))

(defn select-feed-by-language-and-name
  "Returns the feed map with the value of the :language key matching
   the given language string and the value of the :name key matching
   the given feed-name string from the provided feeds sequence."
  [language feed-name feeds]
  (first
   (filter
    (fn [feed]
      (and (= feed-name (:name feed)) (= language (:language feed))))
    feeds)))

(defn get-editor-content
  "Returns the clean HTML string content from the editor with the
   provided key (e.g. :content or :description) or nil if that editor
   doesn't exist in the @editor-fields map."
  [field-key]
  (when-let [editor-field (field-key @editor-fields)]
    (string/replace
     (html-with-clean-image-uris (.getCleanContents editor-field
                                                    editor-field))
     #"id=\"image-dialog-preview\""
     "")))

(defn parse-related-document-id
  "Parses a related document id attribute value (e.g.
   related-image@@@/en/images/foo@@@Foo) which is either provided as a
   string or as a Domina element with the value in the id attribute.
   Returns a map with the document slug mapped to the :slug key and
   the title mapped to the :title key."
  [el-or-string]
  (let [[_ slug title]
        (string/split (if (string? el-or-string)
                        el-or-string
                        (domina/attr el-or-string :id))
                      #"@@@")]
    {:slug slug
     :title title}))

(defn get-document-relations!
  "Retrieves a vector of related pages from the DOM, using the
   provided domina-css-selector string to select the desired
   elements."
  [domina-css-selector]
  (vec
   (map parse-related-document-id (domina/nodes (sel domina-css-selector)))))

(defn get-icon-image!
  "Retrieves a map with the icon image details from the DOM, with
   a :title key for the title and a :slug key for the slug."
  []
  (when-let [icon-container-el
             (domina/single-node (sel ".image-icon-container"))]
    (parse-related-document-id icon-container-el)))

(defn get-document-data-from-dom!
  "Returns a map with the document data extracted from the DOM using
   the provided feed-map for the language and feed-name values, as
   well as to determine the document type."
  [feed-map]
  (let [form-fields [:title
                     :subtitle
                     :slug
                     :start-time
                     :end-time]]
    (merge
     (zipmap form-fields
             (map (fn [field-name]
                    (try
                      (ui/get-form-value (name field-name))
                      (catch js/Error e
                        nil)))
                  form-fields))
     {:language (:language feed-map)
      :feed (:name feed-map)
      :draft (ui/is-checked? "draft")
      :content (if (= (:default-document-type feed-map) "menu")
                 (get-menu-string-from-dom!)
                 (get-editor-content :content))
      :description (get-editor-content :description)
      :icon (get-icon-image!)
      :related-pages (get-document-relations! ".related-page")
      :related-images (get-document-relations! ".related-image")})))

(defn save-document!
  "Saves the document and either returns the new version or an error message.
   Takes a singular feed map, a boolean value for new? that determines
   if its a new or edited document, the current-document-state (a map
   if editing an existing document), the future-state map with the
   output from the get-document-data-from! function and
   slug-validation with the output from a validate-slug call."
  [feed-map new? current-document-state future-state slug-validation]
  (let [save-button-el (sel "#save-document")]
    (domina/set-attr! save-button-el :disabled "disabled")
    (document/append-to-document
     (merge future-state
            (if new?
              {:action :create}
              {:action :update
               :previous-id (:_id current-document-state)}))
     (fn [status document-states]
       (let [[{:keys [slug title]} & _] document-states]
         (cond
          (= status 201)
          (util/navigate-replace-state
           (str (:language feed-map) "/" (:name feed-map) "/edit" slug)
           (str "Edit \"" title "\""))
          (= status 200)
          (display-editor! (:language feed-map) (:name feed-map) slug)
          :else
          (ui/display-error (sel "#status-message")
                            (cond
                             (= (str "There is an existing document "
                                     "with the provided slug.")
                                document-states)
                             (:document-already-exists-error error-messages)
                             (= (str "This document map doesn't contain "
                                     "the most recent :previous-id.")
                                document-states)
                             (:document-update-conflict-error error-messages)
                             :default
                             (:could-not-save-feed-error error-messages))))
         (domina/remove-attr! save-button-el :disabled))))))

(defn save-image-document!
  "Wrapper for save-document! that deals with image documents. See the
   save-document! function for the arguments. If the user is trying to
   add a new image without providing a file an error message is
   displayed."
  [feed-map new? current-document-state future-state slug-validation]
  (let [file (:obj @*file*)
        reader (new js/FileReader)]
    (cond
     ;; if the image has been changed, load it and call save-document!:
     (not (nil? file))
     (do
       (set! (.-onload reader)
             (fn [evt]
               (save-document! feed-map
                               new?
                               current-document-state
                               (merge future-state
                                      {:attachment
                                       {:type (.-type file)
                                        :data (base64/encodeString
                                               (.-result (.-target evt)))}})
                               slug-validation)))
       (. reader (readAsBinaryString file)))
     ;; allow editing existing documents without changing the image:
     (not new?)
     (save-document! feed-map
                     new?
                     current-document-state
                     (merge future-state
                            (select-keys current-document-state [:attachments]))
                     slug-validation)
     ;; display an error message if the user is trying to save a new
     ;; document without providing an image:
     :default
     (ui/display-error (sel "#status-message")
                       (:file-required-error error-messages)))))

(defhandler save-document-button-click-callback!
  "Performs validation when the default-slug-format value changes and
   displays and removes errors when necessary. If the validation is
   successful the save-document! fn is called. Takes a singular feed
   map, a boolean value for new? that determines if this is a fresh or
   existing document, a sequence of document states if editing an
   existing document and a Domina event object."
  [feed-map new? [current-document-state & _] evt]
  (let [status-message-el (sel "#status-message")
        future-state (get-document-data-from-dom! feed-map)
        slug-validation (validate-slug (:slug future-state))]
    (cond
     (string/blank? (:title future-state))
     (ui/display-error status-message-el
                       (:document-title-required-error error-messages))
     (not (= :pass slug-validation))
     (ui/display-error status-message-el (:message slug-validation))
     (= (:default-document-type feed-map) "image")
     (save-image-document! feed-map
                           new?
                           current-document-state
                           future-state
                           slug-validation)
     :default
     (when-not (= (select-keys current-document-state (keys future-state))
                  future-state) ;; don't save if there are no changes
       (save-document! feed-map
                       new?
                       current-document-state
                       future-state
                       slug-validation)))))

(defn detect-duplicate-custom-slug-callback!
  "Detects if the custom slug is a duplicate and adds/removes UI error
   messages accordingly. Used as a callback for vix.document/get-doc."
  [status [doc-current-state & _]]
  (let [slug-el (sel "#slug")
        slug-label-el (sel "#slug-label")
        status-el (sel "#status-message")]
    (if (and (= status 200) (not (= (:action doc-current-state) :delete)))
      (ui/display-error status-el
                        (:slug-not-unique-error error-messages)
                        slug-el
                        slug-label-el)
      (ui/remove-error status-el slug-el slug-label-el))))

(defn increment-slug
  "Increments the provided slug string by either incrementing the last
   character if it is a number or adding '-2' to the end of the
   string."
  [slug]
  (if-let [slug-matches (re-matches #"(.*?)-([0-9]+)$" slug)]
    (str (nth slug-matches 1) "-" (inc (js/parseInt (last slug-matches))))
    (str slug "-2")))

(defhandler slug-validation-callback!
  "Validates custom slugs. Displays an error if required, otherwise
   the doc with the desired slug is requested and validated by the
   detect-duplicate-custom-slug-callback!."
  [evt]
  (when (ui/is-checked? "custom-slug")
    (let [slug-el (events/target evt)
          slug (ui/get-form-value slug-el)
          slug-label-el (sel "#slug-label")
          status-el (sel "#status-message")
          slug-validation (validate-slug slug)]
      (if (= :pass slug-validation)
        (do
          (ui/remove-error status-el slug-el slug-label-el)
          (document/get-doc slug detect-duplicate-custom-slug-callback!))
        (ui/display-error status-el
                          (:message slug-validation)
                          slug-el
                          slug-label-el)))))

(def slugs (atom #{}))

(defn handle-duplicate-slug-callback!
  "Automatically appends '-2' to the end of a duplicate slug, or
   increments n if the slug already ends with '-n' where n is an
   integer. Keeps calling itself until no more duplicates are found
   and finally sets the unique value if that value doesn't match the
   original value."
  [status [doc-current-state & _]]
  (if (and (= status 200) (not (= (:action doc-current-state) :delete)))
    ;; add incremented slug to @slugs and rerun the get-doc call with
    ;; the new slug and this function as the callback:
    (let [new-slug (increment-slug
                    (document/add-initial-slash
                     (:slug doc-current-state)))]
      (swap! slugs conj new-slug)
      (document/get-doc new-slug handle-duplicate-slug-callback!))
    ;; set the final slug value if needed:
    (let [final-slug (last (sort-by count @slugs))
          slug-el (sel "#slug")]
      (when-not (= (ui/get-form-value slug-el) final-slug)
        (ui/set-form-value slug-el final-slug)))))

(defn synchronize-slug
  "Automatically generates a slug value based on the value of the
  provided title element."
  [title-el feed]
  (let [new-slug (create-slug (ui/get-form-value title-el) feed)]
    (when (nil? (ui/get-form-value "custom-slug"))
      (reset! slugs #{new-slug})
      (document/get-doc new-slug handle-duplicate-slug-callback!))))

(defhandler custom-slug-toggle-callback!
  "Toggles the slug input between editable (i.e. custom) and disabled."
  [feed evt]
  (if (nil? (ui/get-form-value (events/target evt)))
    (do
      (ui/disable-element "slug")
      (ui/disable-element "slug-label")
      (synchronize-slug (sel "#title") feed))
    (do
      (ui/enable-element "slug")
      (ui/enable-element "slug-label"))))

(defhandler title-edited-callback!
  "Automatically generates a slug for current title if custom slug
   isn't checked."
  [feed evt]
  (synchronize-slug (events/target evt) feed))

(defhandler handle-datefield-click-callback!
  "Displays a datepicker on click."
  [evt]
  (ui/display-datepicker
   (fn [{:keys [date-string date-object hour minute]}]
     (ui/set-form-value (events/target evt)
                        (if date-object
                          (str date-string " " hour ":" minute)
                          "")))
   true))

(defn display-image-preview
  "Displays a preview of the image contained in given file object with
   the provided title."
  [file title]
  (let [reader (new js/FileReader)]
    (set! (.-onload reader)
          (fn [evt]
            (domina/set-attrs! (sel "#image-preview")
                               {:title title
                                :src (.-result (.-target evt))})))
    (. reader (readAsDataURL file))))

(def mimetype-to-extension-map
  {:image/png "png"
   :image/gif "gif"
   :image/jpeg "jpg"})

(def *file* (atom {}))

(defn handle-image-drop-callback!
  "Is called when a file is dropped on the image drop target. If the
   file is recognized as an image it is displayed in the interface and
   swapped into the *file* atom (under the :obj key). Otherwise an
   invalid filetype message is shown."
  [current-feed-state new? evt]
  ;; domina/prevent-default & domina/stop-propagation don't work for
  ;; these events, so not using defhandler, but a regular function.
  (do
    (. evt (preventDefault))
    (. evt (stopPropagation)))

  (if-let [file (aget (.-files (.-dataTransfer evt)) 0)]
    (let [status-el (sel "#status-message")
          image-information-el (sel "#image-information-container")
          title (image-filename-to-title (.-name file))]
      (if-let [extension (mimetype-to-extension-map (keyword (.-type file)))]
        (do
          (swap! *file* assoc :obj file :data {:extension extension})
          (ui/remove-error status-el)
          (ui/set-form-value (dom/getElement "title") title)
          
          (display-image-preview file title)
          
          (domina/remove-class! image-information-el "hide")
          
          (domina/set-text! (sel "#image-filename") (.-name file))
          (domina/set-text! (sel "#image-filetype") (.-type file))

          (domina/set-text! (sel "#image-size")
                            (format "%.2f"(/ (.-size file) 1000)))
        
          (when new?
            (synchronize-slug (sel "#title") current-feed-state)))
        (do
          (swap! *file* dissoc :obj :data)
          (domina/add-class! image-information-el "hide")
          (ui/display-error status-el
                            (:invalid-filetype-error error-messages)))))))

(defn remove-self-from-documents
  "Removes the map with the given self-slug string as a :slug value
   from the provided sequence of document maps."
  [self-slug documents]
  (remove #(= (:slug %) self-slug) documents))

(defn remove-existing-from-documents
  "Removes any documents with a :slug value that is part of the
   existing-documents vector."
  [existing-documents documents]
  (remove (fn [{:keys [slug]}]
            (not (nil? (some #{slug} (map :slug existing-documents)))))
          documents))

(defn format-option
  "Replaces keyword values in the given option-format sequence with
   the string value for that keyword in the provided option-map and
   leaves string values in the option-format sequence as-is. Returns a
   string version of the new sequence."
  [option-map option-format]
  (apply str
         (map (fn [v]
                (if (string? v)
                  v
                  (option-map v)))
              option-format)))

(defn update-options-in-dialog!
  "Adds select options to the DOM. Takes the select-el, which is the
   Domina element node the options are being added to, the
   snippet-key, which is the key for the HTML snippets in the
   ui/snippets map, the option-xpath-selector, which is the xpath
   selector required to select the newly added option, option-maps,
   which is a sequence of maps containing values for the option,
   title-format and value-format, which are sequences describing the
   format for the particular values (see the format-option function
   for the structure of the format sequences)."
  [select-el
   snippet-key
   option-xpath-selector
   option-maps
   title-format
   value-format]
  (domina/destroy-children! select-el)
  (doseq [option option-maps]
    (domina/append! select-el (ui/snippets snippet-key))
    (doto (xpath option-xpath-selector)
      (domina/set-text! (format-option option title-format))
      (domina/set-attr! :value (format-option option value-format)))))

(defn update-feed-options-in-dialog!
  "Appends select options for given feeds to the provided feeds-el."
  [feeds-el feeds]
  (update-options-in-dialog! feeds-el
                             :editor/.feed-select-option
                             "(//option[@class='feed-select-option'])[last()]"
                             feeds
                             [:title]
                             ["['" :language "','" :name "']"]))

(defn update-document-options-in-dialog!
  "Appends provided select options for given documents to
   documents-el, while removing the document matching self-slug. Any
   documents in the removable-documents vector are removed from the
   documents list."
  [self-slug documents-el documents removable-documents]
  (update-options-in-dialog!
   documents-el
   :editor/.document-select-option
   "(//option[@class='document-select-option'])[last()]"
   (remove-existing-from-documents
    removable-documents
    (remove-self-from-documents self-slug documents))
   [:title]
   ["option@@@" :slug "@@@" :title]))

(defn update-menu-links-options-in-dialog!
  "Adds the provided sequence of document maps to the dialog as select
   options."
  [documents]
  (update-options-in-dialog!
   (sel "select#internal-link")
   :editor/.document-select-option
   "(//option[@class='document-select-option'])[last()]"
   documents
   [:title]
   [:slug]))

(defhandler remove-parent-callback!
  "Event callback that removes the parent of the event target DOM node."
  [evt]
  (domina/destroy! (xpath (events/target evt) "..")))

(defn add-related-page!
  "Adds a li element for the given page to the DOM. This function is
   used as a ui/display-dialog callback!, which provides the dialog-status
   (either :ok or :cancel) and a map with the form fields and their
   values as callback arguments."
  [dialog-status {:keys [internal-link-feed internal-link]}]
  (when (and (= dialog-status :ok) (not (nil? internal-link)))
    (domina/append! (sel "#related-pages-container")
                    (:editor/.related-page ui/snippets))

    (let [{:keys [slug title]} (parse-related-document-id internal-link)]
      (doto (xpath "(//li[@class='related-page'])[last()]")
        (domina/set-attr! :id (str "related-page@@@" slug "@@@" title)))

      (doto (xpath "(//span[@class='related-page-title'])[last()]")
        (domina/set-text! title))

      (events/listen! (xpath "(//a[@class='related-page-delete-link'])[last()]")
                      :click
                      remove-parent-callback!))))

(defhandler image-preview-link-click-callback!
  "Handler for clicks on an image preview link that displays the image
   in a dialog."
  [evt]
  (let [{:keys [title slug]}
        (parse-related-document-id (xpath (events/target evt) ".."))]
    (ui/display-dialog!
        "Image Preview"
        (:editor/image-preview-in-dialog-container ui/snippets)
        nil
        {:auto-close? true
         :modal? true}
        (fn [])
        :cancel)
    (domina/set-attrs! (sel "#image-dialog-preview")
                       {:src (str (get-cdn-hostname) slug)
                        :alt title
                        :title title})))

(defn add-related-image!
  "Adds a li element for the given image to the DOM. This function is
   used as a ui/display-dialog callback, which provides the dialog-status
   (either :ok or :cancel) and a map with the form fields and their
   values as callback arguments."
  [dialog-status {:keys [internal-link-feed internal-link]}]
  (when (and (= dialog-status :ok) (not (nil? internal-link)))
    (domina/append! (sel "#related-images-container")
                    (:editor/.related-image ui/snippets))

    (let [{:keys [slug title]} (parse-related-document-id internal-link)]
      (doto (xpath "(//li[@class='related-image'])[last()]")
        (domina/set-attr! :id (str "related-page@@@" slug "@@@" title)))

      (doto (xpath "(//span[@class='related-image-title'])[last()]")
        (domina/set-text! title))

      (events/listen!
       (xpath "(//a[@class='related-image-delete-link'])[last()]")
       :click
       remove-parent-callback!)

      (events/listen!
       (xpath "(//a[@class='related-image-preview-link'])[last()]")
       :click
       image-preview-link-click-callback!))))

(defn create-icon-image-events!
  "Creates delete and preview events for image icon."
  []
  (events/listen! (sel "#icon-image-delete-link")
                  :click
                  remove-parent-callback!)

  (events/listen! (sel "#icon-image-preview-link")
                  :click
                  image-preview-link-click-callback!))

(defn add-icon-image!
  "Adds a li element for the given image to the DOM. This function is
   used as a ui/display-dialog callback, which provides the dialog-status
   (either :ok or :cancel) and a map with the form fields and their
   values as callback arguments."
  [dialog-status {:keys [internal-link-feed internal-link]}]
  (when (and (= dialog-status :ok) (not (nil? internal-link)))
    (let  [{:keys [slug title]} (parse-related-document-id internal-link)]
      (domina/destroy! (sel ".image-icon-container"))
      
      (domina/append! (sel "#icon-container")
                      (:editor/.image-icon-container ui/snippets))
      
      (domina/set-attr! (sel ".image-icon-container")
                        :id
                        (str "icon@@@" slug "@@@" title))
      (domina/set-text! (sel "#icon-image-title") title)

      (create-icon-image-events!))))

(defn update-dialog-image-preview!
  "Sets the image preview in the image selection dialog to the slug
   and title of the provided document map."
  [{:keys [slug title]}]
  (when (string? slug)
    (domina/set-attrs! (sel "#image-dialog-preview")
                       {:src (str (get-cdn-hostname) slug)
                        :alt title
                        :title title})))

(defn create-dialog-feed-change-event!
  "Creates an event that tracks changes on feed-el and executes the
   update-document-options-fn if a change happens, which will update
   the document select field. The update-document-options-fn needs to
   accept the retrieved document sequence as the first and only value.
   If the dialog contains an image-dialog-preview image the
   update-dialog-image-preview! function is executed as well, using
   the optional relations argument to purge already related images
   from the options."
  [feed-el update-document-options-fn & [relations]]
  (events/listen!
   feed-el
   :change
   (fn [evt]
     (let [[language feed-name]
           (util/pair-from-string (ui/get-form-value (events/target evt)))]
       (document/get-documents-for-feed
        language
        feed-name
        (fn [status {:keys [documents]}]
          (update-document-options-fn documents)
          (when (domina/single-node (sel "img#image-dialog-preview"))
              (update-dialog-image-preview!
               (first
                (remove-existing-from-documents (or relations [])
                                                documents))))))))))

(defn fill-document-select-dialog!
  "Fills the internal-link and internal-link-feeds select fields in a
   previously created dialog and adds an event listener that loads the
   new documents if another feed is selected. Takes node-selector (nil
   or a Domina CSS selector; to pass to the get-document-relations!
   function), self-slug with the slug for the active document, feeds
   with a sequence of feed maps and documents with a sequence of
   document maps for the first feed."
  [node-selector self-slug feeds documents]
  (let [internal-link-el
        (sel "#internal-link")
        internal-link-feed-el
        (sel "#internal-link-feed")
        relations
        (if node-selector
          (get-document-relations! node-selector)
          [])]
    (update-feed-options-in-dialog! internal-link-feed-el feeds)
    (update-document-options-in-dialog! self-slug
                                        internal-link-el
                                        documents
                                        (if node-selector
                                          (get-document-relations!
                                           node-selector)
                                          []))

    (create-dialog-feed-change-event! internal-link-feed-el
                                      #(update-document-options-in-dialog!
                                        self-slug
                                        internal-link-el
                                        %
                                        relations)
                                      relations)))

(defhandler add-related-page-link-callback!
  "Handler for clicks on the add-related-page link that displays a
   dialog that can be used to select a related page for the document.
   Takes self-slug, the slug of the current document, to filter it out
   of the results, as well as a sequence of singular feed maps called
   feeds and a Domina event called evt."
  [self-slug all-feeds evt]
  (let [[{:keys [language name]} :as feeds]
        (filter #(not (= "image" (:default-document-type %))) all-feeds)]
    (document/get-documents-for-feed
     language
     name
     (fn [status {:keys [documents]}]
       (ui/display-dialog!
        "Add Related Page"
        (:editor/add-related-page-dialog-form ui/snippets)
        (sel "#add-related-page-dialog-form")
        {:auto-close? true
         :modal? true}
        add-related-page!)

       (fill-document-select-dialog! ".related-page"
                                     self-slug
                                     feeds
                                     documents)))))

(defn create-image-preview-event!
  "Creates the event that changes the preview image when a new image
   is selected in an image dialog."
  []
  (events/listen! (sel "select#internal-link")
                  :change
                  (fn [evt]
                    (update-dialog-image-preview!
                     (parse-related-document-id
                      (ui/get-form-value (events/target evt)))))))

(defhandler add-related-image-link-callback!
  "Handler for clicks on the add-related-image link that displays a
   dialog that can be used to select a related image for the document.
   Takes self-slug, the slug of the current document, to filter it out
   of the results, as well as a sequence of singular feed maps called
   feeds and a Domina event called evt."
  [self-slug all-feeds evt]
  (let [[{:keys [language name]} :as feeds]
        (filter #(= "image" (:default-document-type %)) all-feeds)]
    (document/get-documents-for-feed
     language
     name
     (fn [status {:keys [documents]}]
       (ui/display-dialog!
        "Add Related Image"
        (:editor/add-image-dialog-form ui/snippets)
        (sel "#add-image-dialog-form")
        {:auto-close? true
         :modal? true}
        add-related-image!)

       (update-dialog-image-preview!
        (first
         (remove-existing-from-documents
          (get-document-relations! ".related-image")
          documents)))

       (create-image-preview-event!)

       (fill-document-select-dialog! ".related-image"
                                     self-slug
                                     feeds
                                     documents)))))

(defhandler add-icon-image-link-callback!
  "Handler for clicks on the add-icon-image-link that displays a
   dialog that can be used to select an icon image for the document.
   Takes self-slug, the slug of the current document, to filter it out
   of the results, as well as a sequence of singular feed maps called
   feeds and a Domina event called evt."
  [self-slug all-feeds evt]
  (let [[{:keys [language name]} :as feeds]
        (filter #(= "image" (:default-document-type %)) all-feeds)]
    (document/get-documents-for-feed
     language
     name
     (fn [status {:keys [documents]}]
       (ui/display-dialog!
        "Add Icon Image"
        (:editor/add-image-dialog-form ui/snippets)
        (sel "#add-image-dialog-form")
        {:auto-close? true
         :modal? true}
        add-icon-image!)

       (update-dialog-image-preview! (first documents))

       (create-image-preview-event!)

       (fill-document-select-dialog! nil
                                     self-slug
                                     feeds
                                     documents)))))

(defhandler editor-add-image-button-callback!
  "Handler for clicks on the add image button in editor toolbars that
   displays a non-modal image selection dialog. Images can be dragged
   into the editor field from the dialog. The all-feeds argument is a
   sequence of singular feed maps and the evt argument contains the
   Domina event object."
  [all-feeds evt]
  (when-not (domina/single-node (sel "div.modal-dialog"))
    (let [[{:keys [language name]} :as feeds]
          (filter #(= "image" (:default-document-type %)) all-feeds)]
      (document/get-documents-for-feed
       language
       name
       (fn [status {:keys [documents]}]
         (ui/display-dialog!
          "Add Image"
          (:editor/add-image-dialog-form ui/snippets)
          (sel "#add-image-dialog-form")
          {:auto-close? true
           :modal? false}
          add-icon-image!
          :ok)

         (domina/remove-class! (sel "h5#drag-to-editor-instruction")
                               "hide")
         
         (update-dialog-image-preview! (first documents))

         (create-image-preview-event!)

         (fill-document-select-dialog! nil
                                       ""
                                       feeds
                                       documents))))))

(defn create-common-editor-events!
  "Creates events that are shared by all editor modes. Takes a boolean
   value new? which determines if it concerns a fresh document and
   feed-map, a singular map containing feed information for the active
   document."
  [new? feed-map]
  (when new?
    (events/listen! (sel "#title")
                    :keyup
                    (partial title-edited-callback! feed-map)))
    
  (events/listen! (sel "#custom-slug")
                  :change
                  (partial custom-slug-toggle-callback! feed-map))

  (events/listen! (sel "#slug")
                  :keyup
                  slug-validation-callback!))

(defn display-editor-callback!
  "Displays the editor UI and creates the relevant events. The feeds
   argument contains a sequence containing all available feeds. The
   feed-map is a map containing feed information for this specific
   document. The status is either an HTTP status integer if editing an
   existing document or nil if editing a new document. The last
   argument contains a map that describes the document as stored in
   the database."  
  [feeds
   {:keys [default-document-type] :as feed-map}
   status
   [{:keys [title
            content
            slug
            _id
            description
            draft
            related-pages
            language
            feed
            subtitle
            icon
            related-images
            created
            start-time-rfc3339
            start-time
            published
            previous-id
            end-time-rfc3339
            end-time
            datestamp
            attachments]} :as document-states]]
  (let [new? (nil? status)]
    (ui/show!
     {:snippet :editor/back-to-overview-link
      :transformations
      [{:selector "//p[@id='back-to-overview-link']/a[1]"
        :attrs {:href (str "/admin/" language "/" feed "/overview")}}]}
     {:snippet :ui/status-message}
     (when (= default-document-type "image")
       [{:snippet :editor/image-drop-target}
        {:snippet :editor/image-information-container
         :transformations
         [(when slug
            {:selector "//img[@id='image-preview']"
             :attrs {:src (str "data:"
                               (get-in attachments [:original :type])
                               ";base64,"
                               (get-in attachments [:original :data]))
                     :alt title
                     :name title}})]}])
     {:snippet :ui/caption
      :transformations
      [{:selector "//h3[@id='caption']"
        :text (if new? "Create Document" "Edit Document")}]}
     {:snippet :editor/title-row
      :transformations
      [{:selector "//input[@id='title']"
        :value title}
       {:selector "//input[@id='draft']"
        :checked draft}]}
     {:snippet :editor/subtitle-row
      :transformations
      [{:selector "//input[@id='subtitle']"
        :value subtitle}]}
     {:snippet :editor/slug-row
      :transformations
      (concat [{:selector "//input[@id='slug']"
                :value slug}]
              (when new?
                [{:selector "//input[@id='custom-slug']"
                  :remove-attr "disabled"}
                 {:selector "//label[@id='custom-slug-label']"
                  :remove-class "disabled"}]))}
     (when (= default-document-type "event")
       [{:snippet :editor/start-time-row
         :transformations
         [{:selector "//input[@id='start-time']"
           :value start-time}]}
        {:snippet :editor/end-time-row
         :transformations
         [{:selector "//input[@id='end-time']"
           :value end-time}]}])
     [(merge
       {:snippet :editor/icon-container}
       (when-not (nil? icon)
         {:children
          [{:snippet :editor/.image-icon-container
            :transformations
            [{:selector "//div[@class='image-icon-container']"
              :attrs {:id  (str "icon@@@" (:slug icon) "@@@" (:title icon))}}
             {:selector "//span[@id='icon-image-title']"
              :text (:title icon)}]}]}))
      {:snippet :editor/document-relations
       :children
       (doall
        (map (fn [{:keys [title slug]}]
               {:snippet :editor/.related-page
                :parent "//ul[@id='related-pages-container']"
                :transformations
                [{:selector "(//li[@class='related-page'])[last()]"
                  :attrs {:id  (str "related-page@@@" slug "@@@" title)}}
                 {:selector "(//span[@class='related-page-title'])[last()]"
                  :text title}]})
             related-pages))}
      {:snippet :editor/image-relations
       :children
       (doall
        (map (fn [{:keys [title slug]}]
               {:snippet :editor/.related-image
                :parent "//ul[@id='related-images-container']"
                :transformations
                [{:selector "(//li[@class='related-image'])[last()]"
                  :attrs {:id  (str "related-image@@@" slug "@@@" title)}}
                 {:selector "(//span[@class='related-image-title'])[last()]"
                  :text title}]})
             related-images))}
      {:snippet :editor/editor-images}
      (when (= default-document-type "with-description")
        {:snippet :editor/description-container})
      {:snippet :editor/content-container}]
     {:snippet :editor/save-button-container})

    (make-editable! :content "content" "toolbar" content)

    (when (= default-document-type "with-description")
      (make-editable! :description
                      "description-content"
                      "description-toolbar"
                      description))

    (when-let [drop-target-el (dom/getElement "image-drop-target")]
      (. drop-target-el (addEventListener "drop"
                                          (partial
                                           handle-image-drop-callback!
                                           feed-map
                                           new?)
                                          false))
      (. drop-target-el (addEventListener "dragover"
                                          (fn [e]
                                            (. e (preventDefault))
                                            (. e (stopPropagation)))
                                          false)))

    (create-common-editor-events! new? feed-map)
    (create-icon-image-events!)

    (events/listen! (sel "#start-time")
                    :click
                    handle-datefield-click-callback!)

    (events/listen! (sel "#end-time")
                    :click
                    handle-datefield-click-callback!)

    (events/listen! (sel "#add-icon-image-link")
                    :click
                    (partial add-icon-image-link-callback!
                             slug
                             feeds))

    (events/listen! (sel ".related-page-delete-link")
                    :click
                    remove-parent-callback!)

    (events/listen! (sel ".related-image-delete-link")
                    :click
                    remove-parent-callback!)

    (events/listen! (sel ".related-image-preview-link")
                    :click
                    image-preview-link-click-callback!)

    (events/listen! (sel "#add-related-page-link")
                    :click
                    (partial add-related-page-link-callback!
                             slug
                             feeds))

    (events/listen! (sel "#add-related-image-link")
                    :click
                    (partial add-related-image-link-callback!
                             slug
                             feeds))

    (events/listen! (xpath "//div[@class='tr-icon tr-image']/../..")
                    :click
                    (partial editor-add-image-button-callback! feeds))

    (events/listen! (sel "#save-document")
                    :click
                    (partial save-document-button-click-callback!
                             feed-map
                             new?
                             document-states))))

(defhandler toggle-add-menu-item-dialog-link-type-callback!
  "Toggles between showing the internal-link-row and the
   external-link-row, depending on the link-type field value."
  [evt]
  (if (= (ui/get-form-value (events/target evt)) "internal")
    (do
      (domina/remove-class! (sel "tr#internal-link-row") "hide")
      (domina/add-class! (sel "tr#external-link-row") "hide"))
    (do
      (domina/remove-class! (sel "tr#external-link-row") "hide")
      (domina/add-class! (sel "tr#internal-link-row") "hide"))))

(defn append-add-sub-item-link!
  "Adds an add-sub-item link the the provided item-details-el,
   accepting a sequence of singular feed maps called as the second
   argument to pass to the display-add-menu-item-dialog-callback!
   function."
  [item-details-el all-feeds]
  (domina/append! item-details-el
                  (:editor/.add-sub-item ui/snippets))

  (events/listen! (xpath item-details-el
                         "(.//a[@class='add-sub-item'])[last()]")
                  :click
                  (partial display-add-menu-item-dialog-callback!
                           (domina/single-node (xpath item-details-el ".."))
                           all-feeds)))

(defhandler delete-menu-item-callback!
  "Deletes the menu item containing the active delete item link,
   as well as the ul containing it if it is the only item node left.
   If the latter is true, the add-sub-item link is also re-added to
   the parent. Accepts a sequence containing singular feed maps as the
   first argument and the triggered Domina event as the second
   argument."
  [all-feeds evt]
  (let [three-levels-up (xpath (events/target evt) "../../..")
        four-levels-up (xpath three-levels-up "..")]
    (if (and (= (domina/attr three-levels-up "class") "nested-menu-category")
             (= (count (domina/children three-levels-up)) 2))
      (do
        ;; re-add add-sub-item to parent if deleting the whole category
        (append-add-sub-item-link!
         (domina/single-node
          (xpath four-levels-up "(.//span[@class='item-details'])"))
         all-feeds)
        ;; and remove the sub-menu ul
        (domina/destroy! three-levels-up)
        )
      ;; just remove the li containing the current item if it has siblings
      (domina/destroy! (xpath (events/target evt) "../..")))))

(defn get-nested-menu-category-el!
  "Returns the .nested-menu-category element for parent-el, creating
   it if necessary."
  [parent-el]
  (or (domina/single-node
       (xpath parent-el "(.//ul[@class='nested-menu-category'])[last()]"))
      (do
        (domina/append! parent-el
                        (:editor/.nested-menu-category ui/snippets))
        (get-nested-menu-category-el! parent-el))))

(defn add-item-details!
  "Adds the item-details span to parent-el, using the link-label,
   link-type, internal-links and external-link arguments to determine
   the contents. Accepts a sequence of singular feed maps as the
   second argument, to pass to the detele-menu-item-callback!
   function."
  [parent-el all-feeds link-label link-type internal-link external-link]
  (domina/append! parent-el (:editor/.item-details ui/snippets))
  
  (let [item-details-el (xpath parent-el ".//span[@class='item-details']")]
    (domina/set-text! (xpath item-details-el ".//span[@class='link-label']")
                      link-label)
                
    (domina/set-text! (xpath item-details-el ".//span[@class='link-uri']")
                      (if (= link-type "internal")
                        internal-link
                        external-link))
        
    (events/listen! (xpath item-details-el
                           ".//a[@class='menu-item-delete-link']")
                    :click
                    (partial delete-menu-item-callback! all-feeds))))

(defn add-nested-menu-item-to-dom!
  "Adds a nested menu item to the menu builder, removes the
   add-sub-item link from the parent element and if this is the first
   nested item also adds an add-item node to the newly created ul.
   Accepts a keyword determining the source (either :string
   or :dialog; determines whether the item is appended or prepended)
   as the first argument. Accepts all-feeds as the second argument,
   containing a sequence of singular feeds maps, parent-el, pointing
   to the element to add the item to, and link-label, link-type,
   internal-link and external-link values to pass to the
   add-item-details! function."
  [source
   all-feeds
   parent-el
   link-label
   link-type
   internal-link
   external-link]
  (domina/destroy!
   (domina/single-node (xpath parent-el "(.//a[@class='add-sub-item'])")))

  (let [nested-menu-category-el (get-nested-menu-category-el! parent-el)]
    ;; if this is the first item, create an add-item node
    (when (zero? (count (domina/children nested-menu-category-el)))
      (domina/append! nested-menu-category-el
                      (:editor/.add-item-node ui/snippets))
      
      (events/listen! (xpath nested-menu-category-el
                             "(.//li)[last()]/a")
                      :click
                      (partial display-add-menu-item-dialog-callback!
                               parent-el
                               all-feeds)))
    
    (if (= source :dialog)
      (domina/prepend! nested-menu-category-el
                       (:editor/.nested-menu-item ui/snippets))
      (domina/insert-before! (xpath nested-menu-category-el "(.//li)[last()]")
                             (:editor/.nested-menu-item ui/snippets)))
    
    (add-item-details! (if (= source :dialog)
                         ;; use first li as parent with a :dialog source
                         (xpath
                          nested-menu-category-el
                          "(.//li[@class='nested-menu-item draggable'])[1]")
                         ;; use last .nested-menu-item-li as parent if :string
                         (xpath
                          nested-menu-category-el
                          (str "(.//li[@class='nested-menu-item draggable'])"
                               "[last()]")))
                       all-feeds
                       link-label
                       link-type
                       internal-link
                       external-link)
  
    (ui/remove-dialog!)))

(defn add-menu-item-to-dom!
  "Adds a top level menu item to the menu builder.
   Accepts all-feeds as the first argument, containing a sequence of
   singular feeds maps, parent-el, pointing to the element to add the
   item to, and link-label, link-type, internal-link and external-link
   values to pass to the add-item-details! function."
  [all-feeds link-label link-type internal-link external-link]
  (domina/append! (sel "#menu-container")
                  (:editor/.top-level-menu-item ui/snippets))
  
  (let [menu-item-el
           (xpath "(//li[@class='top-level-menu-item draggable'])[last()]")]
    (add-item-details! menu-item-el
                       all-feeds
                       link-label
                       link-type
                       internal-link
                       external-link)

    (append-add-sub-item-link!
     (xpath menu-item-el "(.//span[@class='item-details'])[last()]")
     all-feeds))
  
  (ui/remove-dialog!))

(defn handle-add-menu-item-dialog-completion
  "Validates the add-menu-item dialog. If the dialog doesn't validate
   an error message is displayed, if it does it is added the the menu
   builder and the dialog is closed. The next-child-parent argument is
   either nil (in case of interaction with the top level) or points to
   the parent element that the new child is supposed to be added to.
   The all-feeds argument is a sequence of singular feed maps, the
   dialog status is passed by the ui/display-dialog!
   function (either :ok or :cancel) and the final argument is a map
   with form values passed by the ui/display-dialog! function."
  [next-child-parent-el
   all-feeds
   dialog-status
   {:keys [external-link internal-link link-label link-type]}]
  (when (= :ok dialog-status)
    (cond
     (string/blank? link-label)
     (ui/display-error (sel "p#add-link-status")
                       (:link-label-required-error error-messages))
     (link-label-has-invalid-chars? link-label)
     (ui/display-error (sel "p#add-link-status")
                       (:link-label-has-invalid-chars-error error-messages))
     next-child-parent-el
     (add-nested-menu-item-to-dom! :dialog
                                   all-feeds
                                   next-child-parent-el
                                   link-label
                                   link-type
                                   internal-link
                                   external-link)
     :default
     (add-menu-item-to-dom! all-feeds
                            link-label
                            link-type
                            internal-link
                            external-link))))

(defn parse-dummy-menu-ul!
  "Reads the menu data from the dummy DOM object created by
   parse-menu-content-string! and its children. Accepts the dummy DOM
   element as the first and only argument (also used for the child UL
   if calling itself recursively)."
  [element]
  (map (fn [el]
         (let [children (domina/children el)
               last-child (last children)]
           {:label (domina/text (first children))
            :uri (domina/attr (first children) "href")
            :children (when (= (.-tagName last-child) "UL")
                        (parse-dummy-menu-ul! last-child))}))
       (domina/children element)))

(defn parse-menu-content-string!
  "Takes string s and adds it to a dummy DOM elemenet to be parsed by
   the parse-dummy-menu-ul! function."
  [s]
  (if (string? s)
    (let [dummy-list-el (dom/createElement "ul")]
      (set! (.-innerHTML dummy-list-el) (subs s 4 (- (count s) 5)))
      (parse-dummy-menu-ul! dummy-list-el))))

(defn display-existing-menu-from-string!
  "Takes a menu string containing the HTML version of the menu and
   creates the menu builder UI for it, using a sequence of singular feed
   maps as the first argument to the function call."
  [all-feeds menu-string]
  (domina/destroy-children! (sel "ul#menu-container"))
  (doseq [{:keys [label uri children]} (parse-menu-content-string! menu-string)]
    (add-menu-item-to-dom! all-feeds label "internal" uri nil)
    (doseq [{:keys [label uri]} children]
      (add-nested-menu-item-to-dom!
       :string
       all-feeds
       (domina/single-node ;; force computation of Domina xpath lazy seq
        (xpath "(//li[@class='top-level-menu-item draggable'])[last()]"))
       label
       "internal"
       uri
       nil))))

(defn get-item-details-from-dom!
  "Extracts the link label and URI from the text value of the
   .link-label and .link-uri span elements in the provided parent-el
   DOM element. Returns a map with the link label mapped to :label and
   the URI mapped to :uri."
  [parent-el]
  {:label (domina/text (xpath parent-el ".//span[@class='link-label']"))
   :uri (domina/text (xpath parent-el ".//span[@class='link-uri']"))})

(defn get-menu-data-from-dom!
  "Converts the DOM structure of the menu builder to a sequence of
   maps with :label and :uri keys for the corresponding values, as well
   as a :children key for the children elements of top level nodes."
  []
  (map (fn [top-level-menu-item-el]
         (assoc (get-item-details-from-dom! top-level-menu-item-el)
           :children
           (map get-item-details-from-dom!
                (reverse
                 (domina/nodes
                  (xpath top-level-menu-item-el
                         ".//li[@class='nested-menu-item draggable']"))))))
       (reverse
        (domina/nodes
         (xpath "//li[@class='top-level-menu-item draggable']")))))

(defn get-menu-string-from-dom!
  "Returns the HTML string represenation of the menu builder." 
  []
  ;; FIX rewrite using Hiccup or Enfocus
  (let [dummy-list-el (dom/createElement "ul")]
    (doseq [{:keys [label uri children]} (get-menu-data-from-dom!)]
      (let [dummy-li-el (dom/createElement "li")
            dummy-link-el (dom/createElement "a")]
        (domina/set-attr! dummy-link-el "href" uri)
        (domina/set-text! dummy-link-el label)
        (domina/append! dummy-li-el dummy-link-el)

        (when (pos? (count children))
          (let [dummy-sub-menu-el (dom/createElement "ul")]
            (domina/add-class! dummy-sub-menu-el "sub-menu")

            (doseq [{:keys [label uri]} children]
              (let [dummy-li-el (dom/createElement "li")
                    dummy-link-el (dom/createElement "a")]
                (domina/set-attr! dummy-link-el "href" uri)
                (domina/set-text! dummy-link-el label)
                (domina/append! dummy-li-el dummy-link-el)
                (domina/append! dummy-sub-menu-el dummy-li-el)))
            
            (domina/append! dummy-li-el dummy-sub-menu-el)))
              
        (domina/append! dummy-list-el dummy-li-el)))
    (str "<ul id=\"menu\">"(.-innerHTML dummy-list-el) "</ul>")))

(defhandler display-add-menu-item-dialog-callback!
  "Callback for the add menu item links. If the first element is not
   nil it is expected to be the parent element of the child element that
   is being added. The second argument is a sequence of singular feed
   maps and the third argument is a Domina event."
  [next-child-parent-el all-feeds evt]
  (let [[{:keys [language name]} :as feeds]
        (filter #(not (= "image" (:default-document-type %))) all-feeds)
        feed-el
        (sel "select#internal-link-feed")]
    (document/get-documents-for-feed
     language
     name
     (fn [status {:keys [documents]}]
       (ui/display-dialog!
        "Add Menu Item"
        (:editor/add-menu-item-dialog-form ui/snippets)
        (sel "form#add-menu-item-dialog-form")
        {:auto-close? false
         :modal? true}
        (partial handle-add-menu-item-dialog-completion
                 next-child-parent-el
                 all-feeds))

       (update-feed-options-in-dialog! feed-el feeds)
       (update-menu-links-options-in-dialog! documents)

       (create-dialog-feed-change-event! feed-el
                                         update-menu-links-options-in-dialog!)

       (events/listen!  (sel "input#link-type-internal")
                        :change
                        toggle-add-menu-item-dialog-link-type-callback!)

       (events/listen!  (sel "input#link-type-external")
                        :change
                        toggle-add-menu-item-dialog-link-type-callback!)))))

(defn display-menu-editor-callback!
  "Displays the UI for the menu editor and creates the relevant
   events. The feeds argument contains a sequence containing all
   available feeds. The feed-map is a map containing feed information
   for this specific document. The status is either an HTTP status
   integer if editing an existing document or nil if editing a new
   document. The last argument contains a map that describes the
   document as stored in the database."
  [feeds
   feed-map
   status
   [{:keys [title
            content
            slug
            _id
            draft
            language
            feed
            created
            published
            previous-id
            datestamp]} :as document-states]]
  (let [new? (nil? status)]
    (ui/show!
     {:snippet :editor/back-to-overview-link
      :transformations
      [{:selector "//p[@id='back-to-overview-link']/a[1]"
        :attrs {:href (str "/admin/" language "/" feed "/overview")}}]}
     {:snippet :ui/status-message}
     {:snippet :ui/caption
      :transformations
      [{:selector "//h3[@id='caption']"
        :text (if new? "Create Menu" "Edit Menu")}]}
     {:snippet :editor/title-row
      :transformations
      [{:selector "//input[@id='title']"
        :value title}
       {:selector "//input[@id='draft']"
        :checked draft}]}
     {:snippet :editor/slug-row
      :transformations
      (concat [{:selector "//input[@id='slug']"
                :value slug}]
              (when new?
                [{:selector "//input[@id='custom-slug']"
                  :remove-attr "disabled"}
                 {:selector "//label[@id='custom-slug-label']"
                  :remove-class "disabled"}]))}

     {:snippet :editor/menu-builder}
     {:snippet :editor/add-menu-item-container}
     
     {:snippet :editor/save-button-container})
    
    (create-common-editor-events! new? feed-map)
    (display-existing-menu-from-string! feeds content)

    ;; TODO reimplement sorting
    ;; TODO add edit link feature
    ;; TODO add blank link (i.e. '#') option
    
    (events/listen! (sel "a#add-menu-item")
                    :click
                    (partial display-add-menu-item-dialog-callback!
                             nil
                             feeds))

    (events/listen! (sel "#save-document")
                    :click
                    (partial save-document-button-click-callback!
                             feed-map
                             new?
                             document-states))))

(defn display-editor!
  "Displays the editor for given language and feed-name strings,
   as well as document slug if an existing document is being edited."
  [language feed-name & [slug]]
  (swap! *file* dissoc :obj :data)
  (document/get-feeds-list
   (fn [status feeds]
     (let [feed-map
           (select-feed-by-language-and-name language feed-name feeds)
           editor-callback-fn
           (cond
            (= (:default-document-type feed-map) "menu")
            display-menu-editor-callback!
            :default
            display-editor-callback!)]
       (if slug
         (document/get-doc slug (partial editor-callback-fn
                                         feeds
                                         feed-map))
         (editor-callback-fn feeds
                             feed-map
                             nil
                             [{:language language
                               :feed feed-name}]))))))