;; cljs/src/ui.cljs: utility functions related to the user interface.
;;
;; Copyright 2011, F.M. (Filip) de Waard <fmw@vix.io>.
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

(ns vix.ui
  (:require [vix.util :as util]
            [clojure.string :as string]
            [soy :as soy]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [goog.dom.classes :as classes]
            [goog.ui.Dialog :as goog.ui.Dialog]
            [goog.ui.Dialog.ButtonSet :as goog.ui.Dialog.ButtonSet]
            [goog.ui.Dialog.EventType :as goog.ui.Dialog.EventType]
            [goog.events :as events]
            [goog.fx.DragDrop :as goog.fx.DragDrop]
            [goog.fx.DragDropGroup :as goog.fx.DragDropGroup]
         ;; [goog.fx.dom :as fx-dom]
         ;; [goog.fx.Animation :as Animation]
         ;; [goog.fx.Animation.EventType :as transition-event]
         ;; [goog.Timer :as timer]
            ))

(defn button-to-obj [button]
  (util/map-to-obj {:key (name button)
                    :caption (if (= button :ok)
                               "OK"
                               (string/capitalize (name button)))}))

(defn button-set [& buttons]
  (let [button-set (new goog.ui.Dialog.ButtonSet)]
    (doseq [button buttons]
      (doto button-set
        (.addButton (button-to-obj button)
                    (= button (first buttons))
                    (= button :cancel))))
    button-set))

(defn display-dialog
  ([title content handler]
     (display-dialog title content handler :ok :cancel))
  ([title content handler & buttons]
     (let [dialog (new goog.ui.Dialog)]
       (events/listen dialog
                      goog.ui.Dialog.EventType/SELECT
                      handler)
       (doto dialog
         (.setTitle title)
         (.setContent content)
         (.setButtonSet (apply button-set buttons))
         (.setVisible true)))))

(defn remove-dialog []
  (doseq [el (concat (util/get-elements-by-class "modal-dialog")
                     (util/get-elements-by-class "modal-dialog-bg"))]
    (dom/removeNode el)))

(defn display-error [status-el message & other-elements]
  (doseq [el other-elements] (classes/add el "error"))

  (doto status-el
    (classes/remove "status-ok")
    (classes/remove "hide")
    (classes/add "status-error")
    (classes/add "error")
    (dom/setTextContent message))

  ; return false to validation functions
  false)

(defn remove-error [status-el & other-elements]
  (doseq [el other-elements] (classes/remove el "error"))
  
  (doto status-el
    (classes/remove "status-error")
    (classes/remove "error")
    (dom/setTextContent " ")))

(defn enable-element [el-id-or-obj]
  (let [el (util/get-element el-id-or-obj)]
    (doto el
      (classes/remove "disabled")
      (.removeAttribute "disabled"))))

(defn disable-element [el-id-or-obj]
  (let [el (util/get-element el-id-or-obj)]
    (doto el
      (classes/add "disabled")
      (.setAttribute "disabled" "disabled"))))

(defn set-form-value [el-id-or-obj value]
  (let [el (util/get-element el-id-or-obj)]
    (forms/setValue el value)))

(defn get-form-value [el-id-or-obj]
  (let [el (util/get-element el-id-or-obj)]
    (forms/getValue el)))

(defn get-form-value-by-name [form-el-id-or-obj name]
  (let [form (util/get-element form-el-id-or-obj)]
    (forms/getValueByName form name)))

(defn render-template [dom-el template data]
  (soy/renderElement dom-el template (util/col-to-js data)))

(defn render-template-as-string [template data]
  (template (util/col-to-js data)))

(defn trigger-on-class [class trigger-on f]
  ;; converting to vector to avoid issues with doseq and arrays
  (doseq [el (cljs.core.Vector/fromArray (dom/getElementsByClass class))]
    (events/listen el trigger-on f)))

(defn remove-class-from-elements [class elements]
  (doseq [el elements]
    (classes/remove el class)))

(defn not-fixed? [el]
  (not (classes/has el "fixed")))

(def active-el (atom nil))

(defn get-draggable-item
  "Returns the actual draggable item starting from a (possibly nested)
   event target element and going up the tree recursively (up to 5 times).
   The event's target object refers to the element that was clicked on,
   which might be a nested node (e.g. a span or a element inside the li
   node that is the intended draggable item)."
  [el]
  (loop [current-el el
         times-called 0]
    (if (and (= (.tagName current-el) "LI")
             (. current-el (hasAttribute "draggable")))
      (cond
       (= (.draggable current-el) false)
       (if (classes/has current-el "drop-on-grandparent")
         (util/get-parent (util/get-parent current-el))
         nil)
       (= (.draggable current-el) true)
       current-el)
      (if (<= times-called 5)
        (recur (util/get-parent current-el) (inc times-called))
        nil))))

(defn can-add-to-nested-ul? [el-dragged el-dropped-on]
  (not (or (dom/contains el-dragged el-dropped-on)
           (dom/contains el-dropped-on el-dragged))))

(defn get-item-details-el [el]
  (first (util/get-children-by-class el "item-details")))

(defn remove-highlight-from-add-link-elements [elements]
  (doseq [add-link-el elements]
    (when (classes/has add-link-el "highlight-add-link-el")
      (classes/remove add-link-el "highlight-add-link-el")
      (set! (.textContent add-link-el)
            (. (.textContent add-link-el) (substr 2))))))

(defn to-sortable-tree [parent-el after-drop-fn]
  (let [top-level-drop-zone-el (dom/getElement "menu-top-level-drop-zone")
        li-elements (filter not-fixed?
                            (util/get-children-by-tag parent-el "li"))
        add-link-elements (util/get-elements-by-tag-and-class
                           "a"
                           "add-item-to-nested-menu")]

    ;; required to make the top-level-drop-zone a valid drop target
    (. top-level-drop-zone-el
       (addEventListener
        "dragover"
        (fn [e]
          (. e (stopPropagation))
          (. e (preventDefault)))))

    (. top-level-drop-zone-el
       (addEventListener
        "drop"
        (fn [e]
          (. e (stopPropagation))
          (. e (preventDefault))
          (dom/appendChild parent-el @active-el))))
    
    (doseq [el li-elements]
      ;; not using Google's events/listen because it strips .dataTransfer
      (. el (addEventListener
             "dragstart"
             (fn [e]
               (when-let [dragged-el (get-draggable-item (.target e))]
                 (reset! active-el dragged-el)
                 (classes/add dragged-el "dragging")
                 (comment
                   (set! (.effectAllowed (.dataTransfer e)) "move"))
                 (. (.dataTransfer e)
                    (setData "text/html" (.innerHTML dragged-el)))

                 ;; show the top level drop zone (if relevant)
                 (when-not (= (util/get-parent dragged-el) parent-el)
                   (classes/remove top-level-drop-zone-el "invisible"))
                 
                 ;; highlight add link buttons that can be dropped on
                 (doseq [add-link-el add-link-elements]
                   (when-not (or (classes/has add-link-el
                                              "highlight-add-link-el")
                                 (dom/contains dragged-el add-link-el)
                                 (util/is-sibling? dragged-el
                                                   (util/get-parent
                                                    add-link-el)))
                     (classes/add add-link-el "highlight-add-link-el")
                     (set! (.textContent add-link-el)
                           (str "\u21fe " (.textContent add-link-el)))))))))
      
      ;; required to make the node a valid drop target  
      (. el (addEventListener
             "dragover"
             (fn [e]
               (. e (stopPropagation))
               (. e (preventDefault)))))

      (. el (addEventListener
             "drop"
             (fn [e]
               (. e (stopPropagation))
               (. e (preventDefault))
               
               (remove-highlight-from-add-link-elements add-link-elements)
               
               (when-let [drop-target (get-draggable-item (.target e))]
                 (cond
                  ;; when dropped on an add-item node of a nested ul
                  (classes/has (.target e) "add-item-to-nested-menu")
                  (when (can-add-to-nested-ul? @active-el drop-target)
                    (dom/insertSiblingBefore @active-el
                                             (util/get-parent (.target e))))
                  :default
                  ;; when dropped on another node
                  (when-not (= @active-el drop-target)
                    (if
                        ;; when dropped on an ancestor or descendant node
                        (or (dom/contains drop-target @active-el)
                            (dom/contains @active-el drop-target))
                      (let [dt-item-details-el (get-item-details-el
                                                drop-target)
                            data-dummy-el (dom/createElement "div")]
                        (do
                          ;; use dummy el to access the item-details
                          ;; child node of the dragged element
                          (set! (.innerHTML data-dummy-el)
                                (. (.dataTransfer e) (getData "text/html")))
                          (set! (.innerHTML (get-item-details-el @active-el))
                                (.innerHTML dt-item-details-el))
                          (set! (.innerHTML dt-item-details-el)
                                (.innerHTML (first
                                             (util/get-children
                                              data-dummy-el))))))
                      ;; when dropped on an unrelated node
                      (do
                        (set! (.innerHTML @active-el)
                              (.innerHTML drop-target))
                        (set! (.innerHTML drop-target)
                              (. (.dataTransfer e)
                                 (getData "text/html"))))))))
               (after-drop-fn))))

      (. el (addEventListener
             "dragend"
             (fn [e]
               (. e (stopPropagation))
               (. e (preventDefault))
               
               (classes/remove @active-el "dragging")
               (classes/add top-level-drop-zone-el "invisible")
               
               (remove-highlight-from-add-link-elements
                add-link-elements)))))))

; TODO: this function is still a work-in-progress
(comment
  (defn fx!
    ([fx-obj element duration]
       (fx! fx-obj element duration {:begin nil :end nil}))
    ([fx-obj element duration event-handlers]
       (let [begin-fn (:begin event-handlers)
             end-fn (:end event-handlers)
             animation (fx-obj. element duration)]

         (when (fn? begin-fn)
         (events/listen animation transition-event/BEGIN begin-fn))
 
       (when (fn? end-fn)
         (events/listen animation
                        transition-event/END
                        #((do (end-fn)
                              (. animation (destroy))))))
       (. animation (play)))))

(def fade-in! (partial fx! fx-dom/FadeInAndShow true))
(def fade-out! (partial fx! fx-dom/FadeOutAndHide true)))

(comment
  ; TODO: implement a nice animation for displaying status messages

  (defn remove-slug-error [status-el slug-el]
    (let [end-fn (fn []
                   ; make sure we don't remove new errors that
                   ; popped up after the start of the animation
                   (when (classes/has slug-el "error")
                     (swap! fade-out-animation-active false)
                     (classes/remove status-el "status-error")
                     (classes/remove status-el "error")
                     (dom/setTextContent status-el " ")))]
      (when (classes/has slug-el "error")
        (fade-out! status-el 1000 {:begin-fn nil :end-fn end-fn})
        (classes/remove slug-el "error")))))