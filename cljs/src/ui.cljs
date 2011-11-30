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

(defn inside-coordinates? [el x y]
  (and el
       (>= x (.offsetLeft el))
       (< x (+ (.offsetLeft el) (.offsetWidth el)))
       (>= y (.offsetTop el))
       (< y (+ (.offsetTop el) (.offsetHeight el)))))

(defn get-target-from-position [candidate-elements x y]
  (loop [elements candidate-elements]
    (if (and (first elements) (inside-coordinates? (first elements) x y))
      (first elements)
      (when (pos? (count elements))
        (recur (rest elements))))))

(defn not-fixed? [el]
  (not (classes/has el "fixed")))

(defn get-li-children-by-nesting-level [el]
  (let [li-elements (filter not-fixed? (util/get-children-by-tag el "li"))
        get-distance (partial util/get-distance-to-ancestor el)]
    (map
     last
     (reverse
      (sort-by
       first
       (partition
        2
        (interleave (map get-distance li-elements) li-elements)))))))

(defn to-sortable-tree [parent-el after-drop-fn]
  (let [top-level-drop-zone (dom/getElement "menu-top-level-drop-zone")
        tldz-drag-drop-button (new goog.fx.DragDrop top-level-drop-zone
                                                :add-to-top-level)
        li-elements (get-li-children-by-nesting-level parent-el)
        ul-and-groups (cons [(new goog.fx.DragDropGroup) parent-el]
                            (map #(vector (new goog.fx.DragDropGroup) %)
                                 (util/get-children-by-tag parent-el "ul")))
        drop-fn
        (fn [e]
          (let [src-el (.data (.dragSourceItem e))
                tar-el (get-target-from-position
                        (cons top-level-drop-zone li-elements)
                        (.clientX e)
                        (.clientY e))]

            (when tar-el
                (classes/remove tar-el "dragover")
                (classes/remove src-el "dragging")

                (dom/removeNode src-el)

                (if (= tar-el top-level-drop-zone)
                  ;; insert the element at top level
                  (dom/insertChildAt parent-el
                                     src-el
                                     0)
                  ;; insert the element wherever dragged
                  (let [tar-parent (util/get-parent tar-el)
                    src-parent-ul (util/get-parent
                                   (util/get-parent src-el))
                    to-position (util/get-position-in-parent
                                 tar-el
                                 tar-parent)]
                    (classes/remove src-parent-ul "dragging")
                    (dom/insertChildAt tar-parent
                                       src-el
                                       to-position)))

                ;; avoid duplicate events
                (doseq [[g el-] ul-and-groups]
                  (. g (dispose)))
                ;; run after-drop-fn (typically re-renders)
                (after-drop-fn)
                ;; refresh the sortable structure
                (to-sortable-tree
                 (nth (first ul-and-groups) 1)
                 after-drop-fn))))]

    (when (pos? (count ul-and-groups))
      (events/listen tldz-drag-drop-button
                     "drop"
                     drop-fn))
    
    (doseq [[group el] ul-and-groups]
      (let [children (filter not-fixed? (util/get-children el))]
        (when (> (count children) 1)
          (doseq [child children]
            (classes/add child "draggable")
            (. group (addItem child child)))

          (doto group
            ;; add self as drop target
            (.addTarget group)
            ;; add top level box as target
            (.addTarget tldz-drag-drop-button))
          
          ;; add other groups as drop target
          (doseq [[other-group other-el] ul-and-groups]
            (. group (addTarget other-group)))

          (. group (init))

          ;; apply css class on dragstart
          (events/listen group
                         "dragstart"
                         (fn [e]
                           (classes/remove top-level-drop-zone "invisible")

                           (let [src-el (.data (.dragSourceItem e))]
                             (classes/add src-el "dragging")

                             ;; hack around parent getting actived
                             (when (pos? (count (util/get-children-by-class
                                                 src-el
                                                 "dragging")))
                               (classes/remove src-el "dragging")))))

          ;; remove css class on dragend
          (events/listen group
                         "dragend"
                         (fn [e]
                           (classes/add top-level-drop-zone "invisible")
                           (classes/remove (.data (.dragSourceItem e))
                                        "dragging")))
          ;; apply css class on dragover
          (events/listen group
                         "dragover"
                         (fn [e]
                           ;; get rid of existing dragover flags
                           (remove-class-from-elements "dragover" li-elements)
                           ;; hack in a temporary event to access cursor
                           (events/listenOnce
                              parent-el
                              "mouseover"
                              (fn [e]
                                (if-let [tar-el (get-target-from-position
                                                 li-elements
                                                 (.clientX e) 
                                                 (.clientY e))]
                                  (classes/add tar-el "dragover"))))))

          ;; remove css class on dragout
          (events/listen group
                         "dragout"
                         (partial remove-class-from-elements
                                  "dragover"
                                  li-elements))

          ;; move node on drop
          (events/listen group
                         "drop"
                         drop-fn))))))

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