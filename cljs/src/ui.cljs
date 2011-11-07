(ns vix.ui
  (:require [vix.util :as util]
            [soy :as soy]
            [goog.dom :as dom]
            [goog.dom.forms :as forms]
            [goog.dom.classes :as classes]
            [goog.fx.dom :as fx-dom]
            [goog.fx.Animation :as Animation]
            [goog.fx.Animation.EventType :as transition-event]
            [goog.Timer :as timer]))

(defn display-error [status-el message & other-elements]
  (doseq [el other-elements] (classes/add el "error"))

  (doto status-el
    (classes/remove "status-ok")
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

(defn render-template [dom-el template data]
  (soy/renderElement dom-el
                     template
                     (if (map? data)
                       (util/map-to-obj data)
                       data)))

; TODO: this function is still a work-in-progress
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
(def fade-out! (partial fx! fx-dom/FadeOutAndHide true))

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