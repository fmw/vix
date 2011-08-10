(ns vix.ui
  (:require [goog.dom.classes :as classes]
            [goog.dom :as dom]
            [goog.fx.dom :as fx-dom]
            [goog.fx.Animation :as Animation]
            [goog.fx.Animation.EventType :as transition-event]
            [goog.Timer :as timer]))

(defn display-error [status-el message]
  (do
    (dom/setTextContent status-el message)
    (classes/remove status-el "status-ok")
    (classes/add status-el "status-error")))

(defn remove-error [status-el slug-el]
  (do
    (classes/remove status-el "status-error")
    (classes/remove status-el "error")
    (dom/setTextContent status-el " ")))

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