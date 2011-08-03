(ns vix.core
  (:require [vix.editor :as editor]))

(defn routing [uri-path]
  (cond
   (re-matches #"^/admin/[^/]+/new$" uri-path) (editor/start :new uri-path)
   (re-matches #"^/admin/[^/]+/edit.+" uri-path) (editor/start :edit uri-path)))

(defn start-app [uri-path]
  (routing uri-path))

(start-app (js* "location.pathname"))