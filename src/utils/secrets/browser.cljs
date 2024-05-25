(ns utils.secrets.browser
  (:require [utils.secrets.browser-macros :as m]))

(defn read-file [file]
  (m/read-file* file "secrets"))
