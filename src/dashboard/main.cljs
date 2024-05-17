(ns dashboard.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]))

(defn init []
  (go (print "RICHO!")))

(defn ^:dev/before-load-async reload-begin* [done]
  (go (done)))

(defn ^:dev/after-load-async reload-end* [done]
  (go (done)))