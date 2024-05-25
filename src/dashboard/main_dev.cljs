(ns dashboard.main-dev
  (:require [clojure.core.async :as a :refer [go <!]]
            [dashboard.core :as core]
            [utils.gsheets :as gs]
            [utils.secrets.browser :as s]))

(enable-console-print!)

(def API_KEY (s/read-file "secrets/api_key.txt"))

(defn init []
  (print "DEV!")
  (gs/init-api-key! API_KEY)
  (core/init!))

(defn ^:dev/before-load-async reload-begin* [done]
  (go (core/exit!)
      (done)))

(defn ^:dev/after-load-async reload-end* [done]
  (go (<! (core/init!))
      (done)))