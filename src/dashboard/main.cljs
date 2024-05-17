(ns dashboard.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [dashboard.ui :as ui]))

(enable-console-print!)

(def credentials {:client_id "201101636025-l10ovc8h1fl4qnkd4fcpuq7d1gfot4f0.apps.googleusercontent.com"
                  :scope "https://www.googleapis.com/auth/spreadsheets.readonly"})

(defn authorize! []
  (go (try
        (<? (gs/authorize! credentials))
        (println "SUCCESS!")
        (catch :default err
          (println "ERROR" err)))))

(defn init []
  (go (print "RICHO!")
      (ui/initialize-ui! authorize!)))

(defn ^:dev/before-load-async reload-begin* [done]
  (go (init)
      (done)))

(defn ^:dev/after-load-async reload-end* [done]
  (go (done)))