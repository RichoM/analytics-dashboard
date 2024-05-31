(ns dashboard.core
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [dashboard.data :as data]
            [dashboard.ui :as ui]))

(def credentials {:client_id "201101636025-l10ovc8h1fl4qnkd4fcpuq7d1gfot4f0.apps.googleusercontent.com"
                  :scope "https://www.googleapis.com/auth/spreadsheets.readonly"})

(defonce !data (atom nil))

(defn authorize! []
  (go (try
        (<? (gs/authorize! credentials))
        (println "SUCCESS!")
        (catch :default err
          (println "ERROR" err)))))

(defn fetch-data! []
  (go (try
        (println "Fetching data...")
        (let [begin-time (js/Date.now)
              data (<? (data/fetch!))
              end-time (js/Date.now)
              elapsed-s (/ (- end-time begin-time) 1000)]
          (when (seq (:games data))
            (println "Successfully fetched data in" elapsed-s "seconds")
            (reset! !data data)))
        (catch :default err
          (println "ERROR" err)))))

(defn init! []
  (go (try
        (when-not (gs/authorized?)
          (<? (ui/show-authorization-dialog!))
          (<? (ui/show-wait-dialog! "Waiting for google..."
                                    (authorize!))))
        (when (nil? @!data)
          (<? (ui/show-wait-dialog! "Loading data..."
                                    (fetch-data!))))
        (ui/initialize-ui! @!data)
        (catch :default err
          (println "ERROR" err)))))

(defn exit! []
  (ui/clear-ui!))