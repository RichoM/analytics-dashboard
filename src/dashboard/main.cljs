(ns dashboard.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [dashboard.data :as data]
            [dashboard.ui :as ui]))

(enable-console-print!)

(def credentials {:client_id "201101636025-l10ovc8h1fl4qnkd4fcpuq7d1gfot4f0.apps.googleusercontent.com"
                  :scope "https://www.googleapis.com/auth/spreadsheets.readonly"})

(def spreadsheet (gs/Spreadsheet. "1Yj79TCA0I-73SpLtBQztqNNJ8e-ANPYX5TpPLGZmqqI"))

(defonce !state (atom {}))

(defn authorize! []
  (go (try
        (<? (gs/authorize! credentials))
        (println "SUCCESS!")
        (catch :default err
          (println "ERROR" err)))))

(defn fetch-data! []
  (go (try
        (println "Fetching data...")
        (when (nil? (:data @!state))
          (println "Fetching...")
          (swap! !state assoc
                 :data (<? (data/fetch! spreadsheet))))
        (println "Successfully fetched data")
        (catch :default err
          (println "ERROR" err)))))

(defn init []
  (go (try
        (print "RICHO!")
        (<! (ui/show-authorization-dialog!))
        (<? (ui/show-wait-dialog! "Waiting for google..."
                                  (authorize!)))
        (<? (ui/show-wait-dialog! "Loading data..."
                                  (fetch-data!)))
        (ui/initialize-ui! !state)
        (catch :default err
          (println "ERROR" err)))))

(defn ^:dev/before-load-async reload-begin* [done]
  (go (ui/clear-ui!)
      (done)))

(defn ^:dev/after-load-async reload-end* [done]
  (go (<! (init))
      (done)))

(comment

  (def sessions (-> @!state :data :sessions))
  (def matches (-> @!state :data :matches))

  (map :valid? sessions)

  (set (map :version sessions))
  (first sessions)
  (time (data/sessions-by-day sessions))

  )