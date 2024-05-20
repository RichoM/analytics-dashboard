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
      (<! (ui/show-authorization-dialog!))
      (<? (ui/show-wait-dialog! (authorize!)))
      (ui/initialize-ui!)))

(defn ^:dev/before-load-async reload-begin* [done]
  (go (ui/clear-ui!)
      (done)))

(defn ^:dev/after-load-async reload-end* [done]
  (go (<! (init))
      (done)))

(comment
  
  (def !sessions (atom nil))

  (last @!sessions)

  (go 
    (try
      (reset! !sessions (<? (gs/get-values! spreadsheet-papacorps "sessions!A:K")))
      (print "SUCCESS FETCHING SESSIONS!")
      (catch :default err
        (println "ERROR" err))))
  
  (def !matches (atom nil))

  (count @!matches)

  (go
    (try
      (reset! !matches (<? (gs/get-values! spreadsheet-papacorps "matches!A:I")))
      (print "SUCCESS FETCHING MATCHES")
      (catch :default err
        (println "ERROR" err))))
  )