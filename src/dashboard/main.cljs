(ns dashboard.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [crate.core :as crate]))

(enable-console-print!)

(defn get-element-by-id [id]
  (js/document.getElementById id))

(defn main-container []
  (crate/html
   [:div#main-container.container-fluid
    [:div.row
     [:div#side-bar.col-auto
      [:div.row.my-1] 
      [:div.d-grid
       [:button#authorize-button.r-button.btn.btn-sm.btn-outline-dark.rounded-pill
        {:type "button" :data-bs-toggle "button"}
        [:i.fa-solid.fa-cube.me-2]
        "Authorize"]]]]]))

(def credentials {:client_id "201101636025-l10ovc8h1fl4qnkd4fcpuq7d1gfot4f0.apps.googleusercontent.com"
                  :scope "https://www.googleapis.com/auth/spreadsheets.readonly"})

(defn authorize! []
  (go (try
        (<? (gs/authorize! credentials))
        (println "SUCCESS!")
        (catch :default err
          (println "ERROR" err)))))

(defn initialize-ui! []
  (doto js/document.body
    (oset! :innerHTML "")
    (.appendChild (main-container)))
  (let [authorize-btn (get-element-by-id "authorize-button")]
    (bs/on-click authorize-btn authorize!)))

(comment
  
  

  

  (go (print "HOLA")
      (<! (bs/alert "CUIDADO!" "Hombre radioactivo!"))
      (print "CHAU"))
  )

(defn init []
  (go (print "RICHO!")
      (initialize-ui!)))

(defn ^:dev/before-load-async reload-begin* [done]
  (go (init) 
      (done)))

(defn ^:dev/after-load-async reload-end* [done]
  (go (done)))