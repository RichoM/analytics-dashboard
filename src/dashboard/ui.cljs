(ns dashboard.ui
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [crate.core :as crate]))

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

(defn show-authorization-dialog! []
  (bs/show-modal
   (bs/make-modal :header [:h2
                           [:i.fas.fa-exclamation-circle]
                           [:span.ms-2 "Authorization required!"]]
                  :body [:h3 "If you arrived here by accident, please leave quietly. Thanks!"]
                  :footer [:button.btn.btn-primary.btn-lg {:type "button" :data-bs-dismiss "modal" :aria-label "Authorize"} "Authorize"])
   {:backdrop "static"}))

(defn initialize-ui! [authorize!]
  (go
    (oset! js/document.body :innerHTML "")
    (<! (show-authorization-dialog!))
    (let [wait-dialog (bs/make-modal :header [:h2 "Waiting for google..."])]
      (bs/show-modal wait-dialog {:backdrop "static"})
      (<! (authorize!))
      (bs/hide-modal wait-dialog))
    (.appendChild js/document.body (main-container))
    (let [authorize-btn (get-element-by-id "authorize-button")]
      (bs/on-click authorize-btn authorize!))))