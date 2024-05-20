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
       [:button#test-btn.r-button.btn.btn-sm.btn-outline-dark.rounded-pill
        {:type "button" :data-bs-toggle "button"}
        [:i.fa-solid.fa-cube.me-2]
        "Test"]]]
     [:div.col-auto
      [:div.my-1]
      [:div#vis]]]]))

(defn show-authorization-dialog! []
  (bs/show-modal
   (bs/make-modal :body [:h2
                           [:i.fas.fa-exclamation-circle]
                           [:span.ms-2 "Authorization required!"]] 
                  :footer [:button.btn.btn-primary.btn-lg {:type "button" :data-bs-dismiss "modal" :aria-label "Log in"} "Log in"])
   {:backdrop "static"}))

(defn show-wait-dialog! [wait-chan]
  (go-try
   (let [wait-dialog (bs/make-modal :body [:div.container.overflow-hidden
                                           [:div.row.text-center [:h3 "Waiting for google..."]]
                                           [:div.row.m-1]
                                           [:div.row.text-center [:i.fas.fa-circle-notch.fa-spin.fa-4x]]])]
     (bs/show-modal wait-dialog {:backdrop "static"})
     (<? wait-chan)
     (bs/hide-modal wait-dialog))))

(defn show-test-chart! []
  (js/vegaEmbed
   "#vis"
   (clj->js {"data" {"url" "data/seattle-weather.csv"},
             "mark" "bar",
             :width 1024
             :height 512
             "encoding" {"x" {"timeUnit" "month",
                              "field" "date",
                              "type" "ordinal",
                              "title" "Month of the year"},
                         
                         "y" {"aggregate" "count",
                              "type" "quantitative"},
                         "color" {"field" "weather",
                                  "type" "nominal",
                                  "scale" {"domain" ["sun", "fog", "drizzle", "rain", "snow"],
                                           "range" ["#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd"]},
                                  "title" "Weather type"}}})
   (clj->js {:mode :vega-lite})))

(defn initialize-ui! []
  (go
    (doto js/document.body
      (oset! :innerHTML "")
      (.appendChild (main-container)))
    (let [test-btn (get-element-by-id "test-btn")]
      (bs/on-click test-btn #(show-test-chart!)))))

(defn clear-ui! []
  (oset! js/document.body :innerHTML ""))

(comment
  (do
    
    (js/vegaEmbed
     "#vis"
     (clj->js {"data" {"url" "data/seattle-weather.csv"},
               "mark" "bar",
               "encoding" {"x" {"timeUnit" "month",
                                "field" "date",
                                "type" "ordinal",
                                "title" "Month of the year"},
                           "y" {"aggregate" "count",
                                "type" "quantitative"},
                           "color" {"field" "weather",
                                    "type" "nominal",
                                    "scale" {"domain" ["sun", "fog", "drizzle", "rain", "snow"],
                                             "range" ["#e7ba52", "#c7c7c7", "#aec7e8", "#1f77b4", "#9467bd"]},
                                    "title" "Weather type"}}})
     (clj->js {:mode :vega-lite})))
  
  
  )