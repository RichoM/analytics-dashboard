(ns dashboard.ui
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [crate.core :as crate]
            [dashboard.data :as data]))

(defn get-element-by-id [id]
  (js/document.getElementById id))

(defn btn [id text]
  [(keyword (str "button#" id ".r-button.btn.btn-sm.btn-outline-dark.rounded-pill"))
   {:type "button" :data-bs-toggle "button"}
   [:i.fa-solid.fa-cube.me-2]
   text])

(defn show-authorization-dialog! []
  (bs/show-modal
   (bs/make-modal :body [:h2
                           [:i.fas.fa-exclamation-circle]
                           [:span.ms-2 "Authorization required!"]] 
                  :footer [:button.btn.btn-primary.btn-lg {:type "button" :data-bs-dismiss "modal" :aria-label "Log in"} "Log in"])
   {:backdrop "static"}))

(defn show-wait-dialog! [title wait-chan]
  (go-try
   (bs/show-modal
    (bs/make-modal :body [:div.container.overflow-hidden
                          [:div.row.text-center [:h3 title]]
                          [:div.row.m-1]
                          [:div.row.text-center [:i.fas.fa-circle-notch.fa-spin.fa-4x]]])
    {:backdrop "static"})
   (<? wait-chan)
   (bs/hide-modals)))

(defn main-container []
  (crate/html
   [:div#main-container.container-fluid
    [:div.row
     [:div#side-bar.col-auto
      [:div.row.my-1]
      [:div.d-grid (btn "test-btn" "Test")]
      [:div.row.my-1]
      [:div.d-grid (btn "sessions-day-btn" "Sesiones por día")]
      [:div.row.my-1]]
     [:div.col-auto
      [:div.my-1]
      [:div#vis]]]]))

(defn show-chart! [vega-spec]
  (js/vegaEmbed
   "#vis"
   (clj->js vega-spec)
   (clj->js {:mode :vega-lite})))

(defn show-test-chart! []
  (show-chart! {"data" {"url" "data/seattle-weather.csv"},
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
                                     "title" "Weather type"}}}))



(defn show-sessions-per-day! [{:keys [sessions]}]
  (show-chart! {:title "Sesiones por día"
                :width 1024
                :height 512
                :data {:values (data/sessions-by-day (filter :valid? sessions))}
                :encoding {:x {:field :date
                               :type :ordinal
                               :title "Fecha"
                               :axis {:labelAngle -35}}
                           :y {:field :sessions
                               :type :quantitative
                               :title "Cantidad de sesiones"}}
                :layer [{:mark {:type "line" :point true :tooltip true}}]}))

(defn initialize-ui! [!state]
  (go
    (doto js/document.body
      (oset! :innerHTML "")
      (.appendChild (main-container)))
    (let [btn (get-element-by-id "test-btn")]
      (bs/on-click btn #(show-test-chart!)))
    (let [btn (get-element-by-id "sessions-day-btn")]
      (bs/on-click btn #(show-sessions-per-day! (-> @!state :data))))))

(defn clear-ui! []
  (oset! js/document.body :innerHTML ""))
