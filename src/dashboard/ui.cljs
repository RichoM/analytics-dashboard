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
      [:div.d-grid (btn "sessions-day-btn" "Sesiones y partidas")]
      [:div.row.my-1]
      [:div.d-grid (btn "match-duration-btn" "Duración de las partidas")]]
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



(defn show-sessions-per-day! [{:keys [sessions matches]}]
  (show-chart! {:title "Sesiones y partidas por día"
                :width 1024
                :height 512
                :data {:values (data/sessions-by-day (filter :valid? sessions)
                                                     (filter :valid? matches))}
                :encoding {:x {:field :date
                               :type :ordinal
                               :title "Fecha"
                               :axis {:labelAngle -35}}
                           :y {:field :count
                               :type :quantitative
                               :title "Cantidad"}
                           :color {:field :type 
                                   :type :nominal
                                   :title "Tipo"}}
                :layer [{:mark {:type "line" 
                                :point {:size 100} 
                                :tooltip true}}]}))

(defn show-match-duration! [{:keys [matches]}]
  (show-chart! {:width 512
                :height 512
                :data {:values (map (fn [{:keys [game mode local?] :as match}]
                                      (assoc match
                                             :mode (case game
                                                     "AstroBrawl"
                                                     (if (= mode "DEATHMATCH")
                                                       (if local?
                                                         "DEATHMATCH local"
                                                         "DEATHMATCH online")
                                                       mode)
                                                     
                                                     "Wizards of Lezama"
                                                     (if (= mode "PRACTICE")
                                                       "PAPABLANCA"
                                                       mode)
                                                     
                                                     (first (str/split mode #",")))))
                                    (remove #(< (:duration_s %) 3)
                                            (filter :valid? matches)))}
                :encoding {:x {:field :mode
                               :type :nominal
                               :title "Tipo de partida"
                               :axis {:labelAngle -35}
                               :sort {:field :game}}
                           :y {:field :duration_m
                               ;:aggregate :count
                               :type :quantitative
                               :title "Duración (minutos)"}
                           :color {:field :game :title "Juego"}}
                :layer [{:mark {:type "boxplot"}}
                        #_{:mark {:type "errorband"
                                :extent :iqr
                                :point {:size 100}
                                :tooltip true}}
                         #_{:mark {:type :line
                                :point true
                                :tooltip true}}]}))

(defn initialize-ui! [!state]
  (go
    (doto (get-element-by-id "content")
      (oset! :innerHTML "")
      (.appendChild (main-container)))
    (let [btn (get-element-by-id "test-btn")]
      (bs/on-click btn #(show-test-chart!)))
    (let [btn (get-element-by-id "sessions-day-btn")]
      (bs/on-click btn #(show-sessions-per-day! (-> @!state :data))))
    (let [btn (get-element-by-id "match-duration-btn")]
      (bs/on-click btn #(show-match-duration! (-> @!state :data))))))

(defn clear-ui! []
  (oset! (get-element-by-id "content") :innerHTML ""))

(comment
  (initialize-ui! dashboard.main/!state)

  (def data (:data @dashboard.main/!state))
  (def sessions (:sessions data))
  (def matches (:matches data))

  (count sessions)
  (count matches)

  (count (filter :valid? matches))
  (first matches)

  (:session (meta (first matches)))
  
  )