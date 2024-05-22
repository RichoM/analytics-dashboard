(ns dashboard.ui
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [crate.core :as crate]
            [dashboard.data :as data]))

(defonce !state (atom {:charts {:sessions-and-matches {}
                                :match-duration {}}
                       :visible-charts #{:sessions-and-matches}}))

(defn html-vega [element]
  (if (vector? element)
    (let [[type spec] element]
      (case type
        :vega-lite (doto (js/document.createElement "div")
                     (js/vegaEmbed (clj->js spec)
                                   (clj->js {:mode :vega-lite})))
        (vec (keep html-vega element))))
    element))


(defn html [element]
  (let [element (html-vega element)]
    (if (vector? element)
      (crate/html element)
      element)))

(defn get-element-by-id [id]
  (js/document.getElementById id))

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

(defn clear! [element]
  (oset! element :innerHTML ""))

(defn append! [^js element & children]
  (doseq [child children]
    (.appendChild element (if (vector? child)
                            (html child)
                            child))))

(defn show-test-chart! []
  (doto (get-element-by-id "vis")
    (clear!)
    (append! [:vega-lite {"data" {"url" "data/seattle-weather.csv"},
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
                                               "title" "Weather type"}}}])))

(comment
  
  (html [:vega-lite {"data" {"url" "data/seattle-weather.csv"},
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
                                          "title" "Weather type"}}}])
  )


(defn sessions-and-matches [{:keys [sessions matches]}]
  [:vega-lite
   {:title "Sesiones y partidas por día"
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
                    :tooltip true}}]}])

(defn match-duration [{:keys [matches]}]
  [:vega-lite
   {:title "Duración de las partidas"
    :width 512
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
                   :type :quantitative
                   :title "Duración (minutos)"}
               :color {:field :game :title "Juego"}}
    :layer [{:mark {:type "boxplot"}}]}])

(defn toggle-btn [text]
  (html [:button.r-button.btn.btn-sm.btn-outline-dark.rounded-pill
         {:type "button" :data-bs-toggle "button"}
         [:i.fa-solid.fa-cube.me-2]
         text]))

(defn set-pressed! [btn pressed?]
  (if pressed?
    (ocall! btn :classList.add "active")
    (ocall! btn :classList.remove "active"))
  btn)

(defn visible-chart? [chart-id]
  (contains? (:visible-charts @!state) chart-id))

(defn side-bar-btn [chart-id text]
  (doto (toggle-btn text)
    (set-pressed! (visible-chart? chart-id))
    (bs/on-click #(swap! !state update :visible-charts
                         (fn [visible-charts]
                           (if (contains? visible-charts chart-id)
                             (disj visible-charts chart-id)
                             (conj visible-charts chart-id)))))))

(defn main-container []
  [:div#main-container.container-fluid
   [:div.row
    [:div#side-bar.col-auto.w-auto
     [:div.row.my-1]
     [:div.d-grid (side-bar-btn :sessions-and-matches "Sesiones y partidas")]
     [:div.row.my-1]
     [:div.d-grid (side-bar-btn :match-duration "Duración de las partidas")]
     [:div.row.my-2]
     [:div.form-check.form-switch.text-center.mx-3
      [:input#ball-prediction.form-check-input {:type "checkbox" :role "switch" :checked false}]
      [:label.form-check-.ebal {:for "ball-prediction"} "Retro Racing: Double Dash"]]]
    [:div.col.w-auto.overflow-auto.vh-100
     [:div.my-1]
     [:div#vis
      (when (visible-chart? :sessions-and-matches)
        (sessions-and-matches (:data @!state)))
      (when (visible-chart? :match-duration)
        (match-duration (:data @!state)))]]]])


(defn update-ui! []
  (doto (get-element-by-id "content")
      (clear!)
      (append! (main-container))))

(defn initialize-ui! [data]
  (go
    (add-watch !state ::state-change #(update-ui!))
    (swap! !state assoc :data data)))

(defn clear-ui! []
  (clear! (get-element-by-id "content")))

(comment
  (initialize-ui! dashboard.main/!state)

  (keys @dashboard.main/!state)
  (def data (:data @dashboard.main/!state))
  (def sessions (:sessions data))
  (def matches (:matches data))

  (count sessions)
  (count matches)

  (count (filter :valid? matches))
  (first matches)

  (:session (meta (first matches))))