(ns dashboard.ui
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [utils.frequencies :as f]
            [crate.core :as crate]
            [dashboard.data :as data]))

(defonce !state (atom {:charts {:sessions-and-matches {}
                                :match-duration {}}
                       :visible-charts #{:sessions-and-matches}}))

(declare html)

(defonce !vega-views (atom []))

(defonce !vega-cache (atom {}))

(comment
  
  (count @!vega-cache)
  (ocall! (:element (first @!vega-views))
          :remove)
  
  )

(defn vega-finalize! []
  #_(println "vega-finalize!")
  #_(let [[old _] (reset-vals! !vega-views [])]
    (doseq [view old]
      (print "Fin!")
      (ocall! view :finalize))))

(defn vega-embed! [element spec]
  (doto (js/vegaEmbed element
                      (clj->js spec)
                      (clj->js {:mode :vega-lite}))
    (.then (fn [result]
             (swap! !vega-views conj result)))
    (.catch js/console.warn)))

(defn vega-replace! [element]
  (if-let [res (get @!vega-cache element)]
    res
    (let [[tag & content] element
          [attrs] (drop-last content)
          spec (last content)
          res (doto (html [(keyword (str/replace-first (str tag) ":vega-lite" "div"))
                           (or attrs {})])
                (.appendChild (doto (js/document.createElement "div")
                                (vega-embed! spec))))]
      (swap! !vega-cache assoc element res)
      res)))

(defn html-vega [element]
  (if (vector? element)
    (let [[tag & content] element]
      (if (str/starts-with? (str tag) ":vega-lite")
        (vega-replace! element)
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

(defn sessions-and-matches [{:keys [games sessions matches]}]
  [:div
   [:vega-lite.my-4
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
                     :tooltip true}}]}]



   (comment

     (- 1.36425 0.26025)



     (* 1.5 2)

     (def iqr (- 4.32905 1.4943166666666667))
     (def lower-iqr (- 1.4943166666666667 (* 1.5 iqr)))
     (def upper-iqr (+ 4.32905 (* 1.5 iqr)))

     (utils.frequencies/stats
      (->> matches
           (filter :valid?)
           (filter (comp #{"AstroBrawl"} :game))
           (filter (comp #{"PRACTICE"} :mode))
           (map :duration_m)
           (frequencies))
      :percentiles [2 98])






     (utils.core/seek (comp #{"262d7564-667e-45a5-9016-a1abeda6861c"} :id)
                      (filter :valid? sessions))

     (filter (fn [{:keys [id platforms]}] (contains? platforms "Android"))
             (map (fn [[id c]]
                    (let [rows (->> sessions
                                    (filter :valid?)
                                    (filter (comp #{id} :id)))]
                      {:id id
                       :game (set (map :game rows))
                       :platforms (set (map :platform rows))
                       :count c
                       :pc (set (map :pc rows))}))
                  (reverse (sort-by second (filter (fn [[id n]] (> n 1))
                                                   (update-vals (group-by :id sessions) count))))))

     (take 10 (reverse (sort-by second (filter (fn [[id n]] (> n 1))
                                               (update-vals (group-by :id sessions) count)))))

     (utils.core/seek (comp #{"073036cb-fe42-420e-9e0f-de0ca8bc5e0a"} :id)
                      (filter :valid? sessions))
     (meta (utils.core/seek (comp #{"073036cb-fe42-420e-9e0f-de0ca8bc5e0a"} :session)
                            matches))

     (let [sessions-by-game (group-by :game (filter :valid? sessions))
           matches-by-game (group-by :game (filter :valid? matches))]
       (->> (sort games)
            (mapcat (fn [game]
                      (let [matches (group-by :session (matches-by-game game))
                            sessions (->> (sessions-by-game game)
                                          (map (fn [{:keys [id datetime]}]
                                                 {:id id
                                                  :game game :date datetime
                                                  :match_count (count (matches id))})))]
                        sessions))))))


   [:vega-lite.my-4
    {:title "Cantidad de partidas por sesión"
     :width 1024
     :height 512
     :data {:values (data/matches-per-session (filter :valid? sessions)
                                              (filter :valid? matches))}
     :encoding {:x {:field :date
                    :type :ordinal
                    :title "Fecha"
                    :axis {:labelAngle -35}}
                :y {:field :matches-per-session
                    :type :quantitative
                    :title "Partidas por sesión (promedio)"}
                :color {:field :game
                        :type :nominal
                        :title "Juego"}}
     :layer [{:mark {:type "line"
                     :point {:size 100}
                     :tooltip true}}]}]

   [:vega-lite.my-4
    {:title "Duración de las sesiones"
     :width 256
     :height 512
     :data {:values (->> sessions
                         (filter :valid?)
                         (mapv #(select-keys % [:game :duration_m])))}
     :encoding {:x {:field :game
                    :type :nominal
                    :title "Juego"
                    :axis {:labelAngle -35}
                    :sort {:field :game}}
                :y {:field :duration_m
                    :type :quantitative
                    :title "Duración (minutos)"}
                :color {:field :game :title "Juego"}}
     :layer [{:mark {:type "boxplot"}}]}]


   [:vega-lite.my-4
    {:title "Duración de las partidas"
     :width 512
     :height 512
     :data {:values (->> matches
                         (filter :valid?)
                         (map (fn [{:keys [game mode local?] :as match}]
                                (assoc match
                                       :mode (case (str/lower-case game)
                                               
                                               "astrobrawl"
                                               (if (= mode "DEATHMATCH")
                                                 (if local?
                                                   "DEATHMATCH local"
                                                   "DEATHMATCH online")
                                                 mode)

                                               "wizards of lezama"
                                               (if (= mode "PRACTICE")
                                                 "PAPABLANCA"
                                                 mode)
                                               
                                               "retro racing: double dash"
                                               (let [mode (first (str/split mode #","))]
                                                 (if (= mode "RACE")
                                                   (if local?
                                                     "RACE local"
                                                     "RACE online")
                                                   mode))

                                               mode))))
                         (group-by #(select-keys % [:game :mode]))
                         (map (fn [[{:keys [game mode]} matches]]
                                (let [{:keys [percentiles sample-count]}
                                      (f/stats (frequencies (map :duration_m matches))
                                               :percentiles [9 25 50 75 91])]
                                  {:game game :mode mode
                                   :count sample-count
                                   :lower (percentiles :p9)
                                   :q1 (percentiles :p25)
                                   :median (percentiles :p50)
                                   :q3 (percentiles :p75)
                                   :upper (percentiles :p91)}))))},
     :encoding {:x {:field :mode, :type "nominal" 
                    :title "Tipo de partida"
                    :axis {:labelAngle -35}
                    :sort {:field :game}}
                :y {:title "Duración (minutos)"}},
     :layer [{:mark {:type "rule" },
              :encoding {:y {:field :lower, :type "quantitative", :scale {:zero false}},
                         :y2 {:field :upper}}},
             {:mark {:type "bar", :size 14 :tooltip {:content "data"}},
              :encoding {:y {:field :q1, :type "quantitative"},
                         :y2 {:field :q3},
                         :color {:field :game, :type "nominal" :title "Juego"}}},
             {:mark {:type "tick", :color "white", :size 14},
              :encoding {:y {:field :median, :type "quantitative"}}}]}]])

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
                           #{chart-id}
                           #_(if (contains? visible-charts chart-id)
                             (disj visible-charts chart-id)
                             (conj visible-charts chart-id)))))))

(defn game-keyword [game-name]
  (-> game-name
      (str/lower-case)
      (str/replace #"[^a-zA-Z]+" "-")
      (str/replace #"^-" "")
      (keyword)))

(defn game-checkbox [game-name]
  (let [game-key (game-keyword game-name)
        checked? (contains? (:game-filters @!state) game-name)]
    (html [:div.form-check.form-switch.text-center.mx-3
           (doto (html [:input.form-check-input {:id game-key :type "checkbox"
                                                 :role "switch" :checked checked?}])
             (bs/on-click #(swap! !state update :game-filters
                                  (fn [filters]
                                    (if (contains? filters game-name)
                                      (disj filters game-name)
                                      (conj filters game-name))))))
           [:label.form-check-.ebal {:for game-key} game-name]])))

(defn main-container []
  [:div#main-container.container-fluid
   [:div.row
    [:div#side-bar.col-auto.w-auto.my-2
     [:div.row.my-1]
     [:div.d-grid (side-bar-btn :sessions-and-matches "Sesiones y partidas")]
     [:div.row.my-1]
     [:div.d-grid (side-bar-btn :players "Jugadores")]
     [:div.row.my-1]
     [:div.d-grid (side-bar-btn :platforms "Plataformas")]
     [:div.row.my-1]
     [:div.d-grid (side-bar-btn :countries "Países")]
     [:div.row.my-2]
     [:hr]
     [:div
      (map game-checkbox (-> @!state :data :games sort))]]
    [:div#charts.col.w-auto.overflow-auto.vh-100
     [:div.my-1]
     [:div
      (when (visible-chart? :sessions-and-matches)
        (sessions-and-matches (:data @!state)))]]]])


(defn update-ui! []
  (vega-finalize!)
  (let [old-scroll (when-let [charts (get-element-by-id "charts")]
                     (oget charts :scrollTop))]
    (doto (get-element-by-id "content")
      (clear!)
      (append! (main-container)))
    (go (oset! (get-element-by-id "charts") 
               :scrollTop (or old-scroll 0)))))

(defn update-filters! [{:keys [sessions matches]}]
  (let [filters (:game-filters @!state)]
    (swap! !state update :data
           assoc 
           :sessions (filter (comp filters :game) sessions)
           :matches (filter (comp filters :game) matches))))

(defn initialize-ui! [data]
  (go
    (add-watch !state ::state-change
               (fn [_ _ old new]
                 (if (not= (:game-filters old)
                           (:game-filters new))
                   (update-filters! data)
                   (update-ui!))
                 ))
    (swap! !state assoc 
           :data data
           :game-filters (:games data))))

(defn clear-ui! []
  (clear! (get-element-by-id "content")))

(comment
  
  (def games (-> @!state :data :games))
  (def sessions (-> @!state :data :sessions))
  (def matches (-> @!state :data :matches))

  (count sessions)
  (count matches)

  (count (filter :valid? matches))
  (first matches)

  (:session (meta (first matches))) 
  
  )