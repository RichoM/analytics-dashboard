(ns dashboard.ui
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [clojure.set :as set]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [utils.frequencies :as f]
            [utils.core :refer [indexed-by percent seek]]
            [crate.core :as crate]
            [dashboard.vega :as vega]
            [dashboard.data :as data]
            [dashboard.countries :as countries]))

(defonce !state (atom {:charts {:sessions-and-matches {}
                                :match-duration {}}
                       :visible-charts #{:sessions-and-matches}}))

(defn html [element]
  (let [element (vega/html html element)]
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

(defn normalize-mode [{:keys [game mode local?] :as match}]
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

                 mode)))

(defn boxplot-stats [data]
  (let [{:keys [percentiles sample-count]}
        (f/stats (frequencies data)
                 :percentiles [9 25 50 75 91])]
    {:count sample-count
     :lower (percentiles :p9)
     :q1 (percentiles :p25)
     :median (percentiles :p50)
     :q3 (percentiles :p75)
     :upper (percentiles :p91)}))

(defn sessions-and-matches [{:keys [games sessions matches]}]
  [:div.row
   [:div.my-4.col-auto
    [:h6.fw-bold.mx-5 "Sesiones y partidas por día"]
    (vega/line :values (data/sessions-by-day sessions matches)
               :width 1024 ; :height 512
               :x {:field :date
                   :title "Fecha"}
               :y {:field :count
                   :title "Cantidad"}
               :color {:field :type
                       :title "Tipo"})]

   [:div.my-4.col-auto
    [:h6.fw-bold.mx-5 "Partidas por sesión (promedio diario)"]
    (vega/line :values (data/matches-per-session sessions matches)
               :width 1024 ; :height 512
               :x {:field :date
                   :title "Fecha"}
               :y {:field :matches-per-session
                   :title "Partidas"}
               :color {:field :game
                       :title "Juego"})]
   [:div.row.my-4
    [:div.col-auto
     [:h6.fw-bold.mx-5 "Duración de las sesiones"]
     (vega/boxplot :values (->> sessions
                                (group-by :game)
                                (map (fn [[game sessions]]
                                       (assoc (boxplot-stats (map :duration_m sessions))
                                              :game game))))
                   :width 256
                   :x {:field :game}
                   :y {:title "Duración (minutos)"}
                   :color {:field :game :title "Juego"})]

    [:div.col-auto
     [:h6.fw-bold.mx-5 "Duración de las partidas"]
     (vega/boxplot :values (->> matches
                                (map normalize-mode)
                                (group-by #(select-keys % [:game :mode]))
                                (map (fn [[{:keys [game mode]} matches]]
                                       (assoc (boxplot-stats (map :duration_m matches))
                                              :game game
                                              :mode mode))))
                   :width 512
                   :x {:field :mode
                       :title "Tipo de partida"
                       :sort {:field :game}}
                   :y {:title "Duración (minutos)"}
                   :color {:field :game
                           :title "Color"})]]

   [:div.row.my-4
    [:div.col-4
     [:h6.fw-bold.mx-5 "Sesiones por plataforma"]
     (vega/arc :values (let [platforms (map :platform sessions)
                             freq-map (frequencies platforms)
                             total (count platforms)]
                         (map (fn [[platform count]]
                                {:type platform :count count
                                 :percent (percent (/ count total))})
                              freq-map))
               :color {:field :type})]

    [:div.col-4
     [:h6.fw-bold.mx-5 "Partidas por plataforma"]
     (vega/arc :values (let [platforms (map (comp :platform :session meta) matches)
                             freq-map (frequencies platforms)
                             total (count platforms)]
                         (map (fn [[platform count]]
                                {:type platform :count count
                                 :percent (percent (/ count total))})
                              freq-map))
               :color {:field :type})]]

   [:div.row.my-4
    [:div.col-4
     [:h6.fw-bold.mx-5 "Sesiones por versión"]
     (vega/bar :values (->> sessions
                            (map #(select-keys % [:game :version]))
                            (group-by :game)
                            (mapcat (fn [[game sessions]]
                                      (map (fn [[version count]]
                                             {:game game :version (str game " v" version) :count count})
                                           (update-vals (group-by :version sessions) count)))))
               :x {:field :version}
               :y {:field :count :title "Cantidad"}
               :color {:field :game}
               :width 256 :height 256)]

    [:div.col-4
     [:h6.fw-bold.mx-5 "Partidas por versión"]
     (vega/bar :values (->> matches
                            (map (fn [match]
                                   (assoc match :version (-> match meta :session :version))))
                            (map #(select-keys % [:game :version]))
                            (group-by :game)
                            (mapcat (fn [[game matches]]
                                      (map (fn [[version count]]
                                             {:game game :version (str game " v" version) :count count})
                                           (update-vals (group-by :version matches) count)))))
               :x {:field :version}
               :y {:field :count :title "Cantidad"}
               :color {:field :game}
               :width 256 :height 256)]]

   [:div.my-4.col-auto
    [:h6.fw-bold.mx-5 "Sesiones por país"]
    (vega/world-map :values (let [country-map (-> (group-by :country sessions)
                                                  (update-vals count))]
                              (map (fn [country]
                                     (let [count (get country-map country 0)]
                                       {:id (:id country)
                                        :name (:name country)
                                        :tooltip (str (:name country) ": " count)
                                        :count count}))
                                   countries/all-countries)))]

   [:div.my-4.col-auto
    [:h6.fw-bold.mx-5 "Partidas por país"]
    (vega/world-map :values (let [country-map (-> (group-by (comp :country :session meta) matches)
                                                  (update-vals count))]
                              (map (fn [country]
                                     (let [count (get country-map country 0)]
                                       {:id (:id country)
                                        :name (:name country)
                                        :tooltip (str (:name country) ": " count)
                                        :count count}))
                                   countries/all-countries))
                    :color-scheme :purples)]])

(defn players [{:keys [games sessions matches]}]
  [:div.row

   [:div.my-4.col-auto
    [:h6.fw-bold.text-center "Jugadores por día"]
    [:vega-lite {:width 1024
                 :height 512
                 :data {:values (->> sessions
                                     (data/player-count-by-day)
                                     (mapcat (fn [{:keys [date new returning]}]
                                               [{:date date :type :new :count new}
                                                {:date date :type :returning :count returning}])))}
                 :encoding {:x {:field :date
                                :type :ordinal
                                :axis {:labelAngle -35}
                                :title "Fecha"}
                            :y {:field :count
                                :type :quantitative
                                :title "Cantidad"}
                            :color {:field :type
                                    :title nil}}
                 :layer [{:mark {:type :bar :point true :tooltip true}}]}]]

   [:div.my-4.row
    [:div.col-auto
     [:h6.fw-bold.text-center "Jugadores nuevos vs recurrentes (TOTAL)"]
     [:vega-lite {:data {:values (let [pcs (group-by :pc sessions)
                                       freq-map (->> pcs
                                                     (map (fn [[pc sessions]]
                                                            (let [dates (set (map :date sessions))]
                                                              (count dates))))
                                                     (frequencies))
                                       total (count pcs)
                                       new (get freq-map 1 0)
                                       returning (reduce + (vals (dissoc freq-map 1)))]
                                   [{:type :new :count new
                                     :percent (percent (/ new total))}
                                    {:type :returning :count returning
                                     :percent (percent (/ returning total))}])}
                  :encoding {:theta {:field "count", :type "quantitative", :stack "normalize"},
                             :order {:field "count", :type "quantitative", :sort "descending"},
                             :color {:field "type",
                                     :title nil,
                                     :sort {:field "count", :order "descending"}},
                             :text {:field :percent, :type "nominal"}},
                  :layer [{:mark {:type "arc", :innerRadius 50, :point true,
                                  :tooltip {:content "data"}}},
                          {:mark {:type "text", :radius 75, :fill "black"}}]}]]

    [:div.col-auto
     [:h6.fw-bold.text-center "Jugadores nuevos vs recurrentes (por juego)"]
     [:vega-lite {:width 512
                  :height 192
                  :data {:values (->> sessions
                                      (group-by :game)
                                      (mapcat (fn [[game sessions]]
                                                (let [pcs (group-by :pc sessions)
                                                      freq-map (->> pcs
                                                                    (map (fn [[pc sessions]]
                                                                           (let [dates (set (map :date sessions))]
                                                                             (count dates))))
                                                                    (frequencies))
                                                      total (count pcs)
                                                      new (get freq-map 1 0)
                                                      returning (reduce + (vals (dissoc freq-map 1)))]
                                                  [{:game game :type :new :count new
                                                    :percent (percent (/ new total))}
                                                   {:game game :type :returning :count returning
                                                    :percent (percent (/ returning total))}]))))}
                  :encoding {:y {:field :game
                                 :type :nominal
                                                ;:axis {:labelAngle -35}
                                 :title nil}
                             :x {:field :count
                                 :type :quantitative
                                 :stack :normalize
                                 :axis {:format "%"},
                                 :title "Cantidad"}
                             :color {:field :type
                                     :title nil}}
                  :layer [{:mark {:type :bar :point true
                                  :tooltip {:content "data"}}}]}]]]

   [:div.row.my-4
    [:div.col-auto
     [:h6.fw-bold.text-center "Jugadores por plataforma"]
     [:vega-lite {:data {:values (let [platforms-by-pc (update-vals (group-by :pc sessions)
                                                                    (fn [sessions]
                                                                      (:platform (first sessions))))
                                       platforms (vals platforms-by-pc)
                                       freq-map (frequencies platforms)
                                       total (count platforms)]
                                   (map (fn [[platform count]]
                                          {:type platform :count count :percent (percent (/ count total))})
                                        freq-map))}
                  :encoding {:theta {:field "count", :type "quantitative", :stack "normalize"},
                             :order {:field "count", :type "quantitative", :sort "descending"},
                             :color {:field "type",
                                     :title nil,
                                     :sort {:field "count", :order "descending"}},
                             :text {:field :percent, :type "nominal"}},
                  :layer [{:mark {:type "arc", :innerRadius 50, :point true,
                                  :tooltip {:content "data"}}},
                          {:mark {:type "text", :radius 75, :fill "black"}}]}]]]

   (let [data (let [country-map (-> (group-by :country sessions)
                                    (update-vals (fn [sessions]
                                                   (count (set (map :pc sessions))))))]
                (map (fn [country]
                       (let [count (get country-map country 0)]
                         {:id (:id country)
                          :name (:name country)
                          :tooltip (str (:name country) ": " count)
                          :count count}))
                     countries/all-countries))
         domain [0 (apply max (map :count data))]]
     [:div.my-4.col-auto
      [:h6.fw-bold.text-center "Jugadores por país"]
      [:vega-lite {:width 1024
                   :height 512
                   :autosize "none"
                   :signals [{:name "tx", :update "width / 2"},
                             {:name "ty", :update "height / 2"},
                             {:name "scale",
                              :value 150,
                              :on [{:events {:type "wheel", :consume true},
                                    :update "clamp(scale * pow(1.0005, -event.deltaY * pow(16, event.deltaMode)), 150, 3000)"}]},
                             {:name "angles",
                              :value [0, 0],
                              :on [{:events "pointerdown",
                                    :update "[rotateX, centerY]"}]},
                             {:name "cloned",
                              :value nil,
                              :on [{:events "pointerdown",
                                    :update "copy('projection')"}]},
                             {:name "start",
                              :value nil,
                              :on [{:events "pointerdown",
                                    :update "invert(cloned, xy())"}]},
                             {:name "drag", :value nil,
                              :on [{:events "[pointerdown, window:pointerup] > window:pointermove",
                                    :update "invert(cloned, xy())"}]},
                             {:name "delta", :value nil,
                              :on [{:events {:signal "drag"},
                                    :update "[drag[0] - start[0], start[1] - drag[1]]"}]},
                             {:name "rotateX", :value 0,
                              :on [{:events {:signal "delta"},
                                    :update "angles[0] + delta[0]"}]},
                             {:name "centerY", :value 0,
                              :on [{:events {:signal "delta"},
                                    :update "clamp(angles[1] + delta[1], -60, 60)"}]}]
                   :projections [{:name "projection",
                                  :type "mercator",
                                  :scale {:signal "scale"},
                                  :rotate [{:signal "rotateX"}, 0, 0],
                                  :center [0, {:signal "centerY"}],
                                  :translate [{:signal "tx"}, {:signal "ty"}]}]
                   :data [{:name "data"
                           :values data}
                          {:name "world",
                           :url "https://vega.github.io/editor/data/world-110m.json",
                           :format {:type "topojson",
                                    :feature "countries"}
                           :transform [{:type :lookup :from "data" :key :id
                                        :fields [:id] :values [:count :tooltip]}]},
                          {:name "graticule",
                           :transform [{:type "graticule"}]}]
                   :scales [{:name "color"
                             :type "quantize"
                             :domain domain
                             :range {:scheme "purples" :count 7}}]
                   :legends [{:fill "color"
                              :title nil
                              :orient "top-left"}]
                   :marks [{:type "shape",
                            :from {:data "graticule"},
                            :encode {:update {:strokeWidth {:value 1},
                                              :strokeDash {:value [2, 5]},
                                              :stroke {:value "#abc"},
                                              :fill {:value nil}}},
                            :transform [{:type "geoshape", :projection "projection"}]},
                           {:type "shape",
                            :from {:data "world"},
                            :encode {:update {:strokeWidth {:value 0.5},
                                              :stroke {:value "#fff"},
                                              :fill {:scale "color" :field :count},
                                              :zindex {:value 0}
                                              :tooltip {:field :tooltip}}},
                            :transform [{:type "geoshape", :projection "projection"}]}]}]

      [:vega-lite {:height 128
                   :data {:values (->> data
                                       (sort-by :count)
                                       (reverse)
                                       (take 45))}
                   :encoding {:x {:field :name
                                  :type :ordinal
                                  :sort {:field "count", :order "descending"}
                                  :axis {:labelAngle -35}
                                  :title nil}
                              :y {:field :count
                                  :type :quantitative
                                  :title "Cantidad"}
                              :color {:value "#5c3696"}}
                   :layer [{:mark {:type :bar :point true :tooltip true}}]}]])])

(defn- group-related-dudeney-sessions [matches]
  (let [[groups last-group]
        (reduce (fn [[groups last-group] next]
                  (if (= "NEW" (:mode next))
                    [(conj groups last-group) [next]]
                    [groups (conj last-group next)]))
                [[] [(first matches)]]
                (rest matches))]
    (conj groups last-group)))

(defn- merge-dudeney-metadata [match-group]
  (apply merge-with
         (partial merge-with +)
         (keep :metadata match-group)))

(defn dudeney [{:keys [games sessions matches]}]
  (comment
    (do
      (def games (-> @!state :data :games))
      (def sessions (-> @!state :data :sessions))
      (def matches (-> @!state :data :matches))))

  (let [dudeney-matches (->> matches
                             (filter (comp #{"Dudeney's Art Gallery"} :game)))
        matches-by-pc (group-by (comp :pc :session meta) dudeney-matches)
        accumulated-metadata (->> (vals matches-by-pc)
                                  (mapcat group-related-dudeney-sessions)
                                  (keep merge-dudeney-metadata))
        data (reduce
              (fn [acc next]
                (reduce-kv
                 (fn [acc painting
                      {:keys [attempts pivot_changes polygon_rotations
                              ms_thinking ms_working is_solved]}]
                   (let [is_solved (if (int? is_solved)
                                     (not= 0 is_solved)
                                     is_solved)
                         painting (parse-long (str/replace-first (str painting) #":painting_?" ""))]
                     (-> acc
                         (update-in [painting is_solved :count] inc)
                         (update-in [painting is_solved :attempts] conj attempts)
                         (update-in [painting is_solved :pivot_changes] conj pivot_changes)
                         (update-in [painting is_solved :polygon_rotations] conj polygon_rotations)
                         (update-in [painting is_solved :s_thinking] conj (/ ms_thinking 1000))
                         (update-in [painting is_solved :m_working] conj (/ ms_working 1000 60)))))
                 acc
                 next))
              {}
              accumulated-metadata)
        get-stats (memoize (fn [key]
                             (->> data
                                  (map (fn [[painting stats]]
                                         (let [solved-count (get-in stats [true :count])
                                               unsolved-count (get-in stats [false :count])]
                                           (-> (f/stats (frequencies (get-in stats [true key]))
                                                        :percentiles [])
                                               (dissoc :percentiles)
                                               (assoc :painting painting
                                                      :solved-count solved-count
                                                      :unsolved-count unsolved-count
                                                      :total-count (+ solved-count
                                                                      unsolved-count)))))))))]
    [:div.row
     [:div.row
      [:div.col-auto.my-4
       [:h6.fw-bold.text-center "Movimientos (mediana)"]
       [:vega-lite.col-auto {:data {:values (get-stats :polygon_rotations)},
                             :encoding {:x {:field :painting, :type "ordinal"
                                            :title "Nivel"
                                            :axis {:labelAngle 0}}
                                        :y {:field :median
                                            :type "quantitative"
                                            :title nil}},
                             :layer [{:mark {:type "line"
                                             :point {:size 100}
                                             :tooltip {:content "data"}}}]}]]
      [:div.col-auto.my-4
       [:h6.fw-bold.text-center "Cambios de pivot (mediana)"]
       [:vega-lite.col-auto {:data {:values (get-stats :pivot_changes)},
                             :encoding {:x {:field :painting, :type "ordinal"
                                            :title "Pintura"
                                            :axis {:labelAngle 0}}
                                        :y {:field :median
                                            :type "quantitative"
                                            :title nil}},
                             :layer [{:mark {:type "line"
                                             :point {:size 100}
                                             :tooltip {:content "data"}}}]}]]
      [:div.col-auto.my-4
       [:h6.fw-bold.text-center "Minutos trabajando (mediana)"]
       [:vega-lite.col-auto {:data {:values (get-stats :m_working)},
                             :encoding {:x {:field :painting, :type "ordinal"
                                            :title "Pintura"
                                            :axis {:labelAngle 0}}
                                        :y {:field :median
                                            :type "quantitative"
                                            :title nil}},
                             :layer [{:mark {:type "line"
                                             :point {:size 100}
                                             :tooltip {:content "data"}}}]}]]
      [:div.col-auto.my-4
       [:h6.fw-bold.text-center "Segundos pensando (mediana)"]
       [:vega-lite.col-auto {:data {:values (get-stats :s_thinking)},
                             :encoding {:x {:field :painting, :type "ordinal"
                                            :title "Pintura"
                                            :axis {:labelAngle 0}}
                                        :y {:field :median
                                            :type "quantitative"
                                            :title nil}},
                             :layer [{:mark {:type "line"
                                             :point {:size 100}
                                             :tooltip {:content "data"}}}]}]]]]))

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
    [:div#side-bar.col-lg-auto
     [:div.sticky-top.py-2
      [:div.d-grid
       (side-bar-btn :sessions-and-matches "Sesiones y partidas")]
      [:div.d-grid.my-2
       (side-bar-btn :players "Jugadores")]
      (when (contains? (-> @!state :data :games)
                       "Dudeney's Art Gallery")
        [:div.d-grid.my-2
         (side-bar-btn :dudeney "Dudeney")])
      [:hr]
      [:div
       (map game-checkbox (-> @!state :data :games sort))]]]
    [:div#charts.col.w-auto ;overflow-auto.vh-100
     [:div.my-1]
     [:div
      (when (visible-chart? :sessions-and-matches)
        (sessions-and-matches (:data @!state)))
      (when (visible-chart? :players)
        (players (:data @!state)))
      (when (visible-chart? :dudeney)
        (dudeney (:data @!state)))]]]])


(defn update-ui! [old-state new-state]
  (vega/finalize!)
  (doto (get-element-by-id "content")
    (clear!)
    (append! (main-container)))
  (when-let [scroll js/document.scrollingElement]
    (when (not= (:visible-charts old-state)
                (:visible-charts new-state))
      (go (oset! scroll :scrollTop 0)))))

(defn update-filters! [{:keys [sessions matches]}]
  (let [filters (:game-filters @!state)]
    (swap! !state update :data
           assoc
           :sessions (filter (comp filters :game) sessions)
           :matches (filter (comp filters :game) matches))))

(defn initialize-ui! [data]
  (go
    (if (nil? data)
      (js/window.location.replace "https://www.youtube.com/watch?v=dQw4w9WgXcQ")
      (do (add-watch !state ::state-change
                     (fn [_ _ old new]
                       (if (not= (:game-filters old)
                                 (:game-filters new))
                         (update-filters! data)
                         (update-ui! old new))))
          (swap! !state assoc
                 :data data
                 :game-filters (:games data))))))

(defn clear-ui! []
  (clear! (get-element-by-id "content")))

(comment
  (do
    (def games (-> @!state :data :games))
    (def sessions (-> @!state :data :sessions))
    (def matches (-> @!state :data :matches)))

  (count sessions)
  (count matches)

  (def metadata (->> matches
                     (filter (comp #{"Dudeney's Art Gallery"} :game))
                     (keep :metadata)))

  (first metadata)

  (update {:a 1} :a + 2)

  (reduce (fn [acc next]
            (reduce-kv
             (fn [acc painting
                  {:keys [attempts pivot_changes polygon_rotations
                          ms_thinking ms_working is_solved]}]
               (-> acc
                   (update-in [painting is_solved :attempts] conj attempts)
                   (update-in [painting is_solved :pivot_changes] conj pivot_changes)
                   (update-in [painting is_solved :polygon_rotations] conj polygon_rotations)
                   (update-in [painting is_solved :ms_thinking] conj ms_thinking)
                   (update-in [painting is_solved :ms_working] conj ms_working)))
             acc
             next))
          {}
          (->> matches
               (filter (comp #{"Dudeney's Art Gallery"} :game))
               (keep :metadata)))


  ()
  (first sessions)
  (first matches)

  (:session (meta (first matches))))