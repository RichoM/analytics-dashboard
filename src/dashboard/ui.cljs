(ns dashboard.ui
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [clojure.set :as set]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [utils.frequencies :as f]
            [utils.core :refer [indexed-by percent seek average]]
            [crate.core :as crate]
            [dashboard.ui-common :as ui-common]
            [dashboard.vega :as vega]
            [dashboard.data :as data]
            [dashboard.countries :as countries]
            [dashboard.astrobrawl :as ab]))

(comment
  (require '[portal.web :as p])
  (add-tap p/submit)
  )

(defonce !state (atom {:charts {:sessions-and-matches {}
                                :match-duration {}}
                       :visible-charts #{:summary}}))

(def chart-titles {:summary "Resumen ejecutivo"
                   :sessions-and-matches "Sesiones y partidas"
                   :players "Jugadores"
                   :dudeney "Dudeney"
                   :astrobrawl "AstroBrawl"})

(defn summary-by-game [sessions matches game-name]
  (let [filtered-sessions (if (nil? game-name)
                            sessions
                            (->> sessions
                                 (filter (comp #{game-name} :game))))
        filtered-matches (if (nil? game-name)
                           matches
                           (->> matches
                                (filter (comp #{game-name} :game))))
        sessions-by-day (data/group-by-day filtered-sessions)
        matches-by-day (data/group-by-day filtered-matches)
        countries (->> filtered-sessions
                       (map :country_code)
                       (set))
        unique-players (->> filtered-sessions
                            (map :pc)
                            (set)
                            (count))]
    (when-not (zero? (count filtered-matches))
      [:div.card
       [:div.card-header.fw-bold (or game-name "Todos los juegos seleccionados")]
       [:div.card-body
        [:p.card-text
         (count filtered-matches)
         (if (= 1 (count filtered-matches))
           " partida"
           " partidas")
         " (desde "
         (or (:date (first filtered-matches)) "?")
         " a "
         (or (:date (last filtered-matches)) "?")
         ")"]
        [:p.card-text
         (count countries)
         (if (= 1 (count countries))
           " país"
           " países")]
        [:p.card-text unique-players
         (if (= 1 unique-players)
           " jugador único"
           " jugadores únicos")]
        [:p.card-text (.toFixed (->> sessions-by-day
                                     (map (comp count second))
                                     (average))
                                2)
         " sesiones diarias (promedio)"]
        [:p.card-text (.toFixed (->> matches-by-day
                                     (map (comp count second))
                                     (average))
                                2)
         " partidas diarias (promedio)"]
        [:p.card-text "Duración de la sesión: "
         (.toFixed (->> filtered-sessions
                        (map :duration_m)
                        (average))
                   2)
         " minutos (promedio)"]]])))

(defn summary [{:keys [games sessions matches]}]
  [:div.my-2.container-fluid
   (->> (cons (summary-by-game sessions matches nil)
              (keep (partial summary-by-game sessions matches) games))
        (partition-all 2)
        (map (fn [cards]
               [:div.row
                (map (fn [card]
                       [:div.col-lg-4.my-2 card])
                     cards)])))])

(defn sessions-and-matches [{:keys [sessions matches]}]
  [:div.row
   [:div.my-4.col-auto
    (ui-common/title "Sesiones y partidas por día")
    (vega/line :values (data/sessions-by-day sessions matches)
               :width 1024 ; :height 512
               :x {:field :date
                   :title "Fecha"
                   :axis {:labelAngle -35
                          :labelOverlap true}}
               :y {:field :count
                   :title "Cantidad"}
               :color {:field :type
                       :title "Tipo"})]

   [:div.my-4.col-auto
    (ui-common/title "Partidas por sesión (promedio diario)")
    (vega/line :values (data/matches-per-session sessions matches)
               :width 1024 ; :height 512
               :x {:field :date
                   :title "Fecha"
                   :axis {:labelAngle -35
                          :labelOverlap true}}
               :y {:field :matches-per-session
                   :title "Partidas"}
               :color {:field :game
                       :title "Juego"})]
   [:div.row.my-4
    [:div.col-auto
     (ui-common/title "Duración de las sesiones")
     (vega/boxplot :values (->> sessions
                                (group-by :game)
                                (map (fn [[game sessions]]
                                       (assoc (ui-common/boxplot-stats (map :duration_m sessions))
                                              :game game))))
                   :width 256
                   :x {:field :game}
                   :y {:title "Duración (minutos)"}
                   :color {:field :game :title "Juego"})]

    [:div.col-auto
     (ui-common/title "Duración de las partidas")
     (vega/boxplot :values (->> matches
                                (map ui-common/normalize-mode)
                                (group-by #(select-keys % [:game :mode]))
                                (map (fn [[{:keys [game mode]} matches]]
                                       (assoc (ui-common/boxplot-stats (map :duration_m matches))
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
     (ui-common/title "Sesiones por plataforma")
     (vega/arc :values (let [platforms (map :platform sessions)
                             freq-map (frequencies platforms)
                             total (count platforms)]
                         (map (fn [[platform count]]
                                {:type platform :count count
                                 :percent (percent (/ count total))})
                              freq-map))
               :color {:field :type})]

    [:div.col-4
     (ui-common/title "Partidas por plataforma")
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
     (ui-common/title "Sesiones por versión")
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
     (ui-common/title "Partidas por versión")
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
    (ui-common/title "Sesiones por país")
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
    (ui-common/title "Partidas por país")
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

(def recurrentes ui-common/recurrentes)

(defn players [{:keys [games sessions matches]}]
  [:div.row

   [:div.my-4.col-auto
    (ui-common/title "Jugadores por día")
    (vega/bar :values (->> sessions
                           (data/player-count-by-day)
                           (mapcat (fn [{:keys [date new returning]}]
                                     [{:date date :type :new :count new}
                                      {:date date :type :returning :count returning}])))
              :width 1024
              :height 512
              :x {:field :date
                  :title "Fecha"
                  :axis {:labelAngle -35
                         :labelOverlap true}}
              :y {:field :count
                  :title "Cantidad"}
              :color {:field :type})]

   [:div.my-4.row
    [:div.col-auto
     (ui-common/title [:span "Jugadores nuevos vs " recurrentes]
                      "(TOTAL)")
     (vega/arc :values (let [pcs (group-by :pc sessions)
                             freq-map (->> pcs
                                           (map (fn [[_pc sessions]]
                                                  (let [dates (set (map :date sessions))]
                                                    (count dates))))
                                           (frequencies))
                             total (count pcs)
                             new (get freq-map 1 0)
                             returning (reduce + (vals (dissoc freq-map 1)))]
                         (if (zero? total)
                           []
                           [{:type :new :count new
                             :percent (percent (/ new total))}
                            {:type :returning :count returning
                             :percent (percent (/ returning total))}]))
               :color {:field :type})]

    [:div.col-auto
     (ui-common/title [:span "Jugadores nuevos vs " recurrentes]
                      "(por juego)")
     (vega/bar :values (->> sessions
                            (group-by :game)
                            (mapcat (fn [[game sessions]]
                                      (let [pcs (group-by :pc sessions)
                                            freq-map (->> pcs
                                                          (map (fn [[_pc sessions]]
                                                                 (let [dates (set (map :date sessions))]
                                                                   (count dates))))
                                                          (frequencies))
                                            total (count pcs)
                                            new (get freq-map 1 0)
                                            returning (reduce + (vals (dissoc freq-map 1)))]
                                        (if (zero? total)
                                          []
                                          [{:game game :type :new :count new
                                            :percent (percent (/ new total))}
                                           {:game game :type :returning :count returning
                                            :percent (percent (/ returning total))}])))))
               :y {:field :game
                   :type :nominal}
               :x {:field :count
                   :type :quantitative
                   :stack :normalize
                   :axis {:format "%"}}
               :color {:field :type}
               :width 512
               :height 192)]]

   [:div.row.my-4
    [:div.col-auto
     (ui-common/title "Jugadores por plataforma")
     (vega/arc :values (let [platforms-by-pc (update-vals (group-by :pc sessions)
                                                          (fn [sessions]
                                                            (:platform (first sessions))))
                             platforms (vals platforms-by-pc)
                             freq-map (frequencies platforms)
                             total (count platforms)]
                         (map (fn [[platform count]]
                                {:type platform :count count :percent (percent (/ count total))})
                              freq-map))
               :color {:field :type})]

    [:div.col-auto
     (ui-common/title "Jugadores por juego")
     (vega/arc :values (let [games-by-pc (update-vals (group-by :pc sessions)
                                                      (fn [sessions]
                                                        (let [games (set (map :game sessions))]
                                                          (assert (= 1 (count games))
                                                                  "Same pc for multiple games!")
                                                          (first games))))
                             games (vals games-by-pc)
                             freq-map (frequencies games)
                             total (count games)]
                         (map (fn [[game count]]
                                {:game game :count count :percent (percent (/ count total))})
                              freq-map))
               :color {:field :game})]]

   [:div.row.my-4
    [:div.col-auto
     (ui-common/title "Sesiones por jugador")
     (vega/bar :values (let [freq (->> (group-by :pc sessions)
                                       (map (fn [[pc sessions]]
                                              (count sessions)))
                                       (frequencies))
                             max (apply max (keys freq))]
                         (->> (range 1 (inc max))
                              (map (fn [session-count]
                                     {:session-count session-count
                                      :player-count (get freq session-count 0)}))
                              (remove (fn [{:keys [player-count]}]
                                        (< player-count 2)))))
               :x {:field :session-count
                   :title "Sesiones"
                   :type :nominal
                   :axis {:labelAngle 0}}
               :y {:field :player-count
                   :title "Jugadores"
                   :scale {:type :sqrt}})]
    [:div.col-auto
     (ui-common/title [:span "Sesiones por jugador (sólo " recurrentes ")"])
     (vega/bar :values (let [freq (->> (group-by :pc sessions)
                                       (remove (fn [[_ sessions]]
                                                 (= 1 (count (set (map :date sessions))))))
                                       (map (fn [[pc sessions]]
                                              (count sessions)))
                                       (frequencies))
                             max (apply max (keys freq))]
                         (->> (range 2 (inc max))
                              (map (fn [session-count]
                                     {:session-count session-count
                                      :player-count (get freq session-count 0)}))
                              (remove (fn [{:keys [player-count]}]
                                        (< player-count 2)))))
               :x {:field :session-count
                   :title "Sesiones"
                   :type :nominal
                   :axis {:labelAngle 0}}
               :y {:field :player-count
                   :title "Jugadores"
                   :scale {:type :sqrt}})]]


   [:div.row.my-4
    [:div.col-auto
     (ui-common/title "Tiempo de juego (promedio)")
     (vega/bar :values (->> sessions
                            (group-by :game)
                            (map (fn [[game sessions]]
                                   {:play-time (->> (group-by :pc sessions)
                                                    (map (fn [[pc sessions]]
                                                           (reduce + (map :duration_m sessions))))
                                                    (average))
                                    :game game})))

               :width 150
               :height 256
               :x {:field :game
                   :type :nominal
                   :title "Juego"}
               :y {:field :play-time
                   :title "Duración (minutos)"}
               :color {:field :game})]

    [:div.col-auto
     (ui-common/title "Tiempo de juego")
     (vega/boxplot :values (->> sessions
                                (group-by :game)
                                (map (fn [[game sessions]]
                                       (assoc (->> (group-by :pc sessions)
                                                   (map (fn [[pc sessions]]
                                                          (reduce + (map :duration_m sessions))))
                                                   (ui-common/boxplot-stats))
                                              :game game))))

                   :width 150
                   :height 256
                   :x {:field :game
                       :title "Juego"}
                   :y {:title "Duración (minutos)"}
                   :color {:field :game})]

    [:div.col-auto
     (ui-common/title [:span "Tiempo de juego (sólo " recurrentes ")"])
     (vega/boxplot :values (->> sessions
                                (group-by :game)
                                (map (fn [[game sessions]]
                                       (assoc (->> (group-by :pc sessions)
                                                   (remove (fn [[_ sessions]]
                                                             (= 1 (count (set (map :date sessions))))))
                                                   (map (fn [[pc sessions]]
                                                          (reduce + (map :duration_m sessions))))
                                                   (ui-common/boxplot-stats))
                                              :game game))))

                   :width 150
                   :height 256
                   :x {:field :game
                       :title "Juego"}
                   :y {:title "Duración (minutos)"}
                   :color {:field :game})]]

   [:div.my-4.col-auto
    (ui-common/title "Jugadores por país")
    (vega/world-map :values (let [country-map (-> (group-by :country sessions)
                                                  (update-vals (fn [sessions]
                                                                 (count (set (map :pc sessions))))))]
                              (map (fn [country]
                                     (let [count (get country-map country 0)]
                                       {:id (:id country)
                                        :name (:name country)
                                        :tooltip (str (:name country) ": " count)
                                        :count count}))
                                   countries/all-countries))
                    :color-scheme :teals)]])

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
        get-stats (fn [key]
                    (mapcat (fn [[painting metadata]]
                              (map (fn [value]
                                     {:painting painting :value value})
                                   (get-in metadata [true key])))
                            data))]
    [:div.row

     [:div.row
      [:div.col-auto.my-4
       (ui-common/title "Movimientos")
       (vega/scatter :values (get-stats :polygon_rotations)
                     :x {:field :painting
                         :title "Pintura"
                         :axis {:labelAngle 0}}
                     :y {:field :value})]
      [:div.col-auto.my-4
       (ui-common/title "Cambios de pivot")
       (vega/scatter :values (get-stats :pivot_changes)
                     :x {:field :painting
                         :title "Pintura"
                         :axis {:labelAngle 0}}
                     :y {:field :value})]
      [:div.col-auto.my-4
       (ui-common/title "Minutos trabajando")
       (vega/scatter :values (get-stats :m_working)
                     :x {:field :painting
                         :title "Pintura"
                         :axis {:labelAngle 0}}
                     :y {:field :value})]
      [:div.col-auto.my-4
       (ui-common/title "Segundos pensando")
       (vega/scatter :values (get-stats :s_thinking)
                     :x {:field :painting
                         :title "Pintura"
                         :axis {:labelAngle 0}}
                     :y {:field :value})]]

     [:div.col-10
      [:table.table.table-sm.table-hover.table-bordered.font-monospace.text-end
       [:thead
        [:tr
         [:th "#"]
         [:th "TOTAL"]
         [:th "Resueltos"]
         [:th "Movimientos" [:br]
          [:div.row.fw-light
           [:span.col "Median"]
           [:span.col "Min"]
           [:span.col "Max"]]]
         [:th "Cambios de pivot" [:br]
          [:div.row.fw-light
           [:span.col "Median"]
           [:span.col "Min"]
           [:span.col "Max"]]]
         [:th "Minutos trabajando" [:br]
          [:div.row.fw-light
           [:span.col "Median"]
           [:span.col "Min"]
           [:span.col "Max"]]]
         [:th "Segundos pensando" [:br]
          [:div.row.fw-light
           [:span.col "Median"]
           [:span.col "Min"]
           [:span.col "Max"]]]]]
       [:tbody.text-end
        (map (fn [[painting metadata]]
               (let [solved-count (get-in metadata [true :count] 0)
                     unsolved-count (get-in metadata [false :count] 0)
                     format-num (fn [n]
                                  (if (int? n)
                                    (str n)
                                    (.toFixed n 2)))
                     get-stats (fn [key]
                                 (let [{:keys [median min max]}
                                       (f/stats (frequencies (get-in metadata [true key]))
                                                :percentiles [])]
                                   [:div.row
                                    [:span.col (format-num median)]
                                    [:span.col (format-num min)]
                                    [:span.col (format-num max)]]))]
                 [:tr
                  [:th {:scope "row"} (str painting)]
                  [:td (str (+ solved-count unsolved-count))]
                  [:td (str solved-count)]
                  [:td (get-stats :polygon_rotations)]
                  [:td (get-stats :pivot_changes)]
                  [:td (get-stats :m_working)]
                  [:td (get-stats :s_thinking)]]))
             data)]]]]))

(defn toggle-btn [text]
  (ui-common/html [:button.r-button.btn.btn-sm.btn-outline-dark.rounded-pill
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

(defn side-bar-btn [chart-id]
  (doto (toggle-btn (get chart-titles chart-id "???"))
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
        checked? (contains? (-> @!state :filters :games) game-name)]
    (ui-common/html [:div.form-check.form-switch.text-center.mx-3
           (doto (ui-common/html [:input.form-check-input {:id game-key :type "checkbox"
                                                 :role "switch" :checked checked?}])
             (bs/on-click #(swap! !state update-in [:filters :games]
                                  (fn [filters]
                                    (if (contains? filters game-name)
                                      (disj filters game-name)
                                      (conj filters game-name))))))
           [:label.form-check-.ebal {:for game-key} game-name]])))

(defn players-filter [{:keys [selected min max]}]
  (cons [:div "Jugadores:"]
        (map (fn [n]
               (let [id (str "players_" n)]
                 [:div.form-check.form-check-inline
                  (doto (ui-common/html [:input.form-check-input
                               {:id id :type "checkbox"
                                :checked (contains? selected n)}])
                    (bs/on-click #(swap! !state update-in [:filters :players :selected]
                                         (fn [selected]
                                           (if (contains? selected n)
                                             (disj selected n)
                                             (conj selected n))))))
                  [:label.form-check-label {:for id} (str n)]]))
             (range min (inc max)))))

(defn local-online-filter []
  (cons [:div "Tipo de partida:"]
        (map (fn [n]
               (let [name (str/replace (str n) #"\W+" "")
                     id (str name "_checkbox")]
                 [:div.form-check.form-check-inline
                  (doto (ui-common/html [:input.form-check-input
                               {:id id :type "checkbox"
                                :checked (-> @!state :filters n)}])
                    (bs/on-click #(swap! !state update-in [:filters n] not)))
                  [:label.form-check-label {:for id} (str/capitalize name)]]))
             [:local? :online?])))

(defn platform-filter [{:keys [available selected]}]
  (cons [:div "Plataforma:"]
        (map (fn [platform]
               (let [id (str "platform_" platform "_checkbox")]
                 [:div.form-check.form-check-inline
                  (doto (ui-common/html [:input.form-check-input
                               {:id id :type "checkbox"
                                :checked (contains? selected platform)}])
                    (bs/on-click #(swap! !state update-in [:filters :platforms :selected]
                                         (fn [selected]
                                           (if (contains? selected platform)
                                             (disj selected platform)
                                             (conj selected platform))))))
                  [:label.form-check-label {:for id} platform]]))
             available)))

(defn period-filter [selected-period]
  (let [periods [:last-week :last-fortnight :last-month
                 :last-quarter :last-year :all-time]
        period-name {:last-week "Última semana"
                     :last-fortnight "Última quincena"
                     :last-month "Último mes"
                     :last-quarter "Último trimestre"
                     :last-year "Último año"
                     :all-time "Todos los tiempos"}]
    [:div.btn-group-vertical {:role :group}
     (map (fn [period]
            (doto (ui-common/html [:button.btn.btn-sm.btn-outline-dark
                         {:type "button"}
                         (period-name period)])
              (set-pressed! (= selected-period period))
              (bs/on-click #(swap! !state assoc-in [:filters :period] period))))
          periods)]))

(defn update-ui! [{:keys [data filters]}]
  (vega/finalize!)
  (doto (ui-common/get-element-by-id "summary-title")
    (oset! :textContent (->> (:visible-charts @!state)
                             (map chart-titles)
                             (str/join " + "))))
  (let [matches (:matches data)
        sessions (:sessions data)
        players (->> sessions
                     (map :pc)
                     (set))]
    (doto (ui-common/get-element-by-id "summary-label")
      (ui-common/clear!)
      (ui-common/append! [:span.col.navbar-text (str (count matches) (if (= 1 (count matches)) " partida" " partidas") " / "
                           (count sessions) (if (= 1 (count sessions)) " sesión" " sesiones") " / "
                           (count players) (if (= 1 (count players)) " jugador" " jugadores"))])
      (ui-common/append! [:span.col.navbar-text (str " (desde "
                           (or (:date (first matches)) "?")
                           " a "
                           (or (:date (last matches)) "?")
                           ")")])))
  (doto (ui-common/get-element-by-id "filters")
    (ui-common/clear!)
    (ui-common/append! [:div.offcanvas-header
              [:h5.offcanvas-title ""]
              [:button.btn-close.text-reset {:type "button" :data-bs-dismiss "offcanvas"}]]
             [:div.overflow-auto.px-2
              [:div.d-grid
               (side-bar-btn :summary)]
              [:div.d-grid.my-2
               (side-bar-btn :sessions-and-matches)]
              [:div.d-grid.my-2
               (side-bar-btn :players)]
              (when (contains? (:games data) "Dudeney's Art Gallery")
                [:div.d-grid.my-2
                 (side-bar-btn :dudeney)])
              (when (contains? (:games data) "AstroBrawl")
                [:div.d-grid.my-2
                 (side-bar-btn :astrobrawl)])
              [:hr]
              [:div
               (map game-checkbox (sort (:games data)))]
              [:hr]
              [:div.text-center
               (players-filter (:players filters))]
              [:hr]
              [:div.text-center
               (local-online-filter)]
              [:hr]
              [:div.text-center
               (platform-filter (:platforms filters))]
              [:hr]
              [:div.d-grid.my-2
               (period-filter (:period filters))]]))
  (doto (ui-common/get-element-by-id "charts")
    (ui-common/clear!)
    (ui-common/append! [:div
              (when (visible-chart? :summary)
                (summary data))
              (when (visible-chart? :sessions-and-matches)
                (sessions-and-matches data))
              (when (visible-chart? :players)
                (players data))
              (when (visible-chart? :dudeney)
                (dudeney data))
              (when (visible-chart? :astrobrawl)
                (ab/astrobrawl data))])))

(defn subtract-days-from-today [days]
  (js/Date. (- (js/Date.now)
               (* 1000 60 60 24 days))))

(defn period-min-date [period]
  (case period
    :last-year (subtract-days-from-today 365)
    :last-quarter (subtract-days-from-today 90)
    :last-month (subtract-days-from-today 30)
    :last-fortnight (subtract-days-from-today 14)
    :last-week (subtract-days-from-today 7)
    (js/Date. 0)))

(defonce filtered-data
  (memoize
   (fn [filters {:keys [sessions matches]}]
     (let [games (:games filters)
           period (let [min-date (period-min-date (:period filters))
                        max-date (js/Date.now)]
                    (fn [datetime]
                      (and (> datetime min-date)
                           (< datetime max-date))))
           players (-> filters :players :selected)
           platforms (-> filters :platforms :selected)
           local? (-> filters :local?)
           online? (-> filters :online?)
           filtered-matches (->> matches
                                 (filterv (comp games :game))
                                 (filterv (comp period :datetime))
                                 (filterv (comp players :player_count))
                                 (filterv (fn [match]
                                            (case [local? online?]
                                              [false false] false
                                              [false true] (not (:local? match))
                                              [true false] (:local? match)
                                              [true true] true)))
                                 (filterv (fn [match]
                                            (when-let [session (-> match meta :session)]
                                              (platforms (:platform session))))))
           filtered-sessions (let [valid-sessions (->> filtered-matches
                                                       (map :session)
                                                       (set))]
                               (->> sessions
                                    (filterv (comp valid-sessions :id))))]
       {:sessions filtered-sessions
        :matches filtered-matches}))))

(defn update-filters! [data]
  (let [{:keys [sessions matches]}
        (filtered-data (:filters @!state) data)]
    (swap! !state update :data
           assoc
           :sessions sessions
           :matches matches)))

(def main-container
  [:div#main-container.container-fluid
   [:div.row
    [:nav.navbar.sticky-top.navbar-expand-lg.navbar-light.bg-light
     [:div.container-fluid
      [:div.navbar-collapse
       [:div#summary-title.navbar-brand.mb-1.me-5.h1 ""]
       [:div#summary-label.col]
       [:form.d-flex
        [:button#backup-button.btn.btn-primary.me-2.d-none
         {:type "button"}
         [:i.fa-solid.fa-download.pe-2] "Backup"]
        [:button.btn.btn-primary
         {:type "button" :data-bs-toggle "offcanvas" :data-bs-target "#filters"}
         [:i.fa-solid.fa-bars]]]]]]
    [:div#charts.container]
    [:div#filters.offcanvas.offcanvas-end.px-0 {:tabindex -1}]]])

(defn download-backup! []
  (ui-common/show-wait-dialog!
   "Preparing backup..."
   (go
     (let [backup (<? (data/backup!))]
       (js/console.log backup)
       (doseq [[id data] backup]
         (let [blob (js/Blob. [data] #js {:type "text/plain;charset=utf-8"})]
           (js/saveAs blob (str id ".json") #js {:autoBom false})))))))

(defn initialize-ui! [data]
  (go
    (if (nil? data)
      (js/window.location.replace "https://www.youtube.com/watch?v=dQw4w9WgXcQ")
      (do (doto (ui-common/get-element-by-id "content")
            (ui-common/clear!)
            (ui-common/append! main-container))
          (doto (ui-common/get-element-by-id "backup-button")
            (bs/on-click download-backup!))
          (add-watch !state ::state-change
                     (fn [_ _ old new]
                       (if (not= (:filters old)
                                 (:filters new))
                         (update-filters! data)
                         (update-ui! new))))
          (swap! !state assoc
                 :data data
                 :filters (or (:filters @!state) ; Keep the old filters
                              {:games (:games data)
                               :period :all-time
                               :platforms (let [platforms (->> (:sessions data)
                                                               (map :platform)
                                                               (set))]
                                            {:available platforms
                                             :selected platforms})
                               :players (let [players (->> (:matches data)
                                                           (map :player_count)
                                                           (set))]
                                          {:selected players
                                           :min 1
                                           :max (apply max players)})
                               :local? true
                               :online? true}))
          (update-filters! data)))))

(defn clear-ui! []
  (ui-common/clear! (ui-common/get-element-by-id "content")))

(comment


  (-> @!state :filters)

  (tap> (->> (-> @!state :data :matches)
             ;(filter (comp #{"DEATHMATCH"} :mode))
             (filter (fn [{:keys [metadata]}]
                       (and ;(> (count (keys (:player_stats metadata))) 1)
                        (> (reduce + (vals (:player_level metadata ))) 0))))             
             (take 100)
             (vec)))

  (tap> (filterv (comp #{"DEATHMATCH"} :mode) matches))

  (do
    (def games (-> @!state :data :games))
    (def sessions (-> @!state :data :sessions))
    (def matches (-> @!state :data :matches)))

  (first sessions)

  ()

  (- (count matches)
     (->> matches
          (map :id)))

  (- (count sessions)
     (->> sessions
          (map (comp :original-id meta))
          (set)
          (count)))

  (-> (filter (fn [{:keys [session]}]
                (= "27272727-2727-4727-a727-272727272727.0" session))
              (-> @!state :data :matches))
      (first)
      (meta)
      :session)

  (filter (fn [{:keys [game]}]
            (= game "AstroBrawl"))
          (-> @!state :data :sessions))

  (filter (fn [session]
            (= "27272727-2727-4727-a727-272727272727"
               (:original-id (meta session))))
          (-> @!state :data :sessions))

  (type (-> @!state :data :matches))
  (-> (first matches) meta :session :pc)
  (set (map :platform sessions))
  (first matches)

  (->> (group-by :pc sessions)
       (map (fn [[pc sessions]])))

  (->> sessions
       (filter (comp #{"cd7ee4e9-f3fe-4664-a280-2a2977b40a44"} :pc)))


  (->> (group-by :pc sessions)
       (map (fn [[pc sessions]]
              {:pc pc :count (count sessions)
               ;:play-time (reduce + (map :duration_m sessions))
               ;:sessions sessions
               }))
       (sort-by - :count)
       (take 20))

       ;(frequencies)
       ;(ui-common/boxplot-stats))


  (let [users (set (map :pc sessions))])

  (keys @!state)
  (first matches)
  (first sessions)

  (def astrobrawl-sessions (filter (comp #{"AstroBrawl"} :game)
                                   sessions))
  (def astrobrawl-matches (filter (comp #{"AstroBrawl"} :game)
                                  matches))

  (count astrobrawl-matches)
  (count astrobrawl-sessions)

  (/ (count astrobrawl-sessions) (count sessions))
  (/ (count astrobrawl-matches) (count matches))
  )