(ns dashboard.astrobrawl
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.async :as a :refer [go <!]]
            [dashboard.vega :as vega]
            [dashboard.ui-common :as ui]
            [utils.async :refer [go-try <?]]
            [utils.core :refer [indexed-by percent seek average]]
            [dashboard.data :as data]))

(defn older-version?
  [[v1 & v1-rest] [v2 & v2-rest]]
  (cond
    (nil? v1) (> v2 0)
    (nil? v2) false
    (= v1 v2) (older-version? v1-rest v2-rest)
    :else (< v1 v2)))

(defn parse-version [s]
  (mapv parse-long (str/split s #"\D")))

(defn match-version [match]
  (-> match meta :session :version parse-version))

(defn astrobrawl [{:keys [matches sessions]}]
  (let [matches (->> matches
                     (filter (comp #{"AstroBrawl"} :game)))
        sessions (->> sessions
                      (filter (comp #{"AstroBrawl"} :game)))
        matches-by-mode (group-by :mode matches)]
    [:div.container-fluid
     [:div.row.my-4
      [:div.col-auto
       (ui/title "Tutorial success rate")
       (vega/arc :values (let [matches (->> (get matches-by-mode "TUTORIAL" [])
                                            (remove #(get-in % [:metadata :was_completed_before] false))
                                            (map :over?))
                               freq-map (frequencies matches)
                               total (count matches)]
                           (if (zero? total)
                             []
                             (map (fn [[over? count]]
                                    {:type (if over? "FINISHED" "ABANDONED")
                                     :count count
                                     :percent (percent (/ count total))})
                                  freq-map)))
                 :color {:field :type})]
      [:div.col-auto
       (ui/title "Tutorial success rate"
                 "Habiendo jugado por lo menos 10 segundos")
       (vega/arc :values (let [matches (->> (get matches-by-mode "TUTORIAL" [])
                                            (remove #(get-in % [:metadata :was_completed_before] false))
                                            (remove #(< (:duration_s %) 10))
                                            (map :over?))
                               freq-map (frequencies matches)
                               total (count matches)]
                           (if (zero? total)
                             []
                             (map (fn [[over? count]]
                                    {:type (if over? "FINISHED" "ABANDONED")
                                     :count count
                                     :percent (percent (/ count total))})
                                  freq-map)))
                 :color {:field :type})]
      [:div.col-auto
       (ui/title "¿Cuándo deciden abandonar?"
                 "Primeros 5 minutos del tutorial")
       (vega/bar :values (->> (get matches-by-mode "TUTORIAL" [])
                              (remove #(> (:duration_m %) 5))
                              (remove :over?)
                              (remove #(get-in % [:metadata :was_completed_before] false))
                              (mapv #(select-keys % [:duration_s])))
                 :height 256
                 :x {:field :duration_s
                     :bin {:binned false :step 10}
                     :title "Duración (segundos)"}
                 :y {:aggregate :count
                     :title "Cantidad de partidas"})]]
  
     [:div.row.my-4
      [:div.col-auto
       (ui/title "Duración del tutorial")
       (vega/bar :values (->> (get matches-by-mode "TUTORIAL" [])
                              (mapv (fn [{:keys [duration_m over? metadata]}]
                                      {:duration_m duration_m
                                       :type (case [over? (get metadata :was_completed_before false)]
                                               [true true] "ALREADY COMPLETE"
                                               [true false] "FINISHED 1st TIME"
                                               [false true] "ALREADY COMPLETE"
                                               [false false] "ABANDONED")})))
                 :height 256
                 :x {:field :duration_m
                     :bin {:binned false :step 1}
                     :title "Duración (minutos)"}
                 :y {:aggregate :count
                     :title "Cantidad de partidas"}
                 :color {:field :type})]
      [:div.col-auto
       (ui/title "Duración del tutorial"
                 "Habiendo jugado por lo menos 10 segundos")
       (vega/bar :values (->> (get matches-by-mode "TUTORIAL" [])
                              (remove #(< (:duration_s %) 10))
                              (mapv (fn [{:keys [duration_m over? metadata]}]
                                      {:duration_m duration_m
                                       :type (case [over? (get metadata :was_completed_before false)]
                                               [true true] "ALREADY COMPLETE"
                                               [true false] "FINISHED 1st TIME"
                                               [false true] "ALREADY COMPLETE"
                                               [false false] "ABANDONED")})))
                 :height 256
                 :x {:field :duration_m
                     :bin {:binned false :step 1}
                     :title "Duración (minutos)"}
                 :y {:aggregate :count
                     :title "Cantidad de partidas"}
                 :color {:field :type})]]
     [:div.row.my-4
      [:div.col-auto
       (ui/title "Sesiones y partidas por store")
       (vega/bar :values (concat (->> sessions
                                      (keep (fn [{:keys [tags]}]
                                              (when tags
                                                {:type "Sesiones"
                                                 :store (->> tags
                                                             (set/intersection #{:steam :itch :gplay})
                                                             (first))}))))
                                 (->> matches
                                      (keep (fn [match]
                                              (when-some [tags (-> match meta :session :tags)]
                                                {:type "Partidas"
                                                 :store (->> tags
                                                             (set/intersection #{:steam :itch :gplay})
                                                             (first))})))))
                 :height 256
                 :x {:field :store
                     :axis {:labelAngle 0}}
                 :y {:aggregate :count
                     :title "Cantidad"}
                 :xOffset {:field :type
                           :type :nominal
                           :sort ["Sesiones" "Partidas"]}
                 :color {:field :type
                         :type :nominal
                         :sort ["Sesiones" "Partidas"]})]
      [:div.col-auto
       (ui/title "Tiempo de juego total")
       (vega/bar :values (->> matches
                              (keep (fn [{:keys [duration_m] :as match}]
                                      (when-some [tags (-> match meta :session :tags)]
                                        {:duration_h (/ duration_m 60)
                                         :store (->> tags
                                                     (set/intersection #{:steam :itch :gplay})
                                                     (first))}))))
                 :width 128
                 :height 256
                 :x {:field :store
                     :axis {:labelAngle 0}}
                 :y {:field :duration_h
                     :aggregate :sum
                     :title "Horas de juego"}
                 :xOffset {:field :type
                           :type :nominal
                           :sort ["Sesiones" "Partidas"]}
                 :color {:field :store
                         :type :nominal})]
      [:div.col-auto
       (ui/title "Partidas por sesión" "(por versión, excluyendo tutorial)")
       (vega/bar :values (->> matches
                              (remove (comp #{"TUTORIAL"} :mode))
                              (group-by (fn [match]
                                          (str/join "." (take 2 (match-version match)))))
                              (mapv (fn [[version matches]]
                                      (let [session-count (->> matches
                                                               (map :session)
                                                               (set)
                                                               (count))
                                            match-count (count matches)]
                                        {:version version
                                         :sessions session-count
                                         :matches match-count
                                         :ratio (/ match-count session-count)}))))
                 :width 192
                 :x {:field :version}
                 :y {:field :ratio}
                 :color {:field :version})]]
  
     [:div.row.my-4
      [:div.col-auto
       (ui/title [:span "Jugadores nuevos vs " ui/recurrentes]
                 "(por store)")
       (vega/bar :values (->> sessions
                              (remove #(nil? (:tags %)))
                              (group-by (fn [{:keys [tags]}]
                                          (->> tags
                                               (set/intersection #{:steam :itch :gplay})
                                               (first))))
                              (mapcat (fn [[store sessions]]
                                        (let [pcs (group-by :pc sessions)
                                              freq-map (->> pcs
                                                            (map (fn [[_pc sessions]]
                                                                   (let [dates (set (map :date sessions))]
                                                                     (count dates))))
                                                            (frequencies))
                                              total (count pcs)
                                              new (get freq-map 1 0)
                                              returning (reduce + (vals (dissoc freq-map 1)))]
                                          [{:store store :type :new :count new
                                            :percent (percent (/ new total))}
                                           {:store store :type :returning :count returning
                                            :percent (percent (/ returning total))}]))))
                 :y {:field :store
                     :type :nominal}
                 :x {:field :count
                     :type :quantitative
                     :stack :normalize
                     :axis {:format "%"}}
                 :color {:field :type}
                 :width 512
                 :height 192)]
      [:div.col-auto
       (ui/title [:span "Jugadores nuevos vs " ui/recurrentes]
                 "(por versión)")
       (vega/bar :values (->> sessions
                              (group-by (fn [{:keys [version]}]
                                          (str/join "." (take 2 (parse-version version)))))
                              (mapcat (fn [[version sessions]]
                                        (let [pcs (group-by :pc sessions)
                                              freq-map (->> pcs
                                                            (map (fn [[_pc sessions]]
                                                                   (let [dates (set (map :date sessions))]
                                                                     (count dates))))
                                                            (frequencies))
                                              total (count pcs)
                                              new (get freq-map 1 0)
                                              returning (reduce + (vals (dissoc freq-map 1)))]
                                          [{:version version :type :new :count new
                                            :percent (percent (/ new total))}
                                           {:version version :type :returning :count returning
                                            :percent (percent (/ returning total))}]))))
                 :y {:field :version
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
       (ui/title "Duración de la sesión" "(por versión)")
       (vega/boxplot :values (->> sessions
                                  (group-by (fn [{:keys [version]}]
                                              {:version (str/join "." (take 2 (parse-version version)))}))
                                  (map (fn [[{:keys [version]} sessions]]
                                         (assoc (ui/boxplot-stats (map :duration_m sessions))
                                                :version version))))
                     :width 256
                     :x {:field :version
                         :title "Versión"}
                     :y {:title "Duración (minutos)"}
                     :color {:field :version
                             :title "Versión"})]

      [:div.col-auto
       (ui/title "Duración de las partidas" "(por modo de juego y versión)")
       (vega/boxplot :values (->> matches
                                  (group-by (fn [match]
                                              {:version (str/join "." (take 2 (match-version match)))
                                               :mode (:mode match)}))
                                  (map (fn [[{:keys [version mode]} matches]]
                                         (assoc (ui/boxplot-stats (map :duration_m matches))
                                                :version version
                                                :mode mode))))
                     :width 512
                     :x {:field :mode
                         :title "Tipo de partida"
                         :axis {:labelAngle 0}
                         :sort {:field :version}}
                     :y {:title "Duración (minutos)"}
                     :xOffset {:field :version
                               :type :nominal}
                     :color {:field :version
                             :title "Versión"})]]
     [:div.row.my-4
      [:div.col-auto
       (ui/title "Survival difficulty")
       (vega/bar :values (->> matches
                              (filter (comp #{"SURVIVAL"} :mode))
                              (keep :metadata)
                              (map :round_idx)
                              (frequencies)
                              (mapv (fn [[round_idx times]]
                                      {:wave round_idx :count times})))
                 :height 256
                 :x {:field :wave
                     :bin true
                     :title "Wave alcanzada"}
                 :y {:field :count
                     :aggregate "sum"
                     :title "Cantidad de partidas"
                     :scale {:type "log"}})]]]))

(def upgrade-names 
  (->> ["PlayerSpeed"
        "JumpSpeed"
        "AimingSpeed"
        "ArrowSpeed"
        "RocketSpeed"
        "StrongerDash"
        "StrongerShield"
        "LongerStar"
        "LongerInvisibility"
        "LongerBoots"
        "LongerShield"
        "ExtraRockets"]
       (map #(str/replace % #"([A-Z]+)([A-Z][a-z])", "$1_$2"))
       (map #(str/replace % #"([a-z])([A-Z])", "$1_$2"))
       (map str/lower-case)))


(defn- flat-upgrades [metadata]
  (let [upgrades (-> metadata :player_upgrades)]
    (reduce-kv (fn [match key value]
                 (assoc match
                        (keyword (str "player_upgrade_" key))
                        value))
               metadata
               upgrades)))

(defn- flat-stats [metadata]
  (let [stats (-> metadata :player_stats)]
    (reduce-kv (fn [match key value]
                 (assoc match
                        (keyword (str/replace (str "player_stat" key)
                                              ":" "_"))
                        value))
               metadata
               stats)))

(defn- split-metadata
  [{:keys [mode metadata] :as match}]
  (let [player-ids (->> [:player_level :player_upgrades :player_stats :player_team]
                        (map metadata)
                        (map keys)
                        (reduce into #{}))
        end_round (max 1
                       (if (= "SURVIVAL" mode)
                         (get metadata :round_idx)
                         (inc (get metadata :round_idx 0))))
        begin_round (min end_round
                         (->> (get metadata :round_duration)
                              (take-while zero?)
                              (count)
                              (inc)))
        total_rounds (max 1 (inc (- end_round begin_round)))
        practice_score (get metadata :score 0)]
    (->> player-ids
         (map (fn [id]
                {:player_level (get-in metadata [:player_level id])
                 :player_upgrades (->> (get-in metadata [:player_upgrades id] [])
                                       (keep #(nth upgrade-names % nil))
                                       (frequencies))
                 :player_stats (get-in metadata [:player_stats id])
                 :player_team (get-in metadata [:player_team id])
                 :practice_score practice_score
                 :rounds_won (case mode
                               "SURVIVAL" (max 0 ; HACK(Richo)
                                               (- (get metadata :round_idx)
                                                  (->> (get metadata :round_duration)
                                                       (take-while zero?)
                                                       (count))
                                                  1))
                               (count (filter #(= (keyword (str %)) id)
                                              (get metadata :round_winner []))))
                 :begin_round begin_round
                 :end_round end_round
                 :rounds_total total_rounds}))
         (map flat-upgrades)
         (map flat-stats)
         (map (fn [metadata]
                (assoc match :metadata metadata))))))

(defn- flat-session [match]
  (let [session (-> match meta :session)]
    (reduce-kv (fn [match key value]
                 (assoc match 
                        (keyword (str/replace (str "session" key) ":" "_"))
                        value))
               match
               session)))

(defn- flat-metadata [match]
  (let [metadata (-> match :metadata)]
    (reduce-kv (fn [match key value]
                 (assoc match
                        (keyword (str/replace (str "metadata" key) ":" "_"))
                        value))
               match
               metadata)))

(def csv-columns [:date
                  :time
                  :mode
                  :code
                  :player_count
                  :local?
                  :over?
                  :duration_m

                  :session_id
                  :session_pc
                  :session_platform
                  :session_duration_m
                  :session_version
                  :session_match_count
                  :session_country_code
                  :session_ip

                  :metadata_practice_score
                  :metadata_rounds_total
                  :metadata_rounds_won
                  :metadata_begin_round
                  :metadata_end_round
                  :metadata_player_level
                  :metadata_player_team

                  :metadata_player_stat_shots
                  :metadata_player_stat_jumps
                  :metadata_player_stat_dashes
                  :metadata_player_stat_stomps
                  :metadata_player_stat_parries
                  :metadata_player_stat_stuns

                  :metadata_player_stat_shot_arrows
                  :metadata_player_stat_shot_toy_arrows
                  :metadata_player_stat_shot_rockets
                  :metadata_player_stat_shot_arrows_3x
                  :metadata_player_stat_shot_toy_arrows_3x
                  :metadata_player_stat_shot_rockets_3x

                  :metadata_player_stat_deaths
                  :metadata_player_stat_suicides

                  :metadata_player_stat_kills
                  :metadata_player_stat_kills_shot
                  :metadata_player_stat_kills_stomp
                  :metadata_player_stat_kills_dash
                  :metadata_player_stat_kills_star

                  :metadata_player_stat_collected_hearts
                  :metadata_player_stat_collected_rockets
                  :metadata_player_stat_collected_toy_arrows
                  :metadata_player_stat_collected_boots
                  :metadata_player_stat_collected_invisibility
                  :metadata_player_stat_collected_shields
                  :metadata_player_stat_collected_3x
                  :metadata_player_stat_collected_stars
                  :metadata_player_stat_collected_pows
                  :metadata_player_stat_collected_chameleon

                  :metadata_player_stat_frames_idle
                  :metadata_player_stat_frames_running
                  :metadata_player_stat_frames_jumping
                  :metadata_player_stat_frames_aiming
                  :metadata_player_stat_frames_drawing

                  :metadata_player_stat_frames_invisible
                  :metadata_player_stat_frames_shield
                  :metadata_player_stat_frames_stunned
                  :metadata_player_stat_frames_boots
                  :metadata_player_stat_frames_star
                  :metadata_player_stat_frames_ghost
                  :metadata_player_stat_frames_chameleon

                  :metadata_player_upgrade_player_speed
                  :metadata_player_upgrade_jump_speed
                  :metadata_player_upgrade_aiming_speed
                  :metadata_player_upgrade_arrow_speed
                  :metadata_player_upgrade_rocket_speed
                  :metadata_player_upgrade_stronger_dash
                  :metadata_player_upgrade_stronger_shield
                  :metadata_player_upgrade_longer_star
                  :metadata_player_upgrade_longer_invisibility
                  :metadata_player_upgrade_longer_boots
                  :metadata_player_upgrade_longer_shield
                  :metadata_player_upgrade_extra_rockets
                  
                  :play_again_same_mode
                  :play_again_different_mode])

(defn- csv-rows [cols matches]
  (->> matches
       (mapv (fn [match]
               (mapv #(get match % 0) cols)))))

(defn- assoc-play-again 
  [matches-by-session {:keys [session mode] :as match}]
  (let [related-matches (get matches-by-session session [])
        next-matches (->> related-matches
                          (drop-while #(not= % match))
                          (drop 1)
                          (vec))]
    (assoc match
           :play_again_same_mode
           (or (->> next-matches
                    (some #(= mode (:mode %))))
               false)
           
           :play_again_different_mode
           (or (->> next-matches
                    (some #(not= mode (:mode %))))
               false))))

(defn- prepare-csv-data [{:keys [matches]}]
  (go
    (let [matches (->> matches
                       (filter (comp #{"AstroBrawl"} :game))
                       (remove (comp nil? :metadata))) 
          matches-by-session (group-by :session matches)
          chunks (partition-all 2500 matches)
          !rows (atom [(mapv #(subs (str %) 1) csv-columns)])]
      (doseq [chunk chunks]
        (<! (a/timeout 1))
        (swap! !rows concat
               (->> chunk
                    (map (partial assoc-play-again matches-by-session))
                    (mapcat split-metadata)
                    (map flat-metadata)
                    (map flat-session)
                    (csv-rows csv-columns))))
      (->> @!rows
           (map (partial str/join ","))
           (str/join "\n")))))

(defn download-csv! [data]
  (ui/show-wait-dialog!
   "Preparing CSV..."
   (a/go
     (let [csv (<? (prepare-csv-data data))]
       (js/console.log csv)
       (let [blob (js/Blob. [csv] #js {:type "text/plain;charset=utf-8"})]
         (js/saveAs blob (str "astrobrawl_matches.csv") #js {:autoBom false}))))))



(comment

  (let [state @dashboard.ui/!state]
    (def games (-> state :data :games))
    (def sessions (-> state :data :sessions))
    (def matches (-> state :data :matches))
    (defn without-meta [o]
      (with-meta o nil))
    (def matches-by-mode (group-by :mode matches)))

  ;[player_level player_upgrades player_stats player_team
  ; round_idx round_winner round_duration score error was_completed_before]

  (def match (->> matches
                  (filter (comp #{"AstroBrawl"} :game))
                  (remove (comp nil? :metadata))
                          ;(mapcat split-metadata)
                          ;(map flat-metadata)
                          ;(map flat-session)
                          ;(csv-rows csv-columns)
                          ;(map (partial str/join ","))
                          ;(str/join "\n")
                  (filter (comp #{"SURVIVAL"} :mode))
                  second))
  (def mode (:mode match))
  (def metadata (:metadata match))
  
  
  (tap> match)

  (frequencies ["R" 3 3 1 1 2 5 5 5 5 5])

  ;; metadata keys by game mode
  (tap> (->> matches
             (filter (comp #{"AstroBrawl"} :game))
             (remove (comp nil? :metadata))
             (group-by :mode)
             (map (fn [[mode matches]]
                    [mode
                     (->> matches
                          (map :metadata)
                          (mapcat keys)
                          (set))]))
             (into {})))


  )