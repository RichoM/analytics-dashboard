(ns dashboard.astrobrawl-export
  (:require [clojure.string :as str]
            [clojure.core.async :as a :refer [go <!]]
            [dashboard.ui-common :as uic]
            [utils.async :refer [go-try <?]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSV export

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
                  :play_again_different_mode
                  :play_again_different_session
                  :play_again])

(defn- csv-rows [cols matches]
  (->> matches
       (mapv (fn [match]
               (mapv #(get match % 0) cols)))))

(defn- prepare-csv-data [matches]
  (go
    (let [matches (remove (comp nil? :metadata) matches)
          chunks (partition-all 1000 matches)
          !rows (atom [(mapv #(subs (str %) 1) csv-columns)])]
      (doseq [chunk chunks]
        (<! (a/timeout 1))
        (swap! !rows concat
               (->> chunk
                    (mapcat split-metadata)
                    (map flat-metadata)
                    (map flat-session)
                    (csv-rows csv-columns))))
      (->> @!rows
           (map (partial str/join ","))
           (str/join "\n")))))

(defn download-csv! [matches]
  (uic/show-wait-dialog!
   "Preparing CSV..."
   (a/go
     (<! (a/timeout 250)) ; HACK(Richo): Give time to the modal to show
     (let [csv (<? (prepare-csv-data matches))
           blob (js/Blob. [csv] #js {:type "text/plain;charset=utf-8"})]
       (js/saveAs blob (str "astrobrawl_matches.csv") #js {:autoBom false})))))



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