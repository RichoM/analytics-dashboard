(ns dashboard.astrobrawl
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [dashboard.vega :as vega]
            [dashboard.ui-common :as ui]
            [utils.core :refer [indexed-by percent seek average]]))

(comment
  (let [state @dashboard.ui/!state]
    (def games (-> state :data :games))
    (def sessions (-> state :data :sessions))
    (def matches (-> state :data :matches))
    (defn without-meta [o]
      (with-meta o nil))
    (def matches-by-mode (group-by :mode matches)))
  
  (-> (first matches) meta :session)
  (tap> (->> sessions
             (remove #(nil? (:tags %)))
             (mapv without-meta)
             ))
  
  (tap> (->> (get matches-by-mode "TUTORIAL" [])
             (remove #(get-in % [:metadata :was_completed_before] false))
             (map without-meta)))
  (tap> (->> matches
             (filter (comp #{"AstroBrawl"} :game))
             (map without-meta)
             (group-by :mode)))
  (tap> (->> matches
             (filter (comp #{"TUTORIAL"} :mode))
             (map (fn [{:keys [duration_m over? metadata]}]
                    {:duration_m duration_m
                     :type (case [over? (get metadata :was_completed_before false)]
                             [true true] "COMPLETED - ALREADY COMPLETE"
                             [true false] "COMPLETED 1st TIME"
                             [false true] "ABANDONED - ALREADY COMPLETE"
                             [false false] "ABANDONED 1st TIME")}))
             (mapv without-meta)))
  (cond 
    (contains? #{:steam} :itch) "itch.io"
    (contains? #{:steam} :steam) "steam")
  )

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
    [:div.row
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
                         :type :nominal})]]
  
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
                             :title "Versión"})]
      [:div.row.my-4
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
                 :color {:field :version})]
      #_[:div.row.my-4
         (ui/title "Partidas por sesión" "(por versión, excluyendo tutorial)")
         (vega/bar :values (let [matches-by-session (group-by :session matches)]
                             (->> sessions
                                  (group-by (fn [session]
                                              (str/join "." (take 2 (parse-version (:version session))))))
                                  (mapv (fn [[version sessions]]
                                          (map (fn [session]
                                                 (let [matches (get matches-by-session (:id session) [])
                                                       ratio])))))))
                   :width 192
                   :x {:field :version}
                   :y {:field :ratio}
                   :color {:field :version})]]
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
