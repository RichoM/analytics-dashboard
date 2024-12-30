(ns dashboard.astrobrawl
  (:require [clojure.string :as str]
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
  
  (+ 677 151 39)
  (+ 273 490 122)
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

(defn astrobrawl [{:keys [matches]}]
  (let [matches (->> matches
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
                           (map (fn [[over? count]]
                                  {:type (if over? "FINISHED" "ABANDONED")
                                   :count count
                                   :percent (percent (/ count total))})
                                freq-map))
                 :color {:field :type})]
      [:div.col-auto
       (ui/title "Tutorial success rate"
                 "Habiendo jugado por lo menos 5 segundos")
       (vega/arc :values (let [matches (->> (get matches-by-mode "TUTORIAL" [])
                                            (remove #(get-in % [:metadata :was_completed_before] false))
                                            (remove #(< (:duration_s %) 5))
                                            (map :over?))
                               freq-map (frequencies matches)
                               total (count matches)]
                           (map (fn [[over? count]]
                                  {:type (if over? "FINISHED" "ABANDONED")
                                   :count count
                                   :percent (percent (/ count total))})
                                freq-map))
                 :color {:field :type})]
      [:div.col-auto
       (ui/title "¿Cuándo deciden abandonar?"
                 "Primeros 10 segundos del tutorial")
       (vega/bar :values (->> (get matches-by-mode "TUTORIAL" [])
                              (remove #(> (:duration_s %) 10))
                              (remove :over?)
                              (remove #(get-in % [:metadata :was_completed_before] false))
                              (map #(select-keys % [:duration_s])))
                 :height 256
                 :x {:field :duration_s
                     :bin {:binned false :step 1}
                     :title "Duración (segundos)"}
                 :y {:aggregate :count
                     :title "Cantidad de partidas"}
                 )]]
     
     [:div.row.my-4
      [:div.col-auto
       (ui/title "Duración del tutorial")
       (vega/bar :values (->> (get matches-by-mode "TUTORIAL" [])
                              (map (fn [{:keys [duration_m over? metadata]}]
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
                 "Habiendo jugado por lo menos 5 segundos")
       (vega/bar :values (->> (get matches-by-mode "TUTORIAL" [])
                              (remove #(< (:duration_s %) 5))
                              (map (fn [{:keys [duration_m over? metadata]}]
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
