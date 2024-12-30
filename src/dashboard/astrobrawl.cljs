(ns dashboard.astrobrawl
  (:require [clojure.string :as str]
            [dashboard.vega :as vega]
            [dashboard.ui-common :as ui]
            [utils.core :refer [indexed-by percent seek average]]))

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
  (let [pre-2_1-matches (->> matches
                             (filter (comp #{"AstroBrawl"} :game))
                             (filter #(older-version? (match-version %) [2 1])))
        post-2_1-matches (->> matches
                              (filter (comp #{"AstroBrawl"} :game))
                              (remove #(older-version? (match-version %) [2 1])))
        pre-2_1-sessions (->> pre-2_1-matches
                              (map (comp :session meta))
                              (set))
        post-2_1-sessions (->> post-2_1-matches
                               (map (comp :session meta))
                               (set))]
    [:div.row.my-4
     [:div.col-auto
      (ui/title "Partidas")
      (vega/bar :values [{:version "< 2.1" :count (count pre-2_1-matches)}
                         {:version "> 2.1" :count (count post-2_1-matches)}]
                :width 150
                :height 256
                :x {:field :version
                    :title "Versión"
                    :axis {:labelAngle 0}}
                :y {:field :count
                    :title "Cantidad"}
                :color {:field :version})]

     [:div.col-auto
      (ui/title "Duración de las sesiones")
      (vega/boxplot :values [(assoc (ui/boxplot-stats (map :duration_m pre-2_1-sessions))
                                    :version "< 2.1")
                             (assoc (ui/boxplot-stats (map :duration_m post-2_1-sessions))
                                    :version "> 2.1")]
                    :width 150
                    :height 256
                    :x {:field :version
                        :title "Versión"}
                    :y {:title "Duración (minutos)"}
                    :color {:field :version})]

     [:div.col-auto
      (ui/title "Partidas por sesión (promedio)")
      (vega/bar :values [{:version "< 2.1" :count (average (mapv :match_count pre-2_1-sessions))}
                         {:version "> 2.1" :count (average (mapv :match_count post-2_1-sessions))}]
                :width 150
                :height 256
                :x {:field :version
                    :title "Versión"
                    :axis {:labelAngle 0}}
                :y {:field :count
                    :title "Cantidad"}
                :color {:field :version})]
     [:div.col-auto
      (ui/title "Waves")
      (vega/bar :values (->> matches
                             (filter (comp #{"AstroBrawl"} :game))
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
                    :scale {:type "log"}})]]))