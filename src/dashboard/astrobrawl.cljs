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
  (let [matches (->> matches
                     (filter (comp #{"AstroBrawl"} :game)))]    
    [:div.row.my-4
     [:div.col-auto
      (ui/title "Waves")
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
                    :scale {:type "log"}})]]))