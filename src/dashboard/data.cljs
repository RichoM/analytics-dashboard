(ns dashboard.data
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [utils.core :refer [pad-left indexed-by]]))

(defn rows->maps [rows]
  (let [columns (mapv keyword (first rows))]
    (map #(zipmap columns %)
         (rest rows))))

(defn datetime [date time]
  (let [[h m s] (str/split time #"[:.]")]
    (js/Date. (str date "T"
                   (pad-left h 2 "0") ":"
                   (pad-left m 2 "0") ":"
                   (pad-left s 2 "0") ".000Z"))))

(defrecord Session [id game date time datetime
                    duration_ms duration_s duration_m
                    pc ip country_code
                    version platform match_count
                    valid?])

(defrecord Match [game code date time datetime
                  duration_ms duration_s duration_m
                  mode local? player_count session])

(defn enrich-session
  [{:keys [date time duration_ms match_count version] :as session}]
  (let [duration-ms (parse-long duration_ms)]
    (assoc session
           :datetime (datetime date time)
           :duration_ms duration-ms
           :duration_s (/ duration-ms 1000)
           :duration_m (/ duration-ms 1000 60)
           :match_count (parse-long match_count)
           :valid? (and (> duration-ms 0)
                        (not (str/blank? version))
                        (not (str/includes? version "DEMO"))))))

(defn enrich-match 
  [{:keys [date time duration_ms local? player_count] :as match}]
  (let [duration-ms (parse-long duration_ms)]
    (assoc match
           :datetime (datetime date time)
           :duration_ms duration-ms
           :duration_s (/ duration-ms 1000)
           :duration_m (/ duration-ms 1000 60)
           :local? (= local? "TRUE")
           :player_count (parse-long player_count))))

(defn fetch! [spreadsheet]
  (go (try
        (let [sessions (<? (gs/get-values! spreadsheet "sessions!A:K"))
              matches (<? (gs/get-values! spreadsheet "matches!A:I"))]
          {:sessions (->> (rows->maps sessions)
                          (map enrich-session)
                          (mapv map->Session))
           :matches (->> (rows->maps matches)
                         (map enrich-match)
                         (mapv map->Match))})
        (catch :default err
          (println "ERROR" err)))))

(defn dates-between [start end]
  (when (<= start end)
    (lazy-seq (cons (js/Date. start)
                    (dates-between (doto (js/Date. start)
                                     (.setDate (inc (.getDate start))))
                                   end)))))

(defn sessions-by-day [sessions]
  (let [{begin :datetime} (first sessions)
        {end :datetime} (last sessions)
        sessions (group-by :date sessions)]
    (->> (dates-between begin end)
         (map #(.toISOString %))
         (map #(first (str/split % "T")))
         (mapv (fn [date]
                 {:date date :sessions (count (sessions date))})))))