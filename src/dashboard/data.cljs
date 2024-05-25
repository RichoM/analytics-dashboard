(ns dashboard.data
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [clojure.set :as set]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [utils.core :refer [pad-left]]))

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
                  mode local? player_count session
                  valid?])

(def data-sources [(gs/Spreadsheet. "1JFNNtlTGjFk3BJQFfSTTsQ7IuKjkdTPEcxT7MiLkRyY")
                   (gs/Spreadsheet. "1Yj79TCA0I-73SpLtBQztqNNJ8e-ANPYX5TpPLGZmqqI")])

(defn enrich-session
  [{:keys [date time duration_ms match_count version] :as session}]
  (let [duration-ms (parse-long duration_ms)]
    (map->Session
     (assoc session
            :datetime (datetime date time)
            :duration_ms duration-ms
            :duration_s (/ duration-ms 1000)
            :duration_m (/ duration-ms 1000 60)
            :match_count (parse-long match_count)
            :valid? (and (> duration-ms 0)
                         (not (str/blank? version))
                         (not (str/includes? version "DEMO")))))))

(defn get-sessions! [spreadsheet]
  (go
    (try (->> (<? (gs/get-values! spreadsheet "sessions!A:K"))
              (rows->maps)
              (mapv enrich-session))
         (catch :default err
           (println "Error trying to fetch sessions from spreadsheet" 
                    {:spreadsheet spreadsheet :error err})
           []))))

(defn deduplicate-ids [sessions]
  (->> (group-by :id sessions)
       (mapcat (fn [[id duplicate-sessions]]
                 (map-indexed (fn [idx session]
                                (-> session
                                    (assoc :id (str id "." idx))
                                    (vary-meta assoc :original-id id)))
                              duplicate-sessions)))))

(defn get-all-sessions! []
  (go-try
   (->> (<? (->> data-sources
                 (map get-sessions!)
                 (a/map concat)))
        (deduplicate-ids)
        (sort-by :datetime))))

(defn enrich-match
  [sessions-indexed {:keys [game session date time duration_ms local? player_count] :as match}]
  (let [duration-ms (parse-long duration_ms)
        begin-dt (datetime date time)
        candidate-sessions (get-in sessions-indexed [game session])
        actual-session (->> candidate-sessions
                            (filter :valid?)
                            (take-while (fn [{:keys [datetime]}] 
                                          (<= datetime begin-dt)))
                            (last))]
    (-> match
        (assoc :datetime begin-dt
               :duration_ms duration-ms
               :duration_s (/ duration-ms 1000)
               :duration_m (/ duration-ms 1000 60)
               :local? (= local? "TRUE")
               :player_count (parse-long player_count)
               :session (:id actual-session)
               :valid? (and (> duration-ms 0)
                            (some? actual-session)
                            (not (str/blank? (:version actual-session)))
                            (not (str/includes? (:version actual-session) "DEMO"))))
        (map->Match)
        (vary-meta assoc :session actual-session))))

(defn get-matches! [spreadsheet sessions-indexed]
  (go 
    (try (->> (<? (gs/get-values! spreadsheet "matches!A:I"))
               (rows->maps)
               (mapv (partial enrich-match sessions-indexed)))
         (catch :default err
           (println "Error trying to fetch matches from spreadsheet"
                    {:spreadsheet spreadsheet :error err})
           []))))

(defn get-all-matches! [sessions]
  (go-try
   (let [sessions-indexed (update-vals (group-by :game sessions)
                                       (partial group-by (comp :original-id meta)))]
     (sort-by :datetime
              (<? (->> data-sources
                       (map #(get-matches! % sessions-indexed))
                       (a/map concat)))))))

(defn fetch! []
  (go (try
        (let [sessions (<? (get-all-sessions!))
              matches (<? (get-all-matches! sessions))]
          {:games (set (map :game sessions))
           :sessions sessions
           :matches matches})
        (catch :default err
          (println "ERROR" err)))))

(comment

  (def data (atom nil))

  (keys @data)

  (first (-> @data :sessions))

  (first (-> @data :matches))

  (go (reset! data (<! (fetch!))))
  
  
  )

(defn dates-between [start end]
  (when (<= start end)
    (lazy-seq (cons (js/Date. start)
                    (dates-between (doto (js/Date. start)
                                     (.setDate (inc (.getDate start))))
                                   end)))))

(defn group-by-day [data]
  (if (empty? data)
    []
    (let [{begin :datetime} (first data)
          {end :datetime} (last data)
          grouped-data (group-by :date data)]
      (->> (dates-between begin end)
           (map #(.toISOString %))
           (map #(first (str/split % "T")))
           (map (fn [date] [date (get grouped-data date [])]))))))

(defn sessions-by-day [sessions matches]
  (if (empty? sessions)
    []
    (let [{begin :datetime} (first sessions)
          {end :datetime} (last sessions)
          sessions (group-by :date sessions)
          matches (group-by :date matches)]
      (->> (dates-between begin end)
           (map #(.toISOString %))
           (map #(first (str/split % "T")))
           (mapcat (fn [date]
                     [{:date date :type :session :count (count (sessions date))}
                      {:date date :type :match :count (count (matches date))}]))))))

(defn matches-per-session [sessions matches]
  (if (empty? sessions)
    []
    (let [{begin :datetime} (first sessions)
          {end :datetime} (last sessions)
          sessions (group-by :date sessions)
          matches (group-by :session matches)]
      (->> (dates-between begin end)
           (map #(.toISOString %))
           (map #(first (str/split % "T")))
           (mapcat (fn [date]
                     (mapv (fn [[game sessions]]
                             (let [match-count (->> sessions
                                                    (map :id)
                                                    (map matches)
                                                    (map count)
                                                    (reduce +))]
                               {:game game
                                :matches match-count
                                :sessions (count sessions)
                                :matches-per-session (/ match-count
                                                        (count sessions))
                                :date date}))
                           (group-by :game (sessions date)))))))))

(defn player-count-by-day [sessions]
  (let [sessions-by-day (group-by-day sessions)
        players-by-day (volatile! [])
        returning-players (volatile! #{})]
    (doseq [[date sessions] sessions-by-day]
      (let [pcs (set (map :pc sessions))
            total-count (count pcs)
            new-pcs (remove @returning-players pcs)
            new-count (count new-pcs)
            returning-count (- total-count new-count)]
        (vswap! players-by-day conj
                {:date date
                 :total total-count
                 :new new-count
                 :returning returning-count})
        (vswap! returning-players set/union pcs)))
    @players-by-day))