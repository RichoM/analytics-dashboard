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
                  mode local? player_count session
                  valid?])

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
  [sessions-by-id {:keys [session date time duration_ms local? player_count] :as match}]
  (let [{:keys [version] :as actual-session} (sessions-by-id session)
        duration-ms (parse-long duration_ms)]
    (assoc (with-meta match
             {:session actual-session})
           :datetime (datetime date time)
           :duration_ms duration-ms
           :duration_s (/ duration-ms 1000)
           :duration_m (/ duration-ms 1000 60)
           :local? (= local? "TRUE")
           :player_count (parse-long player_count)
           :valid? (and (> duration-ms 0)
                        (not (str/blank? version))
                        (not (str/includes? version "DEMO"))))))

(def data-sources [(gs/Spreadsheet. "1JFNNtlTGjFk3BJQFfSTTsQ7IuKjkdTPEcxT7MiLkRyY")
                   (gs/Spreadsheet. "1Yj79TCA0I-73SpLtBQztqNNJ8e-ANPYX5TpPLGZmqqI")])

(defn get-sessions! [spreadsheet]
  (go-try (->> (<? (gs/get-values! spreadsheet "sessions!A:K"))
               (rows->maps)
               (mapv enrich-session))))

(defn get-all-sessions! []
  (go-try
   (sort-by :datetime
            (<? (->> data-sources
                     (map get-sessions!)
                     (a/map concat))))))

(defn get-matches! [spreadsheet sessions]
  (go-try (->> (<? (gs/get-values! spreadsheet "matches!A:I"))
               (rows->maps)
               (mapv (partial enrich-match sessions)))))

(defn get-all-matches! [sessions]
  (go-try
   (let [sessions (indexed-by :id sessions)]
     (sort-by :datetime
              (<? (->> data-sources
                       (map #(get-matches! % sessions))
                       (a/map concat)))))))

(comment
  
  (def all-sessions (atom nil))

  (count @all-sessions)

  (set (map :game @all-sessions))
  
  (go (reset! all-sessions (<! (get-all-sessions!))))
  
  )

(defn fetch! [spreadsheet]
  (go (try
        (let [sessions (<? (get-all-sessions!))
              matches (<? (get-all-matches! sessions))]
          {:sessions sessions
           :matches matches})
        (catch :default err
          (println "ERROR" err)))))

(defn dates-between [start end]
  (when (<= start end)
    (lazy-seq (cons (js/Date. start)
                    (dates-between (doto (js/Date. start)
                                     (.setDate (inc (.getDate start))))
                                   end)))))

(defn sessions-by-day [sessions matches]
  (let [{begin :datetime} (first sessions)
        {end :datetime} (last sessions)
        sessions (group-by :date sessions)
        matches (group-by :date matches)]
    (->> (dates-between begin end)
         (map #(.toISOString %))
         (map #(first (str/split % "T")))
         (mapcat (fn [date]
                 [{:date date :type :session :count (count (sessions date))}
                  {:date date :type :match :count (count (matches date))}])))))