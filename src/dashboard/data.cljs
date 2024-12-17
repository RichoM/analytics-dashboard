(ns dashboard.data
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [clojure.set :as set]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [utils.core :refer [pad-left]]
            [cljs.core.async.interop :refer [p->c] :refer-macros [<p!]]
            [dashboard.countries :as countries]
            [cognitect.transit :as t]
            [clojure.data :as data]))

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
                    pc ip country_code country
                    version platform match_count
                    valid?])

(defrecord Match [game code date time datetime
                  duration_ms duration_s duration_m
                  mode local? player_count session
                  over? metadata valid?])

(defprotocol DataSource
  (fetch-data! [src]))

(deftype TransitSource [url])

(def historical-sources [(TransitSource. "historical/1JFNNtlTGjFk3BJQFfSTTsQ7IuKjkdTPEcxT7MiLkRyY.json")
                         (TransitSource. "historical/1Yj79TCA0I-73SpLtBQztqNNJ8e-ANPYX5TpPLGZmqqI.json")])

(def gsheet-sources [(gs/Spreadsheet. "1JFNNtlTGjFk3BJQFfSTTsQ7IuKjkdTPEcxT7MiLkRyY")
                     (gs/Spreadsheet. "1Yj79TCA0I-73SpLtBQztqNNJ8e-ANPYX5TpPLGZmqqI")])

(defn enrich-session
  [{:keys [game date time duration_ms match_count version platform country_code ip] :as session}]
  (let [duration-ms (parse-long duration_ms)]
    (map->Session
     (assoc session
            :datetime (datetime date time)
            :duration_ms duration-ms
            :duration_s (/ duration-ms 1000)
            :duration_m (/ duration-ms 1000 60)
            :match_count (parse-long match_count)
            :country (countries/with-code country_code)
            :valid? (and (> duration-ms 0)
                         (> match_count 0)
                         (not (contains? #{"::1" "127.0.0.1" "localhost"} ip))
                         (not (str/blank? version))
                         (not (str/includes? version "DEMO"))
                         (not (str/includes? platform "Editor"))

                         ; HACK(Richo): Discard AstroBrawl v2.1.0 since it's not public yet!
                         (if (= "astrobrawl" (str/lower-case game))
                           (not (str/starts-with? version "2.1.0"))
                           true))))))

(defonce session-idgen (atom 0))

(defn deduplicate-ids 
  "Assigns new ids to all sessions, making sure no two sessions share the same id.
   When could this happen?
   The original id is kept on the session metadata under the :original-id key.
   "
  [sessions]
  (->> (group-by :id sessions)
       (mapcat (fn [[id duplicate-sessions]]
                 (map (fn [session]
                        (-> session
                            (assoc :id (swap! session-idgen inc))
                            (vary-meta assoc :original-id id)))
                      duplicate-sessions)))))

(defn get-sessions! [spreadsheet]
  (go
    (try (->> (<? (gs/get-values! spreadsheet "sessions!A:K"))
              (rows->maps)
              (map enrich-session)
              (deduplicate-ids))
         (catch :default err
           (println "Error trying to fetch sessions from spreadsheet"
                    {:spreadsheet spreadsheet :error err})
           []))))

(defn enrich-match
  [sessions-indexed
   {:keys [game session date time duration_ms local? player_count over? metadata] :as match}]
  (let [duration-ms (parse-long duration_ms)
        begin-dt (datetime date time)
        candidate-sessions (get-in sessions-indexed [game session])
        actual-session (->> candidate-sessions
                            (filter :valid?)
                            (take-while (fn [{:keys [datetime]}]
                                          (<= datetime begin-dt)))
                            (last))]
    (map->Match
     (assoc match
            :datetime begin-dt
            :duration_ms duration-ms
            :duration_s (/ duration-ms 1000)
            :duration_m (/ duration-ms 1000 60)
            :local? (= local? "TRUE")
            :player_count (parse-long player_count)
            :over? (when over? (= over? "TRUE"))
            :metadata (when metadata
                        (js->clj (js/JSON.parse metadata)
                                 :keywordize-keys true))
            :session (:id actual-session)
            :valid? (and (> duration-ms 0)
                         (some? actual-session)
                         (not (str/blank? (:version actual-session)))
                         (not (str/includes? (:version actual-session) "DEMO")))))))

(defn get-matches! [spreadsheet sessions-indexed]
  (go
    (try (->> (<? (gs/get-values! spreadsheet "matches!A:K"))
              (rows->maps)
              (mapv (partial enrich-match sessions-indexed)))
         (catch :default err
           (println "Error trying to fetch matches from spreadsheet"
                    {:spreadsheet spreadsheet :error err})
           []))))

(defn update-match-count [sessions matches]
  (let [matches-indexed (->> matches
                             (filter :valid?)
                             (group-by :session))]
    (->> sessions
         (filter :valid?)
         (map (fn [{:keys [id] :as session}]
                (let [match-count (count (get matches-indexed id []))]
                  (assoc session
                         :match_count match-count
                         :valid? (> match-count 0))))))))

(defn assoc-session [matches sessions]
  (let [sessions-indexed (->> sessions
                              (filter :valid?)
                              (group-by :id))]
    (->> matches
         (filter :valid?)
         (map (fn [{:keys [session] :as match}]
                (let [sessions (get sessions-indexed session)]
                  (assert (= 1 (count sessions))
                          "Only 1 session for each match!")
                  (vary-meta match assoc :session (first sessions))))))))

(extend-type gs/Spreadsheet
  DataSource
  (fetch-data! [spreadsheet]
    (go-try
     (let [; First we get the sessions
           sessions (<? (get-sessions! spreadsheet))

           ; Then we index the sessions by game and id
           sessions-indexed (update-vals (group-by :game sessions)
                                         (partial group-by (comp :original-id meta)))

           ; Then we can get the matches
           matches (<? (get-matches! spreadsheet sessions-indexed))

           ; Then we join them and update some of their data 
           sessions (update-match-count sessions matches)
           matches (assoc-session matches sessions)

           ; Finally, we exclude all the invalid sessions and matches
           sessions (filterv :valid? sessions)
           matches (filterv :valid? matches)]
       {:sessions sessions
        :matches matches}))))

(def write-handlers
  {Session (t/write-handler "Session" (partial into {}))
   Match (t/write-handler "Match" (partial into {}))
   countries/Country (t/write-handler "Country" (partial into {}))})

(def read-handlers
  {"Session" (t/read-handler map->Session)
   "Match" (t/read-handler map->Match)
   "Country" (t/read-handler countries/map->Country)})

(extend-type TransitSource
  DataSource
  (fetch-data! [src]
    (go-try
     (let [res (<? (p->c (js/fetch (.-url src))))
           text (<? (p->c (ocall! res :text)))
           reader (t/reader :json {:handlers read-handlers})
           {:keys [sessions matches]} (t/read reader text)
           
           ; Then we join them and update some of their data 
           sessions (update-match-count sessions matches)
           matches (assoc-session matches sessions)]
       {:sessions sessions
        :matches matches}))))

(defn backup! []
  (go (try
        (let [!result (atom {})
              writer (t/writer :json {:handlers write-handlers})
              reader (t/reader :json {:handlers read-handlers})]
          (doseq [spreadsheet gsheet-sources]
            (let [data (<? (fetch-data! spreadsheet))
                  serialized (t/write writer data)]
              (assert (= data (t/read reader serialized))
                      "Serialized data does not match!")
              (swap! !result assoc (:id spreadsheet) serialized)))
          @!result)
        (catch :default err
          (println "ERROR" err)))))


(defn get-all-data! []
  (go-try
   (<? (->> (concat historical-sources gsheet-sources)
            (map fetch-data!)
            (a/map (partial merge-with concat))))))

(defn fetch! []
  (go (try
        (let [{:keys [sessions matches]} (<? (get-all-data!))
              sessions (sort-by :datetime sessions)
              matches (sort-by :datetime matches)]
          {:games (set (map :game sessions))
           :sessions sessions
           :matches matches})
        (catch :default err
          (println "ERROR" err)))))

(comment

  (keys data)
  (first (:sessions data))

  (go (try
        (let [begin-time (js/Date.now)
              d (<? (fetch-data! (first historical-sources)))
              end-time (js/Date.now)
              elapsed-s (/ (- end-time begin-time) 1000)]
          (def data d)
          (println elapsed-s "seconds"))  
        (println "DONE")
        (catch :default err
          (println "ERROR" err))))
  
  (require '[clojure.data :refer [diff]])
  (def data_diff (atom nil))
  (go (try
        (let [writer (t/writer :json {:handlers write-handlers})
              reader (t/reader :json {:handlers read-handlers})
              data (<? (fetch-data! (first gsheet-sources)))
              serialized (t/write writer data)]
          (reset! data_diff (diff data (t/read reader serialized))))
        (println "DONE!")
        (catch :default err
          (println "ERROR" err))))

  (js/console.log @data_diff)
  (js/console.log (third @data_diff))

  (go (try (let [{:keys [sessions matches]} (<? (->> gsheet-sources
                                                     (map fetch-data!)
                                                     (a/map (partial merge-with concat))))]
             (println [(count sessions)
                       (count matches)]))
           (catch :default err
             (println "ERROR" err))))

  (def data (atom nil))

  (go (try
        (reset! data (<? (fetch-data! (first gsheet-sources))))
        (println "DONE")
        (catch :default err
          (println "ERROR" err))))

  (keys @data)

  (def writer (t/writer :json {:handlers write-handlers}))
  (def reader (t/reader :json {:handlers read-handlers}))

  (def match (first (:matches @data)))

  (meta match)
  (def match2 (t/read reader (t/write writer match)))

  (meta match2)
  (= match match2)

  (-> @data :sessions))

(defn dates-between [start end]
  (when (<= start end)
    (lazy-seq (cons (js/Date. start)
                    (dates-between (doto (js/Date. start)
                                     (.setDate (inc (.getDate start))))
                                   end)))))

(defn group-by-day [data]
  (if (empty? data)
    []
    (let [{begin :date} (first data)
          {end :date} (last data)
          grouped-data (group-by :date data)]
      (->> (dates-between (js/Date. begin)
                          (js/Date. end))
           (map #(.toISOString %))
           (map #(first (str/split % "T")))
           (map (fn [date] [date (get grouped-data date [])]))))))

(defn sessions-by-day [sessions matches]
  (if (empty? sessions)
    []
    (let [{begin :date} (first sessions)
          {end :date} (last sessions)
          sessions (group-by :date sessions)
          matches (group-by :date matches)]
      (->> (dates-between (js/Date. begin)
                          (js/Date. end))
           (map #(.toISOString %))
           (map #(first (str/split % "T")))
           (mapcat (fn [date]
                     [{:date date :type :session :count (count (sessions date))}
                      {:date date :type :match :count (count (matches date))}]))))))

(comment
  (def state @dashboard.ui/!state)


  (def sessions (filter :valid? (-> state :data :sessions)))
  (def matches (filter :valid? (-> state :data :matches)))
  (def begin (:date (first sessions)))
  (def end (:date (last sessions)))

  end)

(defn matches-per-session [sessions matches]
  (if (empty? sessions)
    []
    (let [{begin :date} (first sessions)
          {end :date} (last sessions)
          games (set (map :game sessions))
          sessions (group-by :date sessions)
          matches (group-by :session matches)]
      (->> (dates-between (js/Date. begin)
                          (js/Date. end))
           (map #(.toISOString %))
           (map #(first (str/split % "T")))
           (mapcat (fn [date]
                     (let [sessions-by-game (group-by :game (sessions date))]
                       (mapv (fn [game]
                               (let [sessions (get sessions-by-game game [])
                                     session-count (count sessions)
                                     match-count (if (zero? session-count)
                                                   0
                                                   (->> sessions
                                                        (map :id)
                                                        (map matches)
                                                        (map count)
                                                        (reduce +)))]
                                 {:game game
                                  :matches match-count
                                  :sessions session-count
                                  :matches-per-session (if (zero? session-count)
                                                         0
                                                         (/ match-count session-count))
                                  :date date}))
                             games))))))))

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
