(ns dashboard.play-time
  (:require [dashboard.ui-common :as uic]
            [dashboard.vega :as vega]
            [dashboard.countries :as countries]
            [utils.core :refer [indexed-by percent seek average pad-left]]))

(defn timestamp-to-bucket-idx [t bucket-size]
  (let [h (.getUTCHours t)
        m (.getUTCMinutes t)
        s (.getUTCSeconds t)
        seconds-in-day (+ (* h 60 60)
                          (* m 60)
                          s)
        minutes-in-day (/ seconds-in-day 60)]
    (int (/ minutes-in-day bucket-size))))

(defn bucket-idx-to-timestamp
  ([idx bucket-size] (bucket-idx-to-timestamp idx bucket-size (js/Date.)))
  ([idx bucket-size dt]
   (let [minutes-in-day (* bucket-size idx)
         s 0
         m (mod minutes-in-day 60)
         h (int (/ minutes-in-day 60))]
     (doto (js/Date. dt)
       (.setUTCHours h)
       (.setUTCMinutes m)
       (.setUTCSeconds s)))))

(defn add-seconds [dt s]
  (let [result (js/Date. dt)]
    (doto result
      (.setMilliseconds (+ (.getMilliseconds result)
                           (* s 1000))))))

(defn add-to-bucket! [buckets bucket-size dt duration-s]
  (let [begin-time dt
        end-time (add-seconds dt duration-s)
        begin-idx (timestamp-to-bucket-idx begin-time bucket-size)
        end-idx (timestamp-to-bucket-idx end-time bucket-size)]
    (if (= begin-idx end-idx)
      (swap! buckets update begin-idx + duration-s)
      (let [begin-time-bucket (bucket-idx-to-timestamp begin-idx bucket-size dt)
            seconds-inside-bucket (- (* bucket-size 60)
                                     (/ (- dt begin-time-bucket)
                                        1000.0))]
        (swap! buckets update begin-idx + seconds-inside-bucket)
        (add-to-bucket! buckets
                        bucket-size
                        (add-seconds dt seconds-inside-bucket)
                        (- duration-s seconds-inside-bucket))))))

(defn collect-buckets [matches bucket-size]
  (let [buckets (atom {})]
    (doseq [{:keys [datetime duration_s] :as match} matches]
      (let [country (-> match meta :session :country)
            country-dt (add-seconds datetime (:delta-s country))]
        (add-to-bucket! buckets bucket-size country-dt duration_s)))
    (->> (range (/ (* 24 60) bucket-size))
         (map (fn [idx]
                (let [minutes-in-day (* 15 idx)
                      m (int (mod minutes-in-day 60))
                      h (int (/ minutes-in-day 60))]
                  {:idx idx
                   :time (str (pad-left (str h) 2 "0")
                              ":"
                              (pad-left (str m) 2 "0"))
                   :seconds (get @buckets idx 0)}))))))

(defn play-time [{:keys [sessions matches]}]
  [:div.container-fluid
   [:div.row.my-4
    [:div.col-auto
     (uic/title "Duración de las sesiones")
     (vega/boxplot :values (->> sessions
                                (group-by :game)
                                (map (fn [[game sessions]]
                                       (assoc (uic/boxplot-stats (map :duration_m sessions))
                                              :game game))))
                   :width 256
                   :x {:field :game}
                   :y {:title "Duración (minutos)"}
                   :color {:field :game :title "Juego"})]

    [:div.col-auto
     (uic/title "Duración de las partidas")
     (vega/boxplot :values (->> matches
                                (map uic/normalize-mode)
                                (group-by #(select-keys % [:game :mode]))
                                (map (fn [[{:keys [game mode]} matches]]
                                       (assoc (uic/boxplot-stats (map :duration_m matches))
                                              :game game
                                              :mode mode))))
                   :width 512
                   :x {:field :mode
                       :title "Tipo de partida"
                       :sort {:field :game}}
                   :y {:title "Duración (minutos)"}
                   :color {:field :game
                           :title "Color"})]]

   (let [matches-by-country (->> matches
                                 (map (fn [match]
                                        (assoc match :country
                                               (-> match meta :session :country))))
                                 (group-by :country))
         time-played-by-country (->> matches-by-country
                                     (map (fn [[country matches]]
                                            [country
                                             (->> matches
                                                  (map :duration_m)
                                                  (reduce +))]))
                                     (into {}))
         top-5-countries (->> time-played-by-country
                              (sort-by second >)
                              (take 5)
                              (mapv first))]
     [:div
      [:div.row.my-4
       (let [day-of-week ["Domingo" "Lunes" "Martes" "Miércoles" "Jueves" "Viernes" "Sábado"]]
         [:div.col-auto
          (uic/title "Tiempo de juego por día de la semana"
                     "Sólo los 5 países con más tiempo de juego")
          (vega/bar :values (->> top-5-countries
                                 (mapcat (fn [country]
                                           (->> (matches-by-country country)
                                                (group-by (fn [{:keys [datetime]}]
                                                            (.getUTCDay datetime)))
                                                (map (fn [[day matches]]
                                                       {:country (:name country)
                                                        :day-of-week (nth day-of-week day)
                                                        :minutes (->> matches
                                                                      (map :duration_m)
                                                                      (reduce +))}))))))
                    :x {:field :day-of-week
                        :type :nominal
                        :axis {:labelAngle 0}
                        :sort day-of-week}
                    :y {:field :minutes
                        :type :quantitative
                        :title "Minutos"}
                    :color {:field :country
                            :type :nominal
                            :sort (mapv :name top-5-countries)}
                    :xOffset {:field :country
                              :type :nominal
                              :sort (mapv :name top-5-countries)})])]

      [:div.row
       (let [data (->> top-5-countries
                       (mapv (fn [country]
                               (map #(assoc % :country (:name country))
                                    (collect-buckets (matches-by-country country) 15)))))
             max-seconds (->> data
                              (map #(->> %
                                         (map :seconds)
                                         (apply max)))
                              (apply max))
             domain [0 (vega/find-max-scale max-seconds)]
             colors ["#4c78a8"
                     "#f58518"
                     "#e45756"
                     "#72b7b2"
                     "#54a24b"
                     "#eeca3b"]]
         (vec (concat [:div.col-auto (uic/title "Horarios de juego estimados (en hora local)"
                                                "Sólo los 5 países con más tiempo de juego")]
                      (->> data
                           (map-indexed
                            (fn [idx buckets]
                              [:div.col-12
                               (vega/bar :values buckets
                                         :width 1024
                                         :height 75
                                         :x {:field :time
                                             :axis {:labelAngle 0
                                                    :labelOverlap true}
                                             :sort {:field :idx}}
                                         :y {:field :seconds
                                             :scale {:domain domain}}
                                         :color {:field :country
                                                 :scale {:range [(nth colors idx)]}})]))))))]

      [:div.my-4.col-auto
       (uic/title "Tiempo de juego por país (minutos totales)")
       (vega/world-map :values (let [country-map time-played-by-country]
                                 (map (fn [country]
                                        (let [minutes-played (get country-map country 0)]
                                          {:id (:id country)
                                           :name (:name country)
                                           :tooltip (str (:name country) ": "
                                                         (.toFixed minutes-played 1) " m")
                                           :count minutes-played}))
                                      countries/all-countries)))]])])