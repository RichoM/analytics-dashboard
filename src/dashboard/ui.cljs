(ns dashboard.ui
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [clojure.set :as set]
            [oops.core :refer [oget oset! ocall!]]
            [utils.gsheets :as gs]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [utils.frequencies :as f]
            [utils.core :refer [indexed-by percent seek]]
            [crate.core :as crate]
            [dashboard.data :as data]
            [dashboard.countries :as countries]))

(defonce !state (atom {:charts {:sessions-and-matches {}
                                :match-duration {}}
                       :visible-charts #{:sessions-and-matches}}))

(declare html)

(defonce !vega-views (atom []))

(defonce !vega-cache (atom {}))

(comment
  
  (count @!vega-cache)
  (ocall! (:element (first @!vega-views))
          :remove)
  
  )

(defn vega-finalize! []
  #_(println "vega-finalize!")
  #_(let [[old _] (reset-vals! !vega-views [])]
    (doseq [view old]
      (print "Fin!")
      (ocall! view :finalize))))

(defn vega-embed! [element spec]
  (doto (js/vegaEmbed element
                      (clj->js spec)
                      (clj->js {:mode :vega-lite}))
    (.then (fn [result]
             (swap! !vega-views conj result)))
    (.catch js/console.warn)))

(defn vega-replace! [element]
  (if-let [res (get @!vega-cache element)]
    res
    (let [[tag & content] element
          [attrs] (drop-last content)
          spec (last content)
          res (doto (html [(keyword (str/replace-first (str tag) ":vega-lite" "div"))
                           (or attrs {})])
                (.appendChild (doto (js/document.createElement "div")
                                (vega-embed! spec))))]
      (swap! !vega-cache assoc element res)
      res)))

(defn html-vega [element]
  (if (vector? element)
    (let [[tag & content] element]
      (if (str/starts-with? (str tag) ":vega-lite")
        (vega-replace! element)
        (vec (keep html-vega element))))
    element))

(defn html [element]
  (let [element (html-vega element)]
    (if (vector? element)
      (crate/html element)
      element)))

(defn get-element-by-id [id]
  (js/document.getElementById id))

(defn show-authorization-dialog! []
  (bs/show-modal
   (bs/make-modal :body [:h2
                         [:i.fas.fa-exclamation-circle]
                         [:span.ms-2 "Authorization required!"]]
                  :footer [:button.btn.btn-primary.btn-lg {:type "button" :data-bs-dismiss "modal" :aria-label "Log in"} "Log in"])
   {:backdrop "static"}))

(defn show-wait-dialog! [title wait-chan]
  (go-try
   (bs/show-modal
    (bs/make-modal :body [:div.container.overflow-hidden
                          [:div.row.text-center [:h3 title]]
                          [:div.row.m-1]
                          [:div.row.text-center [:i.fas.fa-circle-notch.fa-spin.fa-4x]]])
    {:backdrop "static"})
   (<? wait-chan)
   (bs/hide-modals)))

(defn clear! [element]
  (oset! element :innerHTML ""))

(defn append! [^js element & children]
  (doseq [child children]
    (.appendChild element (if (vector? child)
                            (html child)
                            child))))

(defn sessions-and-matches [{:keys [games sessions matches]}]
  [:div.row
   [:div.my-4.col-auto
    [:h6.fw-bold.text-center "Sesiones y partidas por día"]
    [:vega-lite {:width 1024
                          :height 512
                          :data {:values (data/sessions-by-day (filter :valid? sessions)
                                                               (filter :valid? matches))}
                          :encoding {:x {:field :date
                                         :type :ordinal
                                         :title "Fecha"
                                         :axis {:labelAngle -35}}
                                     :y {:field :count
                                         :type :quantitative
                                         :title "Cantidad"}
                                     :color {:field :type
                                             :type :nominal
                                             :title "Tipo"}}
                          :layer [{:mark {:type "line"
                                          :point {:size 100}
                                          :tooltip true}}]}]]

   [:div.my-4.col-auto
    [:h6.fw-bold.text-center "Partidas por sesión (promedio diario)"]
    [:vega-lite {:width 1024
                          :height 512
                          :data {:values (data/matches-per-session (filter :valid? sessions)
                                                                   (filter :valid? matches))}
                          :encoding {:x {:field :date
                                         :type :ordinal
                                         :title "Fecha"
                                         :axis {:labelAngle -35}}
                                     :y {:field :matches-per-session
                                         :type :quantitative
                                         :title "Partidas"}
                                     :color {:field :game
                                             :type :nominal
                                             :title "Juego"}}
                          :layer [{:mark {:type "line"
                                          :point {:size 100}
                                          :tooltip true}}]}]]
   [:div.row.my-4
    [:div.col-auto
     [:h6.fw-bold.text-center "Duración de las sesiones"]
     [:vega-lite {:width 256
                  :height 512
                  :data {:values (->> sessions
                                      (filter :valid?)
                                      (mapv #(select-keys % [:game :duration_m])))}
                  :encoding {:x {:field :game
                                 :type :nominal
                                 :title nil
                                 :axis {:labelAngle -35}
                                 :sort {:field :game}}
                             :y {:field :duration_m
                                 :type :quantitative
                                 :title "Duración (minutos)"}
                             :color {:field :game :title "Juego"}}
                  :layer [{:mark {:type "boxplot"}}]}]]

    [:div.col-auto
     [:h6.fw-bold.text-center "Duración de las partidas (excluyendo outliers)"]
     [:vega-lite.my-4.col-auto {:width 512
                                :height 512
                                :data {:values (->> matches
                                                    (filter :valid?)
                                                    (map (fn [{:keys [game mode local?] :as match}]
                                                           (assoc match
                                                                  :mode (case (str/lower-case game)

                                                                          "astrobrawl"
                                                                          (if (= mode "DEATHMATCH")
                                                                            (if local?
                                                                              "DEATHMATCH local"
                                                                              "DEATHMATCH online")
                                                                            mode)

                                                                          "wizards of lezama"
                                                                          (if (= mode "PRACTICE")
                                                                            "PAPABLANCA"
                                                                            mode)

                                                                          "retro racing: double dash"
                                                                          (let [mode (first (str/split mode #","))]
                                                                            (if (= mode "RACE")
                                                                              (if local?
                                                                                "RACE local"
                                                                                "RACE online")
                                                                              mode))

                                                                          mode))))
                                                    (group-by #(select-keys % [:game :mode]))
                                                    (map (fn [[{:keys [game mode]} matches]]
                                                           (let [{:keys [percentiles sample-count]}
                                                                 (f/stats (frequencies (map :duration_m matches))
                                                                          :percentiles [9 25 50 75 91])]
                                                             {:game game :mode mode
                                                              :count sample-count
                                                              :lower (percentiles :p9)
                                                              :q1 (percentiles :p25)
                                                              :median (percentiles :p50)
                                                              :q3 (percentiles :p75)
                                                              :upper (percentiles :p91)}))))},
                                :encoding {:x {:field :mode, :type "nominal"
                                               :title "Tipo de partida"
                                               :axis {:labelAngle -35}
                                               :sort {:field :game}}
                                           :y {:title "Duración (minutos)"}},
                                :layer [{:mark {:type "rule"},
                                         :encoding {:y {:field :lower, :type "quantitative", :scale {:zero false}},
                                                    :y2 {:field :upper}}},
                                        {:mark {:type "bar", :size 14 :tooltip {:content "data"}},
                                         :encoding {:y {:field :q1, :type "quantitative"},
                                                    :y2 {:field :q3},
                                                    :color {:field :game, :type "nominal" :title "Juego"}}},
                                        {:mark {:type "tick", :color "white", :size 14},
                                         :encoding {:y {:field :median, :type "quantitative"}}}]}]]]

   [:div.row.my-4
    [:div.col-4
     [:h6.fw-bold.text-center "Sesiones por plataforma"]
     [:vega-lite {:data {:values (let [platforms (->> sessions
                                                      (filter :valid?)
                                                      (map :platform))
                                       freq-map (frequencies platforms)
                                       total (count platforms)]
                                   (map (fn [[platform count]]
                                          {:type platform :count count
                                           :percent (percent (/ count total))})
                                        freq-map))}
                  :encoding {:theta {:field "count", :type "quantitative", :stack "normalize"},
                             :order {:field "count", :type "quantitative", :sort "descending"},
                             :color {:field "type",
                                     :title nil,
                                     :sort {:field "count", :order "descending"}},
                             :text {:field :percent, :type "nominal"}},
                  :layer [{:mark {:type "arc", :innerRadius 50, :point true,
                                  :tooltip {:content "data"}}},
                          {:mark {:type "text", :radius 75, :fill "black"}}]}]]

    [:div.col-4
     [:h6.fw-bold.text-center "Partidas por plataforma"]
     [:vega-lite {:data {:values (let [platforms (->> matches
                                                                    (filter :valid?)
                                                                    (map (comp :platform :session meta)))
                                                     freq-map (frequencies platforms)
                                                     total (count platforms)]
                                                 (map (fn [[platform count]]
                                                        {:type platform :count count
                                                         :percent (percent (/ count total))})
                                                      freq-map))}
                                :encoding {:theta {:field "count", :type "quantitative", :stack "normalize"},
                                           :order {:field "count", :type "quantitative", :sort "descending"},
                                           :color {:field "type",
                                                   :title nil,
                                                   :sort {:field "count", :order "descending"}},
                                           :text {:field :percent, :type "nominal"}},
                                :layer [{:mark {:type "arc", :innerRadius 50, :point true,
                                                :tooltip {:content "data"}}},
                                        {:mark {:type "text", :radius 75, :fill "black"}}]}]]]

   [:div.row.my-4
    [:div.col-4
     [:h6.fw-bold.text-center "Cantidad de sesiones por versión"]
     [:vega-lite {:width 256
                  :height 256
                  :data {:values (->> sessions
                                      (filter :valid?)
                                      (map #(select-keys % [:game :version]))
                                      (group-by :game)
                                      (mapcat (fn [[game sessions]]
                                                (map (fn [[version count]]
                                                       {:game game :version (str game " v" version) :count count})
                                                     (update-vals (group-by :version sessions) count)))))}
                  :encoding {:x {:field :version
                                 :type :ordinal
                                 :axis {:labelAngle -35}
                                 :title "Fecha"}
                             :y {:field :count
                                 :type :quantitative
                                 :title "Cantidad"}
                             :color {:field :game
                                     :title nil}}
                  :layer [{:mark {:type :bar :point true :tooltip true}}]}]]

    [:div.col-4
     [:h6.fw-bold.text-center "Cantidad de partidas por versión"]
     [:vega-lite {:width 256
                  :height 256
                  :data {:values (->> matches
                                      (filter :valid?)
                                      (map (fn [match]
                                             (assoc match :version (-> match meta :session :version))))
                                      (map #(select-keys % [:game :version]))
                                      (group-by :game)
                                      (mapcat (fn [[game matches]]
                                                (map (fn [[version count]]
                                                       {:game game :version (str game " v" version) :count count})
                                                     (update-vals (group-by :version matches) count)))))}
                  :encoding {:x {:field :version
                                 :type :ordinal
                                 :axis {:labelAngle -35}
                                 :title "Fecha"}
                             :y {:field :count
                                 :type :quantitative
                                 :title "Cantidad"}
                             :color {:field :game
                                     :title nil}}
                  :layer [{:mark {:type :bar :point true :tooltip true}}]}]]]

   (let [data (let [valid-sessions (filter :valid? sessions)
                    total-count (count valid-sessions)
                    country-map (-> (group-by :country valid-sessions)
                                    (update-vals count))]
                (map (fn [country]
                       (let [count (get country-map country 0)]
                         {:id (:id country)
                          :name (:name country)
                          :tooltip (str (:name country) ": " count)
                          :count count}))
                     countries/all-countries))
         domain [0 (apply max (map :count data))]]
     [:div.my-4.col-auto
      [:h6.fw-bold.text-center "Sesiones por país"]
      [:vega-lite {:width 1024
                   :height 512
                   :autosize "none"
                   :signals [{:name "tx", :update "width / 2"},
                             {:name "ty", :update "height / 2"},
                             {:name "scale",
                              :value 150,
                              :on [{:events {:type "wheel", :consume true},
                                    :update "clamp(scale * pow(1.0005, -event.deltaY * pow(16, event.deltaMode)), 150, 3000)"}]},
                             {:name "angles",
                              :value [0, 0],
                              :on [{:events "pointerdown",
                                    :update "[rotateX, centerY]"}]},
                             {:name "cloned",
                              :value nil,
                              :on [{:events "pointerdown",
                                    :update "copy('projection')"}]},
                             {:name "start",
                              :value nil,
                              :on [{:events "pointerdown",
                                    :update "invert(cloned, xy())"}]},
                             {:name "drag", :value nil,
                              :on [{:events "[pointerdown, window:pointerup] > window:pointermove",
                                    :update "invert(cloned, xy())"}]},
                             {:name "delta", :value nil,
                              :on [{:events {:signal "drag"},
                                    :update "[drag[0] - start[0], start[1] - drag[1]]"}]},
                             {:name "rotateX", :value 0,
                              :on [{:events {:signal "delta"},
                                    :update "angles[0] + delta[0]"}]},
                             {:name "centerY", :value 0,
                              :on [{:events {:signal "delta"},
                                    :update "clamp(angles[1] + delta[1], -60, 60)"}]}]
                   :projections [{:name "projection",
                                  :type "mercator",
                                  :scale {:signal "scale"},
                                  :rotate [{:signal "rotateX"}, 0, 0],
                                  :center [0, {:signal "centerY"}],
                                  :translate [{:signal "tx"}, {:signal "ty"}]}]
                   :data [{:name "data"
                           :values data}
                          {:name "world",
                           :url "https://vega.github.io/editor/data/world-110m.json",
                           :format {:type "topojson",
                                    :feature "countries"}
                           :transform [{:type :lookup :from "data" :key :id
                                        :fields [:id] :values [:count :tooltip]}]},
                          {:name "graticule",
                           :transform [{:type "graticule"}]}]
                   :scales [{:name "color"
                             :type "quantize"
                             :domain domain
                             :range {:scheme "blues" :count 7}}]
                   :legends [{:fill "color"
                              :title nil
                              :orient "top-left"}]
                   :marks [{:type "shape",
                            :from {:data "graticule"},
                            :encode {:update {:strokeWidth {:value 1},
                                              :strokeDash {:value [2, 5]},
                                              :stroke {:value "#abc"},
                                              :fill {:value nil}}},
                            :transform [{:type "geoshape", :projection "projection"}]},
                           {:type "shape",
                            :from {:data "world"},
                            :encode {:update {:strokeWidth {:value 0.5},
                                              :stroke {:value "#fff"},
                                              :fill {:scale "color" :field :count},
                                              :zindex {:value 0}
                                              :tooltip {:field :tooltip}}},
                            :transform [{:type "geoshape", :projection "projection"}]}]}]
      [:vega-lite {:height 128
                   :data {:values (->> data
                                       (sort-by :count)
                                       (reverse)
                                       (take 45))}
                   :encoding {:x {:field :name
                                  :type :ordinal
                                  :sort {:field "count", :order "descending"}
                                  :axis {:labelAngle -35}
                                  :title nil}
                              :y {:field :count
                                  :type :quantitative
                                  :title "Cantidad"}}
                   :layer [{:mark {:type :bar :point true :tooltip true}}]}]])

   (let [data (let [valid-matches (filter :valid? matches)
                    country-map (-> (group-by (comp :country :session meta)
                                              valid-matches)
                                    (update-vals count))]
                (map (fn [country]
                       (let [count (get country-map country 0)]
                         {:id (:id country)
                          :name (:name country)
                          :tooltip (str (:name country) ": " count)
                          :count count}))
                     countries/all-countries))
         domain [0 (apply max (map :count data))]]
     [:div.my-4.col-auto
      [:h6.fw-bold.text-center "Partidas por país"]
      [:vega-lite {:width 1024
                   :height 512
                   :autosize "none"
                   :signals [{:name "tx", :update "width / 2"},
                             {:name "ty", :update "height / 2"},
                             {:name "scale",
                              :value 150,
                              :on [{:events {:type "wheel", :consume true},
                                    :update "clamp(scale * pow(1.0005, -event.deltaY * pow(16, event.deltaMode)), 150, 3000)"}]},
                             {:name "angles",
                              :value [0, 0],
                              :on [{:events "pointerdown",
                                    :update "[rotateX, centerY]"}]},
                             {:name "cloned",
                              :value nil,
                              :on [{:events "pointerdown",
                                    :update "copy('projection')"}]},
                             {:name "start",
                              :value nil,
                              :on [{:events "pointerdown",
                                    :update "invert(cloned, xy())"}]},
                             {:name "drag", :value nil,
                              :on [{:events "[pointerdown, window:pointerup] > window:pointermove",
                                    :update "invert(cloned, xy())"}]},
                             {:name "delta", :value nil,
                              :on [{:events {:signal "drag"},
                                    :update "[drag[0] - start[0], start[1] - drag[1]]"}]},
                             {:name "rotateX", :value 0,
                              :on [{:events {:signal "delta"},
                                    :update "angles[0] + delta[0]"}]},
                             {:name "centerY", :value 0,
                              :on [{:events {:signal "delta"},
                                    :update "clamp(angles[1] + delta[1], -60, 60)"}]}]
                   :projections [{:name "projection",
                                  :type "mercator",
                                  :scale {:signal "scale"},
                                  :rotate [{:signal "rotateX"}, 0, 0],
                                  :center [0, {:signal "centerY"}],
                                  :translate [{:signal "tx"}, {:signal "ty"}]}]
                   :data [{:name "data"
                           :values data}
                          {:name "world",
                           :url "https://vega.github.io/editor/data/world-110m.json",
                           :format {:type "topojson",
                                    :feature "countries"}
                           :transform [{:type :lookup :from "data" :key :id
                                        :fields [:id] :values [:count :tooltip]}]},
                          {:name "graticule",
                           :transform [{:type "graticule"}]}]
                   :scales [{:name "color"
                             :type "quantize"
                             :domain domain
                             :range {:scheme "purples" :count 7}}]
                   :legends [{:fill "color"
                              :title nil
                              :orient "top-left"}]
                   :marks [{:type "shape",
                            :from {:data "graticule"},
                            :encode {:update {:strokeWidth {:value 1},
                                              :strokeDash {:value [2, 5]},
                                              :stroke {:value "#abc"},
                                              :fill {:value nil}}},
                            :transform [{:type "geoshape", :projection "projection"}]},
                           {:type "shape",
                            :from {:data "world"},
                            :encode {:update {:strokeWidth {:value 0.5},
                                              :stroke {:value "#fff"},
                                              :fill {:scale "color" :field :count},
                                              :zindex {:value 0}
                                              :tooltip {:field :tooltip}}},
                            :transform [{:type "geoshape", :projection "projection"}]}]}]

      [:vega-lite {:height 128
                   :data {:values (->> data
                                       (sort-by :count)
                                       (reverse)
                                       (take 45))}
                   :encoding {:x {:field :name
                                  :type :ordinal
                                  :sort {:field "count", :order "descending"}
                                  :axis {:labelAngle -35}
                                  :title nil}
                              :y {:field :count
                                  :type :quantitative
                                  :title "Cantidad"}
                              :color {:value "#5c3696"}}
                   :layer [{:mark {:type :bar :point true :tooltip true}}]}]])])

(defn players [{:keys [games sessions matches]}]
  [:div.row

   [:div.my-4.col-auto
    [:h6.fw-bold.text-center "Jugadores por día"]
    [:vega-lite {:width 1024
                 :height 512
                 :data {:values (->> sessions
                                     (filter :valid?)
                                     (data/player-count-by-day)
                                     (mapcat (fn [{:keys [date new returning]}]
                                               [{:date date :type :new :count new}
                                                {:date date :type :returning :count returning}])))}
                 :encoding {:x {:field :date
                                :type :ordinal
                                :axis {:labelAngle -35}
                                :title "Fecha"}
                            :y {:field :count
                                :type :quantitative
                                :title "Cantidad"}
                            :color {:field :type
                                    :title nil}}
                 :layer [{:mark {:type :bar :point true :tooltip true}}]}]]

   [:div.my-4.row
    [:div.col-auto
     [:h6.fw-bold.text-center "Jugadores nuevos vs recurrentes (TOTAL)"]
     [:vega-lite {:data {:values (let [pcs (->> sessions
                                                (filter :valid?)
                                                (group-by :pc))
                                       freq-map (->> pcs
                                                     (map (fn [[pc sessions]]
                                                            (let [dates (set (map :date sessions))]
                                                              (count dates))))
                                                     (frequencies))
                                       total (count pcs)
                                       new (get freq-map 1 0)
                                       returning (reduce + (vals (dissoc freq-map 1)))]
                                   [{:type :new :count new
                                     :percent (percent (/ new total))}
                                    {:type :returning :count returning
                                     :percent (percent (/ returning total))}])}
                  :encoding {:theta {:field "count", :type "quantitative", :stack "normalize"},
                             :order {:field "count", :type "quantitative", :sort "descending"},
                             :color {:field "type",
                                     :title nil,
                                     :sort {:field "count", :order "descending"}},
                             :text {:field :percent, :type "nominal"}},
                  :layer [{:mark {:type "arc", :innerRadius 50, :point true,
                                  :tooltip {:content "data"}}},
                          {:mark {:type "text", :radius 75, :fill "black"}}]}]]

    [:div.col-auto
     [:h6.fw-bold.text-center "Jugadores nuevos vs recurrentes (por juego)"]
     [:vega-lite {:width 512
                  :height 192
                  :data {:values (->> sessions
                                      (filter :valid?)
                                      (group-by :game)
                                      (mapcat (fn [[game sessions]]
                                                (let [pcs (group-by :pc sessions)
                                                      freq-map (->> pcs
                                                                    (map (fn [[pc sessions]]
                                                                           (let [dates (set (map :date sessions))]
                                                                             (count dates))))
                                                                    (frequencies))
                                                      total (count pcs)
                                                      new (get freq-map 1 0)
                                                      returning (reduce + (vals (dissoc freq-map 1)))]
                                                  [{:game game :type :new :count new
                                                    :percent (percent (/ new total))}
                                                   {:game game :type :returning :count returning
                                                    :percent (percent (/ returning total))}]))))}
                  :encoding {:y {:field :game
                                 :type :nominal
                                                ;:axis {:labelAngle -35}
                                 :title nil}
                             :x {:field :count
                                 :type :quantitative
                                 :stack :normalize
                                 :axis {:format "%"},
                                 :title "Cantidad"}
                             :color {:field :type
                                     :title nil}}
                  :layer [{:mark {:type :bar :point true
                                  :tooltip {:content "data"}}}]}]]]

   [:div.row.my-4
    [:div.col-auto
     [:h6.fw-bold.text-center "Jugadores por plataforma"]
     [:vega-lite {:data {:values (let [platforms-by-pc (update-vals (->> sessions
                                                                         (filter :valid?)
                                                                         (group-by :pc))
                                                                    (fn [sessions]
                                                                      (:platform (first sessions))))
                                       platforms (vals platforms-by-pc)
                                       freq-map (frequencies platforms)
                                       total (count platforms)]
                                   (map (fn [[platform count]]
                                          {:type platform :count count :percent (percent (/ count total))})
                                        freq-map))}
                  :encoding {:theta {:field "count", :type "quantitative", :stack "normalize"},
                             :order {:field "count", :type "quantitative", :sort "descending"},
                             :color {:field "type",
                                     :title nil,
                                     :sort {:field "count", :order "descending"}},
                             :text {:field :percent, :type "nominal"}},
                  :layer [{:mark {:type "arc", :innerRadius 50, :point true,
                                  :tooltip {:content "data"}}},
                          {:mark {:type "text", :radius 75, :fill "black"}}]}]]]
   
   (comment
     
     (do
       (def games (-> @!state :data :games))
       (def sessions (-> @!state :data :sessions))
       (def matches (-> @!state :data :matches)))
     

     (->> sessions
          (filter :valid?)
          (filter (comp #{"US"} :country_code))
          (map :pc)
          (set)
          (count))
     
     )

   (let [data (let [country-map (-> (->> sessions
                                         (filter :valid?)
                                         (group-by :country))
                                    (update-vals (fn [sessions]
                                                   (count (set (map :pc sessions))))))]
                (map (fn [country]
                       (let [count (get country-map country 0)]
                         {:id (:id country)
                          :name (:name country)
                          :tooltip (str (:name country) ": " count)
                          :count count}))
                     countries/all-countries))
         domain [0 (apply max (map :count data))]]
     [:div.my-4.col-auto
      [:h6.fw-bold.text-center "Jugadores por país"]
      [:vega-lite {:width 1024
                   :height 512
                   :autosize "none"
                   :signals [{:name "tx", :update "width / 2"},
                             {:name "ty", :update "height / 2"},
                             {:name "scale",
                              :value 150,
                              :on [{:events {:type "wheel", :consume true},
                                    :update "clamp(scale * pow(1.0005, -event.deltaY * pow(16, event.deltaMode)), 150, 3000)"}]},
                             {:name "angles",
                              :value [0, 0],
                              :on [{:events "pointerdown",
                                    :update "[rotateX, centerY]"}]},
                             {:name "cloned",
                              :value nil,
                              :on [{:events "pointerdown",
                                    :update "copy('projection')"}]},
                             {:name "start",
                              :value nil,
                              :on [{:events "pointerdown",
                                    :update "invert(cloned, xy())"}]},
                             {:name "drag", :value nil,
                              :on [{:events "[pointerdown, window:pointerup] > window:pointermove",
                                    :update "invert(cloned, xy())"}]},
                             {:name "delta", :value nil,
                              :on [{:events {:signal "drag"},
                                    :update "[drag[0] - start[0], start[1] - drag[1]]"}]},
                             {:name "rotateX", :value 0,
                              :on [{:events {:signal "delta"},
                                    :update "angles[0] + delta[0]"}]},
                             {:name "centerY", :value 0,
                              :on [{:events {:signal "delta"},
                                    :update "clamp(angles[1] + delta[1], -60, 60)"}]}]
                   :projections [{:name "projection",
                                  :type "mercator",
                                  :scale {:signal "scale"},
                                  :rotate [{:signal "rotateX"}, 0, 0],
                                  :center [0, {:signal "centerY"}],
                                  :translate [{:signal "tx"}, {:signal "ty"}]}]
                   :data [{:name "data"
                           :values data}
                          {:name "world",
                           :url "https://vega.github.io/editor/data/world-110m.json",
                           :format {:type "topojson",
                                    :feature "countries"}
                           :transform [{:type :lookup :from "data" :key :id
                                        :fields [:id] :values [:count :tooltip]}]},
                          {:name "graticule",
                           :transform [{:type "graticule"}]}]
                   :scales [{:name "color"
                             :type "quantize"
                             :domain domain
                             :range {:scheme "purples" :count 7}}]
                   :legends [{:fill "color"
                              :title nil
                              :orient "top-left"}]
                   :marks [{:type "shape",
                            :from {:data "graticule"},
                            :encode {:update {:strokeWidth {:value 1},
                                              :strokeDash {:value [2, 5]},
                                              :stroke {:value "#abc"},
                                              :fill {:value nil}}},
                            :transform [{:type "geoshape", :projection "projection"}]},
                           {:type "shape",
                            :from {:data "world"},
                            :encode {:update {:strokeWidth {:value 0.5},
                                              :stroke {:value "#fff"},
                                              :fill {:scale "color" :field :count},
                                              :zindex {:value 0}
                                              :tooltip {:field :tooltip}}},
                            :transform [{:type "geoshape", :projection "projection"}]}]}]

      [:vega-lite {:height 128
                   :data {:values (->> data
                                       (sort-by :count)
                                       (reverse)
                                       (take 45))}
                   :encoding {:x {:field :name
                                  :type :ordinal
                                  :sort {:field "count", :order "descending"}
                                  :axis {:labelAngle -35}
                                  :title nil}
                              :y {:field :count
                                  :type :quantitative
                                  :title "Cantidad"}
                              :color {:value "#5c3696"}}
                   :layer [{:mark {:type :bar :point true :tooltip true}}]}]])])

(defn toggle-btn [text]
  (html [:button.r-button.btn.btn-sm.btn-outline-dark.rounded-pill
         {:type "button" :data-bs-toggle "button"}
         [:i.fa-solid.fa-cube.me-2]
         text]))

(defn set-pressed! [btn pressed?]
  (if pressed?
    (ocall! btn :classList.add "active")
    (ocall! btn :classList.remove "active"))
  btn)

(defn visible-chart? [chart-id]
  (contains? (:visible-charts @!state) chart-id))

(defn side-bar-btn [chart-id text]
  (doto (toggle-btn text)
    (set-pressed! (visible-chart? chart-id))
    (bs/on-click #(swap! !state update :visible-charts
                         (fn [visible-charts]
                           #{chart-id}
                           #_(if (contains? visible-charts chart-id)
                             (disj visible-charts chart-id)
                             (conj visible-charts chart-id)))))))

(defn game-keyword [game-name]
  (-> game-name
      (str/lower-case)
      (str/replace #"[^a-zA-Z]+" "-")
      (str/replace #"^-" "")
      (keyword)))

(defn game-checkbox [game-name]
  (let [game-key (game-keyword game-name)
        checked? (contains? (:game-filters @!state) game-name)]
    (html [:div.form-check.form-switch.text-center.mx-3
           (doto (html [:input.form-check-input {:id game-key :type "checkbox"
                                                 :role "switch" :checked checked?}])
             (bs/on-click #(swap! !state update :game-filters
                                  (fn [filters]
                                    (if (contains? filters game-name)
                                      (disj filters game-name)
                                      (conj filters game-name))))))
           [:label.form-check-.ebal {:for game-key} game-name]])))

(defn main-container []
  [:div#main-container.container-fluid
   [:div.row
    [:div#side-bar.col-lg-auto
     [:div.sticky-top.py-2
      [:div.d-grid (side-bar-btn :sessions-and-matches "Sesiones y partidas")]
      [:div.row.my-1]
      [:div.d-grid (side-bar-btn :players "Jugadores")]
      [:div.row.my-2]
      [:hr]
      [:div
       (map game-checkbox (-> @!state :data :games sort))]]]
    [:div#charts.col.w-auto ;overflow-auto.vh-100
     [:div.my-1]
     [:div
      (when (visible-chart? :sessions-and-matches)
        (sessions-and-matches (:data @!state)))
      (when (visible-chart? :players)
        (players (:data @!state)))]]]])


(defn update-ui! [old-state new-state]
  (vega-finalize!)
  (doto (get-element-by-id "content")
    (clear!)
    (append! (main-container)))
  (when-let [scroll js/document.scrollingElement]
    (when (not= (:visible-charts old-state)
                (:visible-charts new-state))
      (go (oset! scroll :scrollTop 0)))))

(defn update-filters! [{:keys [sessions matches]}]
  (let [filters (:game-filters @!state)]
    (swap! !state update :data
           assoc 
           :sessions (filter (comp filters :game) sessions)
           :matches (filter (comp filters :game) matches))))

(defn initialize-ui! [data]
  (go
    (if (nil? data)
      (js/window.location.replace "https://www.youtube.com/watch?v=dQw4w9WgXcQ")
      (do (add-watch !state ::state-change
                     (fn [_ _ old new]
                       (if (not= (:game-filters old)
                                 (:game-filters new))
                         (update-filters! data)
                         (update-ui! old new))))
          (swap! !state assoc
                 :data data
                 :game-filters (:games data))))))

(defn clear-ui! []
  (clear! (get-element-by-id "content")))

(comment
  (do
    (def games (-> @!state :data :games))
    (def sessions (-> @!state :data :sessions))
    (def matches (-> @!state :data :matches)))

  (count sessions)
  (count matches)

  (count (filter :valid? matches))
  (first matches)

  (:session (meta (first matches)))

  )