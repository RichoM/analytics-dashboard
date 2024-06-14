(ns dashboard.vega
  (:require [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]))


(defonce !views (atom []))

(defonce !cache (atom {}))

(comment

  (count @!cache)
  (ocall! (:element (first @!views))
          :remove)
  
  )

(defn finalize! []
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
             (swap! !views conj result)))
    (.catch js/console.warn)))

(defn vega-replace! [html* element]
  (if-let [res (get @!cache element)]
    res
    (let [[tag & content] element
          [attrs] (drop-last content)
          spec (last content)
          res (doto (html* [(keyword (str/replace-first (str tag) ":vega-lite" "div"))
                           (or attrs {})])
                (.appendChild (doto (js/document.createElement "div")
                                (vega-embed! spec))))]
      (swap! !cache assoc element res)
      res)))

(defn html [html* element]
  (if (vector? element)
    (let [[tag & _] element]
      (if (str/starts-with? (str tag) ":vega-lite")
        (vega-replace! html* element)
        (vec (keep (partial html html*) element))))
    element))

(defn line [& {:keys [values x y color width height]}]
  [:vega-lite
   (cond-> {:data {:values values}
            :encoding (cond-> {:x {:field :x
                                   :type :ordinal
                                   :title nil
                                   :axis {:labelAngle -35}}
                               :y {:field :y
                                   :type :quantitative
                                   :title nil}}
                        x (update :x merge x)
                        y (update :y merge y)
                        color (assoc :color (merge {:field :color
                                                    :type :nominal
                                                    :title nil}
                                                   color)))
            :layer [{:mark {:type :line
                            :point {:size 100}
                            :tooltip true}}]}
     width (assoc :width width)
     height (assoc :height height))])

(defn boxplot [& {:keys [values x y color width height]}]
  [:vega-lite
   (cond-> {:data {:values values},
            :encoding (cond-> {:x {:field :x, :type :nominal
                                   :title nil
                                   :axis {:labelAngle -35}}
                               :y {:title nil}}
                        x (update :x merge x)
                        y (update :y merge y)),
            :layer [{:mark {:type :rule},
                     :encoding {:y {:field :lower, :type :quantitative, :scale {:zero false}},
                                :y2 {:field :upper}}},
                    {:mark {:type :bar, :size 14 :tooltip {:content :data}},
                     :encoding {:y {:field :q1, :type :quantitative},
                                :y2 {:field :q3}
                                :color (cond-> {:field :color :title nil}
                                         color (merge color))}},
                    {:mark {:type :tick, :color :white, :size 14},
                     :encoding {:y {:field :median, :type :quantitative}}}]}
     width (assoc :width width)
     height (assoc :height height))])

(defn arc [& {:keys [values theta order color width height]}]
  [:vega-lite 
   (cond-> {:data {:values values}
            :encoding {:theta (cond-> {:field :count
                                       :type :quantitative
                                       :stack :normalize}
                                theta (merge theta))
                       :order (cond-> {:field :count
                                       :type :quantitative
                                       :sort :descending}
                                order (merge order))
                       :color (cond-> {:field :color
                                       :title nil
                                       :sort {:field :count :order :descending}}
                                color (merge color))
                       :text {:field :percent, :type :nominal}},
            :layer [{:mark {:type :arc, :innerRadius 50, :point true,
                            :tooltip {:content :data}}},
                    {:mark {:type :text, :radius 75, :fill :black}}]}
     width (assoc :width width)
     height (assoc :height height))])

(defn bar [& {:keys [values x y color width height]}]
  [:vega-lite
   (cond-> {:data {:values values}
            :encoding (cond-> {:x {:field :x
                                   :type :ordinal
                                   :axis {:labelAngle -35}
                                   :title nil}
                               :y {:field :y
                                   :type :quantitative
                                   :title nil}}
                        x (update :x merge x)
                        y (update :y merge y)
                        color (assoc :color (merge {:field :color
                                                    :title nil}
                                                   color)))
            :layer [{:mark {:type :bar :point true :tooltip true}}]}
     width (assoc :width width)
     height (assoc :height height))])

(def color-schemes {:blues "#4c78a8"
                    :purples "#5c3696"})

(defn world-map [& {:keys [values color-scheme]}]
  [:div
   [:vega-lite {:width 1024
                :height 512
                :autosize :none
                :signals [{:name :tx, :update "width / 2"},
                          {:name :ty, :update "height / 2"},
                          {:name :scale,
                           :value 150,
                           :on [{:events {:type :wheel, :consume true},
                                 :update "clamp(scale * pow(1.0005, -event.deltaY * pow(16, event.deltaMode)), 150, 3000)"}]},
                          {:name :angles,
                           :value [0, 0],
                           :on [{:events :pointerdown,
                                 :update "[rotateX, centerY]"}]},
                          {:name :cloned,
                           :value nil,
                           :on [{:events :pointerdown,
                                 :update "copy('projection')"}]},
                          {:name :start,
                           :value nil,
                           :on [{:events :pointerdown,
                                 :update "invert(cloned, xy())"}]},
                          {:name :drag, :value nil,
                           :on [{:events "[pointerdown, window:pointerup] > window:pointermove",
                                 :update "invert(cloned, xy())"}]},
                          {:name :delta, :value nil,
                           :on [{:events {:signal :drag},
                                 :update "[drag[0] - start[0], start[1] - drag[1]]"}]},
                          {:name :rotateX, :value 0,
                           :on [{:events {:signal :delta},
                                 :update "angles[0] + delta[0]"}]},
                          {:name :centerY, :value 0,
                           :on [{:events {:signal :delta},
                                 :update "clamp(angles[1] + delta[1], -60, 60)"}]}]
                :projections [{:name :projection,
                               :type :mercator,
                               :scale {:signal :scale},
                               :rotate [{:signal :rotateX}, 0, 0],
                               :center [0, {:signal :centerY}],
                               :translate [{:signal :tx}, {:signal :ty}]}]
                :data [{:name :data :values values}
                       {:name :world,
                        :url "https://vega.github.io/editor/data/world-110m.json",
                        :format {:type :topojson,
                                 :feature :countries}
                        :transform [{:type :lookup :from :data :key :id
                                     :fields [:id] :values [:count :tooltip]}]},
                       {:name :graticule,
                        :transform [{:type :graticule}]}]
                :scales [{:name :color
                          :type :quantize
                          :domain [0 (apply max (map :count values))]
                          :range {:scheme (or color-scheme :blues) :count 7}}]
                :legends [{:fill :color
                           :title nil
                           :orient :top-left}]
                :marks [{:type :shape,
                         :from {:data :graticule},
                         :encode {:update {:strokeWidth {:value 1},
                                           :strokeDash {:value [2, 5]},
                                           :stroke {:value "#abc"},
                                           :fill {:value nil}}},
                         :transform [{:type :geoshape, :projection :projection}]},
                        {:type :shape,
                         :from {:data :world},
                         :encode {:update {:strokeWidth {:value 0.5},
                                           :stroke {:value "#fff"},
                                           :fill {:scale :color :field :count},
                                           :zindex {:value 0}
                                           :tooltip {:field :tooltip}}},
                         :transform [{:type :geoshape, :projection :projection}]}]}]
   [:vega-lite {:height 128
                :data {:values (->> values
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
                           :color {:value (color-schemes (or color-scheme :blues))}}
                :layer [{:mark {:type :bar :point true :tooltip true}}]}]])