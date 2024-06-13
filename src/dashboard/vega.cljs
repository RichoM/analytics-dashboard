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