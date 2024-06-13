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
  [:vega-lite {:width (or width 1024)
               :height (or height 512)
               :data {:values values}
               :encoding (let [encoding {:x (merge {:field :x
                                                    :type :ordinal
                                                    :title nil
                                                    :axis {:labelAngle -35}}
                                                   x)
                                         :y (merge {:field :y
                                                    :type :quantitative
                                                    :title nil}
                                                   y)}]
                           (if color
                             (assoc encoding 
                                    :color (merge {:field :color
                                                   :type :nominal
                                                   :title nil}
                                                  color))
                             encoding))
               :layer [{:mark {:type "line"
                               :point {:size 100}
                               :tooltip true}}]}])

(defn boxplot [& {:keys [values x y color width height]}]
  [:vega-lite {:width (or width 1024)
               :height (or height 512)
               :data {:values values},
               :encoding {:x (merge {:field :x, :type :nominal
                                     :title nil
                                     :axis {:labelAngle -35}}
                                    x)
                          :y (or y {:title nil})},
               :layer [{:mark {:type "rule"},
                        :encoding {:y {:field :lower, :type "quantitative", :scale {:zero false}},
                                   :y2 {:field :upper}}},
                       {:mark {:type "bar", :size 14 :tooltip {:content "data"}},
                        :encoding {:y {:field :q1, :type "quantitative"},
                                   :y2 {:field :q3}
                                   :color (merge {:field :color :title nil}
                                                 color)}},
                       {:mark {:type "tick", :color "white", :size 14},
                        :encoding {:y {:field :median, :type "quantitative"}}}]}])