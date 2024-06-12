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
