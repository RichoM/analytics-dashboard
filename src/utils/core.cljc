(ns utils.core
  (:refer-clojure :exclude [format])
  (:require [clojure.string :as str]))

(defn seek
  ([pred coll]
   (reduce #(when (pred %2) (reduced %2)) nil coll))
  ([pred coll default-value]
   (or (reduce #(when (pred %2) (reduced %2)) nil coll)
       default-value)))

(defn index-of
  "Returns -1 if not found"
  ^long [^java.util.List v e]
  (.indexOf v e))

(defn indexed-by [f vs]
  (into {} (map (juxt f identity)) vs))

(defn format
  "Simple string formatting function. It doesn't support any fancy features
  (but works in cljs)"
  [text & args]
  (loop [t text, i 0]
    (if-let [val (nth args i nil)]
      (recur
       (str/replace t (str "%" (inc i)) (str val))
       (inc i))
      t)))

(defn log [& msg]
  (apply println (.toISOString (js/Date.)) "-" msg))