(ns utils.secrets.browser-macros
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defmacro read-file* [path root]
  (let [data (into {} (map (fn [f] [(str/replace (.getPath f) "\\" "/") (slurp f)])
                           (filter (memfn isFile)
                                   (file-seq (io/file root)))))]
    `(get ~data ~path nil)))
