(ns dashboard.ui-common
  (:require [clojure.string :as str]
            [dashboard.vega :as vega]
            [crate.core :as crate]
            [utils.frequencies :as f]
            [utils.bootstrap :as bs]
            [utils.async :refer [go-try <?]]
            [oops.core :refer [oget oset! ocall!]]))

(defn html [element]
  (let [element (vega/html html element)]
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

(defn normalize-mode [{:keys [game mode local?] :as match}]
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

                 (str game " " mode))))

(defn boxplot-stats [data]
  (let [{:keys [percentiles sample-count mean]}
        (f/stats (frequencies data)
                 :percentiles [9 25 50 75 91])]
    {:count sample-count
     :mean mean
     :lower (percentiles :p9)
     :q1 (percentiles :p25)
     :median (percentiles :p50)
     :q3 (percentiles :p75)
     :upper (percentiles :p91)}))

(defn title [& text]
  (if (= 1 (count text))
    [:h6.fw-bold.ps-4.text-wrap text]
    [:h6.ps-4.text-wrap
     [:span.fw-bold (first text)][:br]
     (->> (rest text)
          (map (fn [s] [:small [:small s]]))
          (interpose [:br]))]))