(ns utils.gsheets
  (:require [oops.core :refer [oget oget+ oset! ocall! ocall!+]]
            [clojure.core.async :as a :refer [go <!]]
            [cljs.core.async.interop :refer [p->c] :refer-macros [<p!]]
            [utils.async :refer [go-try <?]]))

(def API_KEY "AIzaSyCh3kNODhW_R90EOjjoqrK66HhIuKw9EDQ")
(def DISCOVERY_DOC "https://sheets.googleapis.com/$discovery/rest?version=v4")

(defn promise-error [err]
  (ex-info "Promise error"
           {:error :promise-error}
           err))

(defn- load-gapi-client! []
  (let [c (a/promise-chan)]
    (ocall! js/gapi :load "client"
            #(a/close! c))
    c))

(defn- init-gapi-client! []
  (let [c (a/promise-chan)]
    (doto (ocall! js/gapi :client.init
                  (clj->js {:apiKey API_KEY
                            :discoveryDocs [DISCOVERY_DOC]}))
      (.then #(a/close! c))
      (.catch #(a/put! c (promise-error %))))
    c))

(defn init-token-client! [credentials]
  (let [c (a/promise-chan)
        token-client (ocall! js/google :accounts.oauth2.initTokenClient
                             (clj->js (merge {:callback ""} credentials)))]
    (oset! token-client :callback
           (fn [res]
             (if (some? (oget res :?error))
               (a/put! c (promise-error res))
               (a/close! c))))
    (if (nil? (ocall! js/gapi :client.getToken))
      (ocall! token-client :requestAccessToken #js {:prompt "consent"})
      (ocall! token-client :requestAccessToken #js {:prompt ""}))
    c))

(defn authorize! [credentials]
  (go-try 
   (println "1" (<? (load-gapi-client!)))
   (println "2" (<? (init-gapi-client!)))
   (println "3" (<? (init-token-client! credentials)))))

(defrecord Spreadsheet [id])

(defn get-values! [{:keys [id]} range]
  (let [c (a/promise-chan)]
    (doto (ocall! js/gapi :client.sheets.spreadsheets.values.get
                  (clj->js {:spreadsheetId id :range range}))
      (.then (fn [response]
               (js/console.log response)
               (a/put! c (js->clj (oget response :?result.?values)
                                  :keywordize-keys true)))
             (fn [err]
               (a/put! c (promise-error err)))))
    c))


(comment
  
  (js/console.log (ocall! js/gapi :client.sheets.spreadsheets.values.get
          (clj->js {:spreadsheetId "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms" 
                    :range "Class Data!A2:E"})))
  
  (Spreadsheet. "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms")

  (go (try
        (println (<? (get-values! (Spreadsheet. "1BxiMVs0XRA5nFMdKvBdBZjgmUUqptlbs74OgvE2upms")
                                  "Class Data!A2:E")))
        (catch :default err
          (println "ERROR" err))))


  (defn append! [{:keys [id auth]} range values]
    (go-try
     (let [response (<? (p->c (ocall! auth :spreadsheets.values.append
                                      (clj->js {:spreadsheetId id
                                                :range range
                                                :valueInputOption "USER_ENTERED"
                                                :requestBody {:values values}}))))]
       (js->clj (oget response :?data)
                :keywordize-keys true))))
  
  )