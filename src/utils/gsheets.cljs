(ns utils.gsheets
  (:require [oops.core :refer [oget oget+ oset! ocall! ocall!+]]
            [clojure.core.async :as a :refer [go <!]]
            [cljs.core.async.interop :refer [p->c] :refer-macros [<p!]]
            [utils.async :refer [go-try <?]]))

(def DISCOVERY_DOC "https://sheets.googleapis.com/$discovery/rest?version=v4")

(defonce !api-key (atom nil))
(defonce !token-client (atom nil))

(defn init-api-key! [key]
  (reset! !api-key key))

(defn authorized? []
  (some? @!token-client))

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
                  (clj->js {:apiKey @!api-key
                            :discoveryDocs [DISCOVERY_DOC]}))
      (.then #(a/close! c))
      (.catch #(a/put! c (promise-error %))))
    c))

(defn init-token-client! [credentials]
  (go-try
   (reset! !token-client
           (ocall! js/google :accounts.oauth2.initTokenClient
                   (clj->js (merge {:callback ""} credentials))))))

(defn request-access-token! [token-client]
  (let [c (a/promise-chan)]
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
   (when (nil? @!token-client)
     (println "1" (<? (load-gapi-client!)))
     (println "2" (<? (init-gapi-client!)))
     (println "3" (<? (init-token-client! credentials))))
   (println "4" (<? (request-access-token! @!token-client)))))

(defrecord Spreadsheet [id])

(defn get-values! [{:keys [id]} range]
  (let [c (a/promise-chan)]
    (doto (ocall! js/gapi :client.sheets.spreadsheets.values.get
                  (clj->js {:spreadsheetId id :range range}))
      (.then (fn [response]
               (js/console.log "Success!" response)
               (a/put! c (js->clj (oget response :?result.?values)
                                  :keywordize-keys true)))
             (fn [err]
               (js/console.log "Failure!" err)
               (a/put! c (promise-error err)))))
    c))
