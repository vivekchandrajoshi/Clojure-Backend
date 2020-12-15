(ns cpe.vault
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :as a :refer [<! put! chan promise-chan close!]]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.config :refer [config]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defonce request (nodejs/require "request"))


(defn- msi-token []
  (let [ret-chan (promise-chan)
        {:keys [enable? endpoint secret api-version]} (:msi @config)
        {:keys [resource]} (:vault @config)
        done #(do
                (log/info %2)
                (put! ret-chan {:token %1}))]
    (go
      (if-not (and enable? endpoint secret api-version resource)
        (done nil "skipping msi as disbled!")
        ;; query msi endpoint for token
        (u/ocall request :get
                 #js{:url endpoint
                     :json true
                     :headers #js{"Secret" secret}
                     :qs #js{:api-version api-version
                             :resource resource}}
                 (fn [err res body]
                   (if-let [token (some-> body
                                          (u/oget :access_token))]
                     (done token "obtained token")
                     (done nil "failed to obtain token"))))))
    ret-chan))

(defn- vault-secrets [token]
  (let [ret-chan (promise-chan)
        {:keys [api-version secrets]} (:vault @config)
        ;; try for only those with vault uri specified
        secrets (not-empty (remove (comp nil? :uri val) secrets))
        done #(do
                (log/info %2)
                (put! ret-chan {:secrets %1}))]
    (if-not secrets
      (done nil "skipped import of vault secrets! none specified.")
      (go
        (log/info (str "trying to retrieve vault secrets: " (keys secrets)))
        (let [secrets
              (<! (->>
                   (map (fn [[k {:keys [uri]}]]
                          (let [c (chan 1)]
                            (u/ocall request :get
                                     #js{:url uri
                                         :json true
                                         :headers #js{"Authorization"
                                                      (str "Bearer " token)}
                                         :qs #js{:api-version api-version}}
                                     (fn [err res body]
                                       (if body
                                         (put! c {k (u/oget body :value)}))
                                       (close! c)))
                            c))
                        secrets)
                   (a/merge)
                   (a/reduce merge {})))]
          (done secrets (str "retrieved vault secrets: " (keys secrets))))))
    ret-chan))

(defn- update-config-secrets [secrets]
  (if-let [secrets (not-empty (remove (comp nil? val) secrets))]
    (swap! config
           (fn [config]
             (reduce (fn [config [k v]]
                       (assoc-in config
                                 (get-in config [:vault :secrets k :target])
                                 v))
                        config
                        secrets)))))

(defn init []
  (let [ret-chan (promise-chan)
        done #(do
                (log/info "vault import finished.")
                (put! ret-chan {:err nil}))]
    (go
      (if-not (get-in @config [:msi :enable?])
        (done) ;; skip, since no vault access without MSI
        ;; obtain msi token
        (let [{:keys [token]} (<! (msi-token))]
          (if-not token
            (done) ;; no vault without token
            ;; retrieve vault secrets
            (let [{:keys [secrets]} (<! (vault-secrets token))]
              ;; update config with secrets
              (update-config-secrets secrets)
              (done))))))
    ret-chan))
