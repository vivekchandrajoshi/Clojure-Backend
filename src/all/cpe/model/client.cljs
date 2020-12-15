(ns cpe.model.client
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.custommongodbfn :as c]))

(defn get-by-id [id _]
  (log/trace "get client: id: " id)
  (let [ret-chan (promise-chan)]
    (go
      (let [c (<! (db/get-entity-by-id :client id))
            sc (if-not (:err c)
                 (<! (db/get-entity-by-id :sap-client id)))
            {:keys [err msg]} (cond
                                (:err c) c
                                (:err sc) sc)]
        (if err
          (put! ret-chan {:err err :msg msg})
          (put! ret-chan {:result (merge (:result sc) (:result c))}))))
    ret-chan))

(defn get-search-options [_ _]
  (log/trace "get client search options.")
  (let [ret-chan (promise-chan)]
    (go
      (let [{:keys [err msg result] }
            (<! (db/distinct-values :sap-client :country))]
        (if err
          (put! ret-chan {:err err :msg msg})
          (put! ret-chan {:result {:country result}}))))
    ret-chan))

(defn search [query _]
  (log/trace "search client: query: " (pr-str query))
  (let [{:keys [skip limit plant?] :or {skip 0, limit 30, plant? false}} query
        query (dissoc query :skip :limit :plant?)
        opts {:skip skip, :limit limit, :plant? plant?}]
    (c/find-clients query opts)
    ))

(defn create [data claims]
  (log/trace "create client: data: " (pr-str data))
  (let [by (:id claims)
        date (js/Date.)]
    (db/insert-entity :client (assoc data
                                     :created-by by
                                     :date-created date
                                     :modified-by by
                                     :date-modified date))))

(defn update-fields [data claims]
  (log/trace "update client: id: " (:id data) ", data: " (pr-str data))
  (db/update-entity :client (assoc data
                                   :date-modified (js/Date.)
                                   :modified-by (:id claims))))
