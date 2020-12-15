(ns cpe.model.plant
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.custommongodbfn :as c]))


(defn get-by-id [id _]
  (log/trace "get plant: id: " id)
  (let [ret-chan (promise-chan)]
    (go
      (let [p (<! (db/get-entity-by-id :plant id))
            sp (if-not (:err p)
                 (<! (db/get-entity-by-id :sap-plant id)))
            {:keys [err msg]} (cond
                                (:err p) p
                                (:err sp) sp)]
        (if err
          (put! ret-chan {:err err :msg msg})
          (put! ret-chan {:result (merge (:result sp) (:result p))}))))
    ret-chan))

(defn search [query _]
  (log/trace "search plants: query: " (pr-str query))
  (c/find-plants query)
  )

(defn create [query claims]
  (log/trace "create plant: query " (pr-str query))
  (let [by (:id claims)
        date (js/Date.)]
    (db/insert-entity :plant
                      (-> query
                          (assoc :created-by by
                                 :date-created date
                                 :modified-by by
                                 :date-modified date)
                            ))))

(defn update-settings [query claims]
  (log/trace "update-settings plant: query " (pr-str query))
  (let [ret-chan (promise-chan)
        id (:id query)
        settings (get-in query [:data :settings])]
      (go
        (let [old-plant-data (<! (db/get-entity-by-id :plant id))
              old-settings-data (if-not (:err old-plant-data)
                                    (get-in old-plant-data [:result :settings]))                  
              by (:id claims)
              date (js/Date.)
              uoms (if (:uoms settings)
                        (:uoms settings)
                        (:uoms old-settings-data))
              pinned-charts (if (:pinned-charts settings)
                                (:pinned-charts settings)
                                (:pinned-charts old-settings-data))

              charts-config (if (:charts-config settings)
                              (:charts-config settings)
                              (:charts-config old-settings-data))

              new-settings-data (-> old-settings-data
                                (assoc  :modified-by by
                                        :date-modified date
                                        :uoms uoms
                                        :pinned-charts pinned-charts
                                        :charts-config charts-config))
              updated-plant-settings (<! (db/update-entity :plant {:id id
                                                                 :settings new-settings-data}))
             {:keys [err msg]} (cond
                                (:err old-plant-data) old-plant-data
                                (:err updated-plant-settings) updated-plant-settings)]
          (if err
            (put! ret-chan {:err err :msg msg})
            (put! ret-chan {:result (:result updated-plant-settings)}))
        ))
    ret-chan))

(defn update-config [query claims]
  (log/trace "update-config plant: query " (pr-str query))
  (let [ret-chan (promise-chan)
        id (:id query)
        config (get-in query [:data :config])]
      (go
        (let [old-plant-data (<! (db/get-entity-by-id :plant id))
              old-config-data (if-not (:err old-plant-data)
                                (get-in old-plant-data [:result :config]))                  
              by (:id claims)
              date (js/Date.)
              date-sor (if (:date-sor config)
                          (:date-sor config)
                          (:date-sor old-config-data))
              history-sor (if (:history-sor config)
                                (:history-sor config)
                                (:history-sor old-config-data))
              section (if (:section config)
                            (:section config)
                            (:section old-config-data))
              new-config-data (-> old-config-data
                                (assoc  :modified-by by 
                                        :date-modified date
                                        :date-sor date-sor
                                        :history-sor history-sor
                                        :section section))
              updated-plant-config (<! (db/update-entity :plant {:id id
                                                                 :config new-config-data}))
             {:keys [err msg]} (cond
                                (:err old-plant-data) old-plant-data
                                (:err updated-plant-config) updated-plant-config)]          
          (if err
            (put! ret-chan {:err err :msg msg})
            (put! ret-chan {:result (:result updated-plant-config)}))
        ))
    ret-chan))


