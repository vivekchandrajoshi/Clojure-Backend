(ns cpe.calculations.mongo
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! take! chan promise-chan <! close!]]
            [clojure.string :as str]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.calculations.connection :as con]
            [cpe.config :refer [config]]
            [cpe.entity :refer [from-db to-db attr-key-db schema-db]]
            [clojure.string :as string])
(:require-macros [cljs.core.async.macros :refer [go]]))

(defonce mongo-db (nodejs/require "mongodb"))

(defonce mongo-client (atom nil))

(defonce app-db (atom nil))

(defonce uom (atom {}))
(defonce uom-list (atom {}))
(defonce plant (atom {}))

(defonce raw (atom {}))
(defonce avg-raw (atom {}))
(defonce calculated (atom {}))
(defonce plantId (atom {}))

(defonce calculated-uom-data (atom {}))

(defn init []
  (let [ret-chan (promise-chan)
        {:keys [uri uri-params db] :as db-spec} (get-in @config [:db-spec :mongo])
        db-uri (reduce-kv (fn [uri k p]
                            (str/replace uri p (or (get db-spec k) "")))
                          uri
                          uri-params)]
    (doto (u/oget mongo-db :MongoClient)
      (u/ocall :connect db-uri #js{:ignoreUndefined true}
               (fn [err res]
                 (if err
                   (log/error "Failed to initialize mongo database!" err)
                   (log/info "Connected to mongo database.")
                   #_(find-plant-id))
                 (when-let [client (if-not err res)]
                   (reset! mongo-client client)
                   (reset! app-db (u/ocall client :db db)))
                 (put! ret-chan {:err err}))))
    ret-chan))

(defonce db-schema
         {:sap-client  {:db-name "sap"
                        :coll    "clients"}
          :sap-plant   {:db-name "sap"
                        :coll    "plants"}

          :user        {:coll "user"}
          :client      {:coll "client"}
          :plant       {:coll "plant"}
          :chart       {:coll "chart"}
          :section     {:coll "section"}
          :sensor      {:coll "sensor"}
          :unit-system {:coll "unitSystem"}
          :uom         {:coll "uom"}
          :comment     {:coll "comment"}
          :misc        {:coll "misc"}
          :summary     {:coll "summary"}})

(defn coll
  "Returns the MongoDb collection instance where the desired entity is kept."
  [entity-key]
  (if-let [client @mongo-client]
    (let [{:keys [db-name coll]} (get db-schema entity-key)]
      (if coll
        (->
          (if db-name
            (u/ocall client :db db-name)
            @app-db)
          (u/ocall :collection coll))
        (throw (ex-info "Invalid entity key!" {:entity-key entity-key}))))
    (throw (ex-info "Database not connected!" {}))))

(defn ret-err [c err]
  (put! c
        (-> {:err (->> (u/oget err :code)
                       (str "db-") keyword)
             :msg (u/oget err :message)}
            (update :err #(case %
                            :db-11000 :db-duplicate
                            %)))))

(defn find-one
  "Retrieve an entity from database.
  Usage:
    (find-one entity-key id)
    (find-one entity-key id fields)
  *entity-key* is the key of desired entity
  *id* is the entity id
  *fields* is an object of fields to include or exclude (not both), #js {:a 1}
  Returns a channel from which you can read the result map:
    {:err <error keyword if any>
     :result <entity data map>}"
  ([entity-key query]
   (find-one entity-key query nil))
  ([entity-key query opts]
   (let [ret-c (promise-chan)
         c (promise-chan)
         {:keys [sort skip limit project]} opts]
     (-> (coll entity-key)
         (u/ocall :findOne query #js{:projection project} #(put! c %&)))
     (go
       (let [[err res] (<! c)]
         (if err
           (ret-err ret-c err)
           (put! ret-c {:result (from-db entity-key res)}))))
     ret-c)))


(defn find-entities
  "Retrieve entitites of a given key.
  Usage:
    (find-entities entity-key)
    (find-entities entity-key query)
    (find-entities entity-key query opts)
  *entity-key* is the key of desired entity
  *query* is an object specifying query conditions
  *opts* is a map of options
    {:sort <object specifying sort cond>
     :skip <number of items to skip>
     :limit <maximum number of items to return>
     :project <object for fields projection>}
  Returns a channel from which you can read the result map:
    {:err <error keyword if any>
     :result [e1, e2, e3, ...]}"
  ([entity-key]
   (find-entities entity-key nil nil))
  ([entity-key query]
   (find-entities entity-key query nil))
  ([entity-key query opts]
   (let [ret-c (promise-chan)
         c (promise-chan)
         {:keys [sort skip limit project]} opts]
     ;(print "entity-key" entity-key "query" query, "opts" opts)


     (cond-> (coll entity-key)
             true (u/ocall :find query)
             project (u/ocall :project project)
             sort (u/ocall :sort sort)
             skip (u/ocall :skip skip)
             limit (u/ocall :limit limit)
             true (u/ocall :toArray #(put! c %&)))
     (go
       (let [[err res] (<! c)]
         (if err
           (do
           (ret-err ret-c err)
           (log/error  err))
            (do
              (log/info "uom received")
              ; (print "entity-key" entity-key, "res", (mapv #(from-db entity-key %) res))
              (put! ret-c {:result (mapv #(from-db entity-key %) res)})))))
     ret-c)))


(defn find-uom-data [plant-id]
  (let [req (u/oget con/mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        sql (str "SELECT SensorId,SensorName,Unit,Description FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId where sensor.plantId = " plant-id " and isCalculated = 1 ")]
    (-> request
        (u/ocall :input "PlantId" (u/oget con/mssql :Int) plant-id)
        (u/ocall :query
                 sql
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)
            sensor-data (first (get (js->clj res :keywordize-keys true)
                                    :recordsets))]
        (if err
          (log/error  err)
          (do
            (log/info "base-uom received")
            (swap! uom assoc :base-uom sensor-data )
            (close! ret-c)))))
    ret-c))

(defn find-avg-tag [plant-id]
  (let [req (u/oget con/mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        sql (str "SELECT SensorId,SensorName,Unit,Description FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId where sensor.plantId = " plant-id "and isCalculated = 0  and AverageTypeId = 1")]
    (-> request
        (u/ocall :input "PlantId" (u/oget con/mssql :Int) plant-id)
        (u/ocall :query
                 sql
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)
            sensor-data (first (get (js->clj res :keywordize-keys true)
                                    :recordsets))
            avg-data (reduce-kv (fn [col i d]
                              (-> col
                                  (merge
                                    {(keyword (string/trim (d :SensorName))) (d :SensorId) }
                                    )  ) ){} sensor-data)]
        (if err
          (log/error  err)
          (do

            (log/info "avg-tag received")
            (put! ret-c {:result avg-data})
            (close! ret-c)))))
    ret-c))


(defn find-calculated-tag [plant-id]
  (let [req (u/oget con/mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        sql (str "SELECT SensorId,SensorName,Unit,Description FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId where sensor.plantId = " plant-id " and isCalculated = 1")]
    (-> request
        (u/ocall :input "PlantId" (u/oget con/mssql :Int) plant-id)
        (u/ocall :query
                 sql
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)
            sensor-data (first (get (js->clj res :keywordize-keys true)
                                    :recordsets))
            avg-data (reduce-kv (fn [col i d]
                                  (-> col
                                      (merge
                                        {(keyword (string/trim (d :SensorName))) (d :SensorId) }
                                        )  ) ){} sensor-data)]
        (if err
          (log/error  err)
          (do

            (log/info "Calculated-tag received")
            (put! ret-c {:result avg-data})
            (close! ret-c)))))
    ret-c))

(defn find-plant-id []
  (let [ret-c (chan)]
    (go
      (let [{:keys [err msg result]} (<! (find-one :plant #js{:name "Diesel HDT"}))]
        (if err
          (log/error  err)
          (do
            (reset! plant (get-in result [:config :sql-plant-id] ) )
            (close! ret-c)))))
    ret-c))

(defn initialize-data [plant-id]
  (go
    (let [uom  (<! (find-entities :uom))
          plant-config  (<! (find-one :plant #js{:config.sqlPlantId plant-id}))
          find-uom  (<! (find-uom-data plant-id))
          avg-tag-data  (<! (find-avg-tag plant-id))
          calc-tag-data ((<! (find-calculated-tag plant-id)) :result)
          uom-data (if (:err uom)
                     (log/error (:err uom))
                     (do
                       (log/info "uom added")
                       (:result uom)
                       ))
          plant-config-data (if (:err plant-config)
                     (log/error (:err plant-config))
                     (do
                       (log/info "plant config added")
                       (let [
                             plantId  (get-in plant-config [:result :config :sqlPlantId])
                             plant-constant (vec (concat
                                                   (js->clj (get-in plant-config [:result :config :constant])
                                                            :keywordize-keys true)
                                                   (js->clj(get-in plant-config [:result :settings :constant])
                                                           :keywordize-keys true)))
                             plant-data  (reduce-kv (fn [col i d]
                                                       (merge col
                                                        {(keyword (string/trim (d :paramKey)))  (d :value)})
                                                        ) {} plant-constant)
                             calculated-tag (js->clj (get-in plant-config [:result :settings :calculated-tags])
                                      :keywordize-keys true)
                             schema (->>
                                      (js->clj (get-in plant-config [:result :settings :raw-tags])
                                               :keywordize-keys true)
                                      (reduce-kv (fn [col i d]
                                                   (-> col
                                                       (merge
                                                         {(keyword (string/trim (d :name)))
                                                          (keyword (string/trim (d :paramKey)))}
                                                         ))){}))
                             calculated-sensor (reduce-kv (fn [col i d]
                                                            (-> col
                                                                (merge
                                                                  {(keyword (string/trim (d :paramKey)))
                                                                  (calc-tag-data (keyword (d :name))) }
                                                                  ))){} calculated-tag)
                             calculated-uom (reduce-kv (fn [col i d]
                                                         (-> col
                                                             (merge
                                                               {(keyword (string/trim (d :paramKey)))
                                                                (string/trim (get-in d [:uom :from])) }
                                                               ))){} calculated-tag)]

                         (do
                           (reset! plant  plant-data )
                           (reset! raw schema )
                           (reset! avg-raw (avg-tag-data :result) )
                           (reset! calculated calculated-sensor )
                           (reset! calculated-uom-data calculated-uom)
                           (reset! uom-list uom-data)))))])))



(defn convert-to-base-uom [sensor-value from-uom SensorId]
  (if (and from-uom sensor-value)
    (let [base-uom-name (->> (map (fn [d]
                               (if (= (:SensorId d) SensorId)
                                 (do
                                   (:Unit d))
                                 )) (:base-uom @uom))
                        (remove nil?)
                        (first))
          base-uom-type (->> (map (fn [d]
                                    (if  (= (:SensorId d) SensorId)
                                      (:Description d)))(:base-uom @uom))
                             (remove nil?)
                             (first))
          uom-list (->> (map (fn [d]
                               (if  (= (:name d) base-uom-type)
                                 (js->clj (:units d) :keywordize-keys true) ))@uom-list)
                        (remove nil?)
                        (first))
          base-uom  (->> (map (fn [d]
                                (if  (or (= (:name d) base-uom-name) (= (:id d) base-uom-name))
                                  d)) uom-list)
                         (remove nil?)
                         (first))
          current-uom  (->> (map (fn [d]
                                   (if  (= (:id d) from-uom)
                                     d)) uom-list)
                            (remove nil?)
                            (first))
          a1 (get current-uom :factor 1)
          a2 (get base-uom :factor 1)
          b1 (get current-uom :offset 1)
          b2  (get base-uom :offset 1)
          factor (/  a2 a1)
          offset (- (* a1 b2) (* a1 b1))]
      ;(log/info "from-uom" from-uom  "base-uom" (:name base-uom) "base-uom-name" base-uom-name "output" (* (- sensor-value offset) factor))
      (* (- sensor-value offset) factor)

      )
    sensor-value))

