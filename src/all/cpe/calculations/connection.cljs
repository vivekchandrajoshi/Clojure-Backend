(ns cpe.calculations.connection
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! take! chan promise-chan <! close!]]
            [clojure.string :as str]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.config :refer [config]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defonce mssql (nodejs/require "mssql"))

(defn init []
  (let [ret-chan (promise-chan)
        {:keys             [server db user password]
         {:keys [encrypt]} :options} (get-in @config [:db-spec :sql])
        opts #js{:server  server, :database db
                 :user    user, :password password
                 :options #js{:encrypt encrypt}}]
    (doto (u/ocall mssql :connect opts
                   (fn [err res]
                     (if err
                       (log/error "Failed to initialize sql database!" err)
                       (log/info "Connected to sql database."))
                     (put! ret-chan {:err err}))))
    ret-chan))


(defn ret-err [c err]
  (let [err (u/json-str err)
        errData (js->clj err :keywordize-keys true)
        ]
    (put! c errData)
    ))

(defn find-raw-sensors-data [plant-id date]
  (let [req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        sql (str "SELECT SensorName,SensorTypeId,Unit,SensorValue,SensorDate,SensorTime FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId LEFT JOIN SensorValueDecimal as sd ON sd.SensorId = sensor.SensorId where sensor.plantId = " plant-id "and AverageTypeId = 0 and isCalculated = 0  and SensorDate = " "'" date "'"
              ;"SELECT SensorName, SensorValue FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId LEFT JOIN SensorValue ON SensorValue.SensorId = sensor.SensorId where sensor.plantId = " plant-id "and IsDailyAverage = 0 and SensorDate = " "'" date "'"
              )]

    (-> request
        ;(u/ocall :input "PlantId" (u/oget mssql :Int) plant-id)
        (u/ocall :query
                 sql
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)
            sensor-data (get (js->clj res :keywordize-keys true)
                             :recordsets)]
        (if err
          (ret-err ret-c err)
          (do
            (put! ret-c {:result (as-> sensor-data sd
                                       (first sd)
                                       (group-by #(select-keys % [:SensorName :Unit]) sd)
                                       (map (fn [[k v]] {:sensor-name  (:SensorName k)
                                                         :base-uom     (:Unit k)
                                                         :sensor-value (mapv (fn [d]
                                                                               (:SensorValue d))
                                                                              v)
                                                         }) sd)
                                       )})
            (close! ret-c)))))
    ret-c))


(defn find-sensors-data [plant-id date avg-sensor]
  (let [req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        sql (str "SELECT sv.SensorId,SensorName,SensorTypeId,Unit,SensorValue,SensorDate,SensorTime FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId LEFT JOIN SensorValueDecimal as sv ON sv.SensorId = sensor.SensorId where sensor.plantId = " plant-id " and sv.SensorDate = " "'" date "' and sensor.sensorId in  (" (clojure.string/join ", " avg-sensor) ")" )
        ]
        (-> request
        (u/ocall :input "PlantId" (u/oget mssql :Int) plant-id)
        (u/ocall :query
                 sql
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)
            sensor-data (first (get (js->clj res :keywordize-keys true)
                                    :recordsets))]
        (if err
         (ret-err ret-c err)


            (put! ret-c {:result sensor-data})
            (close! ret-c))))
    ret-c))

(defn find-sensors [plant-id ]
  (let [req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        sql (str "SELECT * from sensor  where sensor.plantId = " plant-id )]
    (-> request
        (u/ocall :input "PlantId" (u/oget mssql :Int) plant-id)
        (u/ocall :query
                 sql
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)
            sensor (first (get (js->clj res :keywordize-keys true)
                                    :recordsets))]
        (if err
          (ret-err ret-c err)
          (do
            (put! ret-c {:result sensor})
            (close! ret-c)))))
    ret-c))


(defn find-uom-data [plant-id]
  (let [req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        sql (str "SELECT SensorId,SensorName,Unit FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId LEFT JOIN SensorValue ON SensorValue.SensorId = sensor.SensorId where sensor.plantId = " plant-id "'")]
    (-> request
        (u/ocall :input "PlantId" (u/oget mssql :Int) plant-id)
        (u/ocall :query
                 sql
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)
            sensor-data (first (get (js->clj res :keywordize-keys true)
                                    :recordsets))]
        (if err
          (ret-err ret-c err)
          (do
            (put! ret-c {:result sensor-data})
            (close! ret-c)))))
    ret-c))



(defn insert-sensor [values]
  (let [req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        ;is-integer (integer? value)
        sql (str
              ;"INSERT INTO SensorValueDecimal (SensorId, SensorDate, SensorTime, SensorValue) VALUES "
              "INSERT INTO SensorValueDecimal (SensorId, SensorDate, SensorTime, SensorValue) VALUES "
              values
              )]
    (-> request
        (u/ocall :query
                 sql
                 #(put! c %&)))

    (go
      (let [[err res] (<! c)]
        (if err
          (ret-err ret-c err)
          (put! ret-c {:result (first (get (js->clj res :keywordize-keys true)
                                           :recordsets))}))))
    ret-c))



(defn insert-bulk-data [data plant-id]
    (let [req (u/oget mssql :Request)
          request (new req)
          ret-c (chan)
          c (promise-chan)
          table (new mssql.Table "SensorValueDecimal")]
      (set! (.-create table) true)
      (.add (.-columns table) "SensorId" mssql.Int #js{:nullable false} )
      (.add (.-columns table) "SensorDate" mssql.Date #js{:nullable false} )
      (.add (.-columns table) "SensorTime" (mssql.Time 7) #js{:nullable false} )
      (.add (.-columns table) "SensorValue" (mssql.Decimal 18 5) #js{:nullable false} )
      (doall
        (map (fn [d]
               (.add (.-rows table) (nth d 0) (nth d 1) (nth d 2) (nth d 3)))
             data))
    (-> request
          (u/ocall :bulk table #(put! c %&)))
    (go
        (let [[err res] (<! c)
              sensor-data (get (js->clj res :keywordize-keys true)
                               :recordsets)]
          (if err
            (ret-err ret-c err)
            (put! ret-c {:result res})
            )))
      ret-c ))

