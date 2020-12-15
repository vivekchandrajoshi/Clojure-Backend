(ns cpe.sqldb
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! take! chan promise-chan <! close!]]
            [clojure.string :as str]
            [cpe.util :as u]
            [cpe.log :as log]
            [cljs-time.core :as t]
            [cljs-time.format :as tf]
            [cpe.entity :refer [from-db to-db attr-key-db schema-db]]
            [cpe.config :refer [config]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defonce mssql (nodejs/require "mssql"))

(defn init []
  (let [ret-chan (promise-chan)
        {:keys [server db user password]
         {:keys [encrypt]} :options} (get-in @config [:db-spec :sql])
        opts #js{:server server, :database db
                 :user user, :password password,
                 :requestTimeout 50000
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
    (log/error "Failed !" err)
    (put! c errData)
    ))

(defn find-sensor-value [sensor-data start-date end-date]
  (let [
        req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        sensor-data (first (get (js->clj sensor-data :keywordize-keys true) :recordsets))
        sensor-id (get (first sensor-data) :SensorId)
        sensor-val-type (get (first sensor-data) :SensorTypeId)
        sensor-uom (get (first sensor-data) :Unit)
        ]

                    (-> request
                        (u/ocall :input "SensorId" (u/oget mssql :Int) sensor-id)
                        (u/ocall :input "SensorDate" (u/oget mssql :Date) start-date)
                        (u/ocall :query
                                 (cond
                                   (= sensor-val-type 1) "SELECT SensorDate, SensorTime, SensorValue FROM sensorvalueinteger where SensorId = @SensorId and SensorDate >= @SensorDate"
                                   (= sensor-val-type 2) "SELECT SensorDate, SensorTime, SensorValue FROM sensorvaluedecimal where SensorId = @SensorId and SensorDate >= @SensorDate"
                                   )
                                 #(put! c %&)))
    (go
      (let [[err res] (<! c)]
        (if err
          (ret-err ret-c err)
          (do
            (put! ret-c {:result
                         {:base-uom sensor-uom :sensor-value (first (get (js->clj res :keywordize-keys true)
                                                                         :recordsets))}})
            (close! ret-c)
            ))))
    ret-c))



(defn find-sensor [plant-id sensor-name start-date end-date]
  (let [req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        ]
    (-> request
        (u/ocall :input "PlantId" (u/oget mssql :Int) plant-id)
        (u/ocall :input "SensorName" sensor-name)
        (u/ocall :query
                 "SELECT SensorId,SensorTypeId,Unit FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId WHERE sensor.PlantId = @PlantId AND sensor.SensorName = @SensorName AND IsCalculated = 1"
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)]
        (if err
          (ret-err ret-c err)
          (do
            (let [{:keys [err msg result]} (<! (find-sensor-value res start-date end-date))]
              (put! ret-c {:result (assoc result :sensor-name sensor-name)})
              (close! ret-c)
              )
            )
          )))
    ret-c))



(defn find-sensors-data [name start-date plant-id]
    (let [req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)]
    (-> request
        (u/ocall :input "PlantId" (u/oget mssql :Int) plant-id)
        (u/ocall :input "SensorDate" (u/oget mssql :Date) start-date)
        (u/ocall :query
                 (str "SELECT SensorName,SensorTypeId,Unit,d.SensorValue,d.SensorDate,d.SensorTime ,i.SensorValue, i.SensorDate, i.SensorTime FROM Sensor s LEFT JOIN UOM u on u.UOMId = s.UOMId LEFT JOIN SensorValueDecimal d on d.SensorId = s.SensorId LEFT JOIN SensorValueInteger i on i.SensorId = s.SensorId WHERE s.plantId = @PlantId AND s.IsCalculated = 1 AND (i.SensorDate IS NOT NULL OR d.SensorDate IS NOT NULL) AND s.SensorName IN " name)
                 ;"SELECT SensorName,SensorTypeId,Unit,SensorValue,SensorDate,SensorTimestamp FROM sensor LEFT JOIN UOM ON UOM.UOMId = sensor.UOMId LEFT JOIN SensorValue ON SensorValue.SensorId = sensor.SensorId where sensor.plantId = @PlantId and  SensorDate >= @SensorDate and SensorValue.SensorId in (select sensorId from sensor where sensorName in " + name + " and IsCalculated = 1 and  plantId = @PlantId)"
                 #(put! c %&)))
    (go
      (let [[err res] (<! c)
            sensor-data (get (js->clj res :keywordize-keys true)
                             :recordsets)]
        (if err
          (ret-err ret-c err)
          (do

            (put! ret-c {:result
                         (as-> sensor-data sd
                               (first sd)
                               (group-by #(select-keys % [:SensorName :Unit]) sd)
                               (map (fn [[k v]] {:sensor-name  (:SensorName k)
                                                 :base-uom     (:Unit k)
                                                 :sensor-value (mapv (fn [d]
                                                                       {:sensor-date (first  (remove nil? (:SensorDate d)))
                                                                        :sensor-time  (first (remove nil? (:SensorTimestamp d)))
                                                                        :sensor-value (first (remove nil? (:SensorValue d)))
                                                                        })
                                                                     #_(select-keys % [:SensorDate :SensorTimestamp :SensorValue]) v)
                                                 }) sd)
                               )})
            (close! ret-c)))))
    ret-c))

(def date-formatter (tf/formatter "yyyy-MM-dd"))
(defn format-date [date] (tf/unparse date-formatter (t/date-time date)))

(defn export-sensors-data [plant-id start-date end-date]
  (let [req (u/oget mssql :Request)
        request (new req)
        ret-c (chan)
        c (promise-chan)
        query-string (str "select t1.*, svd.SensorValue from (select * from (select distinct SensorDate from
        dbo.SensorValueDecimal where SensorDate >=", start-date, " and SensorId in (select sensorid from dbo.Sensor where PlantId =", plant-id, ")) d
        cross join (select s.sensorid ,s.SensorName,u.Unit, s.IsCalculated from dbo.Sensor s
        left join dbo.uom u on u.UOMId = s.UOMId
        where PlantId =" plant-id ") s) t1 left join SensorValueDecimal svd
         on t1.SensorDate = svd.SensorDate and t1.SensorId = svd.SensorId")]
      (-> request
          (u/ocall :query query-string
                   #_"SELECT SensorName,IsCalculated,SensorTypeId,Unit,d.SensorValue,d.SensorDate,d.SensorTime ,i.SensorValue, i.SensorDate, i.SensorTime FROM Sensor s LEFT JOIN UOM u on u.UOMId = s.UOMId LEFT JOIN SensorValueDecimal d on d.SensorId = s.SensorId LEFT JOIN SensorValueInteger i on i.SensorId = s.SensorId WHERE s.plantId = @PlantId AND (i.SensorDate IS NOT NULL OR d.SensorDate IS NOT NULL)"
                   #(put! c %&)))
      (go
      (let [[err res] (<! c)
            sensor-data (first (get (js->clj res :keywordize-keys true)
                                    :recordsets))
            ;sensor-data-converted (as-> sensor-data sd
            ;                            (group-by #(select-keys % [:SensorDate  :IsCalculated]) sd)
            ;                            (map (fn [[k v]] {:sensor-date (:SensorDate k)
            ;                                              :is-calculated (:IsCalculated k)
            ;                                              :sensor-data (mapv (fn [{:keys [SensorValue SensorName Unit]}]
            ;                                                                   {
            ;                                                                    :base-uom Unit
            ;                                                                    :sensor-value  SensorValue
            ;                                                                    :sensor-name SensorName
            ;                                                                    }) v)
            ;                                              }) sd))

            grouped-sensor-data (as-> sensor-data sd
                                      (group-by #(select-keys % [:SensorDate  :IsCalculated]) sd)
                                      (map (fn [[k v]]
                                             {:sensor-date (:SensorDate k)
                                              :is-calculated (:IsCalculated k)
                                              :sensor-data v}) sd))

            sorted-sensor-map (sort-by :sensor-date grouped-sensor-data)
            raw (filter #(-> % :is-calculated not) sorted-sensor-map)
            calculated (filter :is-calculated sorted-sensor-map)

            ;sensor-map  (sort-by :sensor-date sensor-data-converted)
            ;raw (filter #(-> % :is-calculated not) sensor-map)
            ;calculated (filter :is-calculated sensor-map)

            data->excel
            (fn [data]
              (let [sensor-names (atom [])
                    sensor-uoms (atom [])
                    sensor-data-rows
                    (map-indexed (fn [i {:keys [sensor-date sensor-data]}]
                                   (let [run-day    (-> (- sensor-date (js/Date. start-date))
                                                        (/  (* 1000 60 60 24))
                                                        (js/Math.floor )
                                                        (+ 1))

                                         sensor-values (into {}
                                                             (doall (map (fn [{:keys [SensorName Unit SensorValue]}]
                                                                            (if (= i 0)
                                                                              (do
                                                                                (swap! sensor-names into (vector SensorName))
                                                                                (swap! sensor-uoms into (vector Unit))))
                                                                            [SensorName SensorValue]) sensor-data)))

                                         sensor-values-list (doall (map #(get sensor-values %) @sensor-names))
                                         row [(format-date sensor-date) run-day]]
                                     (into row sensor-values-list))) data)
                    final-data (into [ @sensor-names @sensor-uoms] sensor-data-rows)
                    ]
                final-data
                (doall  (-> []
                          (conj (into [nil nil]  @sensor-names))
                          (conj (into [nil nil]  @sensor-uoms))
                          (into sensor-data-rows)))))
            final-raw-data (data->excel raw)
            final-calculated-data (data->excel calculated)]
        (if err
          (ret-err ret-c err)
          (do
            (put! ret-c {:result (mapv #(from-db :export-data  %) (clj->js [{:raw final-raw-data
                                                                          :calculated final-calculated-data
                                                                          }]))
                         })
            (close! ret-c)))))
    ret-c))

