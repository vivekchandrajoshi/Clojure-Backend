(ns cpe.model.sensor
  (:require-macros [cljs.core.async.macros
                    :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :as a :refer [put! take! chan promise-chan <! close!]]
    ; [cljs.core.async :refer [mix admix toggle merge chan <! >! timeout]]

            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.sqldb :as sqldb]
            [cpe.custommongodbfn :as c]))

(defn get-sensor
  [{:keys [name start-date end-date]}]
  (log/trace "get Sensors detail:" (pr-str name))
  (c/find-sensor-by-name :sensor name))

(defn get-sensors
  [data claim]
  (log/trace "get Sensor: id: " data)
  (db/find-entities :sensor data))

(defn sensor-by-value [name start-date plant-id]
  (log/trace "test use")
  (let [ret-chan (promise-chan)]
    (go
      (let [ channels (map (fn [tag]
                             (sqldb/find-sensor plant-id tag
                                                start-date nil)) name)
            merge-chan (a/merge channels)
            reduce-chan (a/reduce conj [] merge-chan)
            data (<! reduce-chan)
            res (mapv (fn [e]
                        (let [{:keys [err msg result]} e]
                          (if err
                            {:err err :msg msg}
                            ;;Removing the keys from :sensor-value vector
                            (let [{:keys [sensor-name sensor-value base-uom]} result
                                  sv (mapv (fn [{:keys [SensorDate SensorTime SensorValue]}]
                                             {:sensor-date SensorDate
                                              :sensor-time SensorTime
                                              :sensor-value SensorValue}
                                             ) sensor-value)]

                              (assoc result :sensor-value sv))
                            ))
                        ) data)
            ]
        (put! ret-chan {:result res})))
    ret-chan))

(defn get-sensor-value [{:keys [name start-date plant-id]}]
  (log/trace "get Sensors name:" (pr-str name) "plant-id:" (pr-str plant-id) "start-date:" (pr-str start-date))
  (let [date (js/Date. start-date)
        day (.getUTCDate date)
        month (+ (.getUTCMonth date) 1)
        year (.getUTCFullYear date)
        date-str (str year "-" month "-" day)]
    (sqldb/find-sensors-data (str "(" (clojure.string/join "," (map #(str "'" % "'") name)) ")") date-str plant-id)
    #_(if (= (first  name) "")
      (sensor-by-value name date-str plant-id)
      (sqldb/find-sensors-data name date-str plant-id))))

(defn export-excel-data [{:keys [name start-date plant-id]}]
  (sqldb/export-sensors-data plant-id start-date nil ))
