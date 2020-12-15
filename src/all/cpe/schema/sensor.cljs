(ns cpe.schema.sensor
  (:require [cpe.util :as u]))

(def schema
  (let [
        sensor-value {:sensor-date  (u/date-field "SensorDate")
                      :sensor-time  "SensorTime"
                      :sensor-value "SensorValue"}
        export-value {:base-uom       "baseUOM"
                      :sensor-value "SensorValue"
                      :sensor-name    "sensorName"}]
    {:sensor {:id          u/id-field
              :name        "name"
              :description  "description"
              :uom-id   "uomId"}

     :sensor/query          {:name      "name"
                             :start-date "startDate"
                             :end-date   "endDate"
                             :plant-id "plantId"}

     :sensor-value {:sensor-name    "sensorName"
                    :is-calculated  "isCalculated"
                    :sensor-value    {:name   "sensorValue"
                                      :schema sensor-value
                                      :array? true}
                    :base-uom       "baseUOM"}
     :export-data {:raw  {:name   "raw"
                          :array? true}
                   :calculated {:name   "calculated"
                               :array? true} }}))


