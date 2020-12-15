(ns cpe.schema.sap-plant
  (:require [cpe.util :as u]))

(def schema
  {:sap-plant {:id            u/id-field
               :name          "name"
               :client-id     "clientId"
               :sap-id        "sapId"
               :capacity      "capacity"
               :capacity-unit "capacityUnit"
               :service       "service"
               :licensor      "licensor"}})
