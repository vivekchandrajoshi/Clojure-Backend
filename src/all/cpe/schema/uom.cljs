(ns cpe.schema.uom
  (:require [cpe.util :as u]))

(def schema
    {:uom {:id   u/id-field
           :name "name"
           :units "units"}})