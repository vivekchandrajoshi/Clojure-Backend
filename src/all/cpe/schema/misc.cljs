(ns cpe.schema.misc
  (:require [cpe.util :as u]))


(def schema
  {:misc {:id     u/id-field
          :name   "name"
          :data   "data"}})