(ns cpe.schema.section
  (:require [cpe.util :as u]))

(def schema
  {:section {:id     u/id-field
             :name "name"
             :charts "charts"}})