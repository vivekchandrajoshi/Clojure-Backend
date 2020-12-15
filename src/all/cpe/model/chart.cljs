(ns cpe.model.chart
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :as a :refer [<! put! chan promise-chan close!]]
            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.sqldb :as sqldb]
            [cpe.custommongodbfn :as c]))

  (defn get-chart-by-id
    [id _]
    (log/trace "get section: id: " id)
    (db/get-entity-by-id :chart id))

  (defn get-charts
    [data claim]
    (c/get-charts :chart))




