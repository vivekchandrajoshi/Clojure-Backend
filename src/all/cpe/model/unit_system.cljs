(ns cpe.model.unit-system
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.custommongodbfn :as c]))

(defn get-unit-system [data claim]
  (log/trace "get-unit-list: query " (pr-str data))
  (c/find-unit-system :unit-system))

(defn find-unit-system-by-name [name]
  (log/trace "get-unit-system-data-by-name:  " (pr-str name))
  (c/find-one :unit-system #js{:name name}))
