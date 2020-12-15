(ns cpe.model.section
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.custommongodbfn :as c]))

(defn get-section
  [id _]
  (log/trace "get section: id: " id)
  (db/get-entity-by-id :section id))

(defn get-sections
  [data claim]
  (log/trace "get sections: data: " (pr-str data)  "claim:" claim)
  (c/get-sections :section ))

