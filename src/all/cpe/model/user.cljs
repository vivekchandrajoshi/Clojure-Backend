(ns cpe.model.user
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]))

(defn get-by-id [id _]
  (log/trace "get user: id: " id)
  (db/get-entity-by-id :user id))

(defn create [data claims]
  (log/trace "create user: id:" (:id claims) ", data:" (pr-str data))
  (db/insert-entity :user (assoc data :id (:id claims))))

(defn update-fields [data _]
  (log/trace "update user: data:" (pr-str data))
  ;; data includes the :id as added by handler from url params
  (db/update-entity :user data))
