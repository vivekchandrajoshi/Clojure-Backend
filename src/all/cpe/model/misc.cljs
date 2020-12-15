(ns cpe.model.misc
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.custommongodbfn :as c]))

(defn get-document-by-name [data claim]
  (log/trace "get-document-by-name: query " (pr-str data))
  (c/find-document-by-name :misc (:name data)))

(defn create-document [data claims]
  (log/trace "create document:" (pr-str data))
  (db/insert-entity :misc data))

(defn update-document [data _]
  (log/trace "update document: data:" (pr-str data))
  ;; data includes the :id as added by handler from url params
  (db/update-entity :misc data #js{:name (:name data)}))

(defn get-misc [data claim]
  (log/trace "get-document-by-name: query " (pr-str data))
  (db/find-entities :misc #js{}))