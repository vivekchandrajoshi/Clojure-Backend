(ns cpe.model.summary
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cljs-time.core :as t]
            [cljs-time.coerce :as tc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.entity :refer [from-db to-db attr-key-db schema-db]]
            [cpe.custommongodbfn :as c]
            [cpe.mail :as mail]))

(defn create-report-summary [data claims]
  (log/trace "create report summary by plantId: claims:" (pr-str claims) ", data:" (pr-str data))
  (db/insert-entity :summary (-> data
                                 (assoc :status "draft"
                                        :created-by    (:id claims)
                                        :date-created  (js/Date.)
                                        :modified-by   (:id claims)
                                        :date-modified (js/Date.)))))

(defn get-report-summary-by-plant-id
  [query]
  (log/trace "get report summary by plantId: query: " query)
  (c/get-report-summary-by-plant-id :summary query))

(defn get-report-summary-by-id
  [query _]
  (log/trace "get report summary by id: " (pr-str query))
  (db/get-entity-by-id :summary query))

(defn update-report-summary [data claims]
  (log/trace "update report summary: data:" (pr-str data))
  ;; data includes the :id as added by handler from url params
  (db/update-entity :summary
                    (assoc {}
                           :subject (:subject data)
                           :modified-by (:id claims)
                           :date-modified (js/Date.))
                    #js{:_id (:id data)}))

(defn publish-report-summary [data claims]
  (log/trace "publish report summary: data:" (pr-str data))
  ;; data includes the :id as added by handler from url params
  (db/update-entity :summary
                    (assoc {}
                           :status "published"
                           :published-by (:id claims)
                           :date-published (js/Date.))
                    #js{:_id (:id data)}))

(defn delete-report-summary [data claims]
  (log/trace "delete report summary: data:" (pr-str data))
  ;; data includes the :id as added by handler from url params
  (db/delete-entity :summary #js{:_id (:id data)}))
