(ns cpe.custommongodbfn
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! take! chan promise-chan <! close!]]
            [clojure.string :as str]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.mongodb :as mongo]
            [cpe.entity :refer [from-db to-db attr-key-db schema-db]]
            )
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn find-unit-system [entity-key]
  (mongo/find-entities :unit-system #js{}))

(defn find-document-by-name [entity-key field-name]
  (mongo/find-entities :misc #js{:name field-name}))

(defn find-sensor-by-name [entity-key field]
  ;;(mongo/find-one entity-key #js{:name field})
  ;(println "entity-key:" entity-key "field:" field)
  (if field
    (mongo/find-entities entity-key (clj->js {:name {:$in field}}))
    (mongo/find-entities entity-key #js{})
    )

  ;:#js{:$in field }
  )

(defn get-charts [entity-key]
  (mongo/find-entities :chart #js{}))

(defn find-one [entity-key query]
  (mongo/find-one entity-key query))

(defn get-comment-by-plant-id [entity-key {:keys [plant-id]}]
  (let [ret-c (promise-chan)
        c (promise-chan)
        agg (clj->js
              [{:$match {:plantId plant-id}}
               {:$project {:numberOfComment            {:$size "$comments"}
                           :_id                        1,
                           :dateCreated                1,
                           :typeId                     1,
                           :plantId                    1,
                           :lastRead                   1,
                           :createdBy                  1,
                           :chartInfo                  1,
                           :lastPost                   1,
                           :subject                    1,
                           :includeInReport            1,
                           :includeInReportPermanently 1}}])]
    (-> (mongo/coll entity-key)
        (u/ocall :aggregate agg)
        (u/ocall :toArray #(put! c %&)))
    (go
      (let [[err result] (<! c)]
        (if err
          (mongo/ret-err ret-c err)
          (put! ret-c {:result
                       (mapv #(from-db :comment %) result)}))))
    ret-c))

(defn get-comments-with-reply [entity-key {:keys [plant-id]}]
  #_(mongo/find-entities entity-key #js{:plantId plant-id})
  (let [ret-c (promise-chan)
        c (promise-chan)
        agg (clj->js
              [{:$match {:plantId plant-id}}
               {:$project {:numberOfComment            {:$size "$comments"}
                           :_id                        1,
                           :dateCreated                1,
                           :typeId                     1,
                           :plantId                    1,
                           :lastRead                   1,
                           :createdBy                  1,
                           :chartInfo                  1,
                           :lastPost                   1,
                           :subject                    1,
                           :includeInReport            1,
                           :includeInReportPermanently 1,
                           :comments 1
                           }}])]
    (-> (mongo/coll entity-key)
        (u/ocall :aggregate agg)
        (u/ocall :toArray #(put! c %&)))
    (go
      (let [[err result] (<! c)]
        (if err
          (mongo/ret-err ret-c err)
          (put! ret-c {:result
                       (mapv #(from-db :comment %) result)}))))
    ret-c))

(defn update-read [entity-key value id]
  (let [ret-c (promise-chan)]
  (go
    (let [last-update (<! (mongo/get-entity-by-id :comment id))
          last-read   (get-in last-update [:result :last-read])
          new-data    (mapv (fn [d]
                              (if (= (d :read-by) (value :read-by))
                                d)) last-read)
          data        (remove nil? new-data)
          response-data (if (not-empty data)
                          (<! (mongo/update-comment entity-key
                                #js{:$set #js{:lastRead.$.readOn (js/Date.)}}
                                #js{:_id id, :lastRead.readBy (value :read-by)} ))
                          (<! (mongo/update-comment entity-key
                                #js{:$push #js{:lastRead (to-db :comment/last-read value) }}
                                #js{:_id id} )))
          {:keys [err msg res]} response-data
          ]
      (if err
        (mongo/ret-err ret-c err)
        (if (zero? (u/oget res :n))
          (put! ret-c {:err :db-not-found})
          (put! ret-c {:result
                       {:ok?       (pos? (u/oget res :ok))
                        :modified? (pos? (u/oget res :nModified))}})))

      ))ret-c))

(defn get-reply-by-comment-id [entity-key {:keys [id]}]
  (mongo/find-one entity-key #js{:_id id}))

(defn get-comment-by-comment-id [entity-key {:keys [id]}]
  (mongo/find-one entity-key #js{:_id id} {:project #js{:comments 0}}))

;(defn update-reply [entity-key set push id]
;  (let [ret-chan (promise-chan)]
;    (go
;      (let [{:keys [err msg result]} (<! (mongo/update-entity entity-key set #js{:_id id}))]
;        (if err
;          (put! ret-chan {:err err :msg msg})
;          (do
;            (let [{:keys [err msg result]} (<! (mongo/update-reply entity-key push #js{:_id id}))]
;              (if err
;                (put! ret-chan {:err err :msg msg})
;                (put! ret-chan {:result result})))))))
;    ret-chan))

(defn find-uom [entity-key]
  (mongo/find-entities entity-key #js{}))

(defn update-comment [entity-key data id]
  (mongo/update-entity entity-key data #js{:_id id}))

#_(defn update-comment-bulk [entity-key data id]
  (let [entity-key :comment
        data nil
        query #js[#js{:updateMany #js{:filter #js{:_id "5b5b064c06ae931960e9ff8a"}
                                     :update #js{:$set #js{:includeInReport true}}
                                     :upsert true}}]]
    (println "\nentity-key:" entity-key)
    (println "\ndata:" data)
    (println "\nquery:" query)
    (mongo/update-many-entities entity-key data query)))

(defn get-sections [entity-key]
  (mongo/find-entities entity-key #js{}))

;(defn find-plants [query]
;  (mongo/find-entities :sap-plant #js{:clientId client-id}))


(defn find-clients [query opts]
  (let [{:keys [plant?]} opts
        query (->> query
                (map (fn [[k v]]
                       [k (if (or (= k :country) (nil? k))
                            v (re-pattern (str "(?i)" v)))]))
                (into {})
                (to-db :client/query))]
    (if plant?
      ;; search only clients with plants
      (let [ret-c (promise-chan)
            c     (promise-chan)
            agg   (clj->js
                    [{:$match query}
                     {:$lookup {:from         "plants"
                                :localField   "_id"
                                :foreignField "clientId"
                                :as           "plants"}}
                     {:$match {:plants {:$ne []}}}
                     {:$project {:plants 0}}
                     {:$sort {:name 1}}
                     {:$skip (or (:skip opts) 0)}
                     {:$limit (or (:limit opts) 30)}])]
        (-> (mongo/coll :sap-client)
          (u/ocall :aggregate agg)
          (u/ocall :toArray #(put! c %&)))
        (go
          (let [[err result] (<! c)]
            (if err
              (mongo/ret-err ret-c err)
              (put! ret-c {:result (mapv #(from-db :sap-client %) result)}))))
        ret-c)
      ;; search any client including those without plants

      (let [opts (assoc (dissoc opts :plant?) :sort #js{:name 1})]
        (mongo/find-entities :sap-client query opts)))))


(defn find-plants [{:keys [client-id]}]
  (mongo/find-entities :sap-plant #js{:clientId client-id}))


(defn get-reply-by-time
  [{:keys [id date-created]}]
  (log/trace "Add comment by commentId:" id "createdTime" (pr-str date-created))
  (let [ret-c (promise-chan)
        c     (promise-chan)
        agg   (clj->js
                [{:$project {:comments 1,
                             :lastPost 1}},
                 {:$unwind {:path "$comments"}},
                 {:$match {:_id                   id,
                           "comments.dateCreated" {:$gt date-created,
                                                   :$lt (js/Date.)}}}])]
    (-> (mongo/coll :comment)
      (u/ocall :aggregate agg)
      (u/ocall :toArray #(put! c %&)))
    (go
      (let [[err result] (<! c)]
        (if err
          (mongo/ret-err ret-c err)
          (put! ret-c {:result {
                                :last-post (get (first (js->clj result :keywordize-keys true)) :lastPost)
                                :comments (mapv (fn [d]
                                                   (-> (from-db :comment/db-data-parsing d)
                                                     (get :comments)
                                                     )) result)
                                }}))))
    ret-c))


(defn get-report-summary-by-plant-id [entity-key {:keys [plant-id]}]
  (mongo/find-entities entity-key #js{:plantId plant-id} {:project #js{}}))

(defn get-report-summary-by-id [entity-key {:keys [id]}]
  (mongo/find-one entity-key #js{:_id id}))
