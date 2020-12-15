(ns cpe.handler
  "all handlers respond in a common manner:
 ```clojure
 {:success <true or false>
  :error   <error key word>
  :message <error description>
  :result  <payload>}
 ```"
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! <! promise-chan chan]]
            [cpe.util :as u :refer [res-ok res-bad]]
            [cpe.log :as log]
            [cpe.entity :refer [from-api to-api]]
            [cpe.model.user :as user]
            [cpe.model.plant :as plant]
            [cpe.model.client :as client]
            [cpe.model.unit-system :as unit]
            [cpe.model.chart :as chart]
            [cpe.model.section :as section]
            [cpe.model.sensor :as sensor]
            [cpe.model.comment :as comment]
            [cpe.model.summary :as summary]
            [cpe.model.uom :as uom]
            [cpe.model.misc :as misc]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common manner functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- return-one [res ekey entity]
  (let [e (to-api ekey entity)]
    (res-ok res e)))

(defn- return-coll [res ekey entities]
  (let [es (to-array (map #(to-api ekey %) entities))]
    (res-ok res es)))

(defn- serve
  "parser: (fn [params query body])"
  [req res ekey serve-fn parser]
  (let [claims (u/oget req :claims)
        data (parser (js->clj (u/oget req :params)
                              :keywordize-keys true)
                     (u/oget req :query)
                     (u/oget req :body))]
    (go
      (let [{:keys [err msg result]} (<! (serve-fn data claims))]
        (if err
          (res-bad res msg err)
          (return-one res ekey result))))))

(defn- get-entity
  [req res ekey get-fn]
  (let [claims (u/oget req :claims)
        id-key (keyword (str (name ekey) "Id"))
        id (u/oget-in req [:params id-key])
        name (u/oget-in req [:params :name])
        pcid (u/oget-in req [:params :clientId])]
    (go
      (let [{:keys                        [err]
             {ecid :client-id :as entity} :result} (<! (get-fn
                                                         (if id
                                                           id
                                                           name) claims))
            entity (if (or (not pcid) (not ecid) (= pcid ecid)) entity)]
        (if err
          (res-bad res)
          (return-one res ekey entity))))))

(defn- find-entities
  "parser: (fn [params query])"
  [req res ekey search-fn query-parser]
  (let [claims (u/oget req :claims)
        query (query-parser (js->clj (u/oget req :params)
                                     :keywordize-keys true)
                            (u/oget req :query))
        ]
    (go
      (let [{:keys [err result]} (<! (search-fn query claims))]
        (if err
          (res-bad res)
          (return-coll res ekey result))))))

(defn- delete-entity [req res ekey delete-fn]
  (let [claims (u/oget req :claims)
        id-key (keyword (str (name ekey) "Id"))
        id (u/oget-in req [:params id-key])
        cid (u/oget-in req [:params :clientId])
        pid (u/oget-in req [:params :plantId])]
    (go
      (let [{:keys [err msg result]}
            (<! (delete-fn {:id        id
                            :client-id cid
                            :plant-id  pid}
                           claims))]
        (if err
          (res-bad res msg err)
          (res-ok res (to-api :res/delete result)))))))

;;;;;;;;;;
;; USER ;;
;;;;;;;;;;

(defn get-user
  "GET /user/:userId
  res body: {:result user}"
  [req res]
  (get-entity req res :user user/get-by-id))

(defn create-user
  "POST /user
  req body: user
  res body: {:result res-create}"
  [req res]
  (->>
    (fn parse-user-post [_ _ body]
      (from-api :user body))
    (serve req res :res/create user/create)))

(defn update-user
  "PUT /user/:userId
  req body: user
  res body: {:result res-update}"
  [req res]
  (->>
    (fn parse-user-put [{id :userId} _ body]
      (-> (from-api :user body)
          (assoc :id id)))
    (serve req res :res/update user/update-fields)))

;;;;;;;;;;;;
;; CLIENT ;;
;;;;;;;;;;;;

(defn get-client
  "GET /client/:clientId
  res body: {:result client}"
  [req res]
  (get-entity req res :client client/get-by-id))

(defn get-client-search-options
  "GET /client/search-options
  res body: {:result search-options}"
  [req res]
  (->>
    (fn dummy [_ _ _] nil)
    (serve req res :client/search-options client/get-search-options)))

(defn find-clients
  "GET /client?name=_&shortName=_&location=_&country=_&skip=_&limit=_&withPlants=_
  res body: {:result [client..]}"
  [req res]
  (->>
    (fn parse-client-query [_ query]
      (from-api :client/query query))
    (find-entities req res :client client/search)))

(defn create-client
  "POST /client
  req body: client
  res body: {:result res-create}"
  [req res]
  (->>
    (fn parse-client-post [_ _ body]
      (from-api :client body))
    (serve req res :res/create client/create)))

(defn update-client
  "PUT /client/:clientId
  req body: client
  res body: {:result res-update}"
  [req res]
  (->>
    (fn parse-client-put [{id :clientId} _ body]
      (-> (from-api :client body)
          (assoc :id id)))
    (serve req res :res/update client/update-fields)))

;;;;;;;;;;;
;; PLANT ;;
;;;;;;;;;;;
(defn get-plant
  "GET /client/:clientId/plant/:plantId
  res body: {:result plant}"
  [req res]
  (get-entity req res :plant plant/get-by-id))

(defn find-plants
  "GET /client/:clientId/plant
  res body: {:result [plant..]}"
  [req res]
  (->>
    (fn parse-plant-query [{cid :clientId} _]
      {:client-id cid})
    (find-entities req res :plant plant/search)))

(defn create-plant
  "POST /client/:clientId/plant
  req body: plant
  res body: {:result res-create}"
  [req res]
  (->>
    (fn parse-plant-post [{client-id :clientId} _ body]
      (-> (from-api :plant body)
          (assoc :client-id client-id)))
    (serve req res :res/create plant/create)))

; (defn update-plant-uom-settings
;   "PUT /client/:clientId/plant/:plantId
;   req body: settings (schema :plant/update-settings)
;   res body: {:result res-update}"
;   [req res]
;   (->>
;     (fn parse-plant-settings [{id  :plantId
;                                cid :clientId} _ body]
;       {:id        id
;        :client-id cid
;        :data      (from-api :plant/update-settings body)})

;     (serve req res :res/update plant/update-settings)
;     ))

; (defn update-plant-pin-chart-settings
;   "PUT /client/:clientId/plant/:plantId
;   req body: settings (schema :plant/update-settings)
;   res body: {:result res-update}"
;   [req res]
;   (->>
;     (fn parse-plant-settings [{id  :plantId
;                                cid :clientId} _ body]
;       {:id        id
;        :client-id cid
;        :data      (from-api :plant/update-settings body)})

;     (serve req res :res/update plant/update-settings)
;     ))

(defn update-plant-settings
  "PUT /client/:clientId/plant/:plantId
  req body: settings (schema :plant/update-settings)
  res body: {:result res-update}"
  [req res]
  (->>
    (fn parse-plant-settings [{id  :plantId
                               cid :clientId} _ body]
      {:id        id
       :client-id cid
       :data      (from-api :plant/update-settings body)})

    (serve req res :res/update plant/update-settings)
    ))

(defn update-plant-config
  "PUT /client/:clientId/plant/:plantId
  req body: settings (schema :plant/update-settings)
  res body: {:result res-update}"
  [req res]
  (->>
    (fn parse-plant-settings [{id  :plantId
                               cid :clientId} _ body]
      {:id        id
       :client-id cid
       :data      (from-api :plant/update-config body)})

    (serve req res :res/update plant/update-config)
    ))


;;;;;;;;;;;;;;;;;
;; UNIT SYSTEM ;;
;;;;;;;;;;;;;;;;;

(defn get-unit-system
  "GET /unitSystem/
 res body: {:result [unit..]}"
  [req res]
  (->>
    (fn parse-unit-system-query [_ query]
      (from-api :unit-system query))
    (find-entities req res :unit-system unit/get-unit-system)))

(defn find-unit-system-by-name
  "GET /unitSystem/:name
  res body: {:result unit}"
  [req res]
  (get-entity req res :unit-system unit/find-unit-system-by-name))

;;;;;;;;;;;;;;;;;
;;     UOM     ;;
;;;;;;;;;;;;;;;;;

(defn get-uom
  "GET /uom/
 res body: {:result [uom..]}"
  [req res]
  (->>
    (fn parse-uom-query [_ query]
      (from-api :uom query))
    (find-entities req res :uom uom/get-uom)))


;;;;;;;;;;;;;;;;;
;;  UNIT LIST  ;;
;;;;;;;;;;;;;;;;;
;;documentName
(defn get-document-by-name
  "GET /misc/:documentName
  res body: {:result [unit..]}"
  [req res]
  (->>
    (fn parse-document-by-name-query [{name :documentName} query]
      (assoc (from-api :misc query) :name name))
    (find-entities req res :misc misc/get-document-by-name)))

(defn create-document
  "POST /misc
  req body: misc
  res body: {:result res-create}"
  [req res]
  (->>
    (fn parse-document-post [_ _ body]
      (from-api :misc body))
    (serve req res :res/create misc/create-document)))

(defn update-document-by-name
  "PUT /misc/:documentName
  req body: :documentName
  res body: {:result res-update}"
  [req res]
  (->>
    (fn parse-unit-list-put [{name :documentName} _ body]
      (-> (from-api :misc body)
          (assoc :name name)))
    (serve req res :res/update misc/update-document)))


;;;;;;;;;;;;;;;;;
;;;; SENSOR ;;;;;
;;;;;;;;;;;;;;;;;

(defn get-sensor-by-name
  "GET /sensor/detail
  res body: {:result sensor}"
  [req res]
  (->>
    (fn parse-sensor-query [_ query]
      (from-api :sensor/query query))
    (find-entities req res :sensor sensor/get-sensor)))

(defn get-sensor-value
  "GET /sensor/
 res body: {:result [sensor..]}"
  [req res]
  (->>
    (fn parse-sensor-query [_ query]
      (let [clj-query-data (js->clj query :keywordize-keys true)
            new-query  #js{:name  (str/split (clj-query-data :name) #",")
                           :startDate (clj-query-data :startDate)
                           :plantId (clj-query-data :plantId)}]
        (from-api :sensor/query new-query)))
    (find-entities req res :sensor-value sensor/get-sensor-value)))

(defn export-excel-data
  "GET /sensor/
 res body: {:result [sensor..]}"
  [req res]
  (->>
    (fn parse-sensor-query [_ query]
      (let [clj-query-data (js->clj query :keywordize-keys true)
            new-query  #js{:name  (str/split (clj-query-data :name) #",")
                           :startDate (clj-query-data :startDate)
                           :plantId (clj-query-data :plantId)}]
        (from-api :sensor/query new-query)))
    (find-entities req res :export-data sensor/export-excel-data)))

;;;;;;;;;;;;;;;;;
;;;; Section ;;;;
;;;;;;;;;;;;;;;;;

(defn get-sections
  "GET /section/
 res body: {:result [section..]}"
  [req res]
  (->>
    (fn parse-section-query [_ query]
      (from-api :section query))
    (find-entities req res :section section/get-sections)))


(defn get-sections-by-id
  "GET /section/:id
  res body: {:result section}"
  [req res]
  (get-entity req res :section section/get-section))

;;;;;;;;;;;;;;;;;
;;;;; Chart ;;;;;
;;;;;;;;;;;;;;;;;

(defn get-charts
  "GET /chart/
  res body: {:result [chart..]}"
  [req res]
  (->>
    (fn parse-chart-query [_ query]
      (from-api :chart query))
    (find-entities req res :chart chart/get-charts)))


(defn get-chart-by-id
  "GET /chart/:id
  res body: {:result chart}"
  [req res]
  (get-entity req res :chart chart/get-chart-by-id)
  )

;;;;;;;;;;;;;;;;;
;;;; Comment ;;;;
;;;;;;;;;;;;;;;;;

(defn create-comment-by-plant-id
  "POST /comment
  req body: comment
  res body: {:result res-create}"
  [req res]
  (->>
    (fn parse-user-post [{pid :plantId} _ body]
      (-> (from-api :comment body)
          (assoc :plant-id pid)))
    (serve req res :res/create comment/create-comment-by-plant-id)))

(defn get-comment-by-plant-id
  "GET /comment/:id
  res body: {:result comment}"
  [req res]
  (->>
   (fn parse-user-post [{pid :plantId} query]
     (-> (from-api :comment query)
         (assoc :plant-id pid)))
   (find-entities req res :comment comment/get-comment-by-plant-id)))

(defn get-comments-with-reply
  "GET /comment/withReply
  res body: {:result comment}"
  [req res]
  (->>
    (fn parse-user-post [{pid :plantId} query]
      (-> (from-api :comment query)
          (assoc :plant-id pid)))
    (find-entities req res :comment comment/get-comments-with-reply)))

(defn get-reply-by-comment-id
  "GET /comment/:id/comments
  res body: {:result comment}"
  [req res]
  (->>
    (fn parse-user-post [{cid :commentId} query]
      (-> (from-api :comment query)
          (assoc :id cid)))
    (find-entities req res :comment/reply comment/get-reply-by-comment-id)))

(defn get-reply-by-comment-time
  "GET /comment/:id/comments
res body: {:result comment}"
  [req res]
  (->>
    (fn parse-user-post [{cid :commentId} query]
      (-> (from-api :comment query)
          (assoc :id cid)))
    (serve  req res :comment comment/get-reply-by-time)) )

(defn get-comment-by-comment-id
  "GET /comment/:id/comments
  res body: {:result comment}"
  [req res]
  (->>
    (fn parse-comment [{cid :commentId} query]
      (-> (from-api :comment query)
          (assoc :id cid)))
    (serve req res :comment comment/get-comment-by-comment-id)))

(defn update-comment
  [req res]
  (->>
    (fn parse-client-put [{id :commentId} _ body]
      (-> (from-api :comment body)
          (assoc :id id)))
    (serve req res :res/update comment/update-comment)))

(defn update-read
  [req res]
  (->>
    (fn parse-client-put [{id :commentId} _ body]
      (-> (from-api :comment body)
          (assoc :id id)))
    (serve req res :res/update comment/update-read)))

(defn update-reply
  [req res]
  (->>
    (fn parse-client-put [{id :commentId
                           plant-id :plantId} _ body]
      (-> (from-api :comment/reply body)
          (assoc :id id)
        (assoc :plant-id plant-id)))
    (serve req res :res/update comment/add-reply)))

(defn update-include-in-report
  [req res]
  (->>
   (fn parse-comment-put [{id :commentId
                           plant-id :plantId} _ body]
     (-> (from-api :comment body)
         (assoc :id id)
         (assoc :plant-id plant-id)))
   (serve req res :res/update comment/update-include-in-report)))

(defn update-include-in-report-multi
  [req res]
  (->>
   (fn parse-comment-put [{plant-id :plantId} _ body]
     (-> (from-api :comment body)
         (assoc :plant-id plant-id)))
   (serve req res :res/update comment/update-include-in-report-multi)))

;;;;;;;;;;;;;;;;;
;;;; Misc ;;;;
;;;;;;;;;;;;;;;;;


(defn get-misc
  "GET /misc/
  res body: {:result [misc..]}"
  [req res]
  (->>
    (fn parse-misc [_ query]
      (from-api :misc query))
    (find-entities req res :misc misc/get-misc)))

;;;;;;;;;;;;;
;; summary ;;
;;;;;;;;;;;;;
(defn create-report-summary
  "POST /summary
  req body: summary
  res body: {:result res-create}"
  [req res]
  (->>
   (fn parse-summary [{pid :plantId} _ body]
     (-> (from-api :summary body)
         (assoc :plant-id pid)))
   (serve req res :res/create summary/create-report-summary)))

(defn get-report-summary
  "GET /summary
  res body: {:result summary}"
  [req res]
  (->>
   (fn parse-summary [{pid :plantId} query]
     (-> (from-api :summary query)
         (assoc :plant-id pid)))
   (find-entities req res :summary summary/get-report-summary-by-plant-id)))

(defn get-report-summary-by-id
  "GET /summary/:summaryId
  res body: {:result summary}"
  [req res]
  (get-entity req res :summary summary/get-report-summary-by-id))

(defn update-report-summary
  [req res]
  (->>
   (fn parse-summary [{id :summaryId} _ body]
     (-> (from-api :summary body)
         (assoc :id id)))
   (serve req res :res/update summary/update-report-summary)))

(defn publish-report-summary
  [req res]
  (->>
   (fn parse-summary [{id :summaryId} _ body]
     (-> (from-api :summary body)
         (assoc :id id)))
   (serve req res :res/update summary/publish-report-summary)))

(defn delete-report-summary
  [req res]
  (->>
   (fn parse-summary [{id :summaryId} _ body]
     (-> (from-api :summary body)
         (assoc :id id)))
   (serve req res :res/update summary/delete-report-summary)))
