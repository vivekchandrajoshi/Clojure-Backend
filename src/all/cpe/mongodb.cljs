(ns cpe.mongodb
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [put! take! chan promise-chan <! close!]]
            [clojure.string :as str]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.config :refer [config]]
            [cpe.entity :refer [from-db to-db attr-key-db schema-db]]
            )
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defonce mongo-db (nodejs/require "mongodb"))

(defonce mongo-client (atom nil))

(defonce app-db (atom nil))

(defn init []
  (let [ret-chan (promise-chan)
        {:keys [uri uri-params db] :as db-spec} (get-in @config [:db-spec :mongo])
        db-uri (reduce-kv (fn [uri k p]
                            (str/replace uri p (or (get db-spec k) "")))
                          uri
                          uri-params)]
    (doto (u/oget mongo-db :MongoClient)
      (u/ocall :connect db-uri #js{:ignoreUndefined true}
               (fn [err res]
                 (if err
                   (log/error "Failed to initialize mongo database!" err)
                   (log/info "Connected to mongo database."))
                 (when-let [client (if-not err res)]
                   (reset! mongo-client client)
                   (reset! app-db (u/ocall client :db db)))
                 (put! ret-chan {:err err}))))
    ret-chan))


;; db schema tells where to find the entity
;;  :db-name - the database name, default: current database
;;  :coll - the collection name

(defonce db-schema
         {:sap-client  {:db-name "sap"
                        :coll    "clients"}
          :sap-plant   {:db-name "sap"
                        :coll    "plants"}

          :user        {:coll "user"}
          :client      {:coll "client"}
          :plant       {:coll "plant"}
          :chart       {:coll "chart"}
          :section     {:coll "section"}
          :sensor      {:coll "sensor"}
          :unit-system {:coll "unitSystem"}
          :uom         {:coll "uom"}
          :comment     {:coll "comment"}
          :misc        {:coll "misc"}
          :summary     {:coll "summary"}})

(defn new-id
  "Returns a new unique Object Id from mongodb. This can be used to
  explicitly set the id while inserting new documents."
  []
  (str (u/ocall mongo-db :ObjectID)))

(defn coll
  "Returns the MongoDb collection instance where the desired entity is kept."
  [entity-key]
  (if-let [client @mongo-client]
    (let [{:keys [db-name coll]} (get db-schema entity-key)]
      (if coll
        (->
          (if db-name
            (u/ocall client :db db-name)
            @app-db)
          (u/ocall :collection coll))
        (throw (ex-info "Invalid entity key!" {:entity-key entity-key}))))
    (throw (ex-info "Database not connected!" {}))))

(defn ret-err [c err]
  (put! c
        (-> {:err (->> (u/oget err :code)
                       (str "db-") keyword)
             :msg (u/oget err :message)}
            (update :err #(case %
                            :db-11000 :db-duplicate
                            %)))))

(defn get-entity-by-id
  "Retrieve an entity from database.  
  Usage:  
    (get-entity-by-id entity-key id)  
    (get-entity-by-id entity-key id fields)  
  *entity-key* is the key of desired entity  
  *id* is the entity id  
  *fields* is an object of fields to include or exclude (not both), #js {:a 1}  
  Returns a channel from which you can read the result map:  
    {:err <error keyword if any>  
     :result <entity data map>}"
  ([entity-key id]
   (get-entity-by-id entity-key id nil))
  ([entity-key id fields]
   ;(print entity-key "entity-key")
   (let [ret-c (promise-chan)
         c (promise-chan)]
     (-> (coll entity-key)
         (u/ocall :findOne #js{:_id id} #js{:projection fields} #(put! c %&)))
     (go
       (let [[err res] (<! c)]
         ;(print res "mongo res ->>>>")
         (if err
           (ret-err ret-c err)
           (put! ret-c {:result (from-db entity-key res)}))))
     ret-c)))

(defn find-entities
  "Retrieve entitites of a given key.  
  Usage:  
    (find-entities entity-key)  
    (find-entities entity-key query)  
    (find-entities entity-key query opts)  
  *entity-key* is the key of desired entity  
  *query* is an object specifying query conditions  
  *opts* is a map of options  
    {:sort <object specifying sort cond>  
     :skip <number of items to skip>  
     :limit <maximum number of items to return>  
     :project <object for fields projection>}  
  Returns a channel from which you can read the result map:  
    {:err <error keyword if any>  
     :result [e1, e2, e3, ...]}"
  ([entity-key]
   (find-entities entity-key nil nil))
  ([entity-key query]
   (find-entities entity-key query nil))
  ([entity-key query opts]
   (let [ret-c (promise-chan)
         c (promise-chan)
         {:keys [sort skip limit project]} opts]
     ;(print "entity-key" entity-key "query" query, "opts" opts)


     (cond-> (coll entity-key)
             true (u/ocall :find query)
             project (u/ocall :project project)
             sort (u/ocall :sort sort)
             skip (u/ocall :skip skip)
             limit (u/ocall :limit limit)
             true (u/ocall :toArray #(put! c %&)))
     (go
       (let [[err res] (<! c)]
         (if err
           (ret-err ret-c err)
           (put! ret-c {:result (mapv #(from-db entity-key %) res)}))))
     ret-c)))

(defn insert-entity
  "Insert an entity of a given key  
  *entity-key* is the key of desired entity  
  *data* is entity to insert  
  On duplicate, err key :db-11000 is returned!  
  Returns {:ok? true, :new-id id}"
  [entity-key data]
  (let [ret-c (promise-chan)
        c (promise-chan)
        id (or (not-empty (:id data)) (new-id))
        data (->> (assoc data :id id)
                  (to-db entity-key))]
    (-> (coll entity-key)
        (u/ocall :insert data #(put! c %&)))
    (go
      (let [[err res] (<! c)]
        (if err
          (ret-err ret-c err)
          (put! ret-c {:result
                       {:ok?    (pos? (u/oget-in res [:result :ok]))
                        :new-id (u/oget-in res [:insertedIds :0])}}))))
    ret-c))

(defn insert-many-entities
  "Insert many entity of a given key
  *entity-key* is the key of desired entity
  *data* is list of entities to insert
  On duplicate, err key :db-11000 is returned!
  Returns {:ok? true, :n <number of entity inserted>}"
  [entity-key data]
  (let [ret-c (promise-chan)
        c (promise-chan)
        new-data (->> data
                      (map (fn [e] (update e :id #(or (not-empty %) (new-id)))))
                      (map #(to-db entity-key %))
                      (to-array))]
    (-> (coll entity-key)
        (u/ocall :insertMany new-data #js{:forceServerObjectId true} #(put! c %&)))
    (go
      (let [[err res] (<! c)]
        (if err
          (ret-err ret-c err)
          (put! ret-c {:result
                       {:ok?     (pos? (u/oget-in res [:result :ok]))
                        :new-ids (js->clj (u/oget res :insertedIds))}}))))
    ret-c))

(defn- include-nil-values
  "include keys in data with nil values as well for sending to db"
  [ekey data set-data]
  (doseq [k (->> (select-keys (schema-db ekey) (keys data))
                 (vals)
                 (map #(if (map? %) (:name %) %))
                 (map #(if (keyword? %) (name %) %)))]
    (if-not (u/ocall set-data :hasOwnProperty k)
      (u/oset set-data k nil))))

(defn update-entity
  "Update an entity of a given key  
  *entity-key* is the key of desired entity  
  *data* is the map of field to value as need to be updated  
  *query* is the criteria to match  
  If no matches, err key :db-not-found is returned!  
  Returns {:ok? true, :modified? true}"
  ([entity-key data]
   (update-entity entity-key data #js{:_id (:id data)}))
  ([entity-key data query]
   (let [ret-c (promise-chan)
         c (promise-chan)
         set-data (to-db entity-key data)]
     (include-nil-values entity-key data set-data)
     (-> (coll entity-key)
         (u/ocall :updateOne query #js{:$set set-data} #(put! c %&)))
     (go
       (let [[err res] (<! c)
             res (if res (u/ocall res :toJSON))]
         (if err
           (ret-err ret-c err)
           (if (zero? (u/oget res :n))
             (put! ret-c {:err :db-not-found})
             (put! ret-c {:result
                          {:ok?       (pos? (u/oget res :ok))
                           :modified? (pos? (u/oget res :nModified))}})))))
     ret-c)))

(defn update-many-entities [entity-key data query]
  (let [ret-c (promise-chan)
        c (promise-chan)]
    (-> (coll entity-key)
        (u/ocall :bulkWrite query #(put! c %&)))
    (go
      (let [[err res] (<! c)
            res (if res (u/ocall res :toJSON))]
        (if err
          (ret-err ret-c err)
          (if (zero? (u/oget res :nModified))
            (put! ret-c {:err :db-not-found})
            (put! ret-c {:result
                         {:ok?       (pos? (u/oget res :ok))
                          :modified? (pos? (u/oget res :nModified))}})))))))

(defn delete-entity
  "Delete an entity of a given key  
  *entity-key* is the key of desired entity  
  *query* is the criteria to match  
  If no matches, err key :db-not-found is returned!  
  Returns {:ok? true}"
  [entity-key query]
  (let [ret-c (promise-chan)
        c (promise-chan)]
    (-> (coll entity-key)
        (u/ocall :deleteOne query #(put! c %&)))
    (go
      (let [[err res] (<! c)
            res (u/ocall res :toJSON)]
        (if err
          (ret-err ret-c err)
          (put! ret-c {:result {:ok?    (pos? (u/oget res :ok))
                                :found? (pos? (u/oget res :n))}}))))
    ret-c))

(defn delete-many-entities
  "Delete multiple entities of a given key if found  
  *entity-key* is the key of desired entity  
  *query* is the criteria to match  
  Returns {:ok? true, :count <count of docs deleted>}"
  [entity-key query]
  (let [ret-c (promise-chan)
        c (promise-chan)]
    (-> (coll entity-key)
        (u/ocall :deleteMany query #(put! c %&)))
    (go
      (let [[err res] (<! c)
            res (u/ocall res :toJSON)]
        (if err
          (ret-err ret-c err)
          (put! ret-c {:result {:ok?   (pos? (u/oget res :ok))
                                :count (u/oget res :n)}}))))
    ret-c))

(defn distinct-values
  "Returns distinct values of given field in a collection  
  *entity-key* is the key of desired entity  
  *field* whose values to be distinct  
  *query* the criteria to match"
  ([entity-key field ]
   (distinct-values entity-key field nil))
  ([entity-key field query]
   (let [ret-c (promise-chan)
         c (promise-chan)
         field (as-> (get (schema-db entity-key) field) $
                     (if (map? $) (:name $) $)
                     (if (keyword? $) (name $) $))]
     (-> (coll entity-key)
         (u/ocall :distinct field query #(put! c %&)))
     (go
       (let [[err res] (<! c)]
         (if err
           (ret-err ret-c err)
           (put! ret-c {:result (vec res)}))))
     ret-c)))

(defn find-one
  "Retrieve an entity from database.
  Usage:
    (find-one entity-key id)
    (find-one entity-key id fields)
  *entity-key* is the key of desired entity
  *id* is the entity id
  *fields* is an object of fields to include or exclude (not both), #js {:a 1}
  Returns a channel from which you can read the result map:
    {:err <error keyword if any>
     :result <entity data map>}"
  ([entity-key query]
   (find-one entity-key query nil))
  ([entity-key query opts]
   (let [ret-c (promise-chan)
         c (promise-chan)
         {:keys [sort skip limit project]} opts]
     (-> (coll entity-key)
         (u/ocall :findOne query #js{:projection project} #(put! c %&)))
     (go
       (let [[err res] (<! c)]
         (if err
           (ret-err ret-c err)
           (put! ret-c {:result (from-db entity-key res)}))))
     ret-c)))

(defn add-reply [entity-key data query]
  (let [ret-c (promise-chan)
        c (promise-chan)
        d #js{:comments (to-db :comment/reply data)}]
    (-> (coll entity-key)
        (u/ocall :updateOne #js{:_id query} #js{:$push d} #(put! c %&)))
    (go
      (let [[err res] (<! c)
            result (if res (u/ocall res :toJSON))]
        (if err
          (ret-err ret-c err)
          (if (zero? (u/oget result :n))
            (put! ret-c {:err :db-not-found})
            (put! ret-c {:result
                         {:ok?       (pos? (u/oget result :ok))
                          :modified? (pos? (u/oget result :nModified))}})))))
    ret-c))

(defn update-comment [entity-key data query ]
  (let [ret-c (promise-chan)
        c (promise-chan)]
    (-> (coll entity-key)
      (u/ocall :updateOne query data #(put! c %&)))
    (go
      (let [[err res] (<! c)
            res (if res (u/ocall res :toJSON))]
        (if err
          (ret-err ret-c err)
          (if (zero? (u/oget res :n))
            (put! ret-c {:err :db-not-found})
            (put! ret-c {:result
                         {:ok?       (pos? (u/oget res :ok))
                          :modified? (pos? (u/oget res :nModified))}})))))
    ret-c))
