(ns cpe.entity
  (:require [goog.object :as g]
            [cpe.util :as u]
            [cpe.schema.sap-client :as sap-client]
            [cpe.schema.sap-plant :as sap-plant]
            [cpe.schema.client :as client]
            [cpe.schema.user :as user]
            [cpe.schema.plant :as plant]
            [cpe.schema.comment :as comments]
            [cpe.schema.unit-system :as unit]
            [cpe.schema.chart :as chart]
            [cpe.schema.section :as section]
            [cpe.schema.sensor :as sensor]
            [cpe.schema.uom :as uom]
            [cpe.schema.misc :as misc]
            [cpe.schema.summary :as summary]))


;; entity schema is a map of field keyword to definition
;; field definition can be a string or keyword or a map.
;; if string/keyword, it is the field name, value is read or write as is.
;; if map, it can have following properties:
;;  :name - the field name
;;  :array? - if true, parse to array
;;  :array-dim - an positive integer (>=2) applicable for nested arrays.
;;               indicates the depth of nesting or the number of dimensions.
;;               ignored if :array? is not true
;;  :parse - optional function to transform from js
;;  :unparse - optional function to transform back to js
;;  :schema - if present, further parse as defined by it.
;;            can be a map or a keyword pointing to a defined entity
;;  :overrides - optional, a map of overrides of above arranged by case-key.
;;               use this to slightly alter for other cases like :db or :api
;;  :scope - optional, a set of keywords, speicifying use cases like :db or :api
;;
;; if both :schema and :parse present, parse is applied after schema
;; if both :schema and :unparse present, unparse is applied before schema
;; if :array? is true, :schema or :parse & :unparse applied to each one

(def ^:private entity-schema
  (merge
   sap-client/schema, sap-plant/schema
   user/schema, client/schema, plant/schema,unit/schema
   comments/schema, chart/schema, section/schema, sensor/schema, uom/schema,
   misc/schema, summary/schema,
   {:res/create ^:api {:new-id "newId"}
    :res/update ^:api {:modified? "isModified"}
    }
   ;:sensor-data {:base-uom "base-uom" :sensor-value "sensor-value"}
   ))

(defn- apply-overrides [schema case-key]
  (if-not (map? schema)
    schema
    (reduce-kv
     (fn [m attr field]
       (if-not (map? field)
         (assoc m attr field)
         (if-not ((get field :scope identity) case-key)
           m ;; skip if field not in scope
           (as-> field $
             (if-let [overrides (get-in $ [:overrides case-key])]
               (merge $ overrides)
               $)
             (dissoc $ :overrides :scope)
             (if-let [schema (:schema $)]
               (assoc $ :schema (apply-overrides schema case-key))
               $)
             (assoc m attr $)))))
     {} schema)))

(defn- derive-schema [case-key]
  (->> entity-schema
       (filter #(let [m (meta (val %))]
                  (or (nil? m) (get m case-key))))
       (map (fn [[ekey schema]]
              [ekey (apply-overrides schema case-key)]))
       (into {})))

(def entity-schema-db (derive-schema :db))
(def entity-schema-api (derive-schema :api))

(defn- map-n-dim
  "n must be positive integer >= 1"
  ([f a n]
   (map-n-dim f a n nil))
  ([f a n res-f]
   (cond-> (mapv (if (> n 1)
                   #(map-n-dim f % (dec n) res-f)
                   f)
                 a)
     res-f res-f)))

(defn- from-js
  "Parse an entity from js object.
  *schema-or-key* can be a map or a keyword. if keyword, it will be
  looked up from the *entity-schema*."
  [schema-or-key object entity-schema]
  (if-not object nil
                 (if-let [schema (if (map? schema-or-key)
                                   schema-or-key
                                   (get entity-schema schema-or-key))]
                   (first
                     (reduce ;; parse the object for each key in schema
                       (fn _parse_fn [[e o] [k a]]
                         [(let [;; attribute name
                                n (cond
                                    (string? a)  a
                                    (keyword? a) (name a)
                                    (map? a)     (let [n (:name a)] (if (keyword? n) (name n) n))
                                    :not-possible
                                    (throw (ex-info "Invalid attribute definition!"
                                                    {:attr-def a})))
                                ;; attribute value
                                v (g/get o n)
                                ;; parse value if applicable
                                v (if (some? v) ;; discard undefined/null
                                    (if-not (map? a)
                                      v
                                      (let [n-dim (get a :array-dim 1)
                                            parse (or (some->> [(:parse a)
                                                                (if-let [s (:schema a)]
                                                                  #(from-js s % entity-schema))]
                                                               (filter fn?)
                                                               (not-empty)
                                                               (apply comp))
                                                      identity)]
                                        (if (:array? a)
                                          (map-n-dim parse v n-dim vec)
                                          (parse v)))))]
                            (if (some? v) (assoc e k v) e)) ;; set the attribute
                          o])
                       [{} object] ;; start with empty {}
                       schema))
                   (throw (ex-info "Invalid schema!" {:schema-or-key schema-or-key})))))


(defn- to-js
  "Unparse an entity to js object.  
  *schema-or-key* can be a map or a keyword. if keyword, it will be
  looked up from the *entity-schema*"
  [schema-or-key entity entity-schema]
  ;(print schema-or-key  "schema-or-key" entity "to-js")
  (if-not entity nil
          (if-let [schema (if (map? schema-or-key)
                            schema-or-key
                            (get entity-schema schema-or-key))]
            (first
             (reduce ;; unparse the entity for each key in schema
              (fn _unparse_fn [[o e] [k a]]
                [(let [;; attribute name
                       n (cond
                           (string? a)  a
                           (keyword? a) (name a)
                           (map? a)     (let [n (:name a)]
                                          (if (keyword? n) (name n) n))
                           :not-possible
                           (throw (ex-info "Invalid attribute definition!"
                                           {:attr-def a})))
                       ;; attribute value
                       v (get e k)
                       ;; unparse value if applicable
                       v (if (some? v) ;; discard undefined/null
                           (if-not (map? a)
                             v
                             (let [n-dim (get a :array-dim 1)
                                   unparse (or (some->> [(if-let [s (:schema a)]
                                                           #(to-js s % entity-schema))
                                                         (:unparse a)]
                                                        (filter fn?)
                                                        (not-empty)
                                                        (apply comp))
                                               identity)]
                               (if (:array? a)
                                 (map-n-dim unparse v n-dim to-array)
                                 (unparse v)))))]
                   (if (some? v) (g/set o n v)) ;; set the attribute
                   o)
                 e])
              [#js{} entity] ;; start with empty #js{}
              schema))
            (throw (ex-info "Invalid schema!" {:schema-or-key schema-or-key})))))

(defn- attr-key [entity-key attr entity-schema]
  (as-> (get-in entity-schema [entity-key attr]) $
    (if (map? $) (:name $) $)
    (if (keyword? $) (name $) $)))

;; API

(defn from-api [entity-key entity]
  (from-js entity-key entity entity-schema-api))

(defn to-api [entity-key entity]
  (to-js entity-key entity entity-schema-api))

(defn attr-key-api [entity-key attr]
  (attr-key entity-key attr entity-schema-api))

(defn schema-api [entity-key]
  (get entity-schema-api entity-key))

;; DB

(defn from-db [entity-key entity]
  (from-js entity-key entity entity-schema-db))

(defn to-db [entity-key entity]
  (to-js entity-key entity entity-schema-db))

(defn attr-key-db [entity-key attr]
  (attr-key entity-key attr entity-schema-db))

(defn schema-db [entity-key]
  (get entity-schema-db entity-key))
