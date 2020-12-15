(ns cpe.schema.client
  (:require [cpe.util :as u]
            [cpe.schema.sap-client :as sap-client]))

(def schema
  {:client                (merge (into {}
                                       (map (fn [[k f]]
                                              [k (if (map? f)
                                                   (assoc f :scope #{:api})
                                                   {:name f, :scope #{:api}})])
                                            (:sap-client sap-client/schema)))
                                 {:id            u/id-field
                                  :name          "name"
                                  :plants        {:name   "plants"
                                                  :schema ::plant
                                                  :array? true}
                                  :created-by    "createdBy"
                                  :date-created  (u/date-field "dateCreated")
                                  :modified-by   "modifiedBy"
                                  :date-modified (u/date-field "dateModified")})

   :client/query          {:name       "name"
                           :short-name "shortName"
                           :location   "location"
                           :country    "country"
                           :skip       {:name  "skip"
                                        :parse u/parse-int}
                           :limit      {:name  "limit"
                                        :parse u/parse-int}
                           :plant?     {:name  "withPlants"
                                        :parse u/parse-bool}}

   :client/search-options ^:api {:country {:name   "country"
                                           :array? true}}

   ::plant                {:id       "id"
                           :name     "name"
                           :capacity "capacity"
                           }})
