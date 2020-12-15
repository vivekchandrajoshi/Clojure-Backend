(ns cpe.schema.user
  (:require [cpe.util :as u]
            [cpe.schema.uoms :as uoms]))

(def schema
  {:user {:id           u/id-field
          :client-id    "clientId"
          :plant-id     "plantId"
          :agreed?      "isAgreed"
          :email-alert? "emailAlert"
          :unit-system "unitSystem"
          :uoms {:name   "uoms"
                 :schema uoms/schema
                 :array? true}}})