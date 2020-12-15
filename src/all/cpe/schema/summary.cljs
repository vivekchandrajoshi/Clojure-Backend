(ns cpe.schema.summary
  (:require [cpe.util :as u]))

(def schema
  {:summary       {:id             u/id-field
                   :plant-id       "plantId"
                   :status         "status"
                   :subject        "subject"
                   :created-by     "createdBy"
                   :date-created   (u/date-field "dateCreated")
                   :modified-by    "modifiedBy"
                   :date-modified  (u/date-field "dateModified")
                   :published-by    "publishedBy"
                   :date-published (u/date-field "datePublished")}})
