(ns cpe.schema.comment
  (:require [cpe.util :as u]))

(def schema
  (let [
        chart-info {:start-of-run-day "startOfRunDay"
                    :end-of-run-day  "endOfRunDay"
                    :chart-id   "chartId"}]
    {:comment       {:id            u/id-field
                     :chart-info    {:name   "chartInfo"
                                     :schema chart-info}
                     :comments   {:name   "comments"
                                  :schema :comment/reply
                                  :array? true}
                     :number-of-comment "numberOfComment"
                     :type-id       "typeId"
                     :subject       "subject"
                     :plant-id      "plantId"
                     :include-in-report-permanently "includeInReportPermanently"
                     :include-in-report "includeInReport"
                     :include-comments-in-report {:name "includeCommentsInReport"
                                                  :schema {:id "id"
                                                           :include-in-report "includeInReport"}
                                                  :array? true}
                     :last-post     (u/date-field "lastPost")
                     :last-read     {:name   "lastRead"
                                     :schema :comment/last-read
                                     :array? true}
                     :created-by    "createdBy"
                     :date-created  (u/date-field "dateCreated")
                     :modified-by   "modifiedBy"
                     :date-modified (u/date-field "dateModified")}
     :comment/reply {:comment       "comment"
                     :created-by    "createdBy"
                     :date-created  (u/date-field "dateCreated")
                     :modified-by   "modifiedBy"
                     :date-modified (u/date-field "dateModified")}
     :comment/query {:last-comment-date       (u/date-field "dateCreated")}

     :comment/db-data-parsing {
                               :last-post (u/date-field "lastPost")
                               :comments {:name   "comments"
                                          :schema :comment/reply }}
     :comment/last-read {:read-by "readBy"
                         :read-on (u/date-field "readOn")}}))
