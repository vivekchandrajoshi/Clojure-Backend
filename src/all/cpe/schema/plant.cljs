(ns cpe.schema.plant
  (:require [cpe.util :as u]
            [cpe.schema.uoms :as uoms]))

(def schema
  (let [
        selected-unit {:unit-param-id      "unitParamId"
                       :selected-unit-name "selectedUnitName"
                       }
        custome-unit-system {:name          "name"
                             :selected-unit {:name   "Selected Unit"
                                             :schema selected-unit
                                             :array? true}}

        settings {
                ;  :unit-system-name             "unitSystemName"
                ;  :custome-unit-system {:name   "customeUnitSystem"
                ;                        :schema custome-unit-system
                ;                        }
                 :uoms                {:name   "uoms"
                                      :schema uoms/schema
                                      :array? true}
                  :charts-config  "chartsConfig"
                 :constant            "constant"
                 :raw-tags           "rawTags"
                 :calculated-tags     "calculatedTags"
                 :pinned-charts       "pinnedCharts"
                 :created-by          "createdBy"
                 :date-created        (u/date-field "dateCreated")
                 :modified-by         "modifiedBy"
                 :date-modified       (u/date-field "dateModified")
                 }

        config {:sql-plant-id  "sqlPlantId"
                :section       "section"
                :constant      "constant"
                :date-sor      (u/date-field "dateSOR")
                :date-eor      (u/date-field "dateEOR")
                :history-sor   "historySOR"
                :created-by    "createdBy"
                :date-created  (u/date-field "dateCreated")
                :modified-by   "modifiedBy"
                :date-modified (u/date-field "dateModified")
                }]
    {:plant {:id            u/id-field
             :client-id     "clientId"
             :name          "name"
             :settings       {:name   "settings"
                             :schema settings
                             }
             :config        {:name   "config"
                             :schema config
                             }
             :created-by    "createdBy"
             :date-created  (u/date-field "dateCreated")
             :modified-by   "modifiedBy"
             :date-modified (u/date-field "dateModified")

             }
     :plant/update-settings ^:api {:modified-by   "modifiedBy"
                                   :date-modified (u/date-field "dateModified")
                                   :settings  {:name   "settings"
                                               :schema settings}}
     :plant/update-config ^:api {:modified-by   "modifiedBy"
                                   :date-modified (u/date-field "dateModified")
                                   :config  {:name   "config"
                                               :schema config
                                               }}}

    )
  )
