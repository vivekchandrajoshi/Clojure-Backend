(ns cpe.info)

(def about
  {:en
   {:description "To evaluate the performance of catalyst."}})

(def features
  [{:id :standard
    :name "Standard"
    :description "Includes standard features like configure sections, charts and post comments."}])

(def operations
  [{:id :view
    :name "View"
    :description "Allow to view trendline, datasets and plant settings, But can not update or add any new datasets."}
   {:id :upload
    :name "Upload"
    :description "Allow to upload reports."}
   {:id :export
    :name "Export"
    :description "Allow to export reports and chart images."}
   {:id :modifyUOMSettings
    :name "Update UOM Settings"
    :description "Allow to update uom settings."}
   {:id :pinChart
    :name "Add and update pin chart's"
    :description "Allow to add and update the pin chart's."}
   {:id :configurePlant
    :name "Update Plant Configuration"
    :description "Allow to update plant configuration."
    :internal? true}])