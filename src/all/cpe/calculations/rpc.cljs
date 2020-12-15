(ns cpe.calculations.rpc
  (:require [cljs.nodejs :as nodejs]
            [cpe.util :as u]
            [cljs.core.async :refer [put! take! chan promise-chan <! close!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def https (nodejs/require "https"))
(defonce xml-js (nodejs/require "xml-js"))
(def options {:host   "topsoedev-rpc-svc.azurewebsites.net" ;old path "HTINERPC1.TOPSOE.DK" port 9700
              :path   "/RPCFunction"
              :method "POST"})
(defonce xml-options (clj->js {:compact false, :ignoreComment true, :spaces 4}))


(defn fetch-rpc-server [body]
  (let [ret-c (promise-chan)]
    (go
      (let [result (atom "")
            data (atom {})
            req (u/ocall https :request
                         (clj->js options)
                         (fn [res]
                           (u/ocall res :on "data" (fn [data]
                                                     (swap! result #(str % data))))
                           (u/ocall res :on "end"
                                    (fn []
                                      (let [result-clj (js->clj (u/ocall xml-js :xml2js
                                                                         @result xml-options) :keywordize-keys true)]
                                        (swap! data assoc result-clj)
                                        (put! ret-c {:result
                                                            (second (map-indexed (fn [idx itm]
                                                                                   (if (= idx 1)
                                                                                     (-> (second itm)
                                                                                         (first)
                                                                                         (get :elements)
                                                                                         (first)
                                                                                         (get-in [:attributes :Value])
                                                                                         ))) (first (second @data))))
                                                     ;       (second (map-indexed (fn [idx itm]
                                                     ;                              (if (= idx 1)
                                                     ;                                (-> (second itm)
                                                     ;                                    (first)
                                                     ;                                    (get :elements)
                                                     ;                                    (first)
                                                     ;                                    (get-in [:attributes :Value]))))
                                                     ;                            (first (first @data))))
                                                     :error nil}))))))]
        (u/ocall req :on "error" (fn [err]
                                   (put! ret-c {:result nil
                                                :error  err})))
        (u/ocall req :write body)
        (u/ocall req :end)))
    ret-c))

;;TODO: fixed for repeated xml

(defn inner-xml [name value]
  {:type       "element"
   :name       "Parameter"
   :attributes {:Type "double" :Value value :Name name}})

(defn prepare-xml [name vec]
  {:type       "element"
   :name       "Calculation"
   :attributes {:CalcualtionName name :Revision ""}
   :elements (mapv (fn [value]
                     (inner-xml (value :name) (value :value)))
                   vec)})

(defn root-xml-element [data]
  {:elements (vector prepare-xml (data :name) (data :vec))})

;;TODO: Use root-xml-element for xml
(defn k-Tnorm [k t e-act t-norm]
  (let [value (atom nil)
        data {:elements [{
                          :type       "element"
                          :name       "Calculation"
                          :attributes {:CalcualtionName "k_TNorm" :Revision ""}
                          :elements   [{:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value k :Name "k"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value t :Name "T"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value e-act :Name "E_act"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value t-norm :Name "T_norm"}}]}]}
        result (fetch-rpc-server (u/ocall xml-js :json2xml (clj->js data) xml-options))]
    result))

(defn t-norm [k-norm k-req e-act t-normc]
  (let [data {:elements [{
                          :type       "element"
                          :name       "Calculation"
                          :attributes {:CalcualtionName "T_norm" :Revision ""}
                          :elements   [{:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value k-norm :Name "K_norm"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value k-req :Name "K_req"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value e-act :Name "E_act"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value t-normc :Name "T_normC"}}]}]}
        result (fetch-rpc-server (u/ocall xml-js :json2xml (clj->js data) xml-options))]
    ;(go
    ;  (let [{:keys [error msg result]} (<! result)]
    ;    (print result "res")
    ;    )
    ;  )
    result))

(defn k-inch [lhsv sinlet soutlet sg gas-to-oil-inlet pbarg xh2 xh2s order kh2s]
  (let [data {:elements [{
                          :type       "element"
                          :name       "Calculation"
                          :attributes {:CalcualtionName "K_inh" :Revision ""}
                          :elements   [{:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value lhsv :Name "LHSV"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value sinlet :Name "SInlet"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value soutlet :Name "SOutlet"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value sg :Name "SG"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value gas-to-oil-inlet :Name "GasToOilInlet"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value pbarg :Name "Pbarg"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value xh2 :Name "xH2"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value xh2s :Name "xH2S"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value order :Name "Order"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value kh2s :Name "K_H2S"}}]}]}
        result (fetch-rpc-server (u/ocall xml-js :json2xml (clj->js data) xml-options))]
    result))

(defn k-inh-norm-correction [convpct]
  (let [data {:elements [{
                          :type       "element"
                          :name       "Calculation"
                          :attributes {:CalcualtionName "KInhNormCorrection" :Revision ""}
                          :elements   [{:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value convpct :Name "ConvPct"}}]}]}
        result (fetch-rpc-server (u/ocall xml-js :json2xml (clj->js data) xml-options))]
    result))

(defn k-tp-norm [k tout pout eact alpha t-norm p-norm]
  (let [data {:elements [{
                          :type       "element"
                          :name       "Calculation"
                          :attributes {:CalcualtionName "k_TPNorm" :Revision ""}
                          :elements   [{:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value k :Name "k"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value tout :Name "T_out"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value pout :Name "P_out"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value eact :Name "E_act"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value alpha :Name "Alpha"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value t-norm :Name "T_norm"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value p-norm :Name "P_norm"}}]}]}
        result (fetch-rpc-server (u/ocall xml-js :json2xml (clj->js data) xml-options))]
    result))

(defn k-sup [heavy-end sg s-feed me-abp]
  (let [data {:elements [{
                          :type       "element"
                          :name       "Calculation"
                          :attributes {:CalcualtionName "K_sup" :Revision ""}
                          :elements   [{:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value heavy-end :Name "HeavyEnd"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value sg :Name "SG"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value s-feed :Name "SFeed"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value me-abp :Name "MeABP"}}]}]}
        result (fetch-rpc-server (u/ocall xml-js :json2xml (clj->js data) xml-options))]
    result))

(defn me-abpc [heavy-end sg s-feed]
  (let [data {:elements [{
                          :type       "element"
                          :name       "Calculation"
                          :attributes {:CalcualtionName "MeABPC" :Revision ""}
                          :elements   [{:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value heavy-end :Name "VABPC"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value sg :Name "T10C"}}
                                       {:type       "element"
                                        :name       "Parameter"
                                        :attributes {:Type "double" :Value s-feed :Name "T90C"}}
                                       ]}]}
        result (fetch-rpc-server (u/ocall xml-js :json2xml (clj->js data) xml-options))]
    result))