(ns cpe.calculations.scheduler
  (:require-macros [cljs.core.async.macros
                    :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.calculations.calculations :as c]
            [cpe.calculations.connection :as con]
            [cpe.calculations.mongo :as mongo]
            [cpe.calculations.error :as e]
            [cpe.config :refer [config]]
            [cljs.core.async :as a :refer [put! take! chan promise-chan <! close!]]))

;(mongo/init)
(defonce schedule (nodejs/require "node-schedule"))
(defonce cron (nodejs/require "node-cron"))
(defonce calculated-data (atom {}))
(defonce mi-calculated-data (atom {}))
(defonce ms-calculated-data (atom {}))
(def task (atom nil))

(defn raw-to-internal-parser [data]
  (->> (mapv (fn [d]
               (let [k (:SensorName d)
                     v (:SensorValue d)
                     ik (@mongo/raw (keyword k))
                     ]
                 (if ik
                   {ik v}))) data)
       (remove nil?)
       (reduce (fn [col d]
                 (into col d)
                 ) {})))


(defn column-sensor [data date]
  (let [sensor-date-by-date (reduce (fn [col [k v]]
                                      (let [key (k @mongo/calculated)
                                            sensor-map {key (first v)}
                                            day (.getUTCDate date)
                                            month (+ (.getUTCMonth date) 1)
                                            year (.getUTCFullYear date)
                                            hour (.getUTCHours date)
                                            min (.getUTCMinutes date)
                                            sec (.getUTCSeconds date)
                                            date-string (str year "-" month "-" day)
                                            time-string (str hour ":" min ":" sec)]
                                        (conj col (if (first v)
                                                    (str "(" key ", '" date-string "', '" time-string "', " (first v) ")")
                                                    nil))

                                        )) [] data)
        remove-nil (remove nil? sensor-date-by-date)
        to-be-insert (apply str (clojure.string/join "," remove-nil))]
    (con/insert-sensor to-be-insert)
    ))

(defn rpc-calculation [data]
  (doall
    (map-indexed (fn [idx dm]
                   (if (= idx 0)
                     (do
                       (swap! mi-calculated-data assoc :Normalized-HDS-WABT-Column-MI [])
                       (swap! ms-calculated-data assoc :Normalized-HDB-WABT-Column-MS [])
                       ))

                   (go
                     ;Normalized HDS WABT Column MI
                     (let [{:keys [result]}
                           (<! (c/NHDSWABT
                                 (dm :eact)
                                 (dm :SOR-Ref-Day-k-Inh)
                                 (dm :n)
                                 (dm :Active-Catalyst-Volume)
                                 (dm :SOR-Ref-Day-K-sup)
                                 (dm :SOR-Ref-Day-Kinh-Corr)
                                 (dm :SOR-Ref-Day-A-Train-Topsoe-WABT)
                                 (dm :Gravity)
                                 (dm :SULFUR-WT%-X-RA-Result)
                                 (dm :Net-Diesel-To-Storage)
                                 (dm :ULSD-Prod-Gravity)
                                 (dm :Sulfur)
                                 (dm :Net-Kerosene-To-Storage)
                                 (dm :Prod-API-Gravity-Naphtha)
                                 (dm :Kero-Sulfur)
                                 (dm :Make-Up-Gas-From-Unicrack-H2)
                                 (dm :Recycle-H2-To-Pass-A)
                                 (dm :Recycle-H2-To-Pass-B)
                                 (dm :H2-Quench-To-MID-RX-A-1)
                                 (dm :H2-Quench-RX-A-2-Inlet)
                                 (dm :H2-Quench-To-MID-RX-B-1)
                                 (dm :H2-Quench-RX-B-2-Inlet)
                                 (dm :Purge-Rate)
                                 (dm :RX-FEED-TO-PASS-A)
                                 (dm :RX-FEED-TO-PASS-B)
                                 (dm :RX-A-1-Inlet-Pressure)
                                 (dm :RX-A-1-Overall-Delta-P)
                                 (dm :Reactor-A-2-Diff-Press)
                                 (dm :Makeup-H2)
                                 (dm :Recycle-Gas-H2)
                                 (dm :Initial-Boiling-Point-Test-Method-D2887)
                                 (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                 (dm :Final-Boiling-Point-Test-Method-D2887)
                                 (dm :wt%-in-bed1)
                                 (dm :wt%-in-bed2)
                                 (dm :wt%-in-bed3)
                                 (dm :dt-rule-in-bed1)
                                 (dm :dt-rule-in-bed2)
                                 (dm :dt-rule-in-bed3)
                                 (dm :RX-A-1-Feed-From-Heater-A)
                                 (dm :RX-A-1-Bot-Bed-Up-G)
                                 (dm :RX-A-1-Bot-Bed-Up-H)
                                 (dm :RX-A-1-Bot-Bed-Up-I)
                                 ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                 (dm :RX-A-1-Top-Bed-Low-D)
                                 (dm :RX-A-1-Top-Bed-Low-E)
                                 (dm :RX-A-1-Top-Bed-Low-F)
                                 ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                 (dm :RX-A-1-Bot-Bed-Low-M)
                                 (dm :RX-A-1-Bot-Bed-Low-N)
                                 (dm :RX-A-1-Bot-Bed-Low-O)
                                 ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                 ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                 ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                 ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                 (dm :Reactor-A-Outlet)
                                 (dm :RX-A-2-Inlet-Top)
                                 (dm :Recycle-Gas-H2S)
                                 (dm :KH2S)
                                 ))]
                       (if result
                         (swap! mi-calculated-data assoc :Normalized-HDS-WABT-Column-MI
                                (-> (get
                                      @mi-calculated-data :Normalized-HDS-WABT-Column-MI)
                                    (conj
                                      (mongo/convert-to-base-uom result
                                                                 (:Normalized-HDS-WABT-Column-MI @mongo/calculated-uom-data)
                                                                 (:Normalized-HDS-WABT-Column-MI @mongo/calculated)))
                                    ))
                         )
                       ))

                   (go
                     ;Normalized HDN WABT Column MS
                     (let [{:keys [result]}
                           (<! (c/NHDNWABT
                                 (dm :RX-FEED-TO-PASS-A)
                                 (dm :RX-FEED-TO-PASS-B)
                                 (dm :Active-Catalyst-Volume)
                                 (dm :Charge-N2)
                                 (dm :Product-N2)
                                 (dm :Make-Up-Gas-From-Unicrack-H2)
                                 (dm :Makeup-H2)
                                 (dm :Recycle-H2-To-Pass-A)
                                 (dm :Recycle-Gas-H2)
                                 (dm :Recycle-H2-To-Pass-B)
                                 (dm :H2-Quench-To-MID-RX-A-1)
                                 (dm :H2-Quench-RX-A-2-Inlet)
                                 (dm :H2-Quench-To-MID-RX-B-1)
                                 (dm :H2-Quench-RX-B-2-Inlet)
                                 (dm :RX-A-1-Feed-From-Heater-A)
                                 (dm :RX-A-1-Bot-Bed-Up-G)
                                 (dm :RX-A-1-Bot-Bed-Up-H)
                                 (dm :RX-A-1-Bot-Bed-Up-I)
                                 ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                 (dm :RX-A-1-Top-Bed-Low-D)
                                 (dm :RX-A-1-Top-Bed-Low-E)
                                 (dm :RX-A-1-Top-Bed-Low-F)
                                 ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                 (dm :RX-A-1-Bot-Bed-Low-M)
                                 (dm :RX-A-1-Bot-Bed-Low-N)
                                 (dm :RX-A-1-Bot-Bed-Low-O)
                                 ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                 (dm :SOR-Ref-Day-A-Train-Topsoe-WABT)
                                 (dm :SOR-Ref-Day-H2-Partial-Pressure)
                                 (dm :SOR-Ref-Day-K-HDN)
                                 (dm :RX-B-1-Inlet-Pressure)
                                 (dm :RX-B-1-Overall-Delta-P)
                                 (dm :Reactor-B-2-Diff-Press)
                                 (dm :Reactor-A-Outlet)
                                 (dm :RX-A-2-Inlet-Top)
                                 (dm :wt%-in-bed1)
                                 (dm :wt%-in-bed2)
                                 (dm :wt%-in-bed3)
                                 (dm :dt-rule-in-bed1)
                                 (dm :dt-rule-in-bed2)
                                 (dm :dt-rule-in-bed3)
                                 (dm :Eact-HDN)
                                 (dm :H2PP-Exp)))]
                       (if result
                         (swap! ms-calculated-data assoc :Normalized-HDB-WABT-Column-MS
                                (-> (get
                                      @ms-calculated-data :Normalized-HDB-WABT-Column-MS)
                                    (conj
                                      (mongo/convert-to-base-uom result
                                                                 (:Normalized-HDB-WABT-Column-MS @mongo/calculated-uom-data)
                                                                 (:Normalized-HDB-WABT-Column-MS @mongo/calculated))
                                      ))))
                       ))
                   ) data)

    ))

(defn calculation [data]
  (doall
    (map-indexed (fn [idx dm]
                   (if (= idx 0)
                     (do
                       (swap! calculated-data assoc :Feed-Sulfur-Column-CZ [])
                       (swap! calculated-data assoc :Product-Sulfur-Column-JT [])
                       (swap! calculated-data assoc :Feed-Nitrogen-Column-DA [])
                       (swap! calculated-data assoc :Product-Nitrogen-Column-JU [])
                       (swap! calculated-data assoc :Feed-Gravity-Column-CX [])
                       (swap! calculated-data assoc :Feed-Silicon-Column-DC [])
                       (swap! calculated-data assoc :Feed-Arsenic-Column-DD [])
                       (swap! calculated-data assoc :10-Per-Trendline-Column-EJ [])
                       (swap! calculated-data assoc :30-Per-Trendline-Column-EL [])
                       (swap! calculated-data assoc :50-Per-TrendlineColumn-EN [])
                       (swap! calculated-data assoc :70-Per-Trendline-Column-EP [])
                       (swap! calculated-data assoc :90-Per-Trendline-Column-ER [])
                       (swap! calculated-data assoc :100-Per-Trendline-Column-ET [])
                       (swap! calculated-data assoc :Feed-600ºF-Column-EW [])
                       (swap! calculated-data assoc :Feed-Rate-Column-C [])
                       (swap! calculated-data assoc :Feed-Ratio-Of-Cracked-Feed-Column-H [])
                       (swap! calculated-data assoc :Observed-WABT-Column-LJ [])
                       (swap! calculated-data assoc :Rate-Column-K [])
                       (swap! calculated-data assoc :Hydrogen-Purity-Column-L [])
                       (swap! calculated-data assoc :Rate-Column-M [])
                       (swap! calculated-data assoc :Hydrogen-Purity-Column-N [])
                       (swap! calculated-data assoc :Rate-Column-W [])
                       (swap! calculated-data assoc :Hydrogen-Purity-Column-P [])
                       (swap! calculated-data assoc :Rate-Column-AA [])
                       (swap! calculated-data assoc :Hydrogen-Purity-Column-AB [])
                       (swap! calculated-data assoc :Total-gas/oil-ratio-Column-LF [])
                       (swap! calculated-data assoc :Hydrogen/oil-ratio-Column-LG [])
                       (swap! calculated-data assoc :Train-A-Column-KU [])
                       (swap! calculated-data assoc :Train-B-Column-KV [])
                       (swap! calculated-data assoc :Hydrogen-Availability-Column-KZ [])
                       (swap! calculated-data assoc :Train-A-Inlet-Pressure-Column-AD [])
                       (swap! calculated-data assoc :Train-B-inlet-Pressure-Column-AH [])
                       (swap! calculated-data assoc
                              :Train-A-Hydrogen-Partial-Pressure-Column-LA [])
                       (swap! calculated-data assoc
                              :Train-B-Hydrogen-Partial-Pressure-Column-LB [])
                       (swap! calculated-data assoc :Total-Pressure-Drop-Column-AG [])
                       (swap! calculated-data assoc :Normalized-Pressure-Drop-Column-LC [])
                       (swap! calculated-data assoc :Total-Pressure-Drop-Column-AK [])
                       (swap! calculated-data assoc :Normalized-Pressure-Drop-Column-LD [])
                       (swap! calculated-data assoc :Train-A-DT-Column-AY [])
                       (swap! calculated-data assoc :Train-B-DT-Column-BM [])
                       (swap! calculated-data assoc :A1-Bed-1-DT-Column-AO [])
                       (swap! calculated-data assoc :A1-Bed-2-DT-Column-AS [])
                       (swap! calculated-data assoc :A2-DT-Column-AX [])
                       (swap! calculated-data assoc :B1-Bed-1-DT-Column-BC [])
                       (swap! calculated-data assoc :B1-Bed-2-DT-Column-BG [])
                       (swap! calculated-data assoc :B2-DT-Column-BL [])
                       (swap! calculated-data assoc :A1-Bed-1-inlet-T-Column-AL [])
                       (swap! calculated-data assoc :A1-Bed-1-outlet-T-Column-AN [])
                       (swap! calculated-data assoc :A1-Bed-2-outlet-T-Column-AR [])
                       (swap! calculated-data assoc :A2-outlet-T-Column-AW [])
                       (swap! calculated-data assoc :B1-Bed-1-inlet-T-Column-AZ [])
                       (swap! calculated-data assoc :B1-Bed-1-outlet-T-Column-BB [])
                       (swap! calculated-data assoc :B1-Bed-2-outlet-T-Column-BF [])
                       (swap! calculated-data assoc :B2-outlet-T-Column-BK [])
                       (swap! calculated-data assoc :Top-radial-spread-Column-BZ [])
                       (swap! calculated-data assoc :Bottom-radial-spread-Column-CA [])
                       (swap! calculated-data assoc :Top-radial-spread-Column-CB [])
                       (swap! calculated-data assoc :Middle-radial-spread-Column-CC [])
                       (swap! calculated-data assoc :Bottom-radial-spread-Column-CD [])
                       (swap! calculated-data assoc :Radial-spread-Column-CE [])
                       (swap! calculated-data assoc :Top-radial-spread-Column-CF [])
                       (swap! calculated-data assoc :Bottom-radial-spread-Column-CG [])
                       (swap! calculated-data assoc :Top-radial-spread-Column-CH [])
                       (swap! calculated-data assoc :Middle-radial-spread-Column-CI [])
                       (swap! calculated-data assoc :Bottom-radial-spread-Column-CJ [])
                       (swap! calculated-data assoc :Radial-spread-Column-CK [])
                       ))

                   ;(c/NHDSWABT)
                   ;(c/NHDNWABT)
                   ;Feed Sulfur Column CZ
                   (swap! calculated-data assoc :Feed-Sulfur-Column-CZ
                          (-> (get
                                @calculated-data :Feed-Sulfur-Column-CZ)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Feed-Sulfur
                                                                        (dm :SULFUR-WT%-X-RA-Result))
                                                           (:Feed-Sulfur-Column-CZ @mongo/calculated-uom-data)
                                                           (:Feed-Sulfur-Column-CZ @mongo/calculated)))))

                   ;Product Sulfur Column JT
                   (swap! calculated-data assoc :Product-Sulfur-Column-JT
                          (-> (get
                                @calculated-data :Product-Sulfur-Column-JT)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Product-Sulfur
                                                                        (dm :Net-Diesel-To-Storage) (dm :ULSD-Prod-Gravity)
                                                                        (dm :Sulfur) (dm :Net-Kerosene-To-Storage)
                                                                        (dm :Prod-API-Gravity-Naphtha) (dm :Kero-Sulfur))
                                                           (:Product-Sulfur-Column-JT @mongo/calculated-uom-data)
                                                           (:Product-Sulfur-Column-JT @mongo/calculated)))))

                   ;Feed Nitrogen Column DA
                   (swap! calculated-data assoc :Feed-Nitrogen-Column-DA
                          (-> (get
                                @calculated-data :Feed-Nitrogen-Column-DA)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Feed-Nitrogen
                                                                        (dm :Charge-N2))
                                                           (:Feed-Nitrogen-Column-DA @mongo/calculated-uom-data)
                                                           (:Feed-Nitrogen-Column-DA @mongo/calculated)))))


                   ;Product Nitrogen Column JU
                   (swap! calculated-data assoc :Product-Nitrogen-Column-JU
                          (-> (get
                                @calculated-data :Product-Nitrogen-Column-JU)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Product-Nitrogen
                                                                        (dm :Product-N2))
                                                           (:Product-Nitrogen-Column-JU @mongo/calculated-uom-data)
                                                           (:Product-Nitrogen-Column-JU @mongo/calculated)))))

                   ;Feed Gravity Column CX
                   (swap! calculated-data assoc :Feed-Gravity-Column-CX
                          (-> (get
                                @calculated-data :Feed-Gravity-Column-CX)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Feed-Gravity
                                                                        (dm :Gravity))
                                                           (:Feed-Gravity-Column-CX @mongo/calculated-uom-data)
                                                           (:Feed-Gravity-Column-CX @mongo/calculated)))))

                   ;Feed Silicon Column DC
                   ;(swap! calculated-data assoc :Feed-Silicon-Column-DC
                   ;       (-> (get
                   ;             @calculated-data :Feed-Silicon-Column-DC)
                   ;           (conj
                   ;             (mongo/convert-to-base-uom (e/try-catch c/Feed-Silicon
                   ;                          (dm :Sifeed))
                   ;                                      (:Feed-Silicon-Column-DC @mongo/calculated-uom-data)
                   ;                                      (:Feed-Silicon-Column-DC @mongo/calculated)))))
                   ;
                   ;;Feed Arsenic Column DD
                   ;(swap! calculated-data assoc :Feed-Arsenic-Column-DD
                   ;       (-> (get
                   ;             @calculated-data :Feed-Arsenic-Column-DD)
                   ;           (conj
                   ;             (mongo/convert-to-base-uom (e/try-catch c/Feed-Arsenic
                   ;                          (dm :Asfeed))
                   ;                                      (:Feed-Arsenic-Column-DD @mongo/calculated-uom-data)
                   ;                                      (:Feed-Arsenic-Column-DD @mongo/calculated)))))
                   ;
                   ;
                   ;
                   ;;Feed Arsenic Column DD
                   ;(swap! calculated-data assoc :Feed-Arsenic-Column-DD
                   ;       (-> (get
                   ;             @calculated-data :Feed-Arsenic-Column-DD)
                   ;           (conj
                   ;             (e/try-catch c/Feed-Arsenic
                   ;                          (dm :Asfeed)))))


                   ;10 Per Trendline Column EJ
                   (swap! calculated-data assoc :10-Per-Trendline-Column-EJ
                          (-> (get
                                @calculated-data :10-Per-Trendline-Column-EJ)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Ten-Per-Trendline
                                                                        (dm :dist-type)
                                                                        (dm :temp-type)
                                                                        (dm :Initial-Boiling-Point-Test-Method-D2887)
                                                                        (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Final-Boiling-Point-Test-Method-D2887))
                                                           (:10-Per-Trendline-Column-EJ @mongo/calculated-uom-data)
                                                           (:10-Per-Trendline-Column-EJ @mongo/calculated))

                                )))



                   ;30 Per Trendline Column EL
                   (swap! calculated-data assoc :30-Per-Trendline-Column-EL
                          (-> (get
                                @calculated-data :30-Per-Trendline-Column-EL)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Thirty-Per-Trendline
                                                                        (dm :dist-type)
                                                                        (dm :temp-type)
                                                                        (dm :Initial-Boiling-Point-Test-Method-D2887)
                                                                        (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Final-Boiling-Point-Test-Method-D2887))
                                                           (:30-Per-Trendline-Column-EL @mongo/calculated-uom-data)
                                                           (:30-Per-Trendline-Column-EL @mongo/calculated))
                                )))


                   ;50 Per TrendlineColumn EN
                   (swap! calculated-data assoc :50-Per-TrendlineColumn-EN
                          (-> (get
                                @calculated-data :50-Per-TrendlineColumn-EN)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Fifty-Per-Trendline
                                                                        (dm :dist-type)
                                                                        (dm :temp-type)
                                                                        (dm :Initial-Boiling-Point-Test-Method-D2887)
                                                                        (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Final-Boiling-Point-Test-Method-D2887))
                                                           (:50-Per-TrendlineColumn-EN @mongo/calculated-uom-data)
                                                           (:50-Per-TrendlineColumn-EN @mongo/calculated))
                                )))

                   ;70 Per Trendline Column EP
                   (swap! calculated-data assoc :70-Per-Trendline-Column-EP
                          (-> (get
                                @calculated-data :70-Per-Trendline-Column-EP)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Seventy-Per-Trendline
                                                                        (dm :dist-type)
                                                                        (dm :temp-type)
                                                                        (dm :Initial-Boiling-Point-Test-Method-D2887)
                                                                        (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Final-Boiling-Point-Test-Method-D2887))
                                                           (:70-Per-Trendline-Column-EP @mongo/calculated-uom-data)
                                                           (:70-Per-Trendline-Column-EP @mongo/calculated))
                                )))

                   ;90 Per Trendline Column ER
                   (swap! calculated-data assoc :90-Per-Trendline-Column-ER
                          (-> (get
                                @calculated-data :90-Per-Trendline-Column-ER)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Ninety-Per-Trendline
                                                                        (dm :dist-type)
                                                                        (dm :temp-type)
                                                                        (dm :Initial-Boiling-Point-Test-Method-D2887)
                                                                        (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Final-Boiling-Point-Test-Method-D2887))
                                                           (:90-Per-Trendline-Column-ER @mongo/calculated-uom-data)
                                                           (:90-Per-Trendline-Column-ER @mongo/calculated))
                                )))

                   ;100 Per Trendline Column ET
                   (swap! calculated-data assoc :100-Per-Trendline-Column-ET
                          (-> (get
                                @calculated-data :100-Per-Trendline-Column-ET)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Hundred-Per-Trendline
                                                                        (dm :dist-type)
                                                                        (dm :temp-type)
                                                                        (dm :Initial-Boiling-Point-Test-Method-D2887)
                                                                        (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Final-Boiling-Point-Test-Method-D2887))
                                                           (:100-Per-Trendline-Column-ET @mongo/calculated-uom-data)
                                                           (:100-Per-Trendline-Column-ET @mongo/calculated))
                                )))



                   ;Feed 600ºF Column EW
                   (swap! calculated-data assoc :Feed-600ºF-Column-EW
                          (-> (get
                                @calculated-data :Feed-600ºF-Column-EW)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Feed-600F
                                                                        (dm :dist-type)
                                                                        (dm :temp-type)
                                                                        (dm :Initial-Boiling-Point-Test-Method-D2887)
                                                                        (dm :Percent-5-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-10-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-20-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-30-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-40-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-50-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-60-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-70-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-80-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-90-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Percent-95-Recovered-Test-Method-ASTM-D2887)
                                                                        (dm :Final-Boiling-Point-Test-Method-D2887))
                                                           (:Feed-600ºF-Column-EW @mongo/calculated-uom-data)
                                                           (:Feed-600ºF-Column-EW @mongo/calculated))
                                )))

                   ;Feed Rate Column C
                   (swap! calculated-data assoc :Feed-Rate-Column-C
                          (-> (get
                                @calculated-data :Feed-Rate-Column-C)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Feed-Rate
                                                                        (dm :RX-FEED-TO-PASS-A)
                                                                        (dm :RX-FEED-TO-PASS-B))
                                                           (:Feed-Rate-Column-C @mongo/calculated-uom-data)
                                                           (:Feed-Rate-Column-C @mongo/calculated)))))

                   ;Feed Ratio Of Cracked Feed Column H
                   (swap! calculated-data assoc :Feed-Ratio-Of-Cracked-Feed-Column-H
                          (-> (get
                                @calculated-data :Feed-Ratio-Of-Cracked-Feed-Column-H)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Feed-Ratio-Of-Cracked-Feed
                                                                        (dm :RX-FEED-TO-PASS-A)
                                                                        (dm :RX-FEED-TO-PASS-B)
                                                                        (dm :Coker-Debutanizer-Bottoms-Feed-Rate)
                                                                        (dm :Net-Coker-Diesel))
                                                           (:Feed-Ratio-Of-Cracked-Feed-Column-H @mongo/calculated-uom-data)
                                                           (:Feed-Ratio-Of-Cracked-Feed-Column-H @mongo/calculated)))))

                   ;Observed WABT  Column LJ
                   (swap! calculated-data assoc :Observed-WABT-Column-LJ
                          (-> (get
                                @calculated-data :Observed-WABT-Column-LJ)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/OWABT
                                                                        (dm :wt%-in-bed1)
                                                                        (dm :wt%-in-bed2)
                                                                        (dm :wt%-in-bed3)
                                                                        (dm :dt-rule-in-bed1)
                                                                        (dm :dt-rule-in-bed2)
                                                                        (dm :dt-rule-in-bed3)
                                                                        (dm :RX-A-1-Feed-From-Heater-A)
                                                                        (dm :RX-A-1-Bot-Bed-Up-G)
                                                                        (dm :RX-A-1-Bot-Bed-Up-H)
                                                                        (dm :RX-A-1-Bot-Bed-Up-I)
                                                                        (dm :RX-A-1-Top-Bed-Low-D)
                                                                        (dm :RX-A-1-Top-Bed-Low-E)
                                                                        (dm :RX-A-1-Top-Bed-Low-F)
                                                                        (dm :RX-A-1-Bot-Bed-Low-M)
                                                                        (dm :RX-A-1-Bot-Bed-Low-N)
                                                                        (dm :RX-A-1-Bot-Bed-Low-O)
                                                                        (dm :RX-B-1-Top-Bed-Low-D)
                                                                        (dm :RX-B-1-Top-Bed-Low-E)
                                                                        (dm :RX-B-1-Top-Bed-Low-F)
                                                                        (dm :RX-B-1-Bot-Bed-Up-G)
                                                                        (dm :RX-B-1-Bot-Bed-Up-H)
                                                                        (dm :RX-B-1-Bot-Bed-Up-I)
                                                                        (dm :RX-B-1-Bot-Bed-Low-M)
                                                                        (dm :RX-B-1-Bot-Bed-Low-N)
                                                                        (dm :RX-B-1-Bot-Bed-Low-O)
                                                                        (dm :RX-A-2-Inlet-Top)
                                                                        (dm :Reactor-A-Outlet)
                                                                        (dm :RX-Feed-From-Heater-B)
                                                                        (dm :RX-B-2-Inlet-Temp)
                                                                        (dm :Reactor-B-2-Outlet))
                                                           (:Observed-WABT-Column-LJ @mongo/calculated-uom-data)
                                                           (:Observed-WABT-Column-LJ @mongo/calculated)))))

                   ;Rate Column K
                   (swap! calculated-data assoc :Rate-Column-K
                          (-> (get
                                @calculated-data :Rate-Column-K)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Make-Up-Gas-Rate
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2))
                                                           (:Rate-Column-K @mongo/calculated-uom-data)
                                                           (:Rate-Column-K @mongo/calculated))
                                )))

                   ;Hydrogen Purity: Column L
                   (swap! calculated-data assoc :Hydrogen-Purity-Column-L
                          (-> (get
                                @calculated-data :Hydrogen-Purity-Column-L)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Make-up-gas-Hydrogen-Purity
                                                                        (dm :Makeup-H2))
                                                           (:Hydrogen-Purity-Column-L @mongo/calculated-uom-data)
                                                           (:Hydrogen-Purity-Column-L @mongo/calculated))

                                )))

                   ;Rate: Column M
                   (swap! calculated-data assoc :Rate-Column-M
                          (-> (get
                                @calculated-data :Rate-Column-M)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Recycle-Gas-Rate
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-H2-To-Pass-B))
                                                           (:Rate-Column-M @mongo/calculated-uom-data)
                                                           (:Rate-Column-M @mongo/calculated)))))

                   ;Hydrogen Purity: Column N
                   (swap! calculated-data assoc :Hydrogen-Purity-Column-N
                          (-> (get
                                @calculated-data :Hydrogen-Purity-Column-N)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Recycle-Gas-Hydrogen-Purity
                                                                        (dm :Recycle-Gas-H2))
                                                           (:Hydrogen-Purity-Column-N @mongo/calculated-uom-data)
                                                           (:Hydrogen-Purity-Column-N @mongo/calculated)))))

                   ;Rate: Column W
                   (swap! calculated-data assoc :Rate-Column-W
                          (-> (get
                                @calculated-data :Rate-Column-W)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Quench-Gas-Rate
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1))
                                                           (:Rate-Column-W @mongo/calculated-uom-data)
                                                           (:Rate-Column-W @mongo/calculated))
                                )))

                   ;Hydrogen Purity: Column P
                   (swap! calculated-data assoc :Hydrogen-Purity-Column-P
                          (-> (get
                                @calculated-data :Hydrogen-Purity-Column-P)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Quench-Gas-Hydrogen-Purity
                                                                        (dm :Recycle-Gas-H2))
                                                           (:Hydrogen-Purity-Column-P @mongo/calculated-uom-data)
                                                           (:Hydrogen-Purity-Column-P @mongo/calculated))

                                )))

                   ;Rate: Column AA
                   (swap! calculated-data assoc :Rate-Column-AA
                          (-> (get
                                @calculated-data :Rate-Column-AA)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Treat-Gas-Rate
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :Purge-Rate))
                                                           (:Rate-Column-AA @mongo/calculated-uom-data)
                                                           (:Rate-Column-AA @mongo/calculated))
                                )))

                   ;Hydrogen Purity: Column AB
                   (swap! calculated-data assoc :Hydrogen-Purity-Column-AB
                          (-> (get
                                @calculated-data :Hydrogen-Purity-Column-AB)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Treat-Gas-Hydrogen-Purity
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Makeup-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-Gas-H2)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :H2-Quench-RX-B-2-Inlet))
                                                           (:Hydrogen-Purity-Column-AB @mongo/calculated-uom-data)
                                                           (:Hydrogen-Purity-Column-AB @mongo/calculated)))))

                   ;Total gas/oil ratio: Column LF
                   (swap! calculated-data assoc :Total-gas/oil-ratio-Column-LF
                          (-> (get
                                @calculated-data :Total-gas/oil-ratio-Column-LF)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Total-Gas-oil-Ratio
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :Purge-Rate)
                                                                        (dm :RX-FEED-TO-PASS-A)
                                                                        (dm :RX-FEED-TO-PASS-B))
                                                           (:Total-gas/oil-ratio-Column-LF @mongo/calculated-uom-data)
                                                           (:Total-gas/oil-ratio-Column-LF @mongo/calculated))
                                )))



                   ;Hydrogen/oil ratio: Column LG
                   (swap! calculated-data assoc :Hydrogen/oil-ratio-Column-LG
                          (-> (get
                                @calculated-data :Hydrogen/oil-ratio-Column-LG)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Hydrogen-Oil-ratio
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :Purge-Rate)
                                                                        (dm :Makeup-H2)
                                                                        (dm :Recycle-Gas-H2)
                                                                        (dm :RX-FEED-TO-PASS-A)
                                                                        (dm :RX-FEED-TO-PASS-B))
                                                           (:Hydrogen/oil-ratio-Column-LG @mongo/calculated-uom-data)
                                                           (:Hydrogen/oil-ratio-Column-LG @mongo/calculated))

                                )))


                   ;Train A: Column KU
                   (swap! calculated-data assoc :Train-A-Column-KU
                          (-> (get
                                @calculated-data :Train-A-Column-KU)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Estimated-Hydrogen-Consumption-Train-A
                                                                        (dm :Reactor-A-Outlet)
                                                                        (dm :RX-A-2-Inlet-Top)
                                                                        (dm :RX-A-1-Bot-Bed-Up-G)
                                                                        (dm :RX-A-1-Bot-Bed-Up-H)
                                                                        (dm :RX-A-1-Bot-Bed-Up-I)
                                                                        ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                                                        (dm :RX-A-1-Top-Bed-Low-D)
                                                                        (dm :RX-A-1-Top-Bed-Low-E)
                                                                        (dm :RX-A-1-Top-Bed-Low-F)
                                                                        ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                                                        (dm :RX-A-1-Bot-Bed-Low-M)
                                                                        (dm :RX-A-1-Bot-Bed-Low-N)
                                                                        (dm :RX-A-1-Bot-Bed-Low-O)
                                                                        ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                                                        (dm :RX-A-1-Feed-From-Heater-A)
                                                                        (dm :Est-oF-SCF-BBL))
                                                           (:Train-A-Column-KU @mongo/calculated-uom-data)
                                                           (:Train-A-Column-KU @mongo/calculated))

                                )))


                   ;Hydrogen availability Column KZ
                   (swap! calculated-data assoc :Hydrogen-Availability-Column-KZ
                          (-> (get
                                @calculated-data :Hydrogen-Availability-Column-KZ)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Hydrogen-Availability
                                                                        (dm :Est-oF-SCF-BBL)
                                                                        (dm :Reactor-B-2-Outlet)
                                                                        (dm :RX-B-2-Inlet-Temp)
                                                                        (dm :RX-B-1-Top-Bed-Low-D)
                                                                        (dm :RX-B-1-Top-Bed-Low-E)
                                                                        (dm :RX-B-1-Top-Bed-Low-F)
                                                                        ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                                                        (dm :RX-B-1-Bot-Bed-Up-G)
                                                                        (dm :RX-B-1-Bot-Bed-Up-H)
                                                                        (dm :RX-B-1-Bot-Bed-Up-I)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                                                        (dm :RX-B-1-Bot-Bed-Low-M)
                                                                        (dm :RX-B-1-Bot-Bed-Low-N)
                                                                        (dm :RX-B-1-Bot-Bed-Low-O)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                                                        (dm :RX-Feed-From-Heater-B)
                                                                        (dm :Makeup-H2)
                                                                        (dm :Recycle-Gas-H2)
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :Purge-Rate)
                                                                        (dm :RX-FEED-TO-PASS-A)
                                                                        (dm :RX-FEED-TO-PASS-B))
                                                           (:Hydrogen-Availability-Column-KZ @mongo/calculated-uom-data)
                                                           (:Hydrogen-Availability-Column-KZ @mongo/calculated))
                                )))

                   ;Train B: Column KV
                   (swap! calculated-data assoc :Train-B-Column-KV
                          (-> (get
                                @calculated-data :Train-B-Column-KV)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Estimated-Hydrogen-Consumption-Train-B
                                                                        (dm :Reactor-B-2-Outlet)
                                                                        (dm :RX-B-2-Inlet-Temp)
                                                                        (dm :RX-B-1-Top-Bed-Low-D)
                                                                        (dm :RX-B-1-Top-Bed-Low-E)
                                                                        (dm :RX-B-1-Top-Bed-Low-F)
                                                                        ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                                                        (dm :RX-B-1-Bot-Bed-Up-G)
                                                                        (dm :RX-B-1-Bot-Bed-Up-H)
                                                                        (dm :RX-B-1-Bot-Bed-Up-I)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                                                        (dm :RX-B-1-Bot-Bed-Low-M)
                                                                        (dm :RX-B-1-Bot-Bed-Low-N)
                                                                        (dm :RX-B-1-Bot-Bed-Low-O)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                                                        (dm :RX-Feed-From-Heater-B)
                                                                        (dm :Est-oF-SCF-BBL))
                                                           (:Train-B-Column-KV @mongo/calculated-uom-data)
                                                           (:Train-B-Column-KV @mongo/calculated)))))



                   ;Train A inlet pressure: Column AD
                   (swap! calculated-data assoc :Train-A-Inlet-Pressure-Column-AD
                          (-> (get
                                @calculated-data :Train-A-Inlet-Pressure-Column-AD)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Inlet-Pressure
                                                                        (dm :RX-A-1-Inlet-Pressure)
                                                                        )
                                                           (:Train-A-Inlet-Pressure-Column-AD @mongo/calculated-uom-data)
                                                           (:Train-A-Inlet-Pressure-Column-AD @mongo/calculated)))))

                   ;Train B inlet pressure: Column AH
                   (swap! calculated-data assoc :Train-B-inlet-Pressure-Column-AH
                          (-> (get
                                @calculated-data :Train-B-inlet-Pressure-Column-AH)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Inlet-Pressure
                                                                        (dm :RX-B-1-Inlet-Pressure)
                                                                        )
                                                           (:Train-B-inlet-Pressure-Column-AH @mongo/calculated-uom-data)
                                                           (:Train-B-inlet-Pressure-Column-AH @mongo/calculated))
                                )))

                   ;Train A hydrogen partial pressure: Column LA
                   (swap! calculated-data assoc :Train-A-Hydrogen-Partial-Pressure-Column-LA
                          (-> (get
                                @calculated-data :Train-A-Hydrogen-Partial-Pressure-Column-LA)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Hydrogen-Partial
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Makeup-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-Gas-H2)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :RX-A-1-Overall-Delta-P)
                                                                        (dm :Reactor-A-2-Diff-Press)
                                                                        (dm :RX-A-1-Inlet-Pressure)
                                                                        )
                                                           (:Train-A-Hydrogen-Partial-Pressure-Column-LA @mongo/calculated-uom-data)
                                                           (:Train-A-Hydrogen-Partial-Pressure-Column-LA @mongo/calculated))

                                )))

                   ;Train A hydrogen partial pressure: Column LB
                   (swap! calculated-data assoc :Train-B-Hydrogen-Partial-Pressure-Column-LB
                          (-> (get
                                @calculated-data :Train-B-Hydrogen-Partial-Pressure-Column-LB)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Hydrogen-Partial
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Makeup-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-Gas-H2)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :RX-B-1-Inlet-Pressure)
                                                                        (dm :RX-B-1-Overall-Delta-P)
                                                                        (dm :Reactor-B-2-Diff-Press))
                                                           (:Train-B-Hydrogen-Partial-Pressure-Column-LB @mongo/calculated-uom-data)
                                                           (:Train-B-Hydrogen-Partial-Pressure-Column-LB @mongo/calculated))
                                )))

                   ;Total pressure drop: Column AG
                   (swap! calculated-data assoc :Total-Pressure-Drop-Column-AG
                          (-> (get
                                @calculated-data :Total-Pressure-Drop-Column-AG)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Total-Pressure-Drop
                                                                        (dm :RX-A-1-Overall-Delta-P)
                                                                        (dm :Reactor-A-2-Diff-Press))
                                                           (:Total-Pressure-Drop-Column-AG @mongo/calculated-uom-data)
                                                           (:Total-Pressure-Drop-Column-AG @mongo/calculated))
                                )))


                   ;Normalized pressure drop: Column LC
                   (swap! calculated-data assoc :Normalized-Pressure-Drop-Column-LC
                          (-> (get
                                @calculated-data :Normalized-Pressure-Drop-Column-LC)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Normalized-Pressure-Drop
                                                                        (dm :SOR-Ref-Day-Feed-Rate)
                                                                        (dm :SOR-Ref-Day-Treat-Gas-Rate)
                                                                        (dm :Feed-Exp)
                                                                        (dm :Treat-Gas-Exp)
                                                                        (dm :RX-A-1-Overall-Delta-P)
                                                                        (dm :Reactor-A-2-Diff-Press)
                                                                        (dm :RX-FEED-TO-PASS-A)
                                                                        (dm :RX-FEED-TO-PASS-B)
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :Purge-Rate))
                                                           (:Normalized-Pressure-Drop-Column-LC @mongo/calculated-uom-data)
                                                           (:Normalized-Pressure-Drop-Column-LC @mongo/calculated)))))



                   ;Total pressure drop: Column AK
                   (swap! calculated-data assoc :Total-Pressure-Drop-Column-AK
                          (-> (get
                                @calculated-data :Total-Pressure-Drop-Column-AK)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Total-Pressure-Drop
                                                                        (dm :RX-B-1-Overall-Delta-P)
                                                                        (dm :Reactor-B-2-Diff-Press))
                                                           (:Total-Pressure-Drop-Column-AK @mongo/calculated-uom-data)
                                                           (:Total-Pressure-Drop-Column-AK @mongo/calculated))
                                )))



                   ;Normalized pressure drop: Column LD
                   (swap! calculated-data assoc :Normalized-Pressure-Drop-Column-LD
                          (-> (get
                                @calculated-data :Normalized-Pressure-Drop-Column-LD)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Normalized-Pressure-Drop
                                                                        (dm :RX-B-1-Overall-Delta-P)
                                                                        (dm :Reactor-B-2-Diff-Press)
                                                                        (dm :SOR-Ref-Day-Feed-Rate)
                                                                        (dm :RX-FEED-TO-PASS-A)
                                                                        (dm :RX-FEED-TO-PASS-B)
                                                                        (dm :Feed-Exp)
                                                                        (dm :SOR-Ref-Day-Treat-Gas-Rate)
                                                                        (dm :Make-Up-Gas-From-Unicrack-H2)
                                                                        (dm :Recycle-H2-To-Pass-A)
                                                                        (dm :Recycle-H2-To-Pass-B)
                                                                        (dm :H2-Quench-To-MID-RX-A-1)
                                                                        (dm :H2-Quench-RX-A-2-Inlet)
                                                                        (dm :H2-Quench-To-MID-RX-B-1)
                                                                        (dm :H2-Quench-RX-B-2-Inlet)
                                                                        (dm :Purge-Rate)
                                                                        (dm :Treat-Gas-Exp))
                                                           (:Normalized-Pressure-Drop-Column-LD @mongo/calculated-uom-data)
                                                           (:Normalized-Pressure-Drop-Column-LD @mongo/calculated))
                                )))

                   ;Train A DT: Column AY
                   (swap! calculated-data assoc :Train-A-DT-Column-AY
                          (-> (get
                                @calculated-data :Train-A-DT-Column-AY)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Total-Temperature-Rises
                                                                        (dm :Reactor-A-Outlet)
                                                                        (dm :RX-A-2-Inlet-Top)
                                                                        (dm :RX-A-1-Bot-Bed-Up-G)
                                                                        (dm :RX-A-1-Bot-Bed-Up-H)
                                                                        (dm :RX-A-1-Bot-Bed-Up-I)
                                                                        ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                                                        (dm :RX-A-1-Top-Bed-Low-D)
                                                                        (dm :RX-A-1-Top-Bed-Low-E)
                                                                        (dm :RX-A-1-Top-Bed-Low-F)
                                                                        ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                                                        (dm :RX-A-1-Bot-Bed-Low-M)
                                                                        (dm :RX-A-1-Bot-Bed-Low-N)
                                                                        (dm :RX-A-1-Bot-Bed-Low-O)
                                                                        ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                                                        (dm :RX-A-1-Feed-From-Heater-A))
                                                           (:Train-A-DT-Column-AY @mongo/calculated-uom-data)
                                                           (:Train-A-DT-Column-AY @mongo/calculated))
                                )))


                   ;Train B DT: Column BM
                   (swap! calculated-data assoc :Train-B-DT-Column-BM
                          (-> (get
                                @calculated-data :Train-B-DT-Column-BM)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Total-Temperature-Rises
                                                                        (dm :Reactor-B-2-Outlet)
                                                                        (dm :RX-B-2-Inlet-Temp)
                                                                        (dm :RX-B-1-Top-Bed-Low-D)
                                                                        (dm :RX-B-1-Top-Bed-Low-E)
                                                                        (dm :RX-B-1-Top-Bed-Low-F)
                                                                        ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                                                        (dm :RX-B-1-Bot-Bed-Up-G)
                                                                        (dm :RX-B-1-Bot-Bed-Up-H)
                                                                        (dm :RX-B-1-Bot-Bed-Up-I)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                                                        (dm :RX-B-1-Bot-Bed-Low-M)
                                                                        (dm :RX-B-1-Bot-Bed-Low-N)
                                                                        (dm :RX-B-1-Bot-Bed-Low-O)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                                                        ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                                                        (dm :RX-Feed-From-Heater-B))
                                                           (:Train-B-DT-Column-BM @mongo/calculated-uom-data)
                                                           (:Train-B-DT-Column-BM @mongo/calculated))
                                )))


                   ;A1 Bed 1 DT: Column AO
                   (swap! calculated-data assoc :A1-Bed-1-DT-Column-AO
                          (-> (get
                                @calculated-data :A1-Bed-1-DT-Column-AO)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-1-Bed-1-Temperature-Rises
                                                                        (dm :RX-A-1-Top-Bed-Low-D)
                                                                        (dm :RX-A-1-Top-Bed-Low-E)
                                                                        (dm :RX-A-1-Top-Bed-Low-F)
                                                                        ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                                                        (dm :RX-A-1-Feed-From-Heater-A))
                                                           (:A1-Bed-1-DT-Column-AO @mongo/calculated-uom-data)
                                                           (:A1-Bed-1-DT-Column-AO @mongo/calculated))

                                )))


                   ;A1 Bed 2 DT: Column AS
                   (swap! calculated-data assoc :A1-Bed-2-DT-Column-AS
                          (-> (get
                                @calculated-data :A1-Bed-2-DT-Column-AS)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-1-Bed-2-Temperature-Rises
                                                                        (dm :RX-A-1-Bot-Bed-Up-G)
                                                                        (dm :RX-A-1-Bot-Bed-Up-H)
                                                                        (dm :RX-A-1-Bot-Bed-Up-I)
                                                                        ;(dm :A-1-Average-Bottom-Bed-Up-Temp)
                                                                        (dm :RX-A-1-Bot-Bed-Low-M)
                                                                        (dm :RX-A-1-Bot-Bed-Low-N)
                                                                        (dm :RX-A-1-Bot-Bed-Low-O)
                                                                        ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                                                        )
                                                           (:A1-Bed-2-DT-Column-AS @mongo/calculated-uom-data)
                                                           (:A1-Bed-2-DT-Column-AS @mongo/calculated))
                                )))


                   ;A2 DT: Column AX
                   (swap! calculated-data assoc :A2-DT-Column-AX
                          (-> (get
                                @calculated-data :A2-DT-Column-AX)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-2-Temperature-Rises
                                                                        (dm :Reactor-A-Outlet)
                                                                        (dm :RX-A-2-Inlet-Top))
                                                           (:A2-DT-Column-AX @mongo/calculated-uom-data)
                                                           (:A2-DT-Column-AX @mongo/calculated))
                                )))


                   ;B1 Bed 1 DT: Column BC
                   (swap! calculated-data assoc :B1-Bed-1-DT-Column-BC
                          (-> (get
                                @calculated-data :B1-Bed-1-DT-Column-BC)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-1-Bed-1-Temperature-Rises
                                                                        (dm :RX-B-1-Top-Bed-Low-D)
                                                                        (dm :RX-B-1-Top-Bed-Low-E)
                                                                        (dm :RX-B-1-Top-Bed-Low-F)
                                                                        ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                                                        (dm :RX-Feed-From-Heater-B))
                                                           (:B1-Bed-1-DT-Column-BC @mongo/calculated-uom-data)
                                                           (:B1-Bed-1-DT-Column-BC @mongo/calculated))
                                )))


                   ;B1 Bed 2 DT: Column BG
                   (swap! calculated-data assoc :B1-Bed-2-DT-Column-BG
                          (-> (get
                                @calculated-data :B1-Bed-2-DT-Column-BG)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-1-Bed-2-Temperature-Rises

                                                                        (dm :RX-B-1-Bot-Bed-Up-G)
                                                                        (dm :RX-B-1-Bot-Bed-Up-H)
                                                                        (dm :RX-B-1-Bot-Bed-Up-I)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Up-Temp)
                                                                        (dm :RX-B-1-Bot-Bed-Low-M)
                                                                        (dm :RX-B-1-Bot-Bed-Low-N)
                                                                        (dm :RX-B-1-Bot-Bed-Low-O)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                                                        )
                                                           (:B1-Bed-2-DT-Column-BG @mongo/calculated-uom-data)
                                                           (:B1-Bed-2-DT-Column-BG @mongo/calculated))
                                )))


                   ;B2 DT: Column BL
                   (swap! calculated-data assoc :B2-DT-Column-BL
                          (-> (get
                                @calculated-data :B2-DT-Column-BL)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-2-Temperature-Rises
                                                                        (dm :Reactor-B-2-Outlet)
                                                                        (dm :RX-B-2-Inlet-Temp))
                                                           (:B2-DT-Column-BL @mongo/calculated-uom-data)
                                                           (:B2-DT-Column-BL @mongo/calculated))
                                )))

                   ;A1 Bed 1 inlet T: Column AL
                   (swap! calculated-data assoc :A1-Bed-1-inlet-T-Column-AL
                          (-> (get
                                @calculated-data :A1-Bed-1-inlet-T-Column-AL)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-1-Bed-1-Inlet
                                                                        (dm :RX-A-1-Feed-From-Heater-A))
                                                           (:A1-Bed-1-inlet-T-Column-AL @mongo/calculated-uom-data)
                                                           (:A1-Bed-1-inlet-T-Column-AL @mongo/calculated))
                                )))


                   ;A1 Bed 1 outlet T: Column AN
                   (swap! calculated-data assoc :A1-Bed-1-outlet-T-Column-AN
                          (-> (get
                                @calculated-data :A1-Bed-1-outlet-T-Column-AN)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-1-Bed-1-Outlet

                                                                        (dm :RX-A-1-Top-Bed-Low-D)
                                                                        (dm :RX-A-1-Top-Bed-Low-E)
                                                                        (dm :RX-A-1-Top-Bed-Low-F)
                                                                        ;(dm :A-1-Average-Top-Bed-Low-Temp)
                                                                        )
                                                           (:A1-Bed-1-outlet-T-Column-AN @mongo/calculated-uom-data)
                                                           (:A1-Bed-1-outlet-T-Column-AN @mongo/calculated))
                                )))


                   ;A1 Bed 2 outlet T: Column AR
                   (swap! calculated-data assoc :A1-Bed-2-outlet-T-Column-AR
                          (-> (get
                                @calculated-data :A1-Bed-2-outlet-T-Column-AR)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-1-Bed-2-Outlet

                                                                        (dm :RX-A-1-Bot-Bed-Low-M)
                                                                        (dm :RX-A-1-Bot-Bed-Low-N)
                                                                        (dm :RX-A-1-Bot-Bed-Low-O)
                                                                        ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                                                                        )
                                                           (:A1-Bed-2-outlet-T-Column-AR @mongo/calculated-uom-data)
                                                           (:A1-Bed-2-outlet-T-Column-AR @mongo/calculated))
                                )))

                   ;A2 outlet T: Column AW
                   (swap! calculated-data assoc :A2-outlet-T-Column-AW
                          (-> (get
                                @calculated-data :A2-outlet-T-Column-AW)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-2-Outlet
                                                                        (dm :Reactor-A-Outlet))
                                                           (:A2-outlet-T-Column-AW @mongo/calculated-uom-data)
                                                           (:A2-outlet-T-Column-AW @mongo/calculated))
                                )))

                   ;B1 Bed 1 inlet T: Column AZ
                   (swap! calculated-data assoc :B1-Bed-1-inlet-T-Column-AZ
                          (-> (get
                                @calculated-data :B1-Bed-1-inlet-T-Column-AZ)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-1-Bed-1-Inlet
                                                                        (dm :RX-Feed-From-Heater-B))
                                                           (:B1-Bed-1-inlet-T-Column-AZ @mongo/calculated-uom-data)
                                                           (:B1-Bed-1-inlet-T-Column-AZ @mongo/calculated))
                                )))

                   ;B1 Bed 1 outlet T: Column BB
                   (swap! calculated-data assoc :B1-Bed-1-outlet-T-Column-BB
                          (-> (get
                                @calculated-data :B1-Bed-1-outlet-T-Column-BB)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-1-Bed-1-Outlet
                                                                        (dm :RX-B-1-Top-Bed-Low-D)
                                                                        (dm :RX-B-1-Top-Bed-Low-E)
                                                                        (dm :RX-B-1-Top-Bed-Low-F)
                                                                        ;(dm :B-1-Average-Top-Bed-Low-Temp)
                                                                        )
                                                           (:B1-Bed-1-outlet-T-Column-BB @mongo/calculated-uom-data)
                                                           (:B1-Bed-1-outlet-T-Column-BB @mongo/calculated))
                                )))


                   ;B1 Bed 2 outlet T: Column BF
                   (swap! calculated-data assoc :B1-Bed-2-outlet-T-Column-BF
                          (-> (get
                                @calculated-data :B1-Bed-2-outlet-T-Column-BF)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-2-Bed-1-Outlet

                                                                        (dm :RX-B-1-Bot-Bed-Low-M)
                                                                        (dm :RX-B-1-Bot-Bed-Low-N)
                                                                        (dm :RX-B-1-Bot-Bed-Low-O)
                                                                        ;(dm :B-1-Average-Bottom-Bed-Low-Temp)
                                                                        )
                                                           (:B1-Bed-2-outlet-T-Column-BF @mongo/calculated-uom-data)
                                                           (:B1-Bed-2-outlet-T-Column-BF @mongo/calculated))
                                )))

                   ;B2 outlet T: Column BK
                   (swap! calculated-data assoc :B2-outlet-T-Column-BK
                          (-> (get
                                @calculated-data :B2-outlet-T-Column-BK)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-2-Outlet
                                                                        (dm :Reactor-B-2-Outlet))
                                                           (:B2-outlet-T-Column-BK @mongo/calculated-uom-data)
                                                           (:B2-outlet-T-Column-BK @mongo/calculated))
                                )))

                   ;Top radial spread: Column BZ
                   (swap! calculated-data assoc :Top-radial-spread-Column-BZ
                          (-> (get
                                @calculated-data :Top-radial-spread-Column-BZ)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Reactor-1-Bed-1-Radial-Temperature-Top-Spreads
                                                                        (dm :RX-A-1-Top-Bed-Up-A)
                                                                        (dm :RX-A-1-Top-Bed-Up-B)
                                                                        (dm :RX-A-1-Top-Bed-Up-C))
                                                           (:Top-radial-spread-Column-BZ @mongo/calculated-uom-data)
                                                           (:Top-radial-spread-Column-BZ @mongo/calculated))
                                )))

                   ;Bottom radial spread: Column CA
                   (swap! calculated-data assoc :Bottom-radial-spread-Column-CA
                          (-> (get
                                @calculated-data :Bottom-radial-spread-Column-CA)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Reactor-1-Bed-1-Radial-Temperature-Bottom-Spreads
                                                                        (dm :RX-A-1-Top-Bed-Low-D)
                                                                        (dm :RX-A-1-Top-Bed-Low-E)
                                                                        (dm :RX-A-1-Top-Bed-Low-F))
                                                           (:Bottom-radial-spread-Column-CA @mongo/calculated-uom-data)
                                                           (:Bottom-radial-spread-Column-CA @mongo/calculated))
                                )))



                   ;Top radial spread: Column CB
                   (swap! calculated-data assoc :Top-radial-spread-Column-CB
                          (-> (get
                                @calculated-data :Top-radial-spread-Column-CB)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Reactor-1-Bed-2-Radial-Temperature-Top-Spreads
                                                                        (dm :RX-A-1-Bot-Bed-Up-G)
                                                                        (dm :RX-A-1-Bot-Bed-Up-H)
                                                                        (dm :RX-A-1-Bot-Bed-Up-I))
                                                           (:Top-radial-spread-Column-CB @mongo/calculated-uom-data)
                                                           (:Top-radial-spread-Column-CB @mongo/calculated))
                                )))



                   ;Middle radial spread: CC
                   (swap! calculated-data assoc :Middle-radial-spread-Column-CC
                          (-> (get
                                @calculated-data :Middle-radial-spread-Column-CC)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Reactor-1-Bed-2-Radial-Temperature-Middle-Spreads
                                                                        (dm :RX-A-1-Bot-Bed-Mid-J)
                                                                        (dm :RX-A-1-Bot-Bed-Mid-K)
                                                                        (dm :RX-A-1-Bot-Bed-Mid-L))
                                                           (:Middle-radial-spread-Column-CC @mongo/calculated-uom-data)
                                                           (:Middle-radial-spread-Column-CC @mongo/calculated))
                                )))



                   ;Bottom radial spread: Column CD
                   (swap! calculated-data assoc :Bottom-radial-spread-Column-CD
                          (-> (get
                                @calculated-data :Bottom-radial-spread-Column-CD)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Reactor-1-Bed-2-Radial-Temperature-Bottom-Spreads
                                                                        (dm :RX-A-1-Bot-Bed-Low-M)
                                                                        (dm :RX-A-1-Bot-Bed-Low-N)
                                                                        (dm :RX-A-1-Bot-Bed-Low-O))
                                                           (:Bottom-radial-spread-Column-CD @mongo/calculated-uom-data)
                                                           (:Bottom-radial-spread-Column-CD @mongo/calculated))
                                )))

                   ;Radial spread: Column CE
                   (swap! calculated-data assoc :Radial-spread-Column-CE
                          (-> (get
                                @calculated-data :Radial-spread-Column-CE)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-A-Reactor-2-Radial-Temperature-Spreads
                                                                        (dm :A-2-Reactor-Bed-A)
                                                                        (dm :A-2-Reactor-Bed-B)
                                                                        (dm :A-2-Reactor-Bed-C))
                                                           (:Radial-spread-Column-CE @mongo/calculated-uom-data)
                                                           (:Radial-spread-Column-CE @mongo/calculated))
                                )))

                   ;Top radial spread: Column CF
                   (swap! calculated-data assoc :Top-radial-spread-Column-CF
                          (-> (get
                                @calculated-data :Top-radial-spread-Column-CF)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Reactor-1-Bed-1-Radial-Temperature-Top-Spreads
                                                                        (dm :RX-B-1-Top-Bed-Up-A)
                                                                        (dm :RX-B-1-Top-Bed-Up-B)
                                                                        (dm :RX-B-1-Top-Bed-Up-C))
                                                           (:Top-radial-spread-Column-CF @mongo/calculated-uom-data)
                                                           (:Top-radial-spread-Column-CF @mongo/calculated))
                                )))

                   ;Bottom radial spread: Column CG
                   (swap! calculated-data assoc :Bottom-radial-spread-Column-CG
                          (-> (get
                                @calculated-data :Bottom-radial-spread-Column-CG)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Reactor-1-Bed-1-Radial-Temperature-Bottom-Spreads
                                                                        (dm :RX-B-1-Top-Bed-Low-D)
                                                                        (dm :RX-B-1-Top-Bed-Low-E)
                                                                        (dm :RX-B-1-Top-Bed-Low-F))
                                                           (:Bottom-radial-spread-Column-CG @mongo/calculated-uom-data)
                                                           (:Bottom-radial-spread-Column-CG @mongo/calculated))
                                )))


                   ;Top radial spread: Column CH
                   (swap! calculated-data assoc :Top-radial-spread-Column-CH
                          (-> (get
                                @calculated-data :Top-radial-spread-Column-CH)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Reactor-1-Bed-2-Radial-Temperature-Top-Spreads
                                                                        (dm :RX-B-1-Bot-Bed-Up-G)
                                                                        (dm :RX-B-1-Bot-Bed-Up-H)
                                                                        (dm :RX-B-1-Bot-Bed-Up-I))
                                                           (:Top-radial-spread-Column-CH @mongo/calculated-uom-data)
                                                           (:Top-radial-spread-Column-CH @mongo/calculated))
                                )))

                   ;Middle radial spread: CI
                   (swap! calculated-data assoc :Middle-radial-spread-Column-CI
                          (-> (get
                                @calculated-data :Middle-radial-spread-Column-CI)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Reactor-1-Bed-2-Radial-Temperature-Middle-Spreads
                                                                        (dm :RX-B-1-Bot-Bed-Mid-J)
                                                                        (dm :RX-B-1-Bot-Bed-Mid-K)
                                                                        (dm :RX-B-1-Bot-Bed-Mid-L))
                                                           (:Middle-radial-spread-Column-CI @mongo/calculated-uom-data)
                                                           (:Middle-radial-spread-Column-CI @mongo/calculated))
                                )))


                   ;Bottom radial spread: Column CJ
                   (swap! calculated-data assoc :Bottom-radial-spread-Column-CJ
                          (-> (get
                                @calculated-data :Bottom-radial-spread-Column-CJ)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Reactor-1-Bed-2-Radial-Temperature-Bottom-Spreads
                                                                        (dm :RX-B-1-Bot-Bed-Low-M)
                                                                        (dm :RX-B-1-Bot-Bed-Low-N)
                                                                        (dm :RX-B-1-Bot-Bed-Low-O))
                                                           (:Bottom-radial-spread-Column-CJ @mongo/calculated-uom-data)
                                                           (:Bottom-radial-spread-Column-CJ @mongo/calculated))
                                )))

                   ;Radial spread: Column CK
                   (swap! calculated-data assoc :Radial-spread-Column-CK
                          (-> (get
                                @calculated-data :Radial-spread-Column-CK)
                              (conj
                                (mongo/convert-to-base-uom (e/try-catch c/Train-B-Reactor-2-Radial-Temperature-Spreads
                                                                        (dm :B-2-Reactor-Bed-A)
                                                                        (dm :B-2-Reactor-Bed-B)
                                                                        (dm :B-2-Reactor-Bed-C)
                                                                        )
                                                           (:Radial-spread-Column-CK @mongo/calculated-uom-data)
                                                           (:Radial-spread-Column-CK @mongo/calculated))
                                )))) data)))
(defn daily-average [plant-id date]
  (let [ret-c (chan)]
    (go
      (let [day (.getUTCDate date)
            month (+ (.getUTCMonth date) 1)
            year (.getUTCFullYear date)
            hour (.getUTCHours date)
            min (.getUTCMinutes date)
            sec (.getUTCSeconds date)
            date-string (str year "-" month "-" day)
            time-string (str hour ":" min ":" sec)
            sql-raw-data (<! (con/find-raw-sensors-data plant-id date-string))]

        (if (sql-raw-data :err)
          (mongo/ret-err ret-c (sql-raw-data :err))
          (let [{:keys [err msg result]} (<! (con/insert-bulk-data
                                               (mapv (fn [d]
                                                       (let [sensor-data (remove nil? (d :sensor-value))
                                                             length (count sensor-data)
                                                             sum (apply + sensor-data)
                                                             avg (/ sum length)]
                                                         (vector (get @mongo/avg-raw (keyword (d :sensor-name)))
                                                                 date date avg)))
                                                     (sql-raw-data :result)) plant-id))]
            (if err
              (mongo/ret-err ret-c err)
              (put! ret-c result)
              (close! ret-c))))))
    ret-c))



(defn get-sensor-data [plant-id date]
  (mongo/initialize-data plant-id)
  (let [ret-chan (promise-chan)]
    (go
      (let [{:keys [err msg result]} (<! (daily-average plant-id date))]
        (if err
          (put! ret-chan {:err err :msg msg})
          (let [day (.getUTCDate date)
                month (+ (.getUTCMonth date) 1)
                year (.getUTCFullYear date)
                date-string (str year "-" month "-" day)
                avg-sensor (vals @mongo/avg-raw)]
            (let [{:keys [err msg result]}
                  (<! (con/find-sensors-data plant-id date-string avg-sensor))]
              (if err
                (put! ret-chan {:err err :msg msg})
                (let [
                      sd (raw-to-internal-parser result)
                      pd @mongo/plant
                      new-data (merge pd sd)]
                  (if (> (count result) 0)
                    (do
                      (reset! calculated-data {})
                      (calculation (vector new-data))
                      (column-sensor @calculated-data date)
                      (reset! mi-calculated-data {})
                      (reset! ms-calculated-data {})
                      (rpc-calculation (vector new-data))
                      (add-watch mi-calculated-data :watcher
                                 (fn [key data old-state new-state]
                                   (let [Normalized-HDS-WABT-Column-MI (first (:Normalized-HDS-WABT-Column-MI new-state))]
                                     (if Normalized-HDS-WABT-Column-MI
                                       (column-sensor @mi-calculated-data date)))))
                      (add-watch ms-calculated-data :watcher
                                 (fn [key data old-state new-state]
                                   (let [Normalized-HDB-WABT-Column-MS (first (:Normalized-HDB-WABT-Column-MS new-state))]
                                     (if Normalized-HDB-WABT-Column-MS
                                       (column-sensor @ms-calculated-data date))))))))))
            (log/info "calculation done for plant-id " plant-id "date" date)
            ))))
    ret-chan))

(defn get-sensor [plant-id]
  (go
    (let [{:keys [err msg result]}
          (<! (con/find-sensors plant-id))]

      )))

(defn temp-calculation [plant-id]
  (let [start-date (js/Date. "2015-05-12T18:30:00.000-00:00") ;"day-1"
        end-date (js/Date. "2015-05-14T18:30:00.000-00:00") ;"day+1"
        date-dif (.floor js/Math (/ (- end-date start-date) 1000 3600 24))]

    (let [xm (map-indexed (fn [i]
                            (fn []
                              (if (> i 0)
                                (get-sensor-data plant-id (js/Date. (.setDate start-date (+ (.getDate start-date) 1))))
                                (get-sensor-data plant-id (js/Date. start-date)))))
                          (range 0 date-dif))
          i (atom 0)
          intind (atom 0)
          intfn (fn [] (let [cf (nth xm @i)]
                         (cf)
                         (swap! i inc)
                         (if (and (> @i date-dif) (not= @intind 0))
                           (js/clearInterval @intind))
                         ))]
      (reset! intind (js/setInterval intfn 10000)))
    ))

(defn init-task []
  (let [tasktemp (u/ocall cron :schedule (get-in @config [:schedule-time :hdt]) (fn []
                                                    (let [d (js/Date.)
                                                          date (js/Date. (.setDate d (- (.getDate d) 1)))]
                                                          (get-sensor-data (get-in @config [:schedule-time :plant-id])
                                                           (js/Date. date)))))]
    (reset! task tasktemp)))

(defn scheduled-task []
  (try
  (do (init-task)
        (u/ocall @task :start)
          (log/info "Initalized scheduler for every day..."))
      (catch  Exception e
        (log/info "Error occured here" (.getMessage e))))
  {:err false})


(defn scheduled-end-task []
  (u/ocall @task :stop)
  (log/info "Stop all task..."))







