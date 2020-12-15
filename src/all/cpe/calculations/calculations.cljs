(ns cpe.calculations.calculations
  (:require [cpe.calculations.dist :as dist]
            [cpe.calculations.rpc :as rpc]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.calculations.error :as e]
            [cljs.core.async :refer [put! take! chan promise-chan <! close!]]
            [clojure.core.async.impl.channels  :as m])
  (:require-macros [cljs.core.async.macros :refer [go]]))

;Missing Purge-Rate {SCFH} [
;Missing SOR Ref. Day

(defn- InterPol [vec-data value]
  (let [sorted-data (sort-by :x vec-data)
        greater (->>
                  (map (fn [d]
                         (if (> (d :x) value)
                           d)) sorted-data)
                  (remove nil?))
        smaller (->> (map (fn [d]
                            (if (< (d :x) value)
                              d)) sorted-data)
                     (remove nil?))
        closest-smaller (if (last smaller)
                          (last smaller)
                          {:x 0 :y 0})
        closest-greater (if (first greater)
                          (first greater)
                          {:x 0 :y 0})

        xa (- (closest-greater :x) value)
        xb (- (closest-greater :x)
              (closest-smaller :x))
        ya1 (/ (closest-greater :y) 100)
        yb1 (/ (closest-smaller :y) 100)
        ya (- ya1 yb1)
        yb (/ (closest-smaller :y) 100)
        dif (- 1 (/ xa xb))
        interpol (+ (* dif ya) yb)]
    interpol))



;;;;;;;;;Common Function ;;;;;;
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column CM (Makeup H2) Unit (Mol%)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column CJ (Recycle Gas H2) Unit (Mol%)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
(defn- Treat-Gas-Purity [Make-Up-Gas-From-Unicrack-H2
                         Makeup-H2
                         Recycle-H2-To-Pass-A
                         Recycle-Gas-H2
                         Recycle-H2-To-Pass-B
                         H2-Quench-To-MID-RX-A-1
                         H2-Quench-RX-A-2-Inlet
                         H2-Quench-To-MID-RX-B-1
                         H2-Quench-RX-B-2-Inlet]
  (let [af (+ H2-Quench-RX-B-2-Inlet
              H2-Quench-To-MID-RX-B-1
              H2-Quench-RX-A-2-Inlet
              H2-Quench-To-MID-RX-A-1)                      ;SUM(AD8+AB8+Z8+X8)
        ah (+ af Make-Up-Gas-From-Unicrack-H2
              Recycle-H2-To-Pass-A
              Recycle-H2-To-Pass-B)                         ;SUM(AF7,R7,T7:U7)
        data (/ (+ (* Make-Up-Gas-From-Unicrack-H2 Makeup-H2)
                   (* Recycle-H2-To-Pass-A Recycle-Gas-H2)
                   (* Recycle-H2-To-Pass-B Recycle-Gas-H2)
                   (* H2-Quench-To-MID-RX-A-1 Recycle-Gas-H2)
                   (* H2-Quench-RX-A-2-Inlet Recycle-Gas-H2)
                   (* H2-Quench-To-MID-RX-B-1 Recycle-Gas-H2)
                   (* H2-Quench-RX-B-2-Inlet Recycle-Gas-H2))
                ah)]
    data))

;Input param ClientData Column D (RX FEED TO PASS A) Unit (BPD)
;Input param ClientData Column E (RX FEED TO PASS B) Unit (BPD)
(defn- Combined-Feed-Rate [RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B]
  (+ RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B)
  )

;Input param ClientData Column AG (RX A-1 OVERALL DELTA P) Unit (PSIG)
;Input param ClientData Column AQ (REACTOR A-2 DIFF PRESS) Unit (PSIG)
(defn- Total-Delta-P-Train-A [RX-A-1-Overall-Delta-P
                              Reactor-A-2-Diff-Press]
  (+ RX-A-1-Overall-Delta-P Reactor-A-2-Diff-Press))

;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param Data-Evaluation Column X (Purge Rate) Unit (SCFH)
(defn- Treat-Gas-Rate-Z [Make-Up-Gas-From-Unicrack-H2
                         Recycle-H2-To-Pass-A
                         Recycle-H2-To-Pass-B
                         H2-Quench-To-MID-RX-A-1
                         H2-Quench-RX-A-2-Inlet
                         H2-Quench-To-MID-RX-B-1
                         H2-Quench-RX-B-2-Inlet
                         Purge-Rate]
  (let [k (/ (* Make-Up-Gas-From-Unicrack-H2 1000) 24)      ;"'Data-Input'!R16*1000/24"
        m (/ (* (+ Recycle-H2-To-Pass-A Recycle-H2-To-Pass-B) 1000) 24) ;"SUM('Data-Input'!T17:U17)*1000/24"
        o (/ (* (+ H2-Quench-To-MID-RX-A-1) 1000) 24)       ;"'Data-Input'!X21*1000/24"
        q (/ (* (+ H2-Quench-RX-A-2-Inlet) 1000) 24)        ;"'Data-Input'!Z18*1000/24"
        s (/ (* (+ H2-Quench-To-MID-RX-B-1) 1000) 24)       ;"'Data-Input'!AB20*1000/24"
        u (/ (* (+ H2-Quench-RX-B-2-Inlet) 1000) 24)        ;"'Data-Input'!AD21*1000/24"
        x Purge-Rate]
    (- (+ k m o q s u) x)))


(defn- Average-Temp [temps]
  (let [count (count temps)
        sum (apply + temps)]
    (/ sum count)))


;;;;;;;;;Chart Calculation Function ;;;;;;

;Feed Sulfur Column CZ
;Input param ClientData Column CZ (RX B-2 Inlet Temp) Unit(WT%)
;Output Feed Sulfur Column CZ
(defn Feed-Sulfur [SULFUR-WT%-X-RA-Result]
  SULFUR-WT%-X-RA-Result)

;Product Sulfur Column JT
;= (KG22*JX22*JZ22+KH22*KB22*KD22) / (KG22*JX22+KH22*KB22)
;kg= 'Data-Input'!CP8 'Client Data'!DC13
;jx= 141.5/ (JY22+131.5)
;jy= 'Data-Input'!CM7
;jz= 'Data-Input'!CR7 'Client Data'!DA13
;kh= 'Data-Input'!CQ8 'Client Data'!DD13
;kb= 141.5/ (KC23+131.5)
;kc= Prod-API-Gravity- (Naphtha)
;kd= 'Data-Input'!CT8 'Client Data'!DI13

;Input param DataEvaluation Column KC (Prod API Gravity Naphtha) Unit(API)
;Input param DataInput Column CM (ULSD Prod Gravity) Unit()
;Input param ClientData Column DA (Sulfur ) Unit(PPM)
;Input param ClientData Column DC (NET DIESEL TO STORAGE) Unit(BPD)
;Input param ClientData Column DD (NET KEROSENE TO STORAGE) Unit(BPD)
;Input param ClientData Column DI (Kero Sulfur) Unit(PPM)
(defn Product-Sulfur [Net-Diesel-To-Storage
                      ULSD-Prod-Gravity
                      Sulfur
                      Net-Kerosene-To-Storage
                      Prod-API-Gravity-Naphtha
                      Kero-Sulfur]
  (let [a (* Net-Diesel-To-Storage (/ 141.5 (+ ULSD-Prod-Gravity 131.5)) Sulfur)
        b (* Net-Kerosene-To-Storage
             (/ 141.5 (+ Prod-API-Gravity-Naphtha 131.5))
             Kero-Sulfur)
        c (* Net-Diesel-To-Storage (/ 141.5 (+ ULSD-Prod-Gravity 131.5)))
        d (* Net-Kerosene-To-Storage
             (/ 141.5 (+ Prod-API-Gravity-Naphtha 131.5)))]
    (/ (+ a b) (+ c d))))

;Feed Nitrogen Column DA
;Input param ClientData Column DG (Charge N2 ) Unit(ppm)
;Output Feed Nitrogen Column DA
(defn Feed-Nitrogen [Charge-N2]
  Charge-N2)

;Product Nitrogen Column JU
;Input param ClientData Column DH (Product N2) Unit(ppm)
;Output Product Nitrogen Column JU
(defn Product-Nitrogen [Product-N2]
  Product-N2)

;Feed Gravity Column CX
;Input param ClientData Column CN (Gravity) Unit (Gravity)
;Output Feed Nitrogen Column CX
(defn Feed-Gravity [Gravity]
  (/ 141.5 (+ Gravity 131.5)))


;Feed Silicon Column DC
;dc= 'Data-Input'!BW7
;VLOOKUP (B7, 'Feed Metals'!$C$50:$H$88, 4, 1) /1000
;Input param DataInput Column BW (Sifeed) Unit (WTPPM)
;(defn Feed-Silicon [Sifeed]
;  Sifeed)

;Feed Arsenic Column DD
;dd= 'Data-Input'!BX7
;VLOOKUP (B7, 'Feed Metals'!$C$50:$H$227, 3, 1) /1000
;Input param DataInput Column BX (Asfeed) Unit (WTPPM)
;(defn Feed-Arsenic [Asfeed]
;  Asfeed)


;Feed Distillation
;10 Per Trendline Column EJ
;ej=IF ($DJ$10= "D86", D86toSimDist (DE22:DQ22, "F"), D86toSimDist (DR22:ED22, "F"))
;;Input param ClientData Column CO (Initial Boiling Point Test Method D2887)
; Unit (DEGF)
;Input param ClientData Column CP
; (10 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CQ
; (20 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CR
; (30 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CS
; (40 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CT
; (50 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CU
; (60 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CV
; (70 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CW
; (80 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CX
; (90 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CY (Final Boiling Point Test Method D2887)
; Unit (DEGF)
(defn Ten-Per-Trendline [type temp-type
                         Initial-Boiling-Point-Test-Method-D2887
                         Percent-5-Recovered-Test-Method-ASTM-D2887
                         Percent-10-Recovered-Test-Method-ASTM-D2887
                         Percent-20-Recovered-Test-Method-ASTM-D2887
                         Percent-30-Recovered-Test-Method-ASTM-D2887
                         Percent-40-Recovered-Test-Method-ASTM-D2887
                         Percent-50-Recovered-Test-Method-ASTM-D2887
                         Percent-60-Recovered-Test-Method-ASTM-D2887
                         Percent-70-Recovered-Test-Method-ASTM-D2887
                         Percent-80-Recovered-Test-Method-ASTM-D2887
                         Percent-90-Recovered-Test-Method-ASTM-D2887
                         Percent-95-Recovered-Test-Method-ASTM-D2887
                         Final-Boiling-Point-Test-Method-D2887]
  (let [data (vector Initial-Boiling-Point-Test-Method-D2887
                     Percent-5-Recovered-Test-Method-ASTM-D2887
                     Percent-10-Recovered-Test-Method-ASTM-D2887
                     Percent-20-Recovered-Test-Method-ASTM-D2887
                     Percent-30-Recovered-Test-Method-ASTM-D2887
                     Percent-40-Recovered-Test-Method-ASTM-D2887
                     Percent-50-Recovered-Test-Method-ASTM-D2887
                     Percent-60-Recovered-Test-Method-ASTM-D2887
                     Percent-70-Recovered-Test-Method-ASTM-D2887
                     Percent-80-Recovered-Test-Method-ASTM-D2887
                     Percent-90-Recovered-Test-Method-ASTM-D2887
                     Percent-95-Recovered-Test-Method-ASTM-D2887
                     Final-Boiling-Point-Test-Method-D2887)
        dist-data (dist/check-Distillation type data temp-type)]
    (dist-data 2)))

;30 Per Trendline Column EL
;el= IF ($DJ$10= "D86", D86toSimDist (DE22:DQ22, "F"), D86toSimDist (DR22:ED22, "F"))
;Input param ClientData Column CO (Initial Boiling Point Test Method D2887)
; Unit (DEGF)
;Input param ClientData Column CP
; (10 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CQ
; (20 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CR
; (30 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CS
; (40 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CT
; (50 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CU
; (60 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CV
; (70 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CW
; (80 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CX
; (90 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CY (Final Boiling Point Test Method D2887)
; Unit (DEGF)
(defn Thirty-Per-Trendline [type temp-type
                            Initial-Boiling-Point-Test-Method-D2887
                            Percent-5-Recovered-Test-Method-ASTM-D2887
                            Percent-10-Recovered-Test-Method-ASTM-D2887
                            Percent-20-Recovered-Test-Method-ASTM-D2887
                            Percent-30-Recovered-Test-Method-ASTM-D2887
                            Percent-40-Recovered-Test-Method-ASTM-D2887
                            Percent-50-Recovered-Test-Method-ASTM-D2887
                            Percent-60-Recovered-Test-Method-ASTM-D2887
                            Percent-70-Recovered-Test-Method-ASTM-D2887
                            Percent-80-Recovered-Test-Method-ASTM-D2887
                            Percent-90-Recovered-Test-Method-ASTM-D2887
                            Percent-95-Recovered-Test-Method-ASTM-D2887
                            Final-Boiling-Point-Test-Method-D2887]
  (let [data (vector Initial-Boiling-Point-Test-Method-D2887
                     Percent-5-Recovered-Test-Method-ASTM-D2887
                     Percent-10-Recovered-Test-Method-ASTM-D2887
                     Percent-20-Recovered-Test-Method-ASTM-D2887
                     Percent-30-Recovered-Test-Method-ASTM-D2887
                     Percent-40-Recovered-Test-Method-ASTM-D2887
                     Percent-50-Recovered-Test-Method-ASTM-D2887
                     Percent-60-Recovered-Test-Method-ASTM-D2887
                     Percent-70-Recovered-Test-Method-ASTM-D2887
                     Percent-80-Recovered-Test-Method-ASTM-D2887
                     Percent-90-Recovered-Test-Method-ASTM-D2887
                     Percent-95-Recovered-Test-Method-ASTM-D2887
                     Final-Boiling-Point-Test-Method-D2887)
        dist-data (dist/check-Distillation type data temp-type)]
    (dist-data 4)))

;50 Per TrendlineColumn EN
;en= IF ($DJ$10= "D86", D86toSimDist (DE22:DQ22, "F"), D86toSimDist (DR22:ED22, "F"))
;Input param ClientData Column CO (Initial Boiling Point Test Method D2887)
; Unit (DEGF)
;Input param ClientData Column CP
; (10 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CQ
; (20 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CR
; (30 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CS
; (40 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CT
; (50 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CU
; (60 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CV
; (70 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CW
; (80 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CX
; (90 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CY (Final Boiling Point Test Method D2887)
; Unit (DEGF)
(defn Fifty-Per-Trendline [type temp-type
                           Initial-Boiling-Point-Test-Method-D2887
                           Percent-5-Recovered-Test-Method-ASTM-D2887
                           Percent-10-Recovered-Test-Method-ASTM-D2887
                           Percent-20-Recovered-Test-Method-ASTM-D2887
                           Percent-30-Recovered-Test-Method-ASTM-D2887
                           Percent-40-Recovered-Test-Method-ASTM-D2887
                           Percent-50-Recovered-Test-Method-ASTM-D2887
                           Percent-60-Recovered-Test-Method-ASTM-D2887
                           Percent-70-Recovered-Test-Method-ASTM-D2887
                           Percent-80-Recovered-Test-Method-ASTM-D2887
                           Percent-90-Recovered-Test-Method-ASTM-D2887
                           Percent-95-Recovered-Test-Method-ASTM-D2887
                           Final-Boiling-Point-Test-Method-D2887]

  (let [data (vector Initial-Boiling-Point-Test-Method-D2887
                     Percent-5-Recovered-Test-Method-ASTM-D2887
                     Percent-10-Recovered-Test-Method-ASTM-D2887
                     Percent-20-Recovered-Test-Method-ASTM-D2887
                     Percent-30-Recovered-Test-Method-ASTM-D2887
                     Percent-40-Recovered-Test-Method-ASTM-D2887
                     Percent-50-Recovered-Test-Method-ASTM-D2887
                     Percent-60-Recovered-Test-Method-ASTM-D2887
                     Percent-70-Recovered-Test-Method-ASTM-D2887
                     Percent-80-Recovered-Test-Method-ASTM-D2887
                     Percent-90-Recovered-Test-Method-ASTM-D2887
                     Percent-95-Recovered-Test-Method-ASTM-D2887
                     Final-Boiling-Point-Test-Method-D2887)
        dist-data (dist/check-Distillation type data temp-type)]
    (dist-data 6)))

;70 Per Trendline Column EP
;ep= IF ($DJ$10= "D86", D86toSimDist (DE23:DQ23, "F"), D86toSimDist (DR23:ED23, "F"))
;Input param ClientData Column CO (Initial Boiling Point Test Method D2887)
; Unit (DEGF)
;Input param ClientData Column CP
; (10 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CQ
; (20 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CR
; (30 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CS
; (40 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CT
; (50 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CU
; (60 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CV
; (70 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CW
; (80 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CX
; (90 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CY (Final Boiling Point Test Method D2887)
; Unit (DEGF)
(defn Seventy-Per-Trendline [type temp-type
                             Initial-Boiling-Point-Test-Method-D2887
                             Percent-5-Recovered-Test-Method-ASTM-D2887
                             Percent-10-Recovered-Test-Method-ASTM-D2887
                             Percent-20-Recovered-Test-Method-ASTM-D2887
                             Percent-30-Recovered-Test-Method-ASTM-D2887
                             Percent-40-Recovered-Test-Method-ASTM-D2887
                             Percent-50-Recovered-Test-Method-ASTM-D2887
                             Percent-60-Recovered-Test-Method-ASTM-D2887
                             Percent-70-Recovered-Test-Method-ASTM-D2887
                             Percent-80-Recovered-Test-Method-ASTM-D2887
                             Percent-90-Recovered-Test-Method-ASTM-D2887
                             Percent-95-Recovered-Test-Method-ASTM-D2887
                             Final-Boiling-Point-Test-Method-D2887]
  (let [data (vector Initial-Boiling-Point-Test-Method-D2887
                     Percent-5-Recovered-Test-Method-ASTM-D2887
                     Percent-10-Recovered-Test-Method-ASTM-D2887
                     Percent-20-Recovered-Test-Method-ASTM-D2887
                     Percent-30-Recovered-Test-Method-ASTM-D2887
                     Percent-40-Recovered-Test-Method-ASTM-D2887
                     Percent-50-Recovered-Test-Method-ASTM-D2887
                     Percent-60-Recovered-Test-Method-ASTM-D2887
                     Percent-70-Recovered-Test-Method-ASTM-D2887
                     Percent-80-Recovered-Test-Method-ASTM-D2887
                     Percent-90-Recovered-Test-Method-ASTM-D2887
                     Percent-95-Recovered-Test-Method-ASTM-D2887
                     Final-Boiling-Point-Test-Method-D2887)
        dist-data (dist/check-Distillation type data temp-type)]
    (dist-data 8)))

;90 Per Trendline Column ER
;er= IF ($DJ$10= "D86", D86toSimDist (DE22:DQ22, "F"), D86toSimDist (DR22:ED22, "F"))
;Input param ClientData Column CO (Initial Boiling Point Test Method D2887)
; Unit (DEGF)
;Input param ClientData Column CP
; (10 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CQ
; (20 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CR
; (30 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CS
; (40 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CT
; (50 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CU
; (60 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CV
; (70 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CW
; (80 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CX
; (90 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CY (Final Boiling Point Test Method D2887)
; Unit (DEGF)
(defn Ninety-Per-Trendline [type temp-type
                            Initial-Boiling-Point-Test-Method-D2887
                            Percent-5-Recovered-Test-Method-ASTM-D2887
                            Percent-10-Recovered-Test-Method-ASTM-D2887
                            Percent-20-Recovered-Test-Method-ASTM-D2887
                            Percent-30-Recovered-Test-Method-ASTM-D2887
                            Percent-40-Recovered-Test-Method-ASTM-D2887
                            Percent-50-Recovered-Test-Method-ASTM-D2887
                            Percent-60-Recovered-Test-Method-ASTM-D2887
                            Percent-70-Recovered-Test-Method-ASTM-D2887
                            Percent-80-Recovered-Test-Method-ASTM-D2887
                            Percent-90-Recovered-Test-Method-ASTM-D2887
                            Percent-95-Recovered-Test-Method-ASTM-D2887
                            Final-Boiling-Point-Test-Method-D2887]
  (let [data (vector Initial-Boiling-Point-Test-Method-D2887
                     Percent-5-Recovered-Test-Method-ASTM-D2887
                     Percent-10-Recovered-Test-Method-ASTM-D2887
                     Percent-20-Recovered-Test-Method-ASTM-D2887
                     Percent-30-Recovered-Test-Method-ASTM-D2887
                     Percent-40-Recovered-Test-Method-ASTM-D2887
                     Percent-50-Recovered-Test-Method-ASTM-D2887
                     Percent-60-Recovered-Test-Method-ASTM-D2887
                     Percent-70-Recovered-Test-Method-ASTM-D2887
                     Percent-80-Recovered-Test-Method-ASTM-D2887
                     Percent-90-Recovered-Test-Method-ASTM-D2887
                     Percent-95-Recovered-Test-Method-ASTM-D2887
                     Final-Boiling-Point-Test-Method-D2887)
        dist-data (dist/check-Distillation type data temp-type)]
    (dist-data 10)))

;100 Per Trendline Column ET
;et= IF ($DJ$10= "D86", D86toSimDist (DE22:DQ22, "F"), D86toSimDist (DR22:ED22, "F"))
;Input param ClientData Column CO (Initial Boiling Point Test Method D2887)
; Unit (DEGF)
;Input param ClientData Column CP
; (10 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CQ
; (20 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CR
; (30 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CS
; (40 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CT
; (50 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CU
; (60 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CV
; (70 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CW
; (80 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CX
; (90 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CY (Final Boiling Point Test Method D2887)
; Unit (DEGF)
(defn Hundred-Per-Trendline [type temp-type
                             Initial-Boiling-Point-Test-Method-D2887
                             Percent-5-Recovered-Test-Method-ASTM-D2887
                             Percent-10-Recovered-Test-Method-ASTM-D2887
                             Percent-20-Recovered-Test-Method-ASTM-D2887
                             Percent-30-Recovered-Test-Method-ASTM-D2887
                             Percent-40-Recovered-Test-Method-ASTM-D2887
                             Percent-50-Recovered-Test-Method-ASTM-D2887
                             Percent-60-Recovered-Test-Method-ASTM-D2887
                             Percent-70-Recovered-Test-Method-ASTM-D2887
                             Percent-80-Recovered-Test-Method-ASTM-D2887
                             Percent-90-Recovered-Test-Method-ASTM-D2887
                             Percent-95-Recovered-Test-Method-ASTM-D2887
                             Final-Boiling-Point-Test-Method-D2887]
  (let [data (vector Initial-Boiling-Point-Test-Method-D2887
                     Percent-5-Recovered-Test-Method-ASTM-D2887
                     Percent-10-Recovered-Test-Method-ASTM-D2887
                     Percent-20-Recovered-Test-Method-ASTM-D2887
                     Percent-30-Recovered-Test-Method-ASTM-D2887
                     Percent-40-Recovered-Test-Method-ASTM-D2887
                     Percent-50-Recovered-Test-Method-ASTM-D2887
                     Percent-60-Recovered-Test-Method-ASTM-D2887
                     Percent-70-Recovered-Test-Method-ASTM-D2887
                     Percent-80-Recovered-Test-Method-ASTM-D2887
                     Percent-90-Recovered-Test-Method-ASTM-D2887
                     Percent-95-Recovered-Test-Method-ASTM-D2887
                     Final-Boiling-Point-Test-Method-D2887)
        dist-data (dist/check-Distillation type data temp-type)]
    (dist-data 12)))


;Feed 600ºF Column EW
;ew= (1-InterPol (EH22:ET22, EH$12:ET$12, 600)*100)
;eh= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist( :ED22,"F"))
;ei= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;ej= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;ek= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;el= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;em= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;en= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;eo= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;ep= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;eq= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;er= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;es= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;et= IF($DJ$10="D86",D86toSimDist(DE22:DQ22,"F"),D86toSimDist(DR22:ED22,"F"))
;$eh=
;$et=
;de= 'Data-Input'!BZ8 'Client Data'!CO13
;df=
;dg='Data-Input'!CB8 'Client Data'!CP13
;dh='Data-Input'!CC8 'Client Data'!CQ13
;di='Data-Input'!CD8 'Client Data'!CR13
;dj='Data-Input'!CE8 'Client Data'!CS13
;dk='Data-Input'!CF8 'Client Data'!CT13
;dl='Data-Input'!CG8 'Client Data'!CU13
;dm='Data-Input'!CH8 'Client Data'!CV13
;dn='Data-Input'!CI8 'Client Data'!CW13
;do='Data-Input'!CJ8 'Client Data'!CX13
;dp=
;dq='Data-Input'!CL8 'Client Data'!DY13

;dr=IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;ds= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;dt= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;du= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;dv= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;dw= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;dx= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;dy= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;dz= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;ea= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;eb= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;ec= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))
;ed= IF($DJ$10="D86",SimDistToD86(EH22:ET22,"F"),SimDistToD86(DE22:DQ22,"F"))

;Input param ClientData Column CO (Initial Boiling Point Test Method D2887)
; Unit (DEGF)
;Input param ClientData Column CP
; (10 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CQ
; (20 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CR
; (30 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CS
; (40 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CT
; (50 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CU
; (60 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CV
; (70 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CW
; (80 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CX
; (90 Percent Recovered Test Method ASTM D2887) Unit (DEGF)
;Input param ClientData Column CY (Final Boiling Point Test Method D2887)
; Unit (DEGF)
(defn Feed-600F [dist-type temp-type
                 Initial-Boiling-Point-Test-Method-D2887
                 Percent-5-Recovered-Test-Method-ASTM-D2887
                 Percent-10-Recovered-Test-Method-ASTM-D2887
                 Percent-20-Recovered-Test-Method-ASTM-D2887
                 Percent-30-Recovered-Test-Method-ASTM-D2887
                 Percent-40-Recovered-Test-Method-ASTM-D2887
                 Percent-50-Recovered-Test-Method-ASTM-D2887
                 Percent-60-Recovered-Test-Method-ASTM-D2887
                 Percent-70-Recovered-Test-Method-ASTM-D2887
                 Percent-80-Recovered-Test-Method-ASTM-D2887
                 Percent-90-Recovered-Test-Method-ASTM-D2887
                 Percent-95-Recovered-Test-Method-ASTM-D2887
                 Final-Boiling-Point-Test-Method-D2887]
  (let [data (vector Initial-Boiling-Point-Test-Method-D2887
                     Percent-5-Recovered-Test-Method-ASTM-D2887
                     Percent-10-Recovered-Test-Method-ASTM-D2887
                     Percent-20-Recovered-Test-Method-ASTM-D2887
                     Percent-30-Recovered-Test-Method-ASTM-D2887
                     Percent-40-Recovered-Test-Method-ASTM-D2887
                     Percent-50-Recovered-Test-Method-ASTM-D2887
                     Percent-60-Recovered-Test-Method-ASTM-D2887
                     Percent-70-Recovered-Test-Method-ASTM-D2887
                     Percent-80-Recovered-Test-Method-ASTM-D2887
                     Percent-90-Recovered-Test-Method-ASTM-D2887
                     Percent-95-Recovered-Test-Method-ASTM-D2887
                     Final-Boiling-Point-Test-Method-D2887)
        dist (dist/check-Distillation dist-type data temp-type)
        interpol-data (vector {:x (dist 0) :y 0}
                              {:x (dist 1) :y 5}
                              {:x (dist 2) :y 10}
                              {:x (dist 3) :y 20}
                              {:x (dist 4) :y 30}
                              {:x (dist 5) :y 40}
                              {:x (dist 6) :y 50}
                              {:x (dist 7) :y 60}
                              {:x (dist 8) :y 70}
                              {:x (dist 9) :y 80}
                              {:x (dist 10) :y 90}
                              {:x (dist 11) :y 95}
                              {:x (dist 12) :y 100})
        interpol (InterPol interpol-data 600)]
    (* (- 1 interpol) 100)))

;Operating conditions
;Feed Rate Column C
;Input param ClientData Column DT (Combined Feed Rate) Unit (Bbl / day)
;Output Feed Rate Column C
(defn Feed-Rate [RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B]
  (Combined-Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B))

;Feed Ratio Of Cracked Feed Column H
;Input param ClientData Column DT (Combined Feed Rate) Unit (Bbl / day)
;Input param ClientData Column DU (Coker Debutanizer Bottoms Feed Rate) Unit (Bbl / day)
;Input param ClientData Column DV (Net Coker Diesel) Unit (Bbl / day)
;Output Feed Rate  Of Cracked Feed Column H
(defn Feed-Ratio-Of-Cracked-Feed [RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B,
                                  Coker-Debutanizer-Bottoms-Feed-Rate,
                                  Net-Coker-Diesel]
  (let [a (+ Net-Coker-Diesel Coker-Debutanizer-Bottoms-Feed-Rate)
        b (/ a (Combined-Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B))]
    (* b 100)))


;Observed WABT  Column LJ () unit ()
;AVERAGE(LH22:LI22)
;lh= Wt___in_Bed_1*(AL22+AO22*DT_Rule_in_Bed_1)+Wt___in_Bed_2*(AP22+AS22*DT_Rule_in_Bed_2)+Wt___in_Bed_3*(AU22+AX22*DT_Rule_in_Bed_3)
;al= 'Data-Input'!AV7 ='Client Data'!K13
;an= 'Data-Input'!AX7 ='Client Data'!S13
;ao= (an-al)
;ap= 'Data-Input'!AY7 ='Client Data'!X13
;ar='Data-Input'!BA7 ='Client Data'!AF13
;as= (ar-ap)
;au= 'Data-Input'!BC8 ='Client Data'!AK13
;aw='Data-Input'!BE8 ='Client Data'!AS13
;ax= (aw-au)
;li= Wt___in_Bed_1*(AZ22+BC22*DT_Rule_in_Bed_1)+Wt___in_Bed_2*(BD22+BG22*DT_Rule_in_Bed_2)+Wt___in_Bed_3*(BI22+BL22*DT_Rule_in_Bed_3)
;az= 'Data-Input'!BF7 ='Client Data'!AV13
;bb = 'Data-Input'!BH8 ='Client Data'!BD13
;bc= (bb-az)
;bd='Data-Input'!BI11 ='Client Data'!BI13
;bf='Data-Input'!BK8 ='Client Data'!BQ13
;bg= (bf-bd)
;bi='Data-Input'!BM7 ='Client Data'!BU13
;bk= 'Data-Input'!BO7  ='Client Data'!CD13
;bl= (bk-bi)

;Input param ClientData Column K (RX A-1 FEED FROM HEATER A) Unit (DEGF)
;Input param ClientData Column S (A-1 Average Top Bed Low Temp) Unit (DEGF)

;Input param ClientData Column X (A-1 Average Bottom Bed Up Temp) Unit (DEGF)
;Input param ClientData Column AF (A-1 Average Bottom Bed Low Temp) Unit(DEGF)

;Input param ClientData Column AK (RX A-2 INLET TOP) Unit(DEGF)
;Input param ClientData Column AS (REACTOR A OUTLET) Unit (DEGF)

;Input param ClientData Column AV (RX FEED FROM HEATER B) Unit (DEGF)
;Input param ClientData Column BD (B-1 Average Top Bed Low Temp) Unit (DEGF)

;Input param ClientData Column BI (B-1 Average Bottom Bed Up Temp) Unit (DEGF)
;Input param ClientData Column BQ (B-1 Average Bottom Bed Low Temp) Unit (DEGF)

;Input param ClientData Column BU (RX B-2 INLET TEMP) Unit (DEGF)
;Input param ClientData Column CD (REACTOR B-2 OUTLET) Unit (DEGF)

(defn OWABT [wt%-in-bed1 wt%-in-bed2 wt%-in-bed3
             dt-rule-in-bed1 dt-rule-in-bed2 dt-rule-in-bed3
             RX-A-1-Feed-From-Heater-A
             RX-A-1-Bot-Bed-Up-G
             RX-A-1-Bot-Bed-Up-H
             RX-A-1-Bot-Bed-Up-I
             RX-A-1-Top-Bed-Low-D
             RX-A-1-Top-Bed-Low-E
             RX-A-1-Top-Bed-Low-F
             RX-A-1-Bot-Bed-Low-M
             RX-A-1-Bot-Bed-Low-N
             RX-A-1-Bot-Bed-Low-O
             RX-B-1-TOP-BED-LOW-D
             RX-B-1-TOP-BED-LOW-E
             RX-B-1-TOP-BED-LOW-F
             RX-B-1-Bot-Bed-Up-G
             RX-B-1-Bot-Bed-Up-H
             RX-B-1-Bot-Bed-Up-I
             RX-B-1-Bot-Bed-Low-M
             RX-B-1-Bot-Bed-Low-N
             RX-B-1-Bot-Bed-Low-O
             RX-A-2-Inlet-Top
             Reactor-A-Outlet
             RX-Feed-From-Heater-B
             RX-B-2-Inlet-Temp
             Reactor-B-2-Outlet]
  (let [A-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-A-1-Bot-Bed-Up-G
                                                      RX-A-1-Bot-Bed-Up-H
                                                      RX-A-1-Bot-Bed-Up-I])
        A-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-A-1-Top-Bed-Low-D
                                                    RX-A-1-Top-Bed-Low-E
                                                    RX-A-1-Top-Bed-Low-F])
        A-1-Average-Bottom-Bed-Low-Temp (Average-Temp [ RX-A-1-Bot-Bed-Low-M
                                                       RX-A-1-Bot-Bed-Low-N
                                                       RX-A-1-Bot-Bed-Low-O])
        B-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-B-1-TOP-BED-LOW-D
                                                    RX-B-1-TOP-BED-LOW-E
                                                    RX-B-1-TOP-BED-LOW-F])
        B-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-B-1-Bot-Bed-Up-G
                                                      RX-B-1-Bot-Bed-Up-H
                                                      RX-B-1-Bot-Bed-Up-I])
        B-1-Average-Bottom-Bed-Low-Temp (Average-Temp [RX-B-1-Bot-Bed-Low-M
                                                       RX-B-1-Bot-Bed-Low-N
                                                       RX-B-1-Bot-Bed-Low-O])
        ao (- A-1-Average-Top-Bed-Low-Temp RX-A-1-Feed-From-Heater-A)
        as (- A-1-Average-Bottom-Bed-Low-Temp A-1-Average-Bottom-Bed-Up-Temp)
        ax (- Reactor-A-Outlet RX-A-2-Inlet-Top)
        a3 (* ao dt-rule-in-bed1)
        a2 (+ RX-A-1-Feed-From-Heater-A a3)
        a1 (* wt%-in-bed1 a2)
        b3 (* as dt-rule-in-bed2)
        b2 (+ A-1-Average-Bottom-Bed-Up-Temp b3)
        b1 (* wt%-in-bed2 b2)
        c3 (* ax dt-rule-in-bed3)
        c2 (+ RX-A-2-Inlet-Top c3)
        c1 (* wt%-in-bed3 c2)
        lh (+ a1 b1 c1)
        bc (- B-1-Average-Top-Bed-Low-Temp RX-Feed-From-Heater-B)
        bg (- B-1-Average-Bottom-Bed-Low-Temp B-1-Average-Bottom-Bed-Up-Temp)
        bl (- Reactor-B-2-Outlet RX-B-2-Inlet-Temp)
        d3 (* bc dt-rule-in-bed1)
        d2 (+ RX-Feed-From-Heater-B d3)
        d1 (* wt%-in-bed1 d2)
        e3 (* bg dt-rule-in-bed2)
        e2 (+ B-1-Average-Bottom-Bed-Up-Temp e3)
        e1 (* wt%-in-bed2 e2)
        f3 (* bl dt-rule-in-bed3)
        f2 (+ RX-B-2-Inlet-Temp f3)
        f1 (* wt%-in-bed3 f2)
        li (+ d1 e1 f1)
        ]
    (/ (+ lh li) 2)
    ))





;;;;;;;;;;;;;;;; Make Up Gas
;Rate: Column K
;Input param ClientData Column I(MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Output Rate: Column K
(defn Make-Up-Gas-Rate [Make-Up-Gas-From-Unicrack-H2]
  (/ (* Make-Up-Gas-From-Unicrack-H2 1000) 24))

;Hydrogen Purity: Column L
;Input param ClientData Column CM (Makeup H2) Unit (Mol%)
;Output Hydrogen Purity: Column L
(defn Make-up-gas-Hydrogen-Purity [Makeup-H2]
  Makeup-H2)


;;;;;;;;;;;;;;;;;;;; Recycle gas
;Rate: Column M
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Output Rate: Column M
(defn Recycle-Gas-Rate [Recycle-H2-To-Pass-A Recycle-H2-To-Pass-B]
  (/ (* (+ Recycle-H2-To-Pass-A Recycle-H2-To-Pass-B) 1000) 24))

;Hydrogen Purity: Column N () unit ()
;Input param ClientData Column CJ (Recycle Gas H2) Unit (Mol%)
(defn Recycle-Gas-Hydrogen-Purity [Recycle-Gas-H2]
  Recycle-Gas-H2)

;;;;;;;;;;;;;;;Quench gas
;Rate: Column W
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Output Rate: Column W
(defn Quench-Gas-Rate [H2-Quench-To-MID-RX-A-1
                       H2-Quench-RX-A-2-Inlet
                       H2-Quench-RX-B-2-Inlet
                       H2-Quench-To-MID-RX-B-1]
  (let [o (/ (* H2-Quench-To-MID-RX-A-1 1000) 24)
        q (/ (* H2-Quench-RX-A-2-Inlet 1000) 24)
        u (/ (* H2-Quench-RX-B-2-Inlet 1000) 24)
        s (/ (* H2-Quench-To-MID-RX-B-1 1000) 24)]
    (+ o q u s)))

;Hydrogen Purity: Column P
;Input param ClientData Column CJ (Recycle Gas H2) Unit (Mol%)
;Output Hydrogen Purity: Column P
(defn Quench-Gas-Hydrogen-Purity [Recycle-Gas-H2]
  Recycle-Gas-H2)

;Treat gas
;Rate: Column AA
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param Data-Evaluation Column X (Purge Rate) Unit (SCFH)
;Output Rate: Column AA
(defn Treat-Gas-Rate [Make-Up-Gas-From-Unicrack-H2
                      Recycle-H2-To-Pass-A
                      Recycle-H2-To-Pass-B
                      H2-Quench-To-MID-RX-A-1
                      H2-Quench-RX-A-2-Inlet
                      H2-Quench-RX-B-2-Inlet
                      H2-Quench-To-MID-RX-B-1
                      Purge-Rate]
  (let [k (/ (* Make-Up-Gas-From-Unicrack-H2 1000) 24)
        m (- (/ (* (+ Recycle-H2-To-Pass-A Recycle-H2-To-Pass-B) 1000) 24) Purge-Rate)
        o (/ (* H2-Quench-To-MID-RX-A-1 1000) 24)
        q (/ (* H2-Quench-RX-A-2-Inlet 1000) 24)
        u (/ (* H2-Quench-RX-B-2-Inlet 1000) 24)
        s (/ (* H2-Quench-To-MID-RX-B-1 1000) 24)]
    (-> (* (+ k m o q u s) 24)
        (/ 1000000))))

;Hydrogen Purity: Column AB
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column CM (Makeup H2) Unit (Mol%)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column CJ (Recycle Gas H2) Unit (Mol%)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Output Hydrogen Purity: Column AB
;(SUM(R8*S8,T8*V8,U8*V8,X8*Y8,Z8*AA8,AB8*AC8,AD8*AE8)/AH8)
(defn Treat-Gas-Hydrogen-Purity [Make-Up-Gas-From-Unicrack-H2
                                 Makeup-H2
                                 Recycle-H2-To-Pass-A
                                 Recycle-Gas-H2
                                 Recycle-H2-To-Pass-B
                                 H2-Quench-To-MID-RX-A-1
                                 H2-Quench-RX-A-2-Inlet
                                 H2-Quench-To-MID-RX-B-1
                                 H2-Quench-RX-B-2-Inlet]
  (Treat-Gas-Purity Make-Up-Gas-From-Unicrack-H2
                    Makeup-H2
                    Recycle-H2-To-Pass-A
                    Recycle-Gas-H2
                    Recycle-H2-To-Pass-B
                    H2-Quench-To-MID-RX-A-1
                    H2-Quench-RX-A-2-Inlet
                    H2-Quench-To-MID-RX-B-1
                    H2-Quench-RX-B-2-Inlet))

;Gas to oil ratio
;Total gas/oil ratio: Column LF
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param Data-Evaluation Column X (Purge Rate) Unit (SCFH)
;Input param ClientData Column DT (Combined Feed Rate) Unit (Bbl / day)
;Output Total gas/oil ratio: Column LF
;SUM(K23,M23,O23,Q23,S23,U23)-X23
(defn Total-Gas-oil-Ratio [Make-Up-Gas-From-Unicrack-H2
                           Recycle-H2-To-Pass-A
                           Recycle-H2-To-Pass-B
                           H2-Quench-To-MID-RX-A-1
                           H2-Quench-RX-A-2-Inlet
                           H2-Quench-To-MID-RX-B-1
                           H2-Quench-RX-B-2-Inlet
                           Purge-Rate
                           RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B]
  (/ (* (Treat-Gas-Rate-Z Make-Up-Gas-From-Unicrack-H2
                          Recycle-H2-To-Pass-A
                          Recycle-H2-To-Pass-B
                          H2-Quench-To-MID-RX-A-1
                          H2-Quench-RX-A-2-Inlet
                          H2-Quench-To-MID-RX-B-1
                          H2-Quench-RX-B-2-Inlet
                          Purge-Rate) 24) (Combined-Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B)))

;Hydrogen/oil ratio: Column LG
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param Data-Evaluation Column X (Purge Rate) Unit (SCFH)
;Input param ClientData Column DT (Combined Feed Rate) Unit (Bbl / day)
;Input param ClientData Column CM (Makeup H2) Unit (Mol%)
;Input param ClientData Column CJ (Recycle Gas H2) Unit (Mol%)
;Output Hydrogen/oil ratio: Column LG
;LF25*LN25
;lf Z24*24/C24
;z =SUM(K23,M23,O23,Q23,S23,U23)-X23
;c 'Data-Input'!D7
;ln AB23 / 100
;ab 'Data-Input'!AI11
;ai = (SUM (R9*S9, T9*V9, U9*V9, X9*Y9, Z9*AA9, AB9*AC9, AD9*AE9) /AH9)
(defn Hydrogen-Oil-ratio [Make-Up-Gas-From-Unicrack-H2
                          Recycle-H2-To-Pass-A
                          Recycle-H2-To-Pass-B
                          H2-Quench-To-MID-RX-A-1
                          H2-Quench-RX-A-2-Inlet
                          H2-Quench-To-MID-RX-B-1
                          H2-Quench-RX-B-2-Inlet
                          Purge-Rate
                          Makeup-H2
                          Recycle-Gas-H2
                          RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B]
  (let [c (Combined-Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B)
        k (/ (* Make-Up-Gas-From-Unicrack-H2 1000) 24)      ;"'Data-Input'!R16*1000/24"
        m (/ (* (+ Recycle-H2-To-Pass-A Recycle-H2-To-Pass-B) 1000) 24) ;"SUM('Data-Input'!T17:U17)*1000/24"
        o (/ (* (+ H2-Quench-To-MID-RX-A-1) 1000) 24)       ;"'Data-Input'!X21*1000/24"
        q (/ (* (+ H2-Quench-RX-A-2-Inlet) 1000) 24)        ;"'Data-Input'!Z18*1000/24"
        s (/ (* (+ H2-Quench-To-MID-RX-B-1) 1000) 24)       ;"'Data-Input'!AB20*1000/24"
        u (/ (* (+ H2-Quench-RX-B-2-Inlet) 1000) 24)        ;"'Data-Input'!AD21*1000/24"
        x Purge-Rate
        z (Treat-Gas-Rate-Z Make-Up-Gas-From-Unicrack-H2
                            Recycle-H2-To-Pass-A
                            Recycle-H2-To-Pass-B
                            H2-Quench-To-MID-RX-A-1
                            H2-Quench-RX-A-2-Inlet
                            H2-Quench-To-MID-RX-B-1
                            H2-Quench-RX-B-2-Inlet
                            Purge-Rate)
        ab (Treat-Gas-Purity Make-Up-Gas-From-Unicrack-H2
                             Makeup-H2
                             Recycle-H2-To-Pass-A
                             Recycle-Gas-H2
                             Recycle-H2-To-Pass-B
                             H2-Quench-To-MID-RX-A-1
                             H2-Quench-RX-A-2-Inlet
                             H2-Quench-To-MID-RX-B-1
                             H2-Quench-RX-B-2-Inlet)
        ln (/ ab 100)
        lf (/ (* z 24) c)]
    (* lf ln)))

;Estimated hydrogen consumption
;Train A: Column KU
;KS22/Est._oF_SCF_BBL
;ks = AY22
;ay =AX24+AS24+AO24
;ax= AW25-AU25
;aw= 'Data-Input'!BE10  'Client Data'!AS19
;au = 'Data-Input'!BC14 'Client Data'!AK21
;as = AR23-AP23
;ar = 'Data-Input'!BA8  'Client Data'!AF15
;ap= 'Data-Input'!AY10  'Client Data'!X30
;ao = AN24-AL24
;an = 'Data-Input'!AX9  'Client Data'!S19
;al = 'Data-Input'!AV9  'Client Data'!K17
;Input param ClientData Column AS (REACTOR A OUTLET) Unit (DEGF)
;Input param ClientData Column AK (RX A-2 INLET TOP) Unit(DEGF)
;Input param ClientData Column AF (A-1 Average Bottom Bed Low Temp) Unit(DEGF)
;Input param ClientData Column X (A-1 Average Bottom Bed Up Temp) Unit (DEGF)
;Input param ClientData Column S (A-1 Average Top Bed Low Temp) Unit (DEGF)
;Input param ClientData Column K (RX A-1 FEED FROM HEATER A) Unit (DEGF)
;Output Train A: Column KU

(defn Estimated-Hydrogen-Consumption-Train-A [Reactor-A-Outlet
                                              RX-A-2-Inlet-Top
                                              RX-A-1-Bot-Bed-Up-G
                                              RX-A-1-Bot-Bed-Up-H
                                              RX-A-1-Bot-Bed-Up-I
                                              ;A-1-Average-Bottom-Bed-Up-Temp
                                              RX-A-1-Top-Bed-Low-D
                                              RX-A-1-Top-Bed-Low-E
                                              RX-A-1-Top-Bed-Low-F
                                              ;A-1-Average-Top-Bed-Low-Temp
                                              RX-A-1-Bot-Bed-Low-M
                                              RX-A-1-Bot-Bed-Low-N
                                              RX-A-1-Bot-Bed-Low-O
                                              ;A-1-Average-Bottom-Bed-Low-Temp
                                              RX-A-1-Feed-From-Heater-A
                                              Est-oF-SCF-BBL]
  (let [A-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-A-1-Bot-Bed-Up-G
                                                      RX-A-1-Bot-Bed-Up-H
                                                      RX-A-1-Bot-Bed-Up-I])
        A-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-A-1-Top-Bed-Low-D
                                                    RX-A-1-Top-Bed-Low-E
                                                    RX-A-1-Top-Bed-Low-F])
        A-1-Average-Bottom-Bed-Low-Temp (Average-Temp [ RX-A-1-Bot-Bed-Low-M
                                                       RX-A-1-Bot-Bed-Low-N
                                                       RX-A-1-Bot-Bed-Low-O])
        ax (- Reactor-A-Outlet RX-A-2-Inlet-Top)
        as (- A-1-Average-Bottom-Bed-Low-Temp
              A-1-Average-Bottom-Bed-Up-Temp)
        ao (- A-1-Average-Top-Bed-Low-Temp
              RX-A-1-Feed-From-Heater-A)
        ay (+ ax as ao)]
    (/ ay Est-oF-SCF-BBL)))

;Train B: Column KV
;kv =KT22/Est._oF_SCF_BBL
;kt= BM22
;bm = BL23+BG23+BC23
;bl = BK22-BI22
;bk = 'Data-Input'!BO10 'Client Data'!CD16
;bi = 'Data-Input'!BM9  'Client Data'!BU17
;bg = BF22-BD22
;bf = 'Data-Input'!BK7  'Client Data'!BQ15
;bd = 'Data-Input'!BI8  'Client Data'!BI14
;bc = BB22-AZ22
;bb = 'Data-Input'!BH7   'Client Data'!BD20
;az = 'Data-Input'!BF7   'Client Data'!AV16
;Input param ClientData Column CD (REACTOR B-2 OUTLET) Unit (DEGF)
;Input param ClientData Column BU (RX B-2 INLET TEMP) Unit (DEGF)
;Input param ClientData Column BQ (B-1 Average Bottom Bed Low Temp) Unit (DEGF)
;Input param ClientData Column BI (B-1 Average Bottom Bed Up Temp) Unit (DEGF)
;Input param ClientData Column BD (B-1 Average Top Bed Low Temp) Unit (DEGF)
;Input param ClientData Column AV (RX FEED FROM HEATER B) Unit (DEGF)
(defn Estimated-Hydrogen-Consumption-Train-B [Reactor-B-2-Outlet
                                              RX-B-2-Inlet-Temp
                                              RX-B-1-TOP-BED-LOW-D
                                              RX-B-1-TOP-BED-LOW-E
                                              RX-B-1-TOP-BED-LOW-F
                                              ;B-1-Average-Top-Bed-Low-Temp
                                              RX-B-1-Bot-Bed-Up-G
                                              RX-B-1-Bot-Bed-Up-H
                                              RX-B-1-Bot-Bed-Up-I
                                              ;B-1-Average-Bottom-Bed-Up-Temp
                                              RX-B-1-Bot-Bed-Low-M
                                              RX-B-1-Bot-Bed-Low-N
                                              RX-B-1-Bot-Bed-Low-O
                                              ;B-1-Average-Bottom-Bed-Low-Temp
                                              RX-Feed-From-Heater-B
                                              Est-oF-SCF-BBL]

  (let [B-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-B-1-TOP-BED-LOW-D
                                                    RX-B-1-TOP-BED-LOW-E
                                                    RX-B-1-TOP-BED-LOW-F])
        B-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-B-1-Bot-Bed-Up-G
                                                      RX-B-1-Bot-Bed-Up-H
                                                      RX-B-1-Bot-Bed-Up-I])
        B-1-Average-Bottom-Bed-Low-Temp (Average-Temp [RX-B-1-Bot-Bed-Low-M
                                                       RX-B-1-Bot-Bed-Low-N
                                                       RX-B-1-Bot-Bed-Low-O])
        bl (- Reactor-B-2-Outlet RX-B-2-Inlet-Temp)
        bg (- B-1-Average-Bottom-Bed-Low-Temp
              B-1-Average-Bottom-Bed-Up-Temp)
        bc (- B-1-Average-Top-Bed-Low-Temp
              RX-Feed-From-Heater-B)
        ay (+ bl bg bc)]
    (/ ay Est-oF-SCF-BBL)))

;Hydrogen availability Column KZ
;KZ=LG24/KV24
;kv= KT23/Est._oF_SCF_BBL
;kt= bm
;bm=BL25+BG25+BC25
;bl= BK26-BI26
;bk= 'Data-Input'!BO7 'Client Data'!CD13
;bi= 'Data-Input'!BM7 'Client Data'!BU13
;bg= BF24-BD24
;bf= 'Data-Input'!BK7 'Client Data'!BQ13
;bd= 'Data-Input'!BI7 'Client Data'!BI13
;bc= BB22-AZ22
;bb= 'Data-Input'!BH7 'Client Data'!BD13
;az= 'Data-Input'!BF7 'Client Data'!AV13
;lg= LF22*LN22
;ln= AB22/100
;ab ='Data-Input'!AI11
;ai = (SUM(R11*S11,T11*V11,U11*V11,X11*Y11,Z11*AA11,AB11*AC11,AD11*AE11)/AH11)
;r= 'Client Data'!I17
;s= 'Client Data'!CM17
;t= 'Client Data'!F17
;v= 'Client Data'!CJ17
;u='Client Data'!G17
;x= 'Client Data'!T17
;y= v
;z= 'Client Data'!AJ17
;aa= v
;ab= 'Client Data'!BE17
;ac= v
;ad= 'Client Data'!BT17
;ae= v
;ah=SUM(AF11,R11,T11:U11)
;af=SUM(AD11+AB11+Z11+X11)
;lf =Z22*24/C22
;c = 'Data-Input'!D7 'Client Data'!DT13
;z= SUM(K22,M22,O22,Q22,S22,U22)-X22
;k= 'Data-Input'!R7*1000/24
;m= SUM('Data-Input'!T7:U7)*1000/24
;o= 'Data-Input'!X8*1000/24
;q= 'Data-Input'!Z8*1000/24
;s= 'Data-Input'!AB7*1000/24
;u= 'Data-Input'!AD7*1000/24
;x= Purge Rate

;Input param ClientData Column CD (REACTOR B-2 OUTLET) Unit (DEGF)
;Input param ClientData Column BU (RX B-2 INLET TEMP) Unit (DEGF)
;Input param ClientData Column BQ (B-1 Average Bottom Bed Low Temp) Unit (DEGF)
;Input param ClientData Column BI (B-1 Average Bottom Bed Up Temp) Unit (DEGF)
;Input param ClientData Column BD (B-1 Average Top Bed Low Temp) Unit (DEGF)
;Input param ClientData Column AV (RX FEED FROM HEATER B) Unit (DEGF)
;Input param ClientData Column CM (Makeup H2) Unit (Mol%)
;Input param ClientData Column CJ (Recycle Gas H2) Unit (Mol%)
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param Data-Evaluation Column X (Purge Rate) Unit (SCFH)
;Input param ClientData Column DT (Combined Feed Rate) Unit (Bbl / day)

(defn Hydrogen-Availability [Est-oF-SCF-BBL

                             Reactor-B-2-Outlet
                             RX-B-2-Inlet-Temp
                             RX-B-1-TOP-BED-LOW-D
                             RX-B-1-TOP-BED-LOW-E
                             RX-B-1-TOP-BED-LOW-F
                             ;B-1-Average-Top-Bed-Low-Temp
                             RX-B-1-Bot-Bed-Up-G
                             RX-B-1-Bot-Bed-Up-H
                             RX-B-1-Bot-Bed-Up-I
                             ;B-1-Average-Bottom-Bed-Up-Temp
                             RX-B-1-Bot-Bed-Low-M
                             RX-B-1-Bot-Bed-Low-N
                             RX-B-1-Bot-Bed-Low-O
                             ;B-1-Average-Bottom-Bed-Low-Temp
                             RX-Feed-From-Heater-B
                             Makeup-H2
                             Recycle-Gas-H2
                             Make-Up-Gas-From-Unicrack-H2
                             Recycle-H2-To-Pass-A
                             Recycle-H2-To-Pass-B
                             H2-Quench-To-MID-RX-A-1
                             H2-Quench-RX-A-2-Inlet
                             H2-Quench-To-MID-RX-B-1
                             H2-Quench-RX-B-2-Inlet
                             Purge-Rate
                             RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B]
  (let [B-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-B-1-TOP-BED-LOW-D
                                                    RX-B-1-TOP-BED-LOW-E
                                                    RX-B-1-TOP-BED-LOW-F])
        B-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-B-1-Bot-Bed-Up-G
                                                      RX-B-1-Bot-Bed-Up-H
                                                      RX-B-1-Bot-Bed-Up-I])
        B-1-Average-Bottom-Bed-Low-Temp (Average-Temp [RX-B-1-Bot-Bed-Low-M
                                                       RX-B-1-Bot-Bed-Low-N
                                                       RX-B-1-Bot-Bed-Low-O])
        z (Treat-Gas-Rate-Z Make-Up-Gas-From-Unicrack-H2
                            Recycle-H2-To-Pass-A
                            Recycle-H2-To-Pass-B
                            H2-Quench-To-MID-RX-A-1
                            H2-Quench-RX-A-2-Inlet
                            H2-Quench-To-MID-RX-B-1
                            H2-Quench-RX-B-2-Inlet
                            Purge-Rate)
        a (/ 24 (Combined-Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B))
        lf (* z a)
        ah (+ (+ H2-Quench-RX-B-2-Inlet
                 H2-Quench-To-MID-RX-B-1
                 H2-Quench-To-MID-RX-A-1
                 H2-Quench-RX-A-2-Inlet)
              Make-Up-Gas-From-Unicrack-H2
              Recycle-H2-To-Pass-A
              Recycle-H2-To-Pass-B)
        ai (+ (* Make-Up-Gas-From-Unicrack-H2 Makeup-H2)
              (* Recycle-H2-To-Pass-A Recycle-Gas-H2)
              (* Recycle-H2-To-Pass-B Recycle-Gas-H2)
              (* H2-Quench-To-MID-RX-A-1 Recycle-Gas-H2)
              (* H2-Quench-RX-A-2-Inlet Recycle-Gas-H2)
              (* H2-Quench-To-MID-RX-B-1 Recycle-Gas-H2)
              (* H2-Quench-RX-B-2-Inlet Recycle-Gas-H2))
        ab (/ ai ah)
        ln (/ ab 100)
        lg (* lf ln)
        bm (+ (- Reactor-B-2-Outlet
                 RX-B-2-Inlet-Temp)
              (- B-1-Average-Bottom-Bed-Low-Temp
                 B-1-Average-Bottom-Bed-Up-Temp)
              (- B-1-Average-Top-Bed-Low-Temp
                 RX-Feed-From-Heater-B))
        kv (/ bm Est-oF-SCF-BBL)
        ]
    (/ lg kv)))

;Unit pressure
;Train A inlet pressure: Column AD
;Input param ClientData Column J (RX A-1 INLET PRESSURE) Unit (PSIG)
;Output Train A inlet pressure: Column AD
(defn Train-A-Inlet-Pressure [RX-A-1-Inlet-Pressure]
  RX-A-1-Inlet-Pressure)

;Train B inlet pressure: Column AH
;Input param ClientData Column AU (RX B-1 INLET PRESSURE) Unit (PSIG)
;Output Train A inlet pressure: Column AH
(defn Train-B-Inlet-Pressure [RX-B-1-Inlet-Pressure]
  RX-B-1-Inlet-Pressure)

;LL25*LN25
;ll= AD26-AG26 / 2
;ad = 'Data-Input'!AJ8 'Client Data'!J14
;ag = SUM (AE23:AF23)
;ae = 'Data-Input'!AL10 'Client Data'!AG15
;af = 'Data-Input'!AO10 'Client Data'!AQ15
;ln= AB22 / 100
;ab= 'Data-Input'!AI9
;(SUM (R10*S10, T10*V10, U10*V10, X10*Y10, Z10*AA10, AB10*AC10, AD10*AE10) /AH10)
;Train A hydrogen partial pressure: Column LA
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column CM (Makeup H2) Unit (Mol%)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column CJ (Recycle Gas H2) Unit (Mol%)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param ClientData Column AG (RX A-1 OVERALL DELTA P) Unit (PSIG)
;Input param ClientData Column AQ (REACTOR A-2 DIFF PRESS) Unit (PSIG)
;Input param ClientData Column J (RX A-1 INLET PRESSURE) Unit (PSIG)
;Output Train A hydrogen partial pressure: Column LA
(defn Train-A-Hydrogen-Partial [Make-Up-Gas-From-Unicrack-H2
                                Makeup-H2
                                Recycle-H2-To-Pass-A
                                Recycle-Gas-H2
                                Recycle-H2-To-Pass-B
                                H2-Quench-To-MID-RX-A-1
                                H2-Quench-RX-A-2-Inlet
                                H2-Quench-To-MID-RX-B-1
                                H2-Quench-RX-B-2-Inlet
                                RX-A-1-Overall-Delta-P
                                Reactor-A-2-Diff-Press
                                RX-A-1-Inlet-Pressure]
  (let [ab (Treat-Gas-Purity Make-Up-Gas-From-Unicrack-H2
                             Makeup-H2
                             Recycle-H2-To-Pass-A
                             Recycle-Gas-H2
                             Recycle-H2-To-Pass-B
                             H2-Quench-To-MID-RX-A-1
                             H2-Quench-RX-A-2-Inlet
                             H2-Quench-To-MID-RX-B-1
                             H2-Quench-RX-B-2-Inlet)
        ln (/ ab 100)
        ag (Total-Delta-P-Train-A RX-A-1-Overall-Delta-P Reactor-A-2-Diff-Press)
        a (/ ag 2)
        ll (- RX-A-1-Inlet-Pressure a)
        ]
    (* ll ln)))

;LM25*LN25
;lm =AH23-AK23 / 2
;ah = 'Data-Input'!AP807 'Client Data'!AU14
;ak = SUM (AI821:AJ821)
;ai = 'Data-Input'!AR807 'Client Data'!BR13
;aj = 'Data-Input'!AU807 'Client Data'!CB13
;ln= AB22 / 100
;ab= 'Data-Input'!AI9
;(SUM (R10*S10, T10*V10, U10*V10, X10*Y10, Z10*AA10, AB10*AC10, AD10*AE10) /AH10)
;Train B hydrogen partial pressure: Column LB
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column CM (Makeup H2) Unit (Mol%)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column CJ (Recycle Gas H2) Unit (Mol%)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param ClientData Column AU (RX B-1 INLET PRESSURE) Unit (PSIG)
;Input param ClientData Column BR (RX B-1 OVERALL DELTA P) Unit (PSIG)
;Input param ClientData Column CB (REACTOR B-2 DIFF PRESS) Unit (PSI)
(defn Train-B-Hydrogen-Partial [Make-Up-Gas-From-Unicrack-H2
                                Makeup-H2
                                Recycle-H2-To-Pass-A
                                Recycle-Gas-H2
                                Recycle-H2-To-Pass-B
                                H2-Quench-To-MID-RX-A-1
                                H2-Quench-RX-A-2-Inlet
                                H2-Quench-To-MID-RX-B-1
                                H2-Quench-RX-B-2-Inlet
                                RX-B-1-Inlet-Pressure
                                RX-B-1-Overall-Delta-P
                                Reactor-B-2-Diff-Press]

  (let [ab (Treat-Gas-Purity Make-Up-Gas-From-Unicrack-H2
                             Makeup-H2
                             Recycle-H2-To-Pass-A
                             Recycle-Gas-H2
                             Recycle-H2-To-Pass-B
                             H2-Quench-To-MID-RX-A-1
                             H2-Quench-RX-A-2-Inlet
                             H2-Quench-To-MID-RX-B-1
                             H2-Quench-RX-B-2-Inlet)
        ln (/ ab 100)

        ak (+ RX-B-1-Overall-Delta-P Reactor-B-2-Diff-Press)
        a (/ ak 2)
        lm (- RX-B-1-Inlet-Pressure a)]
    (* lm ln)))


;Pressure drop

;Train A pressure drop
;Total pressure drop: Column AG
;Input param ClientData Column AG (RX A-1 OVERALL DELTA P) Unit (PSIG)
;Input param ClientData Column AQ (REACTOR A-2 DIFF PRESS) Unit (PSIG)
;Output Total pressure drop: Column AG
;ag = SUM(AE23:AF23)
;ae = 'Data-Input'!AL10 'Client Data'!AG15
;af =  'Data-Input'!AO10 'Client Data'!AQ15
(defn Train-A-Total-Pressure-Drop [RX-A-1-Overall-Delta-P
                                   Reactor-A-2-Diff-Press]
  (Total-Delta-P-Train-A RX-A-1-Overall-Delta-P Reactor-A-2-Diff-Press))

;lc = AG25* ($C$18/C25) ^$LD$6* ($Z$18/Z25) ^$LD$7
;ag = SUM (AE23:AF23)
;ae = 'Data-Input'!AL10 'Client Data'!AG15
;af =  'Data-Input'!AO10 'Client Data'!AQ15af=
;c18= SOR Ref. Day Feed Rate
;c= 'Data-Input'!D7
;ld6= Feed Exp
;z18= SOR Ref. Day Treat Gas Rate
;z= SUM (K23, M23, O23, Q23, S23, U23) -X23
;ld7= Treat Gas Exp
;Normalized pressure drop: Column LC
;Input param Data-Evaluation Column C (SOR-Ref-Day-Feed-Rate)
;Input param Data-Evaluation Column Z (SOR-Ref-Day-Treat-Gas-Rate)

;Input param Data-Evaluation Column LD6 (Feed-Exp)
;Input param Data-Evaluation Column LD7 (Treat-Gas-Exp)

;Input param ClientData Column AG (RX A-1 OVERALL DELTA P) Unit (PSIG)
;Input param ClientData Column AQ (REACTOR A-2 DIFF PRESS) Unit (PSIG)
;Input param ClientData Column DT (Combined Feed Rate) Unit (Bbl / day)
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param Data-Evaluation Column X (Purge Rate) Unit (SCFH)
(defn Train-A-Normalized-Pressure-Drop [SOR-Ref-Day-Feed-Rate
                                        SOR-Ref-Day-Treat-Gas-Rate
                                        Feed-Exp
                                        Treat-Gas-Exp
                                        RX-A-1-Overall-Delta-P
                                        Reactor-A-2-Diff-Press
                                        RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B
                                        Make-Up-Gas-From-Unicrack-H2
                                        Recycle-H2-To-Pass-A
                                        Recycle-H2-To-Pass-B
                                        H2-Quench-To-MID-RX-A-1
                                        H2-Quench-RX-A-2-Inlet
                                        H2-Quench-To-MID-RX-B-1
                                        H2-Quench-RX-B-2-Inlet
                                        Purge-Rate]
  (let [ag (Total-Delta-P-Train-A RX-A-1-Overall-Delta-P Reactor-A-2-Diff-Press)
        a (js/Math.pow (/ SOR-Ref-Day-Feed-Rate (Combined-Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B)) Feed-Exp)
        z (Treat-Gas-Rate-Z Make-Up-Gas-From-Unicrack-H2
                            Recycle-H2-To-Pass-A
                            Recycle-H2-To-Pass-B
                            H2-Quench-To-MID-RX-A-1
                            H2-Quench-RX-A-2-Inlet
                            H2-Quench-To-MID-RX-B-1
                            H2-Quench-RX-B-2-Inlet
                            Purge-Rate)
        b (js/Math.pow (/ SOR-Ref-Day-Treat-Gas-Rate z) Treat-Gas-Exp)]
    (* ag (* a b))))

;Train B pressure drop
;Total pressure drop: Column AK
;Input param ClientData Column BR (RX B-1 OVERALL DELTA P) Unit (PSIG)
;Input param ClientData Column CB (REACTOR B-2 DIFF PRESS) Unit (PSI)
;ak =SUM(AI23:AJ23)
;ai= 'Data-Input'!AR12 BR
;aj= 'Data-Input'!AU9 CB
(defn Train-B-Total-Pressure-Drop [RX-B-1-Overall-Delta-P
                                   Reactor-B-2-Diff-Press]
  (+ RX-B-1-Overall-Delta-P Reactor-B-2-Diff-Press))

;Normalized pressure drop: Column LD
;ld= AK24* ($C$18/C24) ^$LD$6* ($Z$18/Z24) ^$LD$7
;ak= SUM(AI22:AJ22)
;ai= 'Data-Input'!AR7 'Data-Input'!BA7 'Client Data'!AF17
;aj= 'Data-Input'!AU7 'Data-Input'!BC7 'Client Data'!AK14

;c18= SOR Ref. Day Feed Rate
;c= 'Data-Input'!D7  'Client Data'!DT13
;ld6= Feed Exp
;z18= SOR Ref. Day Treat Gas Rate
;z= SUM (K23, M23, O23, Q23, S23, U23) -X23
;k= 'Data-Input'!R8*1000/24 'Client Data'!I13
;m= SUM('Data-Input'!T7:U7)*1000/24
;data-Input t = 'Client Data'!F13
;data-Input u = 'Client Data'!G13
;o= 'Data-Input'!X8*1000/24 'Client Data'!T13
;q= 'Data-Input'!Z8*1000/24 'Client Data'!AJ13
;s= 'Data-Input'!AB7*1000/24 'Client Data'!BE13
;u= 'Data-Input'!AD7*1000/24 'Client Data'!BT13
;x= Purge Rate
;ld7 = Treat Gas Exp
;
;Input param ClientData Column BR (RX B-1 OVERALL DELTA P) Unit (PSIG)
;Input param ClientData Column CB (REACTOR B-2 DIFF PRESS) Unit (PSI)

;Input param Data-Evaluation Column C (SOR Ref. Day Feed Rate)
;Input param Data-Evaluation Column Z (SOR Ref. Day Treat Gas Rate)
;Input param Data-Evaluation Column LD6 (Feed Exp)
;Input param Data-Evaluation Column LD7 (Treat Gas Exp)

;Input param ClientData Column DT (Combined Feed Rate) Unit (Bbl / day)
;Input param ClientData Column I (MAKE-UP GAS FROM UNICRACK H2) Unit (KSCF/D)
;Input param ClientData Column F ("RECYCLE H2 TO PASS ""A""") Unit(KSCF/D)
;Input param ClientData Column G ("RECYCLE H2 TO PASS ""B""") Unit(KSCF/D)
;Input param ClientData Column T (H2 QUENCH TO MID RX A-1) Unit (KSCF/D)
;Input param ClientData Column AJ (H2 QUENCH RX A-2 INLET) Unit (KSCF/D)
;Input param ClientData Column BE (H2 QUENCH TO MID RX B-1) Unit (KSCF/D)
;Input param ClientData Column BT (H2 QUENCH RX B-2 INLET) Unit (KSCF/D)
;Input param Data-Evaluation Column X (Purge Rate) Unit (SCFH)

(defn Train-B-Normalized-Pressure-Drop [RX-B-1-Overall-Delta-P
                                        Reactor-B-2-Diff-Press
                                        SOR-Ref-Day-Feed-Rate
                                        RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B
                                        Feed-Exp
                                        SOR-Ref-Day-Treat-Gas-Rate
                                        Make-Up-Gas-From-Unicrack-H2
                                        Recycle-H2-To-Pass-A
                                        Recycle-H2-To-Pass-B
                                        H2-Quench-To-MID-RX-A-1
                                        H2-Quench-RX-A-2-Inlet
                                        H2-Quench-To-MID-RX-B-1
                                        H2-Quench-RX-B-2-Inlet
                                        Purge-Rate
                                        Treat-Gas-Exp]
  (let [ak (+ RX-B-1-Overall-Delta-P Reactor-B-2-Diff-Press)
        a (js/Math.pow (/ SOR-Ref-Day-Feed-Rate (Combined-Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B)) Feed-Exp)
        z (Treat-Gas-Rate-Z Make-Up-Gas-From-Unicrack-H2
                            Recycle-H2-To-Pass-A
                            Recycle-H2-To-Pass-B
                            H2-Quench-To-MID-RX-A-1
                            H2-Quench-RX-A-2-Inlet
                            H2-Quench-To-MID-RX-B-1
                            H2-Quench-RX-B-2-Inlet
                            Purge-Rate)
        b (js/Math.pow (/ SOR-Ref-Day-Treat-Gas-Rate z) Treat-Gas-Exp)]
    (* ak (* a b))))

;Temperature profiles
;Total temperature rises
;Train A DT: Column AY
;ay = AX22+AS22+AO22
;ax= AW23-AU23
;aw= 'Data-Input'!BE7 'Client Data'!AS14
;au= 'Data-Input'!BC7 'Client Data'!AK14
;as= AR23-AP23
;ar= 'Data-Input'!BA7 'Client Data'!AF17
;ap= 'Data-Input'!AY7 'Client Data'!X15
;ao=AN23-AL23
;an= 'Data-Input'!AX9 'Client Data'!S14
;al= 'Data-Input'!AV9 'Client Data'!K7
;Input param ClientData Column AS (REACTOR A OUTLET) Unit (DEGF)
;Input param ClientData Column AK (RX A-2 INLET TOP) Unit(DEGF)
;Input param ClientData Column AF (A-1 Average Bottom Bed Low Temp) Unit(DEGF)
;Input param ClientData Column X (A-1 Average Bottom Bed Up Temp) Unit (DEGF)
;Input param ClientData Column S (A-1 Average Top Bed Low Temp) Unit (DEGF)
;Input param ClientData Column K (RX A-1 FEED FROM HEATER A) Unit (DEGF)
(defn Train-A-Total-Temperature-Rises [Reactor-A-Outlet
                                       RX-A-2-Inlet-Top
                                       RX-A-1-Bot-Bed-Up-G
                                       RX-A-1-Bot-Bed-Up-H
                                       RX-A-1-Bot-Bed-Up-I
                                       ;A-1-Average-Bottom-Bed-Up-Temp
                                       RX-A-1-Top-Bed-Low-D
                                       RX-A-1-Top-Bed-Low-E
                                       RX-A-1-Top-Bed-Low-F
                                       ;A-1-Average-Top-Bed-Low-Temp
                                       RX-A-1-Bot-Bed-Low-M
                                       RX-A-1-Bot-Bed-Low-N
                                       RX-A-1-Bot-Bed-Low-O
                                       ;A-1-Average-Bottom-Bed-Low-Temp
                                       RX-A-1-Feed-From-Heater-A]
  (let [A-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-A-1-Bot-Bed-Up-G
                                                      RX-A-1-Bot-Bed-Up-H
                                                      RX-A-1-Bot-Bed-Up-I])
        A-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-A-1-Top-Bed-Low-D
                                                    RX-A-1-Top-Bed-Low-E
                                                    RX-A-1-Top-Bed-Low-F])
        A-1-Average-Bottom-Bed-Low-Temp (Average-Temp [ RX-A-1-Bot-Bed-Low-M
                                                       RX-A-1-Bot-Bed-Low-N
                                                       RX-A-1-Bot-Bed-Low-O])

        ax (- Reactor-A-Outlet RX-A-2-Inlet-Top)
        as (- A-1-Average-Bottom-Bed-Low-Temp
              A-1-Average-Bottom-Bed-Up-Temp)
        ao (- A-1-Average-Top-Bed-Low-Temp
              RX-A-1-Feed-From-Heater-A)
        ay (+ ax as ao)]
    ay))

;Train B DT: Column BM
;bm =BL22+BG22+BC22
;bl=BK23-BI23
;bk = 'Data-Input'!BO7 'Client Data'!CD14
;bi= 'Data-Input'!BM7 'Client Data'!BU17
;bg= BF22-BD22
;bf= 'Data-Input'!BK7 'Client Data'!BQ14
;bd= 'Data-Input'!BI7 'Client Data'!BI14
;bc= BB23-AZ23
;bb= 'Data-Input'!BH7 'Client Data'!BD14
;az= 'Data-Input'!BF7 'Client Data'!AV16
;Input param ClientData Column CD (REACTOR B-2 OUTLET) Unit (DEGF)
;Input param ClientData Column BU (RX B-2 INLET TEMP) Unit (DEGF)
;Input param ClientData Column BQ (B-1 Average Bottom Bed Low Temp) Unit (DEGF)
;Input param ClientData Column BI (B-1 Average Bottom Bed Up Temp) Unit (DEGF)
;Input param ClientData Column BD (B-1 Average Top Bed Low Temp) Unit (DEGF)
;Input param ClientData Column AV (RX FEED FROM HEATER B) Unit (DEGF)
(defn Train-B-Total-Temperature-Rises [Reactor-B-2-Outlet
                                       RX-B-2-Inlet-Temp
                                       RX-B-1-TOP-BED-LOW-D
                                       RX-B-1-TOP-BED-LOW-E
                                       RX-B-1-TOP-BED-LOW-F
                                       ;B-1-Average-Top-Bed-Low-Temp
                                       RX-B-1-Bot-Bed-Up-G
                                       RX-B-1-Bot-Bed-Up-H
                                       RX-B-1-Bot-Bed-Up-I
                                       ;B-1-Average-Bottom-Bed-Up-Temp
                                       RX-B-1-Bot-Bed-Low-M
                                       RX-B-1-Bot-Bed-Low-N
                                       RX-B-1-Bot-Bed-Low-O
                                       ;B-1-Average-Bottom-Bed-Low-Temp
                                       RX-Feed-From-Heater-B
                                       ]
  (let [B-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-B-1-TOP-BED-LOW-D
                                                    RX-B-1-TOP-BED-LOW-E
                                                    RX-B-1-TOP-BED-LOW-F])
        B-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-B-1-Bot-Bed-Up-G
                                                      RX-B-1-Bot-Bed-Up-H
                                                      RX-B-1-Bot-Bed-Up-I])
        B-1-Average-Bottom-Bed-Low-Temp (Average-Temp [RX-B-1-Bot-Bed-Low-M
                                                       RX-B-1-Bot-Bed-Low-N
                                                       RX-B-1-Bot-Bed-Low-O])
        bl (- Reactor-B-2-Outlet RX-B-2-Inlet-Temp)
        bg (- B-1-Average-Bottom-Bed-Low-Temp
              B-1-Average-Bottom-Bed-Up-Temp)
        bc (- B-1-Average-Top-Bed-Low-Temp
              RX-Feed-From-Heater-B)
        bm (+ bl bg bc)]
    bm))


;Train A bed temperature rises
;A1 Bed 1 DT: Column AO
;ao= AN22-AL22
;an= 'Data-Input'!AX8 'Client Data'!S7
;al= 'Data-Input'!AV8 'Client Data'!K14
;Input param ClientData Column S (A-1 Average Top Bed Low Temp) Unit (DEGF)
;Input param ClientData Column K (RX A-1 FEED FROM HEATER A) Unit (DEGF)
(defn Train-A-1-Bed-1-Temperature-Rises [ RX-A-1-Top-Bed-Low-D
                                         RX-A-1-Top-Bed-Low-E
                                         RX-A-1-Top-Bed-Low-F
                                         ;A-1-Average-Top-Bed-Low-Temp
                                         RX-A-1-Feed-From-Heater-A]
  (- (Average-Temp [RX-A-1-Top-Bed-Low-D
                    RX-A-1-Top-Bed-Low-E
                    RX-A-1-Top-Bed-Low-F])
     RX-A-1-Feed-From-Heater-A))

;A1 Bed 2 DT: Column AS
;as= AR26-AP26
;ar = 'Data-Input'!BA7 'Client Data'!AF13
;ap = 'Data-Input'!AY7 'Client Data'!X13
;Input param ClientData Column AF (A-1 Average Bottom Bed Low Temp) Unit(DEGF)
;Input param ClientData Column X (A-1 Average Bottom Bed Up Temp) Unit (DEGF)
(defn Train-A-1-Bed-2-Temperature-Rises [RX-A-1-Bot-Bed-Up-G
                                         RX-A-1-Bot-Bed-Up-H
                                         RX-A-1-Bot-Bed-Up-I
                                         ;A-1-Average-Bottom-Bed-Up-Temp
                                         RX-A-1-Bot-Bed-Low-M
                                         RX-A-1-Bot-Bed-Low-N
                                         RX-A-1-Bot-Bed-Low-O
                                         ;A-1-Average-Bottom-Bed-Low-Temp
                                         ]
  (- (Average-Temp [ RX-A-1-Bot-Bed-Low-M
                    RX-A-1-Bot-Bed-Low-N
                    RX-A-1-Bot-Bed-Low-O])
     (Average-Temp [RX-A-1-Bot-Bed-Up-G
                    RX-A-1-Bot-Bed-Up-H
                    RX-A-1-Bot-Bed-Up-I])))

;A2 DT: Column AX
;ax= AW22-AU22
;aw= 'Data-Input'!BE10  'Client Data'!AS19
;au = 'Data-Input'!BC14 'Client Data'!AK21
;Input param ClientData Column AS (REACTOR A OUTLET) Unit (DEGF)
;Input param ClientData Column AK (RX A-2 INLET TOP) Unit(DEGF)
(defn Train-A-2-Temperature-Rises [Reactor-A-Outlet
                                   RX-A-2-Inlet-Top]
  (- Reactor-A-Outlet
     RX-A-2-Inlet-Top))

;Train B bed temperature rises
;B1 Bed 1 DT: Column BC
;bc = BB24-AZ24
;bb= 'Data-Input'!BH7 'Client Data'!BD14
;az= 'Data-Input'!BF7 'Client Data'!AV16
;Input param ClientData Column BD (B-1 Average Top Bed Low Temp) Unit (DEGF)
;Input param ClientData Column AV (RX FEED FROM HEATER B) Unit (DEGF)
(defn Train-B-1-Bed-1-Temperature-Rises [RX-B-1-TOP-BED-LOW-D
                                         RX-B-1-TOP-BED-LOW-E
                                         RX-B-1-TOP-BED-LOW-F
                                         ;B-1-Average-Top-Bed-Low-Temp
                                         RX-Feed-From-Heater-B]
  (- (Average-Temp [RX-B-1-TOP-BED-LOW-D
                    RX-B-1-TOP-BED-LOW-E
                    RX-B-1-TOP-BED-LOW-F])
     RX-Feed-From-Heater-B))

;B1 Bed 2 DT: Column BG
;bg = BF22-BD22
;bf = 'Data-Input'!BK7  'Client Data'!BQ15
;bd = 'Data-Input'!BI8  'Client Data'!BI14
;Input param ClientData Column BQ (B-1 Average Bottom Bed Low Temp) Unit (DEGF)
;Input param ClientData Column BI (B-1 Average Bottom Bed Up Temp) Unit (DEGF)
(defn Train-B-1-Bed-2-Temperature-Rises [RX-B-1-Bot-Bed-Up-G
                                         RX-B-1-Bot-Bed-Up-H
                                         RX-B-1-Bot-Bed-Up-I
                                         ;B-1-Average-Bottom-Bed-Up-Temp
                                         RX-B-1-Bot-Bed-Low-M
                                         RX-B-1-Bot-Bed-Low-N
                                         RX-B-1-Bot-Bed-Low-O
                                         ;B-1-Average-Bottom-Bed-Low-Temp
                                         ]
  (- (Average-Temp [RX-B-1-Bot-Bed-Low-M
                    RX-B-1-Bot-Bed-Low-N
                    RX-B-1-Bot-Bed-Low-O])
     (Average-Temp [RX-B-1-Bot-Bed-Up-G
                    RX-B-1-Bot-Bed-Up-H
                    RX-B-1-Bot-Bed-Up-I])))

;B2 DT: Column BL
;bl =BK22-BI22
;bk = 'Data-Input'!BO10 'Client Data'!CD16
;bi = 'Data-Input'!BM9  'Client Data'!BU17
;Input param ClientData Column CD (REACTOR B-2 OUTLET) Unit (DEGF)
;Input param ClientData Column BU (RX B-2 INLET TEMP) Unit (DEGF)
(defn Train-B-2-Temperature-Rises [Reactor-B-2-Outlet
                                   RX-B-2-Inlet-Temp]
  (- Reactor-B-2-Outlet
     RX-B-2-Inlet-Temp))

;Train A bed inlet/outlet temperatures
;A1 Bed 1 inlet T: Column AL
;al = 'Data-Input'!AV7 'Client Data'!K15
;Input param ClientData Column K (RX A-1 FEED FROM HEATER A) Unit (DEGF)
(defn Train-A-1-Bed-1-Inlet [RX-A-1-Feed-From-Heater-A]
  RX-A-1-Feed-From-Heater-A)

;A1 Bed 1 outlet T: Column AN
;an= 'Data-Input'!AX7 'Client Data'!S14
;Input param ClientData Column S (A-1 Average Top Bed Low Temp) Unit (DEGF)
(defn Train-A-1-Bed-1-Outlet [RX-A-1-Top-Bed-Low-D
                              RX-A-1-Top-Bed-Low-E
                              RX-A-1-Top-Bed-Low-F
                              ;A-1-Average-Top-Bed-Low-Temp
                              ]
  (Average-Temp [RX-A-1-Top-Bed-Low-D
                 RX-A-1-Top-Bed-Low-E
                 RX-A-1-Top-Bed-Low-F]))

;A1 Bed 2 outlet T: Column AR
;ar= 'Data-Input'!BA8 'Client Data'!AF15
;Input param ClientData Column AF (A-1 Average Bottom Bed Low Temp) Unit(DEGF)
(defn Train-A-1-Bed-2-Outlet [RX-A-1-Bot-Bed-Low-M
                              RX-A-1-Bot-Bed-Low-N
                              RX-A-1-Bot-Bed-Low-O
                              ;(dm :A-1-Average-Bottom-Bed-Low-Temp)
                              ; A-1-Average-Bottom-Bed-Low-Temp
                              ]
  (Average-Temp [RX-A-1-Bot-Bed-Low-M
                 RX-A-1-Bot-Bed-Low-N
                 RX-A-1-Bot-Bed-Low-O]))

;A2 outlet T: Column AW
;aw= 'Data-Input'!BE7 'Client Data'!AS19
;Input param ClientData Column AS (REACTOR A OUTLET) Unit (DEGF)
(defn Train-A-2-Outlet [Reactor-A-Outlet]
  Reactor-A-Outlet)

;Train B bed inlet/outlet temperatures
;B1 Bed 1 inlet T: Column AZ
;az= 'Data-Input'!BF8 'Client Data'!AV13
;Input param ClientData Column AV (RX FEED FROM HEATER B) Unit (DEGF)
(defn Train-B-1-Bed-1-Inlet [RX-Feed-From-Heater-B]
  RX-Feed-From-Heater-B)

;B1 Bed 1 outlet T: Column BB
;bb= 'Data-Input'!BH7 'Client Data'!BD15
;Input param ClientData Column BD (B-1 Average Top Bed Low Temp) Unit (DEGF)
(defn Train-B-1-Bed-1-Outlet [RX-B-1-TOP-BED-LOW-D
                              RX-B-1-TOP-BED-LOW-E
                              RX-B-1-TOP-BED-LOW-F
                              ;B-1-Average-Top-Bed-Low-Temp
                              ]
  (Average-Temp [RX-B-1-TOP-BED-LOW-D
                 RX-B-1-TOP-BED-LOW-E
                 RX-B-1-TOP-BED-LOW-F]))

;B1 Bed 2 outlet T: Column BF
;bf= 'Data-Input'!BK7 'Client Data'!BQ15
;Input param ClientData Column BQ (B-1 Average Bottom Bed Low Temp) Unit (DEGF)
(defn Train-B-2-Bed-1-Outlet [ RX-B-1-Bot-Bed-Low-M
                              RX-B-1-Bot-Bed-Low-N
                              RX-B-1-Bot-Bed-Low-O
                              ;B-1-Average-Bottom-Bed-Low-Temp
                             ]
  (Average-Temp [RX-B-1-Bot-Bed-Low-M
                 RX-B-1-Bot-Bed-Low-N
                 RX-B-1-Bot-Bed-Low-O]))

;B2 outlet T: Column BK
;bk= 'Data-Input'!BO7 'Client Data'!CD22
;Input param ClientData Column CD (REACTOR B-2 OUTLET) Unit (DEGF)
(defn Train-B-2-Outlet [Reactor-B-2-Outlet]
  Reactor-B-2-Outlet)

;Train A, reactor 1 bed 1 radial temperature spreads
;Top radial spread: Column BZ
;bz= 'Thermocouple temperatures'!BE9
;MAX (E775:G775) -MIN (E775:G775)
;e= 'Client Data'!L13
;f= 'Client Data'!M13
;g= 'Client Data'!N13
;Input param ClientData Column L (RX A-1 TOP BED UP A) Unit (DEGF)
;Input param ClientData Column M (RX A-1 TOP BED UP B) Unit (DEGF)
;Input param ClientData Column N (RX A-1 TOP BED UP C) Unit (DEGF)
(defn Train-A-Reactor-1-Bed-1-Radial-Temperature-Top-Spreads
  [RX-A-1-Top-Bed-Up-A
   RX-A-1-Top-Bed-Up-B
   RX-A-1-Top-Bed-Up-C]
  (- (max RX-A-1-Top-Bed-Up-A
          RX-A-1-Top-Bed-Up-B
          RX-A-1-Top-Bed-Up-C)
     (min RX-A-1-Top-Bed-Up-A
          RX-A-1-Top-Bed-Up-B
          RX-A-1-Top-Bed-Up-C)))

;Bottom radial spread: Column CA
;ca= 'Thermocouple temperatures'!BF9
;=MAX (H8:J8) -MIN (H8:J8)
;h= 'Client Data'!p13
;i= 'Client Data'!Q13
;j= 'Client Data'!R13
;Input param ClientData Column P (RX A-1 TOP BED LOW D) Unit (DEGF)
;Input param ClientData Column Q (RX A-1 TOP BED LOW E) Unit (DEGF)
;Input param ClientData Column R (RX A-1 TOP BED LOW F) Unit (DEGF)
(defn Train-A-Reactor-1-Bed-1-Radial-Temperature-Bottom-Spreads
  [RX-A-1-Top-Bed-Low-D
   RX-A-1-Top-Bed-Low-E
   RX-A-1-Top-Bed-Low-F]
  (- (max RX-A-1-Top-Bed-Low-D
          RX-A-1-Top-Bed-Low-E
          RX-A-1-Top-Bed-Low-F)
     (min RX-A-1-Top-Bed-Low-D
          RX-A-1-Top-Bed-Low-E
          RX-A-1-Top-Bed-Low-F)))

;Train A, reactor 1 bed 2 radial temperature spreads
;Top radial spread: Column CB
;cb='Thermocouple temperatures'!BG8
;=MAX (K8:M8) -MIN (K8:M8)
;k= 'Client Data'!U13
;l= 'Client Data'!V13
;m= 'Client Data'!W13
;Input param ClientData Column U (RX A-1 BOT BED UP G) Unit (DEGF)
;Input param ClientData Column V (RX A-1 BOT BED UP H) Unit (DEGF)
;Input param ClientData Column W (RX A-1 BOT BED UP I) Unit (DEGF)
(defn Train-A-Reactor-1-Bed-2-Radial-Temperature-Top-Spreads
  [RX-A-1-Bot-Bed-Up-G
   RX-A-1-Bot-Bed-Up-H
   RX-A-1-Bot-Bed-Up-I]
  (- (max RX-A-1-Bot-Bed-Up-G
          RX-A-1-Bot-Bed-Up-H
          RX-A-1-Bot-Bed-Up-I)
     (min RX-A-1-Bot-Bed-Up-G
          RX-A-1-Bot-Bed-Up-H
          RX-A-1-Bot-Bed-Up-I)))

;Middle radial spread: CC
;cc= 'Thermocouple temperatures'!BH8
;=MAX (N8:P8) -MIN (N8:P8)
;n='Client Data'!Y14
;o='Client Data'!Z14
;p= 'Client Data'!AA14
;Input param ClientData Column Y (RX A-1 BOT BED MID J) Unit (DEGF)
;Input param ClientData Column Z (RX A-1 BOT BED MID K) Unit (DEGF)
;Input param ClientData Column AA (RX A-1 BOT BED MID L) Unit (DEGF)
(defn Train-A-Reactor-1-Bed-2-Radial-Temperature-Middle-Spreads
  [RX-A-1-Bot-Bed-Mid-J
   RX-A-1-Bot-Bed-Mid-K
   RX-A-1-Bot-Bed-Mid-L]
  (- (max RX-A-1-Bot-Bed-Mid-J
          RX-A-1-Bot-Bed-Mid-K
          RX-A-1-Bot-Bed-Mid-L)
     (min RX-A-1-Bot-Bed-Mid-J
          RX-A-1-Bot-Bed-Mid-K
          RX-A-1-Bot-Bed-Mid-L)))

;Bottom radial spread: Column CD
;cd= 'Thermocouple temperatures'!BI8
;MAX (Q8:S8) -MIN (Q8:S8)
;q= 'Client Data'!AC13
;r= 'Client Data'!AD13
;s= 'Client Data'!AE13
;Input param ClientData Column AC (RX A-1 BOT BED LOW M) Unit (DEGF)
;Input param ClientData Column AD (RX A-1 BOT BED LOW N) Unit (DEGF)
;Input param ClientData Column AE (RX A-1 BOT BED LOW O) Unit (DEGF)
(defn Train-A-Reactor-1-Bed-2-Radial-Temperature-Bottom-Spreads
  [RX-A-1-Bot-Bed-Low-M
   RX-A-1-Bot-Bed-Low-N
   RX-A-1-Bot-Bed-Low-O]
  (- (max RX-A-1-Bot-Bed-Low-M
          RX-A-1-Bot-Bed-Low-N
          RX-A-1-Bot-Bed-Low-O)
     (min RX-A-1-Bot-Bed-Low-M
          RX-A-1-Bot-Bed-Low-N
          RX-A-1-Bot-Bed-Low-O)))

;Train A, reactor 2 radial temperature spreads
;Radial spread: Column CE
;ce= 'Thermocouple temperatures'!BJ8
;=MAX (T9:V9) -MIN (T9:V9)
;t= 'Client Data'!AL15
;u= 'Client Data'!AM15
;v= 'Client Data'!AN15
;Input param ClientData Column AL (A-2 REACTOR BED A) Unit (DEGF)
;Input param ClientData Column AM (A-2 REACTOR BED B) Unit (DEGF)
;Input param ClientData Column AN (A-2 REACTOR BED C) Unit (DEGF)
(defn Train-A-Reactor-2-Radial-Temperature-Spreads [A-2-Reactor-Bed-A
                                                    A-2-Reactor-Bed-B
                                                    A-2-Reactor-Bed-C]
  (- (max A-2-Reactor-Bed-A
          A-2-Reactor-Bed-B
          A-2-Reactor-Bed-C)
     (min A-2-Reactor-Bed-A
          A-2-Reactor-Bed-B
          A-2-Reactor-Bed-C)))

;Train B, reactor 1 bed 1 radial temperature spreads
;Top radial spread: Column CF
;cf= 'Thermocouple temperatures'!BK8
;=MAX (W8:Y8) -MIN (W8:Y8)
;w= 'Client Data'!AW13
;x= 'Client Data'!AX13
;y= 'Client Data'!AY13
;Input param ClientData Column AW (RX B-1 TOP BED UP A) Unit (DEGF)
;Input param ClientData Column AX (RX B-1 TOP BED UP B) Unit (DEGF)
;Input param ClientData Column AY (RX B-1 TOP BED UP C) Unit (DEGF)
(defn Train-B-Reactor-1-Bed-1-Radial-Temperature-Top-Spreads
  [RX-B-1-Top-Bed-Up-A
   RX-B-1-Top-Bed-Up-B
   RX-B-1-Top-Bed-Up-C]
  (- (max RX-B-1-Top-Bed-Up-A
          RX-B-1-Top-Bed-Up-B
          RX-B-1-Top-Bed-Up-C)
     (min RX-B-1-Top-Bed-Up-A
          RX-B-1-Top-Bed-Up-B
          RX-B-1-Top-Bed-Up-C)))

;Bottom radial spread: Column CG
;cg= 'Thermocouple temperatures'!BL8
;MAX (Z8:AB8) -MIN (Z8:AB8)
;z= 'Client Data'!BA14
;aa= 'Client Data'!BB14
;ab= 'Client Data'!BC14
;Input param ClientData Column BA (RX B-1 TOP BED LOW D) Unit (DEGF)
;Input param ClientData Column BB (RX B-1 TOP BED LOW E) Unit (DEGF)
;Input param ClientData Column BC (RX B-1 TOP BED LOW F) Unit (DEGF)
(defn Train-B-Reactor-1-Bed-1-Radial-Temperature-Bottom-Spreads
  [RX-B-1-Top-Bed-Low-D
   RX-B-1-Top-Bed-Low-E
   RX-B-1-Top-Bed-Low-F]
  (- (max RX-B-1-Top-Bed-Low-D
          RX-B-1-Top-Bed-Low-E
          RX-B-1-Top-Bed-Low-F)
     (min RX-B-1-Top-Bed-Low-D
          RX-B-1-Top-Bed-Low-E
          RX-B-1-Top-Bed-Low-F)))

;Train B, reactor 1 bed 2 radial temperature spreads
;Top radial spread: Column CH
;ch= 'Thermocouple temperatures'!BM8
;=MAX (AC8:AE8) -MIN (AC8:AE8)
;ac= 'Client Data'!BF13
;ad= 'Client Data'!BG13
;ae= 'Client Data'!BH13
;Input param ClientData Column BF (RX B-1 BOT BED UP G) Unit (DEGF)
;Input param ClientData Column BG (RX B-1 BOT BED UP H) Unit (DEGF)
;Input param ClientData Column BH (RX B-1 BOT BED UP I) Unit (DEGF)
(defn Train-B-Reactor-1-Bed-2-Radial-Temperature-Top-Spreads
  [RX-B-1-Bot-Bed-Up-G
   RX-B-1-Bot-Bed-Up-H
   RX-B-1-Bot-Bed-Up-I]
  (- (max RX-B-1-Bot-Bed-Up-G
          RX-B-1-Bot-Bed-Up-H
          RX-B-1-Bot-Bed-Up-I)
     (min RX-B-1-Bot-Bed-Up-G
          RX-B-1-Bot-Bed-Up-H
          RX-B-1-Bot-Bed-Up-I)))

;Middle radial spread: CI
;ci= 'Thermocouple temperatures'!BN8
;MAX (AF8:AH8) -MIN (AF8:AH8)
;af= 'Client Data'!BJ14
;ag= 'Client Data'!BK14
;ah= 'Client Data'!BL13
;Input param ClientData Column BJ (RX B-1 BOT BED MID J) Unit (DEGF)
;Input param ClientData Column BK (RX B-1 BOT BED MID K) Unit (DEGF)
;Input param ClientData Column BL (RX B-1 BOT BED MID L) Unit (DEGF)
(defn Train-B-Reactor-1-Bed-2-Radial-Temperature-Middle-Spreads
  [RX-B-1-Bot-Bed-Mid-J
   RX-B-1-Bot-Bed-Mid-K
   RX-B-1-Bot-Bed-Mid-L]
  (- (max RX-B-1-Bot-Bed-Mid-J
          RX-B-1-Bot-Bed-Mid-K
          RX-B-1-Bot-Bed-Mid-L)
     (min RX-B-1-Bot-Bed-Mid-J
          RX-B-1-Bot-Bed-Mid-K
          RX-B-1-Bot-Bed-Mid-L)))

;Bottom radial spread: Column CJ
;cj= 'Thermocouple temperatures'!BO8
;MAX (AI8:AK8) -MIN (AI8:AK8)
;ai= 'Client Data'!BN14
;aj= 'Client Data'!BO14
;ak= 'Client Data'!BP14
;Input param ClientData Column BN (RX B-1 BOT BED LOW M) Unit (DEGF)
;Input param ClientData Column BO (RX B-1 BOT BED LOW N) Unit (DEGF)
;Input param ClientData Column BP (RX B-1 BOT BED LOW O) Unit (DEGF)
(defn Train-B-Reactor-1-Bed-2-Radial-Temperature-Bottom-Spreads
  [RX-B-1-Bot-Bed-Low-M
   RX-B-1-Bot-Bed-Low-N
   RX-B-1-Bot-Bed-Low-O]
  (- (max RX-B-1-Bot-Bed-Low-M
          RX-B-1-Bot-Bed-Low-N
          RX-B-1-Bot-Bed-Low-O)
     (min RX-B-1-Bot-Bed-Low-M
          RX-B-1-Bot-Bed-Low-N
          RX-B-1-Bot-Bed-Low-O)))

;Train B, reactor 2 radial temperature spreads
;Radial spread: Column CK
;ck= 'Thermocouple temperatures'!BP8
;MAX (AL8:AN8) -MIN (AL8:AN8)
;al= 'Client Data'!BV14
;am= 'Client Data'!BW14
;an= 'Client Data'!BX14
;Input param ClientData Column BV (B-2 REACTOR BED A) Unit (DEGF)
;Input param ClientData Column BW (B-2 REACTOR BED B) Unit (DEGF)
;Input param ClientData Column BX (B-2 REACTOR BED C) Unit (DEGF)
(defn Train-B-Reactor-2-Radial-Temperature-Spreads [B-2-Reactor-Bed-A
                                                    B-2-Reactor-Bed-B
                                                    B-2-Reactor-Bed-C]
  (- (max B-2-Reactor-Bed-A
          B-2-Reactor-Bed-B
          B-2-Reactor-Bed-C)
     (min B-2-Reactor-Bed-A
          B-2-Reactor-Bed-B
          B-2-Reactor-Bed-C)))


;Normalized HDS WABT Column MI
(defn NHDSWABT [eact
                SOR-Ref-Day-k-Inh
                n
                Active-Catalyst-Volume
                SOR-Ref-Day-K-sup
                SOR-Ref-Day-Kinh-Corr
                SOR-Ref-Day-A-Train-Topsoe-WABT
                Gravity
                SULFUR-WT%-X-RA-Result
                Net-Diesel-To-Storage
                ULSD-Prod-Gravity
                Sulfur
                Net-Kerosene-To-Storage
                Prod-API-Gravity-Naphtha
                Kero-Sulfur
                Make-Up-Gas-From-Unicrack-H2
                Recycle-H2-To-Pass-A
                Recycle-H2-To-Pass-B
                H2-Quench-To-MID-RX-A-1
                H2-Quench-RX-A-2-Inlet
                H2-Quench-To-MID-RX-B-1
                H2-Quench-RX-B-2-Inlet
                Purge-Rate
                RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B
                RX-A-1-Inlet-Pressure
                RX-A-1-Overall-Delta-P
                Reactor-A-2-Diff-Press
                Makeup-H2
                Recycle-Gas-H2
                Initial-Boiling-Point-Test-Method-D2887
                Percent-5-Recovered-Test-Method-ASTM-D2887
                Percent-10-Recovered-Test-Method-ASTM-D2887
                Percent-20-Recovered-Test-Method-ASTM-D2887
                Percent-30-Recovered-Test-Method-ASTM-D2887
                Percent-40-Recovered-Test-Method-ASTM-D2887
                Percent-50-Recovered-Test-Method-ASTM-D2887
                Percent-60-Recovered-Test-Method-ASTM-D2887
                Percent-70-Recovered-Test-Method-ASTM-D2887
                Percent-80-Recovered-Test-Method-ASTM-D2887
                Percent-90-Recovered-Test-Method-ASTM-D2887
                Percent-95-Recovered-Test-Method-ASTM-D2887
                Final-Boiling-Point-Test-Method-D2887
                wt%-in-bed1
                wt%-in-bed2
                wt%-in-bed3
                dt-rule-in-bed1
                dt-rule-in-bed2
                dt-rule-in-bed3
                RX-A-1-Feed-From-Heater-A
                RX-A-1-Bot-Bed-Up-G
                RX-A-1-Bot-Bed-Up-H
                RX-A-1-Bot-Bed-Up-I
                ;A-1-Average-Bottom-Bed-Up-Temp
                RX-A-1-Top-Bed-Low-D
                RX-A-1-Top-Bed-Low-E
                RX-A-1-Top-Bed-Low-F
                ;A-1-Average-Top-Bed-Low-Temp
                RX-A-1-Bot-Bed-Low-M
                RX-A-1-Bot-Bed-Low-N
                RX-A-1-Bot-Bed-Low-O
                ;A-1-Average-Bottom-Bed-Low-Temp
                Reactor-A-Outlet
                RX-A-2-Inlet-Top
                Recycle-Gas-H2S
                KH2S]

  (let [ret-c (promise-chan)]
    (go

      (let [A-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-A-1-Bot-Bed-Up-G
                                                          RX-A-1-Bot-Bed-Up-H
                                                          RX-A-1-Bot-Bed-Up-I])
            A-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-A-1-Top-Bed-Low-D
                                                        RX-A-1-Top-Bed-Low-E
                                                        RX-A-1-Top-Bed-Low-F])
            A-1-Average-Bottom-Bed-Low-Temp (Average-Temp [ RX-A-1-Bot-Bed-Low-M
                                                           RX-A-1-Bot-Bed-Low-N
                                                           RX-A-1-Bot-Bed-Low-O])

            lh18 (-> (- SOR-Ref-Day-A-Train-Topsoe-WABT 32)
                     (/ 1.8))
            cx (Feed-Gravity Gravity)
            cz (Feed-Sulfur SULFUR-WT%-X-RA-Result)
            jt (Product-Sulfur Net-Diesel-To-Storage
                               ULSD-Prod-Gravity
                               Sulfur
                               Net-Kerosene-To-Storage
                               Prod-API-Gravity-Naphtha
                               Kero-Sulfur)
            le (let [c (Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B)
                     data (/ 5.6145833 24 Active-Catalyst-Volume)
                     ]
                 (* c data))
            lf (Total-Gas-oil-Ratio Make-Up-Gas-From-Unicrack-H2
                                    Recycle-H2-To-Pass-A
                                    Recycle-H2-To-Pass-B
                                    H2-Quench-To-MID-RX-A-1
                                    H2-Quench-RX-A-2-Inlet
                                    H2-Quench-To-MID-RX-B-1
                                    H2-Quench-RX-B-2-Inlet
                                    Purge-Rate
                                     RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B)
            ll (let [ad (Train-A-Inlet-Pressure RX-A-1-Inlet-Pressure)
                     ag (Train-A-Total-Pressure-Drop RX-A-1-Overall-Delta-P
                                                     Reactor-A-2-Diff-Press)
                     data (/ ag 2)
                     ] (- ad data))
            ln (-> (Treat-Gas-Hydrogen-Purity
                     Make-Up-Gas-From-Unicrack-H2
                     Makeup-H2
                     Recycle-H2-To-Pass-A
                     Recycle-Gas-H2
                     Recycle-H2-To-Pass-B
                     H2-Quench-To-MID-RX-A-1
                     H2-Quench-RX-A-2-Inlet
                     H2-Quench-To-MID-RX-B-1
                     H2-Quench-RX-B-2-Inlet)
                   (/ 100))
            lob (+ Make-Up-Gas-From-Unicrack-H2
                   Recycle-H2-To-Pass-A
                   Recycle-H2-To-Pass-B
                   H2-Quench-To-MID-RX-A-1
                   H2-Quench-RX-A-2-Inlet
                   H2-Quench-To-MID-RX-B-1
                   H2-Quench-RX-B-2-Inlet)
            loa (* (+ Recycle-H2-To-Pass-A
                      Recycle-H2-To-Pass-B)
                   Recycle-Gas-H2S)
            lo (-> (/ loa lob)
                   (/ 1000000))

            ; ((SUM(T7:U7)*W7))/SUM(R7,T7:U7,X7,Z7,AB7,AD7)
            ; (/ RX-A-1-Overall-Delta-P 1000000)

            d86Data (vector Initial-Boiling-Point-Test-Method-D2887
                            Percent-5-Recovered-Test-Method-ASTM-D2887
                            Percent-10-Recovered-Test-Method-ASTM-D2887
                            Percent-20-Recovered-Test-Method-ASTM-D2887
                            Percent-30-Recovered-Test-Method-ASTM-D2887
                            Percent-40-Recovered-Test-Method-ASTM-D2887
                            Percent-50-Recovered-Test-Method-ASTM-D2887
                            Percent-60-Recovered-Test-Method-ASTM-D2887
                            Percent-70-Recovered-Test-Method-ASTM-D2887
                            Percent-80-Recovered-Test-Method-ASTM-D2887
                            Percent-90-Recovered-Test-Method-ASTM-D2887
                            Percent-95-Recovered-Test-Method-ASTM-D2887
                            Final-Boiling-Point-Test-Method-D2887)
            dist-data (dist/check-Distillation nil d86Data "F")
            remove-nil-zero (remove nil? (map-indexed (fn [i d]
                                                        (if (and (and (> i 1) (< i 11)) (not (zero? d)))
                                                          d))
                                                      dist-data))
            ee (/ (apply + remove-nil-zero) (count remove-nil-zero))

            sim-dist (dist/check-Distillation nil d86Data "F")
            interpol-data (vector {:x (sim-dist 0) :y 0}
                                  {:x (sim-dist 1) :y 5}
                                  {:x (sim-dist 2) :y 10}
                                  {:x (sim-dist 3) :y 20}
                                  {:x (sim-dist 4) :y 30}
                                  {:x (sim-dist 5) :y 40}
                                  {:x (sim-dist 6) :y 50}
                                  {:x (sim-dist 7) :y 60}
                                  {:x (sim-dist 8) :y 70}
                                  {:x (sim-dist 9) :y 80}
                                  {:x (sim-dist 10) :y 90}
                                  {:x (sim-dist 11) :y 95}
                                  {:x (sim-dist 12) :y 100})
            interpol (InterPol interpol-data 600)
            lq (* (- 1 interpol) 100)
            lp (let [{:keys [error msg result]} (<! (rpc/me-abpc (/ (- ee 32) 1.8)
                                                                 (/ (- (dist-data 2) 32) 1.8)
                                                                 (/ (- (dist-data 10) 32) 1.8)))]
                 (log/trace "me-abpc" (/ (- ee 32) 1.8) "1"
                            (/ (- (dist-data 2) 32) 1.8) "2"
                            (/ (- (dist-data 10) 32) 1.8) "3" "data-pass" result)
                 (+ (* result 1.8) 32))                     ;ef22
            k-inh-std (let [{:keys [error msg result]} (<! (rpc/k-inch le cz (/ jt 10000) cx (/ lf 5.93416)
                                                                       (-> (+ ll 14.7)
                                                                           (/ 14.5)) ln lo n KH2S))]
                        (log/trace "k-inch" le "1" cz "2" (/ jt 10000) "3" cx "4" (/ lf 5.93416) "5"
                                   (-> (+ ll 14.7)
                                       (/ 14.5)) "6" ln "7" lo "8" n "9" KH2S "10" "data-pass" result)
                       result)


            k-inh-corr-std (let [{:keys [error msg result]} (<! (rpc/k-inh-norm-correction (-> (- 1 (/ jt 10000 cz))
                                                                                               (* 100))))]
                             (log/trace "k-inh-norm-correction" (-> (- 1 (/ jt 10000 cz))
                                                                    (* 100)) "1" "data-pass" result)
                             result)

            k-sup (let [{:keys [error msg result]} (<! (rpc/k-sup lq cx cz (/ (- lp 32) 1.8)))]
                    (log/trace "k-sup" lq "1" cx "2" cz "3" (/ (- lp 32) 1.8) "4" "data-pass" result)
                    result)




            mf (/ SOR-Ref-Day-K-sup k-sup)
            mg (* k-inh-std mf (/ SOR-Ref-Day-Kinh-Corr k-inh-corr-std))
            ao (- A-1-Average-Top-Bed-Low-Temp RX-A-1-Feed-From-Heater-A)
            as (- A-1-Average-Bottom-Bed-Low-Temp A-1-Average-Bottom-Bed-Up-Temp)
            ax (- Reactor-A-Outlet RX-A-2-Inlet-Top)
            a3 (* ao dt-rule-in-bed1)
            a2 (+ RX-A-1-Feed-From-Heater-A a3)
            a1 (* wt%-in-bed1 a2)
            b3 (* as dt-rule-in-bed2)
            b2 (+ A-1-Average-Bottom-Bed-Up-Temp b3)
            b1 (* wt%-in-bed2 b2)
            c3 (* ax dt-rule-in-bed3)
            c2 (+ RX-A-2-Inlet-Top c3)
            c1 (* wt%-in-bed3 c2)
            lh (/ (- (+ a1 b1 c1) 32) 1.8)
            mh (let [{:keys [error msg result]} (<! (rpc/k-Tnorm mg lh
                                                                 eact lh18))]
                 (log/trace "k-Tnorm" mg "1" lh "2"
                            eact "3" lh18 "4" "data-pass" result)
                 ;(if (e/check-valid-number result)
                 ;  result)
                 result)
            mi (let [{:keys [error msg result]} (<! (rpc/t-norm mh SOR-Ref-Day-k-Inh
                                                                eact lh18))]
                 (log/trace "t-norm" mh "1" SOR-Ref-Day-k-Inh "2"
                            eact "3" lh18 "4" "data-pass" result)
                 ;(if (e/check-valid-number result)
                 ;  result)
                 result
                 )
            NHDSWABT (+ (* mi 1.8) 32)
            ]
        (log/trace "mi" mi)
        (put! ret-c {:result  (if (e/check-valid-number (js/parseFloat mi))
                                (+ (* mi 1.8) 32)
                                nil)
                     })
        )) ret-c))

;Normalized HDB WABT Column MS
(defn NHDNWABT [RX-FEED-TO-PASS-A, RX-FEED-TO-PASS-B,
                Active-Catalyst-Volume
                Charge-N2
                Product-N2
                Make-Up-Gas-From-Unicrack-H2
                Makeup-H2
                Recycle-H2-To-Pass-A
                Recycle-Gas-H2
                Recycle-H2-To-Pass-B
                H2-Quench-To-MID-RX-A-1
                H2-Quench-RX-A-2-Inlet
                H2-Quench-To-MID-RX-B-1
                H2-Quench-RX-B-2-Inlet
                RX-A-1-Feed-From-Heater-A
                RX-A-1-Bot-Bed-Up-G
                RX-A-1-Bot-Bed-Up-H
                RX-A-1-Bot-Bed-Up-I
                ;A-1-Average-Bottom-Bed-Up-Temp
                RX-A-1-Top-Bed-Low-D
                RX-A-1-Top-Bed-Low-E
                RX-A-1-Top-Bed-Low-F
                ;A-1-Average-Top-Bed-Low-Temp
                RX-A-1-Bot-Bed-Low-M
                RX-A-1-Bot-Bed-Low-N
                RX-A-1-Bot-Bed-Low-O
                ;A-1-Average-Bottom-Bed-Low-Temp
                SOR-Ref-Day-A-Train-Topsoe-WABT
                SOR-Ref-Day-H2-Partial-Pressure
                SOR-Ref-Day-K-HDN
                RX-B-1-Inlet-Pressure
                RX-B-1-Overall-Delta-P
                Reactor-B-2-Diff-Press
                Reactor-A-Outlet
                RX-A-2-Inlet-Top
                wt%-in-bed1
                wt%-in-bed2
                wt%-in-bed3
                dt-rule-in-bed1
                dt-rule-in-bed2
                dt-rule-in-bed3
                Eact-HDN
                H2PP-Exp]
  (let [ret-c (promise-chan)]
    (go
      (let [A-1-Average-Bottom-Bed-Up-Temp (Average-Temp [RX-A-1-Bot-Bed-Up-G
                                                          RX-A-1-Bot-Bed-Up-H
                                                          RX-A-1-Bot-Bed-Up-I])
            A-1-Average-Top-Bed-Low-Temp (Average-Temp [RX-A-1-Top-Bed-Low-D
                                                        RX-A-1-Top-Bed-Low-E
                                                        RX-A-1-Top-Bed-Low-F])
            A-1-Average-Bottom-Bed-Low-Temp (Average-Temp [ RX-A-1-Bot-Bed-Low-M
                                                           RX-A-1-Bot-Bed-Low-N
                                                           RX-A-1-Bot-Bed-Low-O])
            le (let [c (Feed-Rate RX-FEED-TO-PASS-A RX-FEED-TO-PASS-B)
                     data (/ 5.6145833 24 Active-Catalyst-Volume)
                     ]
                 (* c data))
            da (Feed-Nitrogen Charge-N2)
            ju (Product-Nitrogen Product-N2)
            ln-num (/ da ju)
            lnb (js/Math.log ln-num)
            mq (let [resu (* le lnb)]
                 (log/trace le "le" lnb "lnb")
                 resu)                               ;LE22*LN(DA22/JU22)
            ah (Train-B-Inlet-Pressure RX-B-1-Inlet-Pressure)
            ak (Train-B-Total-Pressure-Drop RX-B-1-Overall-Delta-P
                                            Reactor-B-2-Diff-Press)
            lm (- ah (/ ak 2))
            ln (/ (Treat-Gas-Hydrogen-Purity Make-Up-Gas-From-Unicrack-H2
                                             Makeup-H2
                                             Recycle-H2-To-Pass-A
                                             Recycle-Gas-H2
                                             Recycle-H2-To-Pass-B
                                             H2-Quench-To-MID-RX-A-1
                                             H2-Quench-RX-A-2-Inlet
                                             H2-Quench-To-MID-RX-B-1
                                             H2-Quench-RX-B-2-Inlet) 100)
            ph (* lm ln)
            ao (- A-1-Average-Top-Bed-Low-Temp RX-A-1-Feed-From-Heater-A)
            as (- A-1-Average-Bottom-Bed-Low-Temp A-1-Average-Bottom-Bed-Up-Temp)
            ax (- Reactor-A-Outlet RX-A-2-Inlet-Top)
            a3 (* ao dt-rule-in-bed1)
            a2 (+ RX-A-1-Feed-From-Heater-A a3)
            a1 (* wt%-in-bed1 a2)
            b3 (* as dt-rule-in-bed2)
            b2 (+ A-1-Average-Bottom-Bed-Up-Temp b3)
            b1 (* wt%-in-bed2 b2)
            c3 (* ax dt-rule-in-bed3)
            c2 (+ RX-A-2-Inlet-Top c3)
            c1 (* wt%-in-bed3 c2)
            lh (/ (- (+ a1 b1 c1) 32) 1.8)
            lh18 (-> (- SOR-Ref-Day-A-Train-Topsoe-WABT 32)
                     (/ 1.8))
            lb18 (/ SOR-Ref-Day-H2-Partial-Pressure 14.5)
            mr (let [{:keys [error msg result]} (<! (rpc/k-tp-norm mq lh (/ ph 14.5) Eact-HDN H2PP-Exp
                                                                   lh18 lb18))]
                 (log/trace "k-tp-norm" mq "mq" lh "lh" (/ ph 14.5) "ph" Eact-HDN "Eact-HDN" H2PP-Exp "H2PP-Exp"
                            lh18 "lh18" lb18 "lb18" "data-pass" result)

                 ;(if (e/check-valid-number result)
                 ;  result)
                 result
                 )
            ms (let [{:keys [error msg result]} (<! (rpc/t-norm mr SOR-Ref-Day-K-HDN
                                                                Eact-HDN
                                                                lh18))]
                 (log/trace "t-norm" mr "mr" SOR-Ref-Day-K-HDN "SOR-Ref-Day-K-HDN"
                            Eact-HDN "Eact-HDN"
                            lh18 "lh18"  "data-pass" result)
                 ;(if (e/check-valid-number result)
                 ;  result)
                 result
                 )

            ]
        (log/trace "ms" ms)
        (put! ret-c {:result  (if (e/check-valid-number (js/parseFloat ms))
                                (+ (* ms 1.8) 32)
                                nil)
                     })
        )) ret-c))

