(ns cpe.calculations.naphtha)


;CS20=>>>> Wt___in_Bed_1*(AR68+BA68*DT_Rule_in_Bed_1)+Wt___in_Bed_2*(AT68+BB68*DT_Rule_in_Bed_2)+Wt___in_Bed_3*(AV68+BC68*DT_Rule_in_Bed_3)+Wt___in_Bed_4*(AX68+BD68*DT_Rule_in_Bed_4)
;AR68= AQ20
;AQ20= 'Client Data'!AP21
;BA68= AS20-AR20
;AS20=  AZ20
;AZ20= Client Data'!AQ21
;AR20= AQ20
;AT20= IF(ISBLANK(AT$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AT$8,Client_data_tags,0)))
;BB68= AU20-AT20
;AU20= IF(ISBLANK(AU$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AU$8,Client_data_tags,0)))
;AV68=IF(ISBLANK(AV$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AV$8,Client_data_tags,0)))
;BC68= AW20-AV20
;AW20= IF(ISBLANK(AW$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AW$8,Client_data_tags,0)))
;AX20 =IF(ISBLANK(AX$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AX$8,Client_data_tags,0)))
;AY20 =IF(ISBLANK(AY$8),0,INDEX(Client_Data,MATCH($A21,Client_Data_dates,0),MATCH(AY$8,Client_data_tags,0)))
;BD20 = AY20-AX20

;Input param ClientData Column AP (Reactor 1 Inlet) Unit(°F)
;Input param ClientData Column AQ (Reactor Outlet TI) Unit(°F)
;Input param  Constant Column CT2  (Wt % in Bed 1) Unit(%)
;Input param  Constant Column CT3  (Wt % in Bed 2) Unit(%)
;Input param  Constant Column CT4  (Wt % in Bed 3) Unit(%)
;Input param  Constant Column CT5  (Wt % in Bed 4) Unit(%)
;Input param  Constant Column CV2  (DT Rule in Bed 1) Unit()
;Input param  Constant Column CV3  (DT Rule in Bed 2) Unit()
;Input param  Constant Column CV4  (DT Rule in Bed 3) Unit()
;Input param  Constant Column CV5  (DT Rule in Bed 4) Unit()

;(Observed-WABT  248.7502289 268.5762329
;                100 nil nil nil 0.666666666666667 nil nil nil 0 0 0 0 0 0)

(defn Observed-WABT [Reactor-1-Inlet
                     Reactor-Outlet-TI
                     wt%-in-bed1 wt%-in-bed2 wt%-in-bed3 wt%-in-bed4
                     dt-rule-in-bed1 dt-rule-in-bed2 dt-rule-in-bed3 dt-rule-in-bed4
                     temp-tag-1-AT
                     temp-tag-2-AU
                     temp-tag-3-AV
                     temp-tag-4-AW
                     temp-tag-5-AX
                     temp-tag-6-AY]
  (let [ar Reactor-1-Inlet
        az Reactor-Outlet-TI
        at (if temp-tag-1-AT temp-tag-1-AT 0)
        au (if temp-tag-2-AU temp-tag-2-AU 0)
        bb (- au at)
        av (if temp-tag-3-AV temp-tag-3-AV 0)
        aw (if temp-tag-4-AW temp-tag-4-AW 0)
        ax (if temp-tag-5-AX temp-tag-5-AX 0)
        ay (if temp-tag-6-AY temp-tag-6-AY 0)
        bd (- ay ax)
        bc (- aw av)
        ba (- az ar)
        cs (+ (* (* wt%-in-bed1 0.01) (+ ar (* ba dt-rule-in-bed1)))
              (* (* wt%-in-bed2 0.01) (+ at (* bb dt-rule-in-bed2)))
              (* (* wt%-in-bed3 0.01) (+ av (* bc dt-rule-in-bed3)))
              (* (* wt%-in-bed4 0.01) (+ ax (* bd dt-rule-in-bed4))))]
    cs))


;EG20=>>>>  ( 1 / (1 / (($CS$16-32)/1.8+273.15) - 1.987 / $EG$6 * LN($EF$16 / EF22))-273.15)*1.8+32
;$CS$16 = fixed
;$EG$6 = fixed
;$EF$16 = fixed
;EF22 =  ED21 * EXP(($EG$6 / 1.987) * (1 / ((CS21-32)/1.8+273.15) - 1 / (($CS$16-32)/1.8+273.15)))* ($DH$16 / DH21) ^ $EG$7
;ED20= CR20*LN(BL20/CD20)
;CR20= (C72/24*5.61458)/Catalyst_Volume
;Catalyst_Volume = fixed
;BL20= 'Client Data'!AV55
;CD20= 'Client Data'!L77*10000
;CS20 = Wt___in_Bed_1*(AR68+BA68*DT_Rule_in_Bed_1)+Wt___in_Bed_2*(AT68+BB68*DT_Rule_in_Bed_2)+Wt___in_Bed_3*(AV68+BC68*DT_Rule_in_Bed_3)+Wt___in_Bed_4*(AX68+BD68*DT_Rule_in_Bed_4)
;AR68= AQ20
;AQ20= 'Client Data'!AP21
;BA68= AS20-AR20
;AS20=  AZ20
;AR20= AQ20
;AT20= IF(ISBLANK(AT$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AT$8,Client_data_tags,0)))
;BB68= AU20-AT20
;AU20= IF(ISBLANK(AU$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AU$8,Client_data_tags,0)))
;AV68=IF(ISBLANK(AV$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AV$8,Client_data_tags,0)))
;BC68= AW20-AV20
;AW20= IF(ISBLANK(AW$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AW$8,Client_data_tags,0)))
;AV20= IF(ISBLANK(AV$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AV$8,Client_data_tags,0)))
;AX68=IF(ISBLANK(AX$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AX$8,Client_data_tags,0)))
;BD68=AY20-AX20
;AY20= IF(ISBLANK(AY$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AY$8,Client_data_tags,0)))
;AX20= IF(ISBLANK(AX$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AX$8,Client_data_tags,0)))
;$DH$16  = fixed
;DH20 =  DD20/(CY20+DC20)*(AB20+14.7)
;CY20= C20/150.96*1000/CX20*BK20
;C20 = 'Client Data'!V21
;CX20 = 20.486*((CU20+273.15)*1.8)^1.26007*BK20^4.98308*EXP(0.0001165*((CU20+273.15)*1.8)-7.78712*BK20+0.0011582*((CU20+273.15)*1.8)*BK20)
;CU20 =  (CV85-32)/1.8
;CV20 = CT85-EXP(-0.94402-0.00865*(CT85-32)^0.6667+2.99791*((BT85-BP85)/80)^0.333)
;CT20= AVERAGE(BP85:BT85)
;BT20= 'Client Data'!BM86
;BP20= 'Client Data'!BI86
;BK20=  141.5/(BJ92+131.5)
;BJ20 =  'Client Data'!I88
;AB20= 'Client Data'!D21
;DD20= DC20*W20/100
;DC20 =V20*1000000/37.326/24/22.414
;w20 = (J91*K91+L91*M91)/V20
;j20 = 'Client Data'!AJ86/1000
;k20 = 'Client Data'!AK86
;L20= 'Client Data'!AM86/1000
;M20= 'Client Data'!AN86
;V20=  J20+L20
;j20 = 'Client Data'!AJ21/1000
;l20 = 'Client Data'!AM21/1000
;$EG$7 = fixed
;Input param ClientData Column D (Reactor Inlet) Unit(psig)
;Input param ClientData Column V (Combined Feed Rate) Unit(Bbl / day)
;Input param DataEvaluation Column AJ (Recycle Hydrogen Gas Rate) Unit(MSCFH)
;Input param DataInput Column AK (Recycle Gas Hydrogen) Unit(Mol %)
;Input param ClientData Column AM (Makeup Hydrogen Gas Rate) Unit(MSCFH)
;Input param ClientData Column AN (Makeup Gas Hydrogen) Unit( Mol %)
;Input param  Constant Column EK7  (Eact) Unit(H2 PP Factor)
;Input param ClientData Column I () Unit(API)
;Input param ClientData Column BM (90%) Unit(°F)
;Input param ClientData Column BI (10%) Unit(°F)
;(NHDNWAB 7345.390625 79.337 772.665832519531 99.8 8118.6767578125
;                     242 132 151	172	198 72.60 437.811207830905
;          0.41 0.11 248.7502289 268.5762329
;          477.80000000 0.05 100 nil nil nil 0.666666666666667 nil nil nil
;          223.6657958768880 15500 0.68 59.897323705933200
;         595.9151000976560 0 0 0 0 0 0)

(defn NHDNWAB [Recycle-Hydrogen-Gas-Rate
               Recycle-Gas-Hydrogen
               Makeup-Hydrogen-Gas-Rate
               Makeup-Gas-Hydrogen
               Combined-Feed-Rate
               per-90
               per-10
               per-30
               per-50
               per-70
               API
               Reactor-Inlet
               Feed-Nitrogen
               Product-Nitrogen
               Reactor-1-Inlet
               Reactor-Outlet-TI
               Catalyst-Volume
               Nitrogen-Out
               wt%-in-bed1 wt%-in-bed2 wt%-in-bed3 wt%-in-bed4
               dt-rule-in-bed1 dt-rule-in-bed2 dt-rule-in-bed3 dt-rule-in-bed4
               H2-PP-Feed-Evap
               Eact1
               Eact2
               KNorm
               Observed-WABT
               temp-tag-1-AT
               temp-tag-2-AU
               temp-tag-3-AV
               temp-tag-4-AW
               temp-tag-5-AX
               temp-tag-6-AY
               Sulfur-Out
               Feed-Sulfur]
  (let [j (/ Recycle-Hydrogen-Gas-Rate 1000)
        k Recycle-Gas-Hydrogen
        l (/ Makeup-Hydrogen-Gas-Rate 1000)
        m Makeup-Gas-Hydrogen
        bp per-10
        bq per-30
        br per-50
        bs per-70
        bt per-90
        bj API
        c Combined-Feed-Rate
        v (+ j l)
        w (/ (+ (* j k) (* l m)) v)
        dc (/ (* v 1000000) 37.326 24 22.414)
        ct (/ (+ bt br bq bp bs) 5)
        cv (- ct (js/Math.exp (+ (- 0.94402) (- (* 0.00865 (js/Math.pow (- ct 32) 0.6667)))
                                 (* 2.99791 (js/Math.pow (/ (- bt bp) 80) 0.333)))))
        cu (/ (- cv 32) 1.8)
        bk (/ 141.5 (+ bj 131.5))
        cx (* 20.486
              (js/Math.pow (* (+ cu 273.15) 1.8) 1.26007)
              (js/Math.pow bk 4.98308)
              (js/Math.exp
                (+ (- (* 0.0001165 (* (+ cu 273.15) 1.8)) (* 7.78712 bk)) (* 0.0011582 (* (+ cu 273.15) 1.8) bk))))
        cy (* (/ c 150.96) (/ 1000 cx) bk)
        dd (/ (* dc w) 100)

        ab Reactor-Inlet
        dh (* (/ dd (+ cy dc)) (+ ab 14.7))                 ;(/ dd (* (+ cy dc) ab))
        cr (/ (* (/ c 24) 5.61458) Catalyst-Volume)
        bm Feed-Nitrogen
        cf (if (< Product-Nitrogen Nitrogen-Out)
             Nitrogen-Out
             Product-Nitrogen)
        ee (* cr (js/Math.log (/ bm cf)))
        ar Reactor-1-Inlet
        az Reactor-Outlet-TI
        at (if temp-tag-1-AT temp-tag-1-AT 0)
        au (if temp-tag-2-AU temp-tag-2-AU 0)
        bb (- au at)
        av (if temp-tag-3-AV temp-tag-3-AV 0)
        aw (if temp-tag-4-AW temp-tag-4-AW 0)
        ax (if temp-tag-5-AX temp-tag-5-AX 0)
        ay (if temp-tag-6-AY temp-tag-6-AY 0)
        bd (- ay ax)
        bc (- aw av)
        ba (- az ar)
        cs (+ (* (* wt%-in-bed1 0.01) (+ ar (* ba dt-rule-in-bed1)))
              (* (* wt%-in-bed2 0.01) (+ at (* bb dt-rule-in-bed2)))
              (* (* wt%-in-bed3 0.01) (+ av (* bc dt-rule-in-bed3)))
              (* (* wt%-in-bed4 0.01) (+ ax (* bd dt-rule-in-bed4))))
        cd  (* Sulfur-Out 10000)                ;0.3
        bl  Feed-Sulfur               ;134
        cr (/ (* (/ c 24) 5.61458)  Catalyst-Volume)
        ed (* (js/Math.log (/ bl cd)) cr)
        _cs Observed-WABT
        _ef 59.897323705933200 ;nil
        _dh 223.66579587688800  ;nil
        ef          (* ed (js/Math.exp (* (/ Eact1 1.987)
                                          (-  (/ 1  (+ (/ (- cs 32) 1.8 ) 273.15))
                                              (/ 1  (+ (/ (- _cs 32) 1.8 ) 273.15)))))
                       (js/Math.pow (/ _dh dh) Eact2))
        eg (+ (* (- (/ 1  (-  (/ 1  (+ (/ (- Observed-WABT 32)1.8) 273.15) )
                              (* (/  1.987 Eact1) (js/Math.log(/ KNorm  ef))))) 273.15) 1.8)32)]

    eg))

;EK20=>>>>  ( 1 / (1 / (($CS$16-32)/1.8+273.15) - 1.987 / $EK$6 * LN($EJ$16 / EJ20))-273.15)*1.8+32
;EJ20= EE21 * EXP(($EK$6 / 1.987) * (1 / ((CS21-32)/1.8+273.15) - 1 / (($CS$16-32)/1.8+273.15)))* ($DH$16 / DH21) ^ $EK$7
;EE20= CR20*LN(BM20/CF20)
;CR20=(C20/24*5.61458)/Catalyst_Volume
;BM20= Client Data'!AW20
;CF = =IF('Client Data'!BR20<$CF$7,$CF$7,'Client Data'!BR20)
;DH20 =  DD20/(CY20+DC20)*(AB20+14.7)
;CY20= C20/150.96*1000/CX20*BK20
;C20 = 'Client Data'!V21
;CX20 = 20.486*((CU20+273.15)*1.8)^1.26007*BK20^4.98308*EXP(0.0001165*((CU20+273.15)*1.8)-7.78712*BK20+0.0011582*((CU20+273.15)*1.8)*BK20)
;CU20 =  (CV85-32)/1.8
;CV20 = CT85-EXP(-0.94402-0.00865*(CT85-32)^0.6667+2.99791*((BT85-BP85)/80)^0.333)
;CT20= AVERAGE(BP85:BT85)
;BT20= 'Client Data'!BM86
;BP20= 'Client Data'!BI86
;BK20=  141.5/(BJ92+131.5)
;BJ20 =  'Client Data'!I88
;AB20= 'Client Data'!D21
;DD20= DC20*W20/100
;DC20 =V20*1000000/37.326/24/22.414
;w20 = (J91*K91+L91*M91)/V20
;j20 = 'Client Data'!AJ86/1000
;k20 = 'Client Data'!AK86
;L20= 'Client Data'!AM86/1000
;M20= 'Client Data'!AN86
;V20=  J20+L20
;j20 = 'Client Data'!AJ21/1000
;l20 = 'Client Data'!AM21/1000
;
;CS20 = Wt___in_Bed_1*(AR68+BA68*DT_Rule_in_Bed_1)+Wt___in_Bed_2*(AT68+BB68*DT_Rule_in_Bed_2)+Wt___in_Bed_3*(AV68+BC68*DT_Rule_in_Bed_3)+Wt___in_Bed_4*(AX68+BD68*DT_Rule_in_Bed_4)
;AR68= AQ20
;AQ20= 'Client Data'!AP21
;BA68= AS20-AR20
;AS20=  AZ20
;AZ20= Client Data'!AQ21
;AR20= AQ20
;AT20= IF(ISBLANK(AT$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AT$8,Client_data_tags,0)))
;BB68= AU20-AT20
;AU20= IF(ISBLANK(AU$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AU$8,Client_data_tags,0)))
;AV68=IF(ISBLANK(AV$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AV$8,Client_data_tags,0)))
;BC68= AW20-AV20
;AW20= IF(ISBLANK(AW$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AW$8,Client_data_tags,0)))
;AX20 =IF(ISBLANK(AX$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AX$8,Client_data_tags,0)))
;AY20 =IF(ISBLANK(AY$8),0,INDEX(Client_Data,MATCH($A21,Client_Data_dates,0),MATCH(AY$8,Client_data_tags,0)))
;BD20 = AY20-AX20
;Input param DataEvaluation Column AJ (Recycle Hydrogen Gas Rate) Unit(MSCFH)
;Input param DataInput Column AK (Recycle Gas Hydrogen) Unit(Mol %)
;Input param ClientData Column AM (Makeup Hydrogen Gas Rate ) Unit(MSCFH)
;Input param ClientData Column AN (Makeup Gas Hydrogen) Unit( Mol %)
;Input param ClientData Column V (Combined Feed Rate) Unit(Bbl / day)
;Input param ClientData Column BM (90%) Unit(°F)
;Input param ClientData Column BI (10%) Unit(°F)
;Input param ClientData Column BJ (30%) Unit(°F)
;Input param ClientData Column BK (50%) Unit (°F)
;Input param ClientData Column I () Unit(API)
;Input param ClientData Column D (Reactor Inlet) Unit(psig)

;Input param ClientData Column AW (Feed Nitrogen) Unit(wppm)
;Input param ClientData Column BR (Product Nitrogen) Unit(wppm)
;Input param ClientData Column AP (Reactor 1 Inlet) Unit(°F)
;Input param ClientData Column AQ (Reactor Outlet TI) Unit(°F)

;Input param  Constant Column CP2  (Catalyst_Volume) Unit(ft3)
;Input param  Constant Column CF7  ( Nitrogen Out) Unit(wppm)
;Input param  Constant Column CT2  (Wt % in Bed 1) Unit(%)
;Input param  Constant Column CT3  (Wt % in Bed 2) Unit(%)
;Input param  Constant Column CT4  (Wt % in Bed 3) Unit(%)
;Input param  Constant Column CT5  (Wt % in Bed 4) Unit(%)
;Input param  Constant Column CV2  (DT Rule in Bed 1) Unit()
;Input param  Constant Column CV3  (DT Rule in Bed 2) Unit()
;Input param  Constant Column CV4  (DT Rule in Bed 3) Unit()
;Input param  Constant Column CV5  (DT Rule in Bed 4) Unit()

;Input param  Constant Column DH16  (H2 PP w/ Feed Evap) Unit(psia)
;Input param  Constant Column EK7  (Eact) Unit(H2 PP Factor)
;Input param  Constant Column EK6  (Eact) Unit(H2 PP Factor)

;Input param  Constant Column CS16  (Observed WABT) Unit(°F)
;Input param  Constant Column EJ16  (K_Norm) Unit()
;(NHDSWABT 7345.390625 79.337 772.665832519531 99.8 8118.6767578125
;                     242 132 151	172	198 72.60 437.811207830905
;          0.41 0.11 248.7502289 268.5762329
;          477.80000000 0.05 100 nil nil nil 0.666666666666667 nil nil nil
;          223.6657958768880 21000 1.5 16.7985576287067000
;          595.9151000976560 0 0 0 0 0 0)

(defn NHDSWABT [Recycle-Hydrogen-Gas-Rate
                Recycle-Gas-Hydrogen
                Makeup-Hydrogen-Gas-Rate
                Makeup-Gas-Hydrogen
                Combined-Feed-Rate
                per-90
                per-10
                per-30
                per-50
                per-70
                API
                Reactor-Inlet
                Feed-Nitrogen
                Product-Nitrogen
                Reactor-1-Inlet
                Reactor-Outlet-TI
                Catalyst-Volume
                Nitrogen-Out
                wt%-in-bed1 wt%-in-bed2 wt%-in-bed3 wt%-in-bed4
                dt-rule-in-bed1 dt-rule-in-bed2 dt-rule-in-bed3 dt-rule-in-bed4
                H2-PP-Feed-Evap
                Eact1
                Eact2
                KNorm
                Observed-WABT
                temp-tag-1-AT
                temp-tag-2-AU
                temp-tag-3-AV
                temp-tag-4-AW
                temp-tag-5-AX
                temp-tag-6-AY]

  (let [j (/ Recycle-Hydrogen-Gas-Rate 1000)
        k Recycle-Gas-Hydrogen
        l (/ Makeup-Hydrogen-Gas-Rate 1000)
        m Makeup-Gas-Hydrogen
        bp per-10
        bq per-30
        br per-50
        bs per-70
        bt per-90
        bj API
        c Combined-Feed-Rate
        v (+ j l)
        w (/ (+ (* j k) (* l m)) v)
        dc (/ (* v 1000000) 37.326 24 22.414)
        ct (/ (+ bt br bq bp bs) 5)
        cv (- ct (js/Math.exp (+ (- 0.94402) (- (* 0.00865 (js/Math.pow (- ct 32) 0.6667)))
                                 (* 2.99791 (js/Math.pow (/ (- bt bp) 80) 0.333)))))
        cu (/ (- cv 32) 1.8)
        bk (/ 141.5 (+ bj 131.5))
        cx (* 20.486
              (js/Math.pow (* (+ cu 273.15) 1.8) 1.26007)
              (js/Math.pow bk 4.98308)
              (js/Math.exp
                (+ (- (* 0.0001165 (* (+ cu 273.15) 1.8)) (* 7.78712 bk)) (* 0.0011582 (* (+ cu 273.15) 1.8) bk))))
        cy (* (/ c 150.96) (/ 1000 cx) bk)
        dd (/ (* dc w) 100)

        ab Reactor-Inlet
        dh (* (/ dd (+ cy dc)) (+ ab 14.7))                 ;(/ dd (* (+ cy dc) ab))
        cr (/ (* (/ c 24) 5.61458) Catalyst-Volume)
        bm Feed-Nitrogen
        cf (if (< Product-Nitrogen Nitrogen-Out)
             Nitrogen-Out
             Product-Nitrogen)
        ee (* cr (js/Math.log (/ bm cf)))
        ar Reactor-1-Inlet
        az Reactor-Outlet-TI
        at (if temp-tag-1-AT temp-tag-1-AT 0)
        au (if temp-tag-2-AU temp-tag-2-AU 0)
        bb (- au at)
        av (if temp-tag-3-AV temp-tag-3-AV 0)
        aw (if temp-tag-4-AW temp-tag-4-AW 0)
        ax (if temp-tag-5-AX temp-tag-5-AX 0)
        ay (if temp-tag-6-AY temp-tag-6-AY 0)
        bd (- ay ax)
        bc (- aw av)
        ba (- az ar)
        cs (+ (* (* wt%-in-bed1 0.01) (+ ar (* ba dt-rule-in-bed1)))
              (* (* wt%-in-bed2 0.01) (+ at (* bb dt-rule-in-bed2)))
              (* (* wt%-in-bed3 0.01) (+ av (* bc dt-rule-in-bed3)))
              (* (* wt%-in-bed4 0.01) (+ ax (* bd dt-rule-in-bed4))))
        ej (* ee (js/Math.exp (* (/ Eact1 1.987)
                                 (- (/ 1 (+ (/ (- cs 32) 1.8) 273.15))
                                    (/ 1 (+ (/ (- Observed-WABT 32) 1.8) 273.15)))
                                 ))
              (js/Math.pow (/ H2-PP-Feed-Evap dh) Eact2))]
    (+ (* (- (/ 1  (-  (/ 1  (+ (/ (- Observed-WABT 32)1.8) 273.15) ) (* (/  1.987 Eact1) (js/Math.log(/ KNorm  ej))))) 273.15) 1.8)32)))

;EB
;(1-CD20/BL20)*100
;CD ='Client Data'!L34
;BL = 'Client Data'!AV4
(defn Per-HDS [Sulfur-Out Feed-Sulfur]
  (* (- 1 (/ (* Sulfur-Out  10000) Feed-Sulfur)) 100))

;EC
;(1-CF77/BM77)*100
;CF =IF('Client Data'!BR72<$CF$7,$CF$7,'Client Data'!BR72)
;BM = 'Client Data'!AV4
(defn Per-HDN [Product-Nitrogen Feed-Nitrogen Nitrogen-Out-Fixed]
  (let [cf (if (< Product-Nitrogen Nitrogen-Out-Fixed) Nitrogen-Out-Fixed Product-Nitrogen)]
    (* (- 1 (/ cf Feed-Nitrogen)) 100)))

;bl
(defn Feed-Sulfur [Feed-Sulfur]
  Feed-Sulfur)

;bm
(defn Feed-Nitrogen [Feed-Nitrogen]
  Feed-Nitrogen)

;BJ
(defn Feed-Gravity [Feed-Gravity]
  Feed-Gravity)

;Feed Si BV
;VLOOKUP($A20,Contaminants,3)/1000
(defn Feed-Silicon [Lab-Feed-Silicon]
  (/ Lab-Feed-Silicon 1000))

;Product Silicon CH
;VLOOKUP($A20,Contaminants,4)/1000
(defn Product-Silicon [Lab-Product-Silicon]
  (/ Lab-Product-Silicon 1000))

;BY =IF(BW20<BX20,0,BW20-BX20)
;Input param ClientData Column V (Combined Feed Rate) Unit(Bbl / day)
;Input param ClientData Column I () Unit(API)
;Test Data (Accumulated-Silicon 19956.998046875
;                     68.60
;                     124.9427818189330
;                     335
;                     5424) output 150.1146923681205
(defn Accumulated-Silicon [Combined-Feed-Rate
                           API
                           previous-data
                           Lab-Product-Silicon
                           Lab-Feed-Silicon]
  (let [c Combined-Feed-Rate
        bj API
        ch (/ Lab-Product-Silicon 1000)
        bv (/ Lab-Feed-Silicon 1000)
        bk (/ 141.5 (+ bj 131.5))
        bw (/ (* 350.49285 c bk bv) 1000000)
        bx (/ (* 350.49285 c bk ch) 1000000)
        final-data  (if (< bw bx) 0 (- bw bx ))]
    ;(print "c" c "bj" bj "ch" ch "bv"  bv  "bk" bk "bw" bw "bx" bx "final-data" final-data)
    (+ final-data previous-data)))

;BO
(defn Feed-Distillation-IBP [IBP]
  IBP)

;BR
(defn Feed-Distillation-T50 [T50]
  T50)

;BT
(defn Feed-Distillation-T90 [T90]
  T90)

;BU
(defn Feed-Distillation-FBP [FBP]
  FBP)


;c
(defn Combined-Feed-Rate [Combined-Feed-Rate]
  Combined-Feed-Rate)

;cr
;(C22/24*5.61458)/Catalyst_Volume
(defn LHSV [Combined-Feed-Rate Catalyst-Volume]
  (/ (* (/ Combined-Feed-Rate 24) 5.61458) Catalyst-Volume))

;V
;J20+L20
(defn Treat-Gas-Rate [Recycle-Hydrogen-Gas-Rate
                      Makeup-Hydrogen-Gas-Rate]
  (+ Recycle-Hydrogen-Gas-Rate
     Makeup-Hydrogen-Gas-Rate))

;w (J20*K20+L20*M20)/V20
(defn TG-H2-Purity [Recycle-Hydrogen-Gas-Rate
                    Recycle-Gas-Hydrogen
                    Makeup-Hydrogen-Gas-Rate
                    Makeup-Gas-Hydrogen ]
  (let [v (+ Recycle-Hydrogen-Gas-Rate
             Makeup-Hydrogen-Gas-Rate)
        w (/ (+ (* Recycle-Hydrogen-Gas-Rate Recycle-Gas-Hydrogen) (* Makeup-Hydrogen-Gas-Rate Makeup-Gas-Hydrogen)) v)]
    w))

;CK
(defn H2-per-Oil [Combined-Feed-Rate
                  Recycle-Hydrogen-Gas-Rate
                  Recycle-Gas-Hydrogen
                  Makeup-Hydrogen-Gas-Rate
                  Makeup-Gas-Hydrogen ]
  (let [v (+ Recycle-Hydrogen-Gas-Rate
             Makeup-Hydrogen-Gas-Rate)
        w (/ (+ (* Recycle-Hydrogen-Gas-Rate Recycle-Gas-Hydrogen) (* Makeup-Hydrogen-Gas-Rate Makeup-Gas-Hydrogen)) v)
        z (* v (/ 1000000 Combined-Feed-Rate)) ]
    (* z (/ w 100))))

;H2/Oil DE
;DE20=>>>>  DD20/CY20
;DD20= DC20*W20/100
;DC20 =V20*1000000/37.326/24/22.414
;w20 = (J91*K91+L91*M91)/V91
;j20 = 'Client Data'!AJ86/1000
;k20 = 'Client Data'!AK86
;L20= 'Client Data'!AM86/1000
;M20= 'Client Data'!AN86
;V20=  J20+L20
;j20 = 'Client Data'!AJ21/1000
;l20 = 'Client Data'!AM21/1000
;CY20= C20/150.96*1000/CX20*BK20
;C20 = 'Client Data'!V21
;CX20 = 20.486*((CU20+273.15)*1.8)^1.26007*BK20^4.98308*EXP(0.0001165*((CU20+273.15)*1.8)-7.78712*BK20+0.0011582*((CU20+273.15)*1.8)*BK20)
;CU20 =  (CV85-32)/1.8
;CV20 = CT85-EXP(-0.94402-0.00865*(CT85-32)^0.6667+2.99791*((BT85-BP85)/80)^0.333)
;CT20= AVERAGE(BP85:BT85)
;BT20= 'Client Data'!BM86
;BP20= 'Client Data'!BI86
;BK20=  141.5/(BJ92+131.5)
;BJ20 =  'Client Data'!I88


;Input param DataEvaluation Column AJ (Recycle Hydrogen Gas Rate) Unit(MSCFH)
;Input param DataInput Column AK (Recycle Gas Hydrogen) Unit(Mol %)
;Input param ClientData Column AM (Makeup Hydrogen Gas Rate ) Unit(MSCFH)
;Input param ClientData Column AN (Makeup Gas Hydrogen) Unit( Mol %)
;Input param ClientData Column V (Combined Feed Rate) Unit(Bbl / day)
;Input param ClientData Column BM (90%) Unit(°F)
;Input param ClientData Column BI (10%) Unit(°F)
;Input param ClientData Column BJ (30%) Unit(°F)
;Input param ClientData Column BK (50%) Unit (°F)
;Input param ClientData Column Bl (70%) Unit(API)
;Input param ClientData Column I () Unit(API)
; Test Data (H2-Oil 7345.390625 79.337 772.665832519531 99.8 8118.6767578125
;        242 132 151	172	198 72.60
;        )  0.7912770461889436
(defn H2-Oil [Recycle-Hydrogen-Gas-Rate
              Recycle-Gas-Hydrogen
              Makeup-Hydrogen-Gas-Rate
              Makeup-Gas-Hydrogen
              Combined-Feed-Rate
              per-90
              per-10
              per-30
              per-50
              per-70
              API]
  (let [j (/ Recycle-Hydrogen-Gas-Rate 1000)
        k Recycle-Gas-Hydrogen
        l (/ Makeup-Hydrogen-Gas-Rate 1000)
        m Makeup-Gas-Hydrogen
        bp per-10
        bq per-30
        br per-50
        bs per-70
        bt per-90
        bj API
        c Combined-Feed-Rate
        v (+ j l)
        w (/ (+ (* j k)  (* l m)) v)
        dc (/ (* v 1000000) 37.326 24 22.414)
        ct (/ (+ bt br bq bp bs ) 5)
        cv (- ct (js/Math.exp (+ (- 0.94402 ) (-  (* 0.00865 (js/Math.pow (- ct 32)  0.6667 )))
                                 (* 2.99791 (js/Math.pow (/ (- bt bp) 80) 0.333)))))
        cu (/ (- cv 32) 1.8)
        bk  (/ 141.5 (+ bj 131.5) )
        cx (* 20.486
              (js/Math.pow (* (+ cu 273.15) 1.8) 1.26007)
              (js/Math.pow bk 4.98308)
              (js/Math.exp
                (+  (- (* 0.0001165 (* (+ cu 273.15) 1.8)) (* 7.78712 bk)) (* 0.0011582 (* (+ cu 273.15) 1.8) bk))))
        cy (* (/ c 150.96) (/ 1000 cx ) bk)
        dd (/ (* dc w) 100)]
    (/ dd cy)))

;AB
(defn Reactor-Inlet-Pressure [Reactor-Inlet]
  Reactor-Inlet)

;DG
;AVERAGE(AB21+14.7,AJ21+14.7)/14.5*DD21*((CK21-DL21)/(CK21))/(CY21+DC21-DK21*C21/150.96/22.414)*14.5
;(H2-PP-Outlet 437.811207830905 0.662159979
;              7345.390625 79.337 772.665832519531
;              99.8 8118.6767578125
;              242 132 151 172 198
;              72.6 0.3 134 0.29 )
(defn H2-PP-Outlet[Reactor-Inlet Reactor-Delta-P Recycle-Hydrogen-Gas-Rate Recycle-Gas-Hydrogen
                   Makeup-Hydrogen-Gas-Rate Makeup-Gas-Hydrogen
                   Combined-Feed-Rate
                   per-90
                   per-10
                   per-30
                   per-50
                   per-70
                   API
                   Sulfur-Out
                   Feed-Sulfur
                   Bromine-Number
                   ]
  (let [ab (+ Reactor-Inlet 14.7)
        aj (+ (- Reactor-Inlet Reactor-Delta-P) 14.7)
        j (/ Recycle-Hydrogen-Gas-Rate 1000)
        k Recycle-Gas-Hydrogen
        l (/ Makeup-Hydrogen-Gas-Rate 1000)
        m Makeup-Gas-Hydrogen
        c Combined-Feed-Rate
        v (+ j l)
        w (/ (+ (* j k)  (* l m)) v)
        dc (/ (* v 1000000) 37.326 24 22.414)
        dd (/ (* dc w) 100)
         z (* v (/ 1000000 c))
        ck  (* z (/ w 100))
        dc (/ (* v 1000000) 37.326 24 22.414)
        bp per-10
        bq per-30
        br per-50
        bs per-70
        bt per-90
        bj API
        ct (/ (+ bt br bq bp bs) 5)
        cv (- ct (js/Math.exp (+ (- 0.94402) (- (* 0.00865 (js/Math.pow (- ct 32) 0.6667)))
                                 (* 2.99791 (js/Math.pow (/ (- bt bp) 80) 0.333)))))
        cu (/ (- cv 32) 1.8)
        bk (/ 141.5 (+ bj 131.5))
        cx (* 20.486
              (js/Math.pow (* (+ cu 273.15) 1.8) 1.26007)
              (js/Math.pow bk 4.98308)
              (js/Math.exp
                (+ (- (* 0.0001165 (* (+ cu 273.15) 1.8)) (* 7.78712 bk)) (* 0.0011582 (* (+ cu 273.15) 1.8) bk))))
        cy (* (/ c 150.96) (/ 1000 cx) bk)
        cd  (* Sulfur-Out 10000)
        bl  Feed-Sulfur
        bn Bromine-Number
        di (* (/ (/ (- bl cd ) 1000000) 32) cx cy 3 (/ 22.4 (/ c 150.96)))
        dj (* (/ (/ (/ (- bn 0.1) 100) 79.9 ) 2) 22.4 cx (/ cy (/ c 150.96)))
        dk (+ dj di)
        dl  (* dk 5.93416)
        avg  (/ (+ ab aj ) 2 )]
     (*  (/ avg 14.5) dd   (/ (/ (- ck dl) ck) (- (+ dc cy) (* (/ (/ c 150.96 ) 22.414) dk)))  14.5)))

;DH
(defn H2-PP-Feed-Evap [Recycle-Hydrogen-Gas-Rate
                       Recycle-Gas-Hydrogen
                       Makeup-Hydrogen-Gas-Rate
                       Makeup-Gas-Hydrogen
                       Combined-Feed-Rate
                       per-90
                       per-10
                       per-30
                       per-50
                       per-70
                       API
                       Reactor-Inlet]
  (let [j (/ Recycle-Hydrogen-Gas-Rate 1000)
        k Recycle-Gas-Hydrogen
        l (/ Makeup-Hydrogen-Gas-Rate 1000)
        m Makeup-Gas-Hydrogen
        bp per-10
        bq per-30
        br per-50
        bs per-70
        bt per-90
        bj API
        ab Reactor-Inlet
        c Combined-Feed-Rate
        v (+ j l)
        w (/ (+ (* j k) (* l m)) v)
        dc (/ (* v 1000000) 37.326 24 22.414)
        ct (/ (+ bt br bq bp bs) 5)
        cv (- ct (js/Math.exp (+ (- 0.94402) (- (* 0.00865 (js/Math.pow (- ct 32) 0.6667)))
                                 (* 2.99791 (js/Math.pow (/ (- bt bp) 80) 0.333)))))
        cu (/ (- cv 32) 1.8)
        bk (/ 141.5 (+ bj 131.5))
        cx (* 20.486
              (js/Math.pow (* (+ cu 273.15) 1.8) 1.26007)
              (js/Math.pow bk 4.98308)
              (js/Math.exp
                (+ (- (* 0.0001165 (* (+ cu 273.15) 1.8)) (* 7.78712 bk)) (* 0.0011582 (* (+ cu 273.15) 1.8) bk))))
        cy (* (/ c 150.96) (/ 1000 cx) bk)
        dd (/ (* dc w) 100)
        dh (* (/ dd (+ cy dc)) (+ ab 14.7))]
    dh))

;AO
(defn Overall-dP [Reactor-Delta-P]
  Reactor-Delta-P)

;EA
;AO21*(($CY$16+$DC$16)/(CY21+DC21))^2
(defn Overall-Norm-dP [Reactor-Delta-P
                       Combined-Feed-Rate
                       Feed-Gravity
                       per-10
                       per-30
                       per-50
                       per-70
                       per-90
                       Recycle-Hydrogen-Gas-Rate
                       Makeup-Hydrogen-Gas-Rate
                       Feed-Rate
                       TG-Rate]
  (let [ao Reactor-Delta-P
        c Combined-Feed-Rate
        bj Feed-Gravity
        bk (/ 141.5 (+ bj 131.5))
        bp per-10
        bq per-30
        br per-50
        bs per-70
        bt per-90
        j (/ Recycle-Hydrogen-Gas-Rate 1000)
        l (/ Makeup-Hydrogen-Gas-Rate 1000)
        ct (/ (+ bt br bq bp bs) 5)
        cv (- ct (js/Math.exp (+ (- 0.94402 ) (-  (* 0.00865 (js/Math.pow (- ct 32)  0.6667 )))
                                 (* 2.99791 (js/Math.pow (/ (- bt bp) 80) 0.333)))))
        cu (/ (- cv 32) 1.8)
        cx (* 20.486
              (js/Math.pow (* (+ cu 273.15) 1.8) 1.26007)
              (js/Math.pow bk 4.98308)
              (js/Math.exp
                (+ (- (* 0.0001165 (* (+ cu 273.15) 1.8)) (* 7.78712 bk)) (* 0.0011582 (* (+ cu 273.15) 1.8) bk))))
        cy (* (/ c 150.96) (/ 1000 cx) bk)
        dc  (* (+ j l) (/ (/ (/ 1000000 37.326 ) 24) 22.414))]
    (* (js/Math.pow (/ (+ Feed-Rate TG-Rate) (+ cy dc) ) 2) ao)))

;AR
(defn Bed-1-Inlet-Temp [Reactor-1-Inlet]
  Reactor-1-Inlet)

;AS
(defn Bed-1-Outlet-Temp [Reactor-Outlet-TI]
  Reactor-Outlet-TI)


;H2 PP w/ Feed Evap DH
;=  DD20/(CY20+DC20)*(AB20+14.7)
;DD20 = DC20*W20/100
;w20 = (J20*K20+L20*M20)/V20
;j20 = 'Client Data'!AJ20/1000
;k20 = 'Client Data'!AK20
;L20= 'Client Data'!AM20/1000
;M20= 'Client Data'!AN20
;CY20 = C20/150.96*1000/CX20*BK20
;C20 = 'Client Data'!V21
;CX20 = 20.486*((CU20+273.15)*1.8)^1.26007*BK20^4.98308*EXP(0.0001165*((CU20+273.15)*1.8)-7.78712*BK20+0.0011582*((CU20+273.15)*1.8)*BK20)
;CU20 =  (CV20-32)/1.8
;CV20 = CT20-EXP(-0.94402-0.00865*(CT20-32)^0.6667+2.99791*((BT20-BP20)/80)^0.333)
;CT20= AVERAGE(BP20:BT20)
;BT20= 'Client Data'!BM20
;BP20= 'Client Data'!BI20
;BK20=  141.5/(BJ20+131.5)
;BJ20 =  'Client Data'!I20
;DC20= V20*1000000/37.326/24/22.414
;V20=  J20+L20
;j20 = 'Client Data'!AJ20/1000
;l20 = 'Client Data'!AM20/1000
;AB20=  'Client Data'!D20

;Input param DataEvaluation Column AJ (Recycle Hydrogen Gas Rate) Unit(MSCFH)
;Input param DataInput Column AK (Recycle Gas Hydrogen) Unit(Mol %)
;Input param ClientData Column AM (Makeup Hydrogen Gas Rate ) Unit(MSCFH)
;Input param ClientData Column AN (Makeup Gas Hydrogen) Unit( Mol %)
;Input param ClientData Column V (Combined Feed Rate) Unit(Bbl / day)
;Input param ClientData Column BM (90%) Unit(°F)
;Input param ClientData Column BI (10%) Unit(°F)
;Input param ClientData Column BJ (30%) Unit(°F)
;Input param ClientData Column BK (50%) Unit (°F)
;Input param ClientData Column Bl (70%) Unit(API)
;Input param ClientData Column I () Unit(API)
;Input param ClientData Column D (Reactor Inlet) Unit(psig)
; Test Data (Feed-Evap 7345.390625 79.337 772.665832519531 99.8 8118.6767578125
;           242 132 151	172	198 72.60 437.811207830905
;           ) ~181.43815084833807
(defn Feed-Evap [Recycle-Hydrogen-Gas-Rate
                 Recycle-Gas-Hydrogen
                 Makeup-Hydrogen-Gas-Rate
                 Makeup-Gas-Hydrogen
                 Combined-Feed-Rate
                 per-90
                 per-10
                 per-30
                 per-50
                 per-70
                 API
                 Reactor-Inlet]
  (let [j (/ Recycle-Hydrogen-Gas-Rate 1000)
        k Recycle-Gas-Hydrogen
        l (/ Makeup-Hydrogen-Gas-Rate 1000)
        m Makeup-Gas-Hydrogen
        bp per-10
        bq per-30
        br per-50
        bs per-70
        bt per-90
        bj API
        c Combined-Feed-Rate
        ab Reactor-Inlet
        v (+ j l)
        w (/ (+ (* j k)  (* l m)) v)
        dc (/ (* v 1000000) 37.326 24 22.414)
        ct (/ (+ bt br bq bp bs ) 5)
        cv (- ct (js/Math.exp (+ (- 0.94402 ) (-  (* 0.00865 (js/Math.pow (- ct 32)  0.6667 )))
                                 (* 2.99791 (js/Math.pow (/ (- bt bp) 80) 0.333)))))


        cu (/ (- cv 32) 1.8)
        bk  (/ 141.5 (+ bj 131.5) )
        cx (* 20.486
              (js/Math.pow (* (+ cu 273.15) 1.8) 1.26007)
              (js/Math.pow bk 4.98308)
              (js/Math.exp
                (+  (- (* 0.0001165 (* (+ cu 273.15) 1.8)) (* 7.78712 bk)) (* 0.0011582 (* (+ cu 273.15) 1.8) bk))))
        cy (* (/ c 150.96) (/ 1000 cx ) bk)
        dd (/ (* dc w) 100)]
    (* (/ dd (+ cy dc))  (+ ab 14.7))))



;BE
(defn Overall-dT [Reactor-Delta-T]
  Reactor-Delta-T)























;FH
;EE20 * EXP(($FG$6 / 1.987) *  (1 / ((CS20-32)/1.8+273.15) - 1 / ($FG$4+273.15)))*  (($FG$5 *14.5)/ (AB20*W20/100)) ^ $FG$7
;EE = CR20*LN(BM20/CF20)
;CR = (C20/24*5.61458)/Catalyst_Volume
;BM = 'Client Data'!AW52
;CF = IF('Client Data'!BR21<$CF$7,$CF$7,'Client Data'!BR21)
;CS = Wt___in_Bed_1*(AR33+BA33*DT_Rule_in_Bed_1)+Wt___in_Bed_2*(AT33+BB33*DT_Rule_in_Bed_2)+Wt___in_Bed_3*(AV33+BC33*DT_Rule_in_Bed_3)+Wt___in_Bed_4*(AX33+BD33*DT_Rule_in_Bed_4)
;AB = 'Client Data'!D34
;W = (J33*K33+L33*M33)/V33
;(Knorm-HDN 7345.390625 79.337 772.665832519531 99.8 8118.6767578125
;                    437.811207830905 0.41 0.11 248.7502289 268.5762329
;                    477.80000000 100 nil nil nil 0.666666666666667 nil nil nil 0 0 0 0 0 0
;                    0.05 320 25 21000 1.5)
;(defn Knorm-HDN [ Recycle-Hydrogen-Gas-Rate
;                          Recycle-Gas-Hydrogen
;                          Makeup-Hydrogen-Gas-Rate
;                          Makeup-Gas-Hydrogen
;                          Combined-Feed-Rate
;                          Reactor-Inlet
;                          Feed-Nitrogen
;                          Product-Nitrogen
;                          Reactor-1-Inlet
;                          Reactor-Outlet-TI
;                          Catalyst-Volume
;                          wt%-in-bed1 wt%-in-bed2 wt%-in-bed3 wt%-in-bed4
;                          dt-rule-in-bed1 dt-rule-in-bed2 dt-rule-in-bed3 dt-rule-in-bed4
;                          temp-tag-1-AT
;                          temp-tag-2-AU
;                          temp-tag-3-AV
;                          temp-tag-4-AW
;                          temp-tag-5-AX
;                          temp-tag-6-AY
;                          Nitrogen-Out-Fixed
;                          fg4
;                          fg5
;                          fg6
;                          fg7]
;  (let [j (/ Recycle-Hydrogen-Gas-Rate 1000)
;        k Recycle-Gas-Hydrogen
;        l (/ Makeup-Hydrogen-Gas-Rate 1000)
;        m Makeup-Gas-Hydrogen
;        v (+ j l)
;        ar Reactor-1-Inlet
;        az Reactor-Outlet-TI
;        at (if temp-tag-1-AT temp-tag-1-AT 0)
;        au (if temp-tag-2-AU temp-tag-2-AU 0)
;        bb (- au at)
;        av (if temp-tag-3-AV temp-tag-3-AV 0)
;        aw (if temp-tag-4-AW temp-tag-4-AW 0)
;        ax (if temp-tag-5-AX temp-tag-5-AX 0)
;        ay (if temp-tag-6-AY temp-tag-6-AY 0)
;        bd (- ay ax)
;        bc (- aw av)
;        ba (- az ar)
;        w (/ (+ (* j k)  (* l m)) v)
;        ab Reactor-Inlet
;        cs (+ (* (* wt%-in-bed1 0.01) (+ ar (* ba dt-rule-in-bed1)))
;              (* (* wt%-in-bed2 0.01) (+ at (* bb dt-rule-in-bed2)))
;              (* (* wt%-in-bed3 0.01) (+ av (* bc dt-rule-in-bed3)))
;              (* (* wt%-in-bed4 0.01) (+ ax (* bd dt-rule-in-bed4))))
;        cf (if (< Product-Nitrogen Nitrogen-Out-Fixed) Nitrogen-Out-Fixed Product-Nitrogen)
;        bm Feed-Nitrogen
;        cr  (/ (* (/ Combined-Feed-Rate 24) 5.61458) Catalyst-Volume)
;        ee (* cr (js/Math.log (/ bm cf)))
;        fh  (* ee (* (js/Math.exp (* (/ fg6 1.987) (- (/ 1 (+ (/ (- cs 32) 1.8) 273.15)) (/ 1 (+ fg4 273.15))) ) )
;                     (js/Math.pow (/ (* fg5 14.5) (* ab (/ w 100))) fg7)))]
;fh))

;FF
;ED20 * EXP(($FG$6 / 1.987) *  (1 / ((CS20-32)/1.8+273.15) - 1 / ($FG$4+273.15)))*  (($FG$5 *14.5)/ (AB20*W20/100)) ^ $FG$7
;ED = CR20*LN(BM20/CF20)
;CR = (C20/24*5.61458)/Catalyst_Volume
;BM = 'Client Data'!AW52
;CF = IF('Client Data'!BR21<$CF$7,$CF$7,'Client Data'!BR21)
;CS = Wt___in_Bed_1*(AR33+BA33*DT_Rule_in_Bed_1)+Wt___in_Bed_2*(AT33+BB33*DT_Rule_in_Bed_2)+Wt___in_Bed_3*(AV33+BC33*DT_Rule_in_Bed_3)+Wt___in_Bed_4*(AX33+BD33*DT_Rule_in_Bed_4)
;AB = 'Client Data'!D34
;W = (J33*K33+L33*M33)/V33
;(Knorm-HDS 7345.390625 79.337 772.665832519531 99.8 8118.6767578125
;           437.811207830905 0.41 0.11 248.7502289 268.5762329
;           477.80000000 100 nil nil nil 0.666666666666667 nil nil nil 0 0 0 0 0 0
;           0.05 320 25 15500 0.68)
;(defn Knorm-HDS [ Recycle-Hydrogen-Gas-Rate
;                 Recycle-Gas-Hydrogen
;                 Makeup-Hydrogen-Gas-Rate
;                 Makeup-Gas-Hydrogen
;                 Combined-Feed-Rate
;                 Reactor-Inlet
;                 Feed-Nitrogen
;                 Product-Nitrogen
;                 Reactor-1-Inlet
;                 Reactor-Outlet-TI
;                 Catalyst-Volume
;                 wt%-in-bed1 wt%-in-bed2 wt%-in-bed3 wt%-in-bed4
;                 dt-rule-in-bed1 dt-rule-in-bed2 dt-rule-in-bed3 dt-rule-in-bed4
;                 temp-tag-1-AT
;                 temp-tag-2-AU
;                 temp-tag-3-AV
;                 temp-tag-4-AW
;                 temp-tag-5-AX
;                 temp-tag-6-AY
;                 Nitrogen-Out-Fixed
;                 fg4
;                 fg5
;                 fg6
;                 fg7]
;  (let [j (/ Recycle-Hydrogen-Gas-Rate 1000)
;        k Recycle-Gas-Hydrogen
;        l (/ Makeup-Hydrogen-Gas-Rate 1000)
;        m Makeup-Gas-Hydrogen
;        v (+ j l)
;        c Combined-Feed-Rate
;        ar Reactor-1-Inlet
;        az Reactor-Outlet-TI
;        at (if temp-tag-1-AT temp-tag-1-AT 0)
;        au (if temp-tag-2-AU temp-tag-2-AU 0)
;        bb (- au at)
;        av (if temp-tag-3-AV temp-tag-3-AV 0)
;        aw (if temp-tag-4-AW temp-tag-4-AW 0)
;        ax (if temp-tag-5-AX temp-tag-5-AX 0)
;        ay (if temp-tag-6-AY temp-tag-6-AY 0)
;        bd (- ay ax)
;        bc (- aw av)
;        ba (- az ar)
;        w (/ (+ (* j k)  (* l m)) v)
;        ab Reactor-Inlet
;        cs (+ (* (* wt%-in-bed1 0.01) (+ ar (* ba dt-rule-in-bed1)))
;              (* (* wt%-in-bed2 0.01) (+ at (* bb dt-rule-in-bed2)))
;              (* (* wt%-in-bed3 0.01) (+ av (* bc dt-rule-in-bed3)))
;              (* (* wt%-in-bed4 0.01) (+ ax (* bd dt-rule-in-bed4))))
;        cf (if (< Product-Nitrogen Nitrogen-Out-Fixed) Nitrogen-Out-Fixed Product-Nitrogen)
;        bm Feed-Nitrogen
;        cr  (/ (* (/ c 24) 5.61458) Catalyst-Volume)
;        cd 0.3                                              ;Sulfur-Out
;        bl 134                                              ;Feed-Sulfur
;        cr (/ (* (/ c 24) 5.61458)  Catalyst-Volume)
;        ed (* (js/Math.log (/ bl cd)) cr)
;        ff  (* ed (* (js/Math.exp (* (/ fg6 1.987) (- (/ 1 (+ (/ (- cs 32) 1.8) 273.15)) (/ 1 (+ fg4 273.15))) ) )
;                     (js/Math.pow (/ (* fg5 14.5) (* ab (/ w 100))) fg7)))]
;    ff))








;cy
;C20/150.96*1000/CX20*BK20
;BK 141.5/(BJ49+131.5)
;CX 20.486*((CU20+273.15)*1.8)^1.26007*BK20^4.98308*EXP(0.0001165*((CU20+273.15)*1.8)-7.78712*BK20+0.0011582*((CU20+273.15)*1.8)*BK20)
;(defn Feed-Rate [Combined-Feed-Rate
;                 Feed-Gravity
;                 per-10
;                 per-30
;                 per-50
;                 per-70
;                 per-90]
;  (let [c Combined-Feed-Rate
;        bj Feed-Gravity
;        bk (/ 141.5 (+ bj 131.5))
;        bp per-10
;        bq per-30
;        br per-50
;        bs per-70
;        bt per-90
;        ct (/ (+ bt br bq bp bs) 5)
;        cv (- ct (js/Math.exp (+ (- 0.94402 ) (-  (* 0.00865 (js/Math.pow (- ct 32)  0.6667 )))
;                                 (* 2.99791 (js/Math.pow (/ (- bt bp) 80) 0.333)))))
;        cu (/ (- cv 32) 1.8)
;        cx (* 20.486
;              (js/Math.pow (* (+ cu 273.15) 1.8) 1.26007)
;              (js/Math.pow bk 4.98308)
;              (js/Math.exp
;                (+ (- (* 0.0001165 (* (+ cu 273.15) 1.8)) (* 7.78712 bk)) (* 0.0011582 (* (+ cu 273.15) 1.8) bk))))
;        cy (* (/ c 150.96) (/ 1000 cx) bk)]
;    cy))






;DC
;V20*1000000/37.326/24/22.414
;(defn TG-Rate [Recycle-Hydrogen-Gas-Rate
;                      Makeup-Hydrogen-Gas-Rate]
;  (* (+ Recycle-Hydrogen-Gas-Rate
;        Makeup-Hydrogen-Gas-Rate) (/ 1000000 37.326 24 22.414)))


;DD
;DC20*W20/100
;(defn H2-Rate [Recycle-Hydrogen-Gas-Rate
;               Recycle-Gas-Hydrogen
;               Makeup-Hydrogen-Gas-Rate
;               Makeup-Gas-Hydrogen ]
;  (let [v (+ Recycle-Hydrogen-Gas-Rate
;             Makeup-Hydrogen-Gas-Rate)
;        w (/ (+ (* Recycle-Hydrogen-Gas-Rate Recycle-Gas-Hydrogen) (* Makeup-Hydrogen-Gas-Rate Makeup-Gas-Hydrogen)) v)
;        dc  (* v (/ 1000000 37.326 24 22.414))]
;    (* (/ w 100) dc)))



;AT
;IF(ISBLANK(AT$8),0,INDEX(Client_Data,MATCH($A20,Client_Data_dates,0),MATCH(AT$8,Client_data_tags,0)))
(defn Bed-2-Inlet-Temp [temp-tag-AT]
  (if temp-tag-AT temp-tag-AT 0))

;;AU
(defn Bed-2-Outlet-Temp [temp-tag-AU]
  (if temp-tag-AU temp-tag-AU 0))














