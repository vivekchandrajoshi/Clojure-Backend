(ns cpe.calculations.dist)

(def Simd86 [206.64 303.70 343.50 386.68 414.04 438.77 460.80
             484.26 508.26 542.42 584.76 598.28 605.17])
(def d86 [131.20 0 301.20 364.50 404.60 439.50
          470.60 500.20 530.50 566.40 610.90 0 649.80])

(defn- InterPolDist [X, X1, X2, Y1, Y2]
  (let [a1 (- X X1)
        a2 (- X2 X1)
        a (/ a1 a2)
        b1 (- Y2 Y1)
        b (* a b1)]
    (+ b Y1)))

(defn- D86toSimDist [d86 temp-unit]
  (let [D86TBPA [0.8718, 7.4012, 4.9004, 3.0305, 2.5282, 3.0419, 0.11798, 1]
        D86TBPB [1.0258, 0.60244, 0.71644, 0.80076, 0.82002, 0.75497, 1.6606, 1]
        D86TBPE [600, 100, 250, 250, 150, 100, 1000, 100000]
        SDTBPA [1, 0.15779, 0.011903, 0.05342, 0.19861, 0.31531, 0.97476, 0.02172]
        SDTBPB [1, 1.4296, 2.0253, 1.6988, 1.3975, 1.2938, 0.8723, 1.9733]
        SDTBPE [700, 40, 75, 75, 75, 75, 40, 30]
        vol-off [1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99]
        TempTBP (atom {:Temp-TBP (vector 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)})
        TinF (atom {:tin-F (vector)})
        TempSD (atom {:Temp-SD (vector 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)})
        ; Calculate probability factors
        prob-F (mapv (fn [d]
                       (let [a-p (if (> d 50)
                                   (- 1 (/ d 100))
                                   (/ d 100))
                             e-p (js/Math.pow (js/Math.log
                                                (/ 1 (* a-p a-p))) 0.5)
                             a1 (* 0.27061 e-p)
                             a (+ 2.3075377 a1)
                             b1 (* 0.99229 e-p)
                             b2 (* 0.04481 e-p e-p)
                             b (+ 1 b1 b2)
                             prob-f (- e-p (/ a b))]
                         (if (< d 50)
                           (- prob-f)
                           prob-f))) vol-off)

        tin-F-atom (swap! TinF assoc :tin-F
                          (cond
                            (= temp-unit "C") (map (fn [d]
                                                     (let [a (/ 9 5)
                                                           b (* d a)
                                                           c (+ b 32)] c)
                                                     ) d86)
                            (= temp-unit "F") d86))

        ; Checking that the distillation has at least two entries
        non-zero-index (->> (map-indexed
                              (fn [idx itm]
                                (if-not (= itm 0)
                                  idx
                                  nil))
                              (@TinF :tin-F))
                            (remove nil?)
                            (into []))
        ]
    (if (> (count non-zero-index) 2)
      ;	; Fill in missing major points by probability interpolation
      (do
        (if (= ((@TinF :tin-F) 0) 0)
          (let [first-non-zero-index (first non-zero-index)
                second-non-zero-index (second non-zero-index)
                tin-f-0 (InterPolDist
                          (prob-F 0)
                          (prob-F first-non-zero-index)
                          (prob-F second-non-zero-index)
                          ((@TinF :tin-F) first-non-zero-index)
                          ((@TinF :tin-F) second-non-zero-index))]
            (swap! TinF assoc-in [:tin-F 0] tin-f-0)))

        (if (= ((@TinF :tin-F) 12) 0)
          (let [count (count non-zero-index)
                last-non-zero-index (nth non-zero-index count)
                second-last-non-zero-index (nth
                                             non-zero-index
                                             (- count 1))
                tin-f-12 (InterPolDist
                           (prob-F 12)
                           (prob-F last-non-zero-index)
                           (prob-F second-last-non-zero-index)
                           ((@TinF :tin-F) last-non-zero-index)
                           ((@TinF :tin-F)
                             second-last-non-zero-index))]
            (swap! TinF assoc-in [:tin-F 12] tin-f-12)))

        (map-indexed (fn [idx itm]
                       (let [i idx
                             j (- idx 1)
                             k (+ idx 1)]
                         (if (and (> idx 2) (< idx 10) (mod idx 2))
                           (if (= ((@TinF :tin-F) idx) 0)
                             (swap! TinF assoc-in [:tin-F idx]
                                    (InterPolDist
                                      (prob-F i)
                                      (prob-F j)
                                      (prob-F k)
                                      ((@TinF :tin-F) j)
                                      ((@TinF :tin-F) k))))))
                       ) (@TinF :tin-F))

        ; Fill in missing values by numerical integration
        (if (= ((@TinF :tin-F) 3) 0)
          (let [a1 (+ ((@TinF :tin-F) 2) ((@TinF :tin-F) 4))
                a (* 0.5625 a1)
                b1 (+ ((@TinF :tin-F) 0) ((@TinF :tin-F) 6))
                b (* 0.0625 b1)
                data (- a b)
                ] (swap! TinF assoc-in [:tin-F 3] data)))
        (if (= ((@TinF :tin-F) 5) 0)
          (let [a1 (+ ((@TinF :tin-F) 4) ((@TinF :tin-F) 6))
                a (* 0.5625 a1)
                b1 (+ ((@TinF :tin-F) 2) ((@TinF :tin-F) 8))
                b (* 0.0625 b1)
                data (- a b)
                ] (swap! TinF assoc-in [:tin-F 5] data)))
        (if (= ((@TinF :tin-F) 7) 0)
          (let [a1 (+ ((@TinF :tin-F) 6) ((@TinF :tin-F) 8))
                a (* 0.5625 a1)
                b1 (+ ((@TinF :tin-F) 4) ((@TinF :tin-F) 10))
                b (* 0.0625 b1)
                data (- a b)
                ] (swap! TinF assoc-in [:tin-F 7] data)))
        (if (= ((@TinF :tin-F) 9) 0)
          (let [a1 (+ ((@TinF :tin-F) 8) ((@TinF :tin-F) 10))
                a (* 0.5625 a1)
                b1 (+ ((@TinF :tin-F) 6) ((@TinF :tin-F) 12))
                b (* 0.0625 b1)
                data (- a b)
                ] (swap! TinF assoc-in [:tin-F 9] data)))
        (if (= ((@TinF :tin-F) 1) 0)
          (let [a1 (* 3.5 ((@TinF :tin-F) 2))
                b1 (* 0.5 ((@TinF :tin-F) 0))
                c (+ a1 b1)
                d (- c ((@TinF :tin-F) 3))
                data (/ d 3)
                ]
            (swap! TinF assoc-in [:tin-F 1] data)))

        (if (= ((@TinF :tin-F) 11) 0)
          (let [a1 (* 2.5 ((@TinF :tin-F) 10))
                b1 (* 0.5 ((@TinF :tin-F) 9))
                c (- a1 b1)
                d (+ ((@TinF :tin-F) 12) c)
                data (/ d 3)
                ] (swap! TinF assoc-in [:tin-F 11] data)))

        (let [m-TBP (let [m-tbp (->> (map-indexed (fn [idx itm]
                                                    (if (and (> idx 0)
                                                             (< idx 7))
                                                      (- ((@TinF :tin-F) (* 2 idx))
                                                         ((@TinF :tin-F) (- (* 2 idx)
                                                                            2)))))
                                                  (@TinF :tin-F))
                                     (into []))]
                      (assoc m-tbp 0 ((@TinF :tin-F) 6)))

              n-TBP (->> (map-indexed (fn [idx itm]
                                        (if (and (>= idx 0)
                                                 (< idx 8))
                                          (let [mi (m-TBP idx)
                                                ai (D86TBPA idx)
                                                bi (D86TBPB idx)
                                                a1 (js/Math.pow mi
                                                                bi)
                                                ni (* ai a1)
                                                new-mi (max mi ni)]
                                            (if (= idx 6)
                                              (let [b (* mi 1.6)
                                                    a (+ mi 50)]
                                                (min a b new-mi)
                                                )
                                              (max mi ni)
                                              )
                                            )
                                          )) (@TinF :tin-F))
                         (into []))
              TempTBP-data (let [temp-data #(let [temp-data @TempTBP
                                                  temp-tbp (get temp-data
                                                                :Temp-TBP)]
                                              (temp-tbp %1)
                                              )]

                             (swap! TempTBP assoc-in [:Temp-TBP 6] (n-TBP 0))
                             (swap! TempTBP assoc-in [:Temp-TBP 4]
                                    (- (temp-data
                                         6)
                                       (n-TBP 3)))
                             (swap! TempTBP assoc-in [:Temp-TBP 2]
                                    (- (temp-data
                                         4) (n-TBP 2)))
                             (swap! TempTBP assoc-in [:Temp-TBP 0]
                                    (- (temp-data
                                         2) (n-TBP 1)))
                             (swap! TempTBP assoc-in [:Temp-TBP 8]
                                    (+ (temp-data 6) (n-TBP
                                                       4)))
                             (swap! TempTBP assoc-in [:Temp-TBP 10]
                                    (+ (temp-data 8) (n-TBP
                                                       5)))
                             (swap! TempTBP assoc-in [:Temp-TBP 12]
                                    (+ (temp-data 10) (n-TBP 6))))

              data-Temp (->> (map-indexed (fn [idx itm]
                                            (if (and (> idx 0) (< idx 12) (odd? idx))
                                              (let [temp-tbp @TempTBP
                                                    temp-data (get temp-tbp
                                                                   :Temp-TBP)
                                                    tin-F @TinF
                                                    tin-f-data (tin-F :tin-F)
                                                    a1 (- (tin-f-data idx)
                                                          (tin-f-data (- idx 1)))
                                                    a2 (- (tin-f-data (+ idx 1))
                                                          (tin-f-data (- idx 1)))
                                                    a3 (- (temp-data (+ idx 1))
                                                          (temp-data (- idx 1)))
                                                    b1 (/ a1 a2)
                                                    b2 (* b1 a3)]
                                                (swap! TempTBP assoc-in [:Temp-TBP idx]
                                                       (+ b2 (temp-data (- idx 1)))
                                                       )
                                                )))
                                          (@TinF :tin-F))
                             (into []))

              sim-dist-m (-> (into [] (map-indexed (fn [idx itm]
                                                     (if (and (> idx 0)
                                                              (< idx 7))
                                                       (- ((@TempTBP :Temp-TBP)
                                                            (* 2 idx))
                                                          ((@TempTBP :Temp-TBP)
                                                            (- (* 2 idx) 2)))
                                                       0))
                                                   (@TempTBP :Temp-TBP)))
                             (assoc 0 ((@TempTBP :Temp-TBP) 6))
                             (assoc 1 (- ((@TempTBP :Temp-TBP) 2) ((@TempTBP :Temp-TBP) 1)))
                             (assoc 7 (- ((@TempTBP :Temp-TBP) 12) ((@TempTBP :Temp-TBP) 11)))
                             (assoc 6 (- ((@TempTBP :Temp-TBP) 11) ((@TempTBP :Temp-TBP) 10))))

              sim-dist-n (-> (into [] (map-indexed (fn [idx itm]
                                                     (if (and (> idx 0) (< idx 8))
                                                       (let [a1 (/ (sim-dist-m idx)
                                                                   (SDTBPA idx))
                                                             a2 (/ 1 (SDTBPB idx))
                                                             a (js/Math.pow a1 a2)]
                                                         (max (sim-dist-m idx) a))))
                                                   SDTBPA))
                             (assoc 0 (n-TBP 0)))

              TempSD-data (let
                            [temp-sd #(let [temp-sd @TempSD
                                            tempsd (get temp-sd
                                                        :Temp-SD)]
                                        (tempsd %1)
                                        )]

                            (swap! TempSD assoc-in [:Temp-SD 6] (sim-dist-n 0))

                            (swap! TempSD assoc-in [:Temp-SD 4]
                                   (- (temp-sd 6) (sim-dist-n 3)))
                            (swap! TempSD assoc-in [:Temp-SD 2]
                                   (- (temp-sd 4) (sim-dist-n 2)))
                            (swap! TempSD assoc-in [:Temp-SD 0]
                                   (- (temp-sd 2) (sim-dist-n 1)))
                            (swap! TempSD assoc-in [:Temp-SD 8]
                                   (+ (temp-sd 6) (sim-dist-n 4)))
                            (swap! TempSD assoc-in [:Temp-SD 10]
                                   (+ (temp-sd 8) (sim-dist-n 5)))
                            (swap! TempSD assoc-in [:Temp-SD 12]
                                   (+ (temp-sd 10) (sim-dist-n 6)))
                            )
              new-TempSD (let [temp-sd (map-indexed (fn [idx value]
                                                      (if (and (> idx 0)
                                                               (< idx 12)
                                                               (not= (mod idx 2) 0))
                                                        (let [temp-data @TempTBP
                                                              temp-tbp (get temp-data
                                                                            :Temp-TBP)
                                                              temp-sd @TempSD
                                                              tempsd (get temp-sd
                                                                          :Temp-SD)
                                                              idx+1 (+ idx 1)
                                                              idx-1 (- idx 1)
                                                              a1 (- (temp-tbp idx)
                                                                    (temp-tbp (- idx 1)))
                                                              a2 (- (temp-tbp (+ idx 1))
                                                                    (temp-tbp (- idx 1)))
                                                              a3 (- (tempsd (+ idx 1))
                                                                    (tempsd (- idx 1)))
                                                              b1 (/ a1 a2)
                                                              b2 (* b1 a3)
                                                              ]
                                                          (swap! TempSD assoc-in [:Temp-SD
                                                                                  idx]
                                                                 (+ b2 (tempsd (- idx 1))))
                                                          ))
                                                      )
                                                    (@TempTBP :Temp-TBP))
                               tempsd ((nth (into [] temp-sd) 11) :Temp-SD)
                               ]
                           (swap! TempSD assoc-in [:Temp-SD
                                                   1] (- (tempsd 2)
                                                         (sim-dist-n 1)))
                           (swap! TempSD assoc-in [:Temp-SD
                                                   0]
                                  (let [
                                        temp-data @TempTBP
                                        temp-tbp (get temp-data
                                                      :Temp-TBP)
                                        tempsd1 (- (tempsd 2)
                                                   (sim-dist-n 1))
                                        temp-tbp0 (temp-tbp 0)
                                        temp-tbp1 (temp-tbp 1)
                                        temp-tbp2 (temp-tbp 2)

                                        a1 (- temp-tbp0
                                              temp-tbp1)
                                        a2 (- temp-tbp2
                                              temp-tbp1)
                                        a3 (- (tempsd 2)
                                              tempsd1)
                                        b1 (/ a1 a2)
                                        b2 (* b1 a3)
                                        ]
                                    (+ b2 tempsd1)))
                           (swap! TempSD assoc-in [:Temp-SD
                                                   11]
                                  (+ ((@TempTBP :Temp-TBP) 10)
                                     (sim-dist-n 6)))

                           (swap! TempSD assoc-in [:Temp-SD
                                                   12]
                                  (+
                                    (+ ((@TempTBP :Temp-TBP) 10)
                                       (sim-dist-n 6))
                                    (sim-dist-n 7))))
              ]
          (if (= temp-unit "C")
            (map-indexed (fn [idx itm]
                           (let [a (- itm 32)
                                 b (/ 5 9)
                                 c (* a b)]
                             (swap! TempSD assoc-in [:Temp-SD
                                                     idx] c)
                             )
                           ) (@TempSD :Temp-SD)))
          (get @TempSD :Temp-SD))))))

(defn- SimDistToD86 [SimDist temp-unit]
  (let [D86TBPA [0.8718, 7.4012, 4.9004, 3.0305, 2.5282, 3.0419, 0.11798, 1]
        D86TBPB [1.0258, 0.60244, 0.71644, 0.80076, 0.82002, 0.75497, 1.6606, 1]
        D86TBPE [600, 100, 250, 250, 150, 100, 1000, 100000]
        SDTBPA [1, 0.15779, 0.011903, 0.05342, 0.19861, 0.31531, 0.97476, 0.02172]
        SDTBPB [1, 1.4296, 2.0253, 1.6988, 1.3975, 1.2938, 0.8723, 1.9733]
        SDTBPE [700, 40, 75, 75, 75, 75, 40, 30]
        vol-off [1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 99]
        TempTBP (atom {:Temp-TBP (vector 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)})
        TinF (atom {:tin-F (vector)})
        TempD86 (atom {:Temp-D86 (vector 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)})
        ; Calculate probability factors
        prob-F (mapv (fn [d]
                       (let [a-p (if (> d 50)
                                   (- 1 (/ d 100))
                                   (/ d 100))
                             e-p (js/Math.pow (js/Math.log
                                                (/ 1 (* a-p a-p))) 0.5)
                             a1 (* 0.27061 e-p)
                             a (+ 2.3075377 a1)
                             b1 (* 0.99229 e-p)
                             b2 (* 0.04481 e-p e-p)
                             b (+ 1 b1 b2)
                             prob-f (- e-p (/ a b))]
                         (if (< d 50)
                           (- prob-f)
                           prob-f))) vol-off)

        tin-F-atom (swap! TinF assoc :tin-F
                          (cond
                            (= temp-unit "C") (map (fn [d]
                                                     (let [a (/ 9 5)
                                                           b (* d a)
                                                           c (+ b 32)] c)
                                                     ) SimDist)
                            (= temp-unit "F") SimDist))

        ; Checking that the distillation has at least two entries
        non-zero-index (->> (map-indexed
                              (fn [idx itm]
                                (if-not (= itm 0)
                                  idx
                                  nil))
                              (@TinF :tin-F))
                            (remove nil?)
                            (into []))
        ]
    ;;Checked data
    (if (> (count non-zero-index) 2)
      ;	; Fill in missing major points by probability interpolation
      (do
        (if (= ((@TinF :tin-F) 0) 0)
          (let [first-non-zero-index (first non-zero-index)
                second-non-zero-index (second non-zero-index)
                tin-f-0 (InterPolDist
                          (prob-F 0)
                          (prob-F first-non-zero-index)
                          (prob-F second-non-zero-index)
                          ((@TinF :tin-F) first-non-zero-index)
                          ((@TinF :tin-F) second-non-zero-index))]
            (swap! TinF assoc-in [:tin-F 0] tin-f-0)))

        (if (= ((@TinF :tin-F) 12) 0)
          (let [count (count non-zero-index)
                last-non-zero-index (nth non-zero-index count)
                second-last-non-zero-index (nth
                                             non-zero-index
                                             (- count 1))
                tin-f-12 (InterPolDist
                           (prob-F 12)
                           (prob-F last-non-zero-index)
                           (prob-F second-last-non-zero-index)
                           ((@TinF :tin-F) last-non-zero-index)
                           ((@TinF :tin-F)
                             second-last-non-zero-index))]
            (swap! TinF assoc-in [:tin-F 12] tin-f-12)))

        (map-indexed (fn [idx itm]
                       (let [i idx
                             j (- idx 1)
                             k (+ idx 1)]
                         (if (and (> idx 2) (< idx 10) (mod idx 2))
                           (if (= ((@TinF :tin-F) idx) 0)
                             (swap! TinF assoc-in [:tin-F idx]
                                    (InterPolDist
                                      (prob-F i)
                                      (prob-F j)
                                      (prob-F k)
                                      ((@TinF :tin-F) j)
                                      ((@TinF :tin-F) k))))))
                       ) (@TinF :tin-F))

        ; Fill in missing values by numerical integration
        (if (= ((@TinF :tin-F) 3) 0)
          (let [a1 (+ ((@TinF :tin-F) 2) ((@TinF :tin-F) 4))
                a (* 0.5625 a1)
                b1 (+ ((@TinF :tin-F) 0) ((@TinF :tin-F) 6))
                b (* 0.0625 b1)
                data (- a b)
                ] (swap! TinF assoc-in [:tin-F 3] data)))
        (if (= ((@TinF :tin-F) 5) 0)
          (let [a1 (+ ((@TinF :tin-F) 4) ((@TinF :tin-F) 6))
                a (* 0.5625 a1)
                b1 (+ ((@TinF :tin-F) 2) ((@TinF :tin-F) 8))
                b (* 0.0625 b1)
                data (- a b)
                ] (swap! TinF assoc-in [:tin-F 5] data)))
        (if (= ((@TinF :tin-F) 7) 0)
          (let [a1 (+ ((@TinF :tin-F) 6) ((@TinF :tin-F) 8))
                a (* 0.5625 a1)
                b1 (+ ((@TinF :tin-F) 4) ((@TinF :tin-F) 10))
                b (* 0.0625 b1)
                data (- a b)
                ] (swap! TinF assoc-in [:tin-F 7] data)))
        (if (= ((@TinF :tin-F) 9) 0)
          (let [a1 (+ ((@TinF :tin-F) 8) ((@TinF :tin-F) 10))
                a (* 0.5625 a1)
                b1 (+ ((@TinF :tin-F) 6) ((@TinF :tin-F) 12))
                b (* 0.0625 b1)
                data (- a b)
                ] (swap! TinF assoc-in [:tin-F 9] data)))
        (if (= ((@TinF :tin-F) 1) 0)
          (let [a1 (* 3.5 ((@TinF :tin-F) 2))
                b1 (* 0.5 ((@TinF :tin-F) 0))
                c (+ a1 b1)
                d (- c ((@TinF :tin-F) 3))
                data (/ d 3)
                ]
            (swap! TinF assoc-in [:tin-F 1] data)))
        (if (= ((@TinF :tin-F) 11) 0)
          (let [a1 (* 2.5 ((@TinF :tin-F) 10))
                b1 (* 0.5 ((@TinF :tin-F) 9))
                c (- a1 b1)
                d (+ ((@TinF :tin-F) 12) c)
                data (/ d 3)
                ] (swap! TinF assoc-in [:tin-F 11] data)))

        (let [m-TBP (let [m-tbp (->> (map-indexed (fn [idx itm]
                                                    (if (and (> idx 0)
                                                             (< idx 7))
                                                      (- ((@TinF :tin-F) (* 2 idx))
                                                         ((@TinF :tin-F) (- (* 2 idx)
                                                                            2)))))
                                                  (@TinF :tin-F))
                                     (into []))]
                      (-> m-tbp
                          (assoc 0 ((@TinF :tin-F) 6))
                          (assoc 1 (- ((@TinF :tin-F) 2)
                                      ((@TinF :tin-F) 1)))
                          (assoc 7 (- ((@TinF :tin-F) 12)
                                      ((@TinF :tin-F) 11)))
                          (assoc 6 (- ((@TinF :tin-F) 11)
                                      ((@TinF :tin-F) 10)))))

              n-TBP (->> (map-indexed (fn [idx itm]
                                        (if (and (>= idx 0)
                                                 (< idx 8))
                                          (let [mi (m-TBP idx)
                                                ai (SDTBPA idx)
                                                bi (SDTBPB idx)
                                                a1 (js/Math.pow mi bi)
                                                ni (* ai a1)]
                                            (min mi ni))
                                          )) (@TinF :tin-F))
                         (into []))

              TempTBP-data (let [temp-data #(let [temp-data @TempTBP
                                                  temp-tbp (get temp-data
                                                                :Temp-TBP)]
                                              (temp-tbp %1)
                                              )]

                             (swap! TempTBP assoc-in [:Temp-TBP 6] (n-TBP 0))
                             (swap! TempTBP assoc-in [:Temp-TBP 4]
                                    (- (temp-data 6)
                                       (n-TBP 3)))
                             (swap! TempTBP assoc-in [:Temp-TBP 2]
                                    (- (temp-data 4) (n-TBP 2)))
                             (swap! TempTBP assoc-in [:Temp-TBP 0]
                                    (- (temp-data 2) (n-TBP 1)))
                             (swap! TempTBP assoc-in [:Temp-TBP 8]
                                    (+ (temp-data 6) (n-TBP 4)))
                             (swap! TempTBP assoc-in [:Temp-TBP 10]
                                    (+ (temp-data 8) (n-TBP 5)))
                             (swap! TempTBP assoc-in [:Temp-TBP 12]
                                    (+ (temp-data 10) (n-TBP 6))))

              data-Temp (do
                          (->> (map-indexed (fn [idx itm]
                                              (if (and (> idx 0) (< idx 12) (odd? idx))
                                                (let [temp-tbp @TempTBP
                                                      temp-data (get temp-tbp
                                                                     :Temp-TBP)
                                                      tin-F @TinF
                                                      tin-f-data (tin-F :tin-F)
                                                      a1 (- (tin-f-data idx)
                                                            (tin-f-data (- idx 1)))
                                                      a2 (- (tin-f-data (+ idx 1))
                                                            (tin-f-data (- idx 1)))
                                                      a3 (- (temp-data (+ idx 1))
                                                            (temp-data (- idx 1)))
                                                      b1 (/ a1 a2)
                                                      b2 (* b1 a3)]
                                                  (swap! TempTBP assoc-in [:Temp-TBP idx]
                                                         (+ b2 (temp-data (- idx 1)))
                                                         )
                                                  )))
                                            (@TinF :tin-F))
                               (into []))
                          (swap! TempTBP assoc-in [:Temp-TBP 1]
                                 (- ((@TempTBP :Temp-TBP) 2) (n-TBP 1)))
                          (swap! TempTBP assoc-in [:Temp-TBP 0]
                                 (let [a (- ((@TinF :tin-F) 0)
                                            ((@TinF :tin-F) 1))
                                       b (- ((@TinF :tin-F) 2)
                                            ((@TinF :tin-F) 1))
                                       c (- ((@TempTBP :Temp-TBP) 2)
                                            ((@TempTBP :Temp-TBP) 1))
                                       d (/ a b)
                                       e (* d c)]
                                   (+ e ((@TempTBP :Temp-TBP) 1))))
                          (swap! TempTBP assoc-in [:Temp-TBP 11]
                                 (+ ((@TempTBP :Temp-TBP) 10) (n-TBP 6)))
                          (swap! TempTBP assoc-in [:Temp-TBP 12]
                                 (+ ((@TempTBP :Temp-TBP) 11) (n-TBP 7))))

              ;'Convert from TBP to SimDist
              sim-dist-m (-> (into [] (map-indexed
                                        (fn [idx itm]
                                          (if (and (> idx 0) (< idx 7))
                                            (- ((@TempTBP :Temp-TBP) (* 2 idx))
                                               ((@TempTBP :Temp-TBP) (- (* 2 idx) 2)))
                                            0)) (@TinF :tin-F)))
                             (assoc 0 (m-TBP 0))
                             (assoc 7 (m-TBP 7)))
              sim-dist-n (into [] (map-indexed (fn [idx itm]
                                                 (if (and (> idx -1)
                                                          (< idx 8))
                                                   (let [a1 (/ (sim-dist-m idx)
                                                               (D86TBPA idx))
                                                         a2 (/ 1 (D86TBPB idx))
                                                         ni (js/Math.pow
                                                              a1 a2)
                                                         mi (sim-dist-m
                                                              idx)]
                                                     (if (= idx 6)
                                                       (max (- mi 50)
                                                            (/ mi 1.6)
                                                            ni))
                                                     (min mi ni))))
                                               SDTBPA))
              TempSD-data (do
                            (swap! TempD86 assoc-in [:Temp-D86 6]
                                   (sim-dist-n 0))
                            (swap! TempD86 assoc-in [:Temp-D86 4]
                                   (- ((@TempD86 :Temp-D86) 6) (sim-dist-n 3)))
                            (swap! TempD86 assoc-in [:Temp-D86 2]
                                   (- ((@TempD86 :Temp-D86) 4) (sim-dist-n 2)))
                            (swap! TempD86 assoc-in [:Temp-D86 0]
                                   (- ((@TempD86 :Temp-D86) 2) (sim-dist-n 1)))
                            (swap! TempD86 assoc-in [:Temp-D86 8]
                                   (+ ((@TempD86 :Temp-D86) 6) (sim-dist-n 4)))
                            (swap! TempD86 assoc-in [:Temp-D86 10]
                                   (+ ((@TempD86 :Temp-D86) 8) (sim-dist-n 5)))
                            (swap! TempD86 assoc-in [:Temp-D86 12]
                                   (+ ((@TempD86 :Temp-D86) 10) (sim-dist-n 6))))

              new-TempSD (let [temp-sd (map-indexed
                                         (fn [idx value]
                                           (if (and (> idx 0)
                                                    (< idx 12)
                                                    (odd? idx))
                                             (let [temp-data @TempTBP
                                                   temp-tbp (get temp-data
                                                                 :Temp-TBP)
                                                   a1 (- (temp-tbp idx)
                                                         (temp-tbp (- idx 1)))
                                                   a2 (- (temp-tbp (+ idx 1))
                                                         (temp-tbp (- idx 1)))
                                                   a3 (- ((@TempD86 :Temp-D86)
                                                           (+ idx 1))
                                                         ((@TempD86 :Temp-D86)
                                                           (- idx 1)))
                                                   b1 (/ a1 a2)
                                                   b2 (* b1 a3)
                                                   data (+ b2 ((@TempD86 :Temp-D86)
                                                                (- idx 1)))]
                                               (swap! TempD86
                                                      assoc-in
                                                      [:Temp-D86 idx]
                                                      data)

                                               ))) (@TempTBP :Temp-TBP))
                               tempsd ((nth (into [] temp-sd) 11) :Temp-SD)]
                           tempsd)]
          (get @TempD86 :Temp-D86))))))

(defn- check-Distillation [type data temp-type]
  (cond
    (= type "D86") (D86toSimDist data temp-type)
    (= type "SimDist") (SimDistToD86 (D86toSimDist data temp-type) temp-type)
    :else (SimDistToD86 data temp-type)
    ))