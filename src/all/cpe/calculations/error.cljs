(ns cpe.calculations.error
  (:require [cpe.log :as log]
            [cpe.util :as u]
            [clojure.string :as string]
            [clojure.core.async.impl.channels  :as m]))

(defn isNan [number]
  (js/Number.isNaN number))

(defn isFinite [number]
  (js/Number.isFinite number))

(defn isUndefined [number]
  (undefined? number))

(defn isNil [number]
  (nil? number))

(defn isString [number]
 (if (or (= number "F")  (= number "C") (= number "SimDist") (= number "D86"))
   true
   false))

(defn check-valid-number [number]
  (if (or (and (not (isNan number) ) (isFinite number) (not (isUndefined number)) (not (isNil number))) (isString number))
    number
    nil))

(defn fn-name [f]
  (last (u/oget (string/split f  #"\$+") :tail)))

(defn try-catch [f & params]
  (log/trace "Calculation Name :" (fn-name (u/oget f :name)))
  (let [chack-params (try (apply check-valid-number params)
                          (catch js/Error e (log/error e)))
        calc-output (try (apply f params)
                         (catch js/Error e (log/error e)))
        check-valid (if chack-params
                      (check-valid-number calc-output))]
    ; (print "chack-params" chack-params)
    (if chack-params
      (if check-valid
        (do
          (log/trace "Calculation done  params :"(u/oget params :arr) "Calculation output :" calc-output)
          check-valid)
        (do
          (log/error "Calculation failed params :" (u/oget params :arr) "Calculation output :" calc-output)
          check-valid)))))