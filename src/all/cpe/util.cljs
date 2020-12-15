(ns cpe.util
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [<! put! chan promise-chan]]
            [clojure.string :as str]
            [cljs.reader :as r]
            [goog.object :as g]
            [goog.date :as gdate]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON parse & stringify   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn json-str [o]
  (.call (g/get js/JSON "stringify") js/JSON o))

(defn json-parse [s]
  (if (string? s)
    (.call (g/get js/JSON "parse") js/JSON s)))


;;;;;;;;;;;;;;;;;;;;;
;; object handling ;;
;;;;;;;;;;;;;;;;;;;;;

(defn oget [o k]
  (g/get o (name k)))

(defn oset [o k v]
  (g/set o (name k) v))

(defn oget-in [o ks]
  (let [ks (map name ks)]
    (g/getValueByKeys o (to-array ks))))

(defn oset-in [o ks v]
  (let [ks (map name ks)
        k (last ks)
        ks (not-empty (butlast ks))
        o (if-not ks o (g/getValueByKeys o (to-array ks)))]
    (if (object? o)
      (g/set o k v)
      (throw (js/Error. (if ks
                          (str "not an object at ." (str/join "." ks))
                          "not an object" ))))))

(defn oset-in+ [o ks v]
  (if-not (object? o)
    (js/Error. "not an object"))
  (loop [[k & ks] (map name ks)
         o o]
    (if (empty? ks)
      (g/set o k v)
      (let [oo (g/get o k)
            oo (if (object? oo) oo (js-obj))]
        (g/set o k oo)
        (recur ks oo)))))

(defn ocall [o f & args]
  (.apply (oget o f) o (to-array args)))

(defn oapply [o f & args]
  (let [args (concat (butlast args) (last args))]
    (.apply (oget o f) o (to-array args))))


;;;;;;;;;;;;;;;;;;
;; file system  ;;
;;;;;;;;;;;;;;;;;;

(defonce fs (nodejs/require "fs"))

(defn read-file [file-path]
  (let [c (promise-chan)]
    (ocall fs :readFile file-path
           (fn [err data]
             (if err
               (put! c {:err err})
               (put! c {:data (str data)}))))
    c))

(defn read-edn-file [file-path]
  (let [c (promise-chan)]
    (go
      (let [{:keys [err data]} (<! (read-file file-path))]
        (if err
          (put! c {:err err})
          (try
            (let [d (r/read-string data)]
              (put! c {:data d}))
            (catch js/Error e (put! c {:err e}))))))
    c))

;;;;;;;;;;;;;;;;;;;;
;; custom parsers ;;
;;;;;;;;;;;;;;;;;;;;

(defn parse-date
  "parse date. returns nil if not valid."
  [txt]
  (if-let [txt (if (string? txt) (not-empty txt))]
    (let [d (js/Date. txt)]
      (if (pos? (ocall d :valueOf)) d))))

(defn parse-int
  "parse int. returns nil if not valid."
  [txt]
  (if-let [txt (if (string? txt) (not-empty txt))]
    (let [i (js/parseInt txt)]
      (if-not (js/isNaN i) i))))

(defn parse-bool
  "parse bool. returns nil if not valid."
  [txt]
  (if-let [txt (if (string? txt) (not-empty txt))]
    (case txt
      "true" true
      "false" false
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;
;; custom formatters ;;
;;;;;;;;;;;;;;;;;;;;;;;


(defn format-date [date]
  (if date
    (-> date (gdate/DateTime.) (.toUTCRfc3339String))))


;;;;;;;;;;;;;;;;;;;;
;; schema fields  ;;
;;;;;;;;;;;;;;;;;;;;

(defn date-field [field-name]
  {:name field-name
   :overrides {:api {:parse parse-date
                     :unparse format-date}}})

(def id-field {:name "id"
               :overrides {:db {:name "_id"}}})


;;;;;;;;;;;;;;;;;;;;;
;; respond helpers ;;
;;;;;;;;;;;;;;;;;;;;;

(defn res-ok
  ([res]
   (res-ok res nil))
  ([res result]
   (doto res
     (ocall :status 200)
     (ocall :json (clj->js {:success true
                            :result result})))))

(defn res-bad
  ([res]
   (res-bad res nil nil nil))
  ([res message]
   (res-bad res message nil nil))
  ([res message error-key]
   (res-bad res message error-key nil))
  ([res message error-key status]
   (let [message (or message "Failed!")
         error   (if error-key (name error-key))
         status  (or status 500)]
     (doto res
       (ocall :status status)
       (ocall :json #js{:success false
                        :message message
                        :error   error})))))
