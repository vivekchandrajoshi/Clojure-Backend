(ns cpe.config
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [<! put! chan promise-chan]]
            [clojure.string :as str]
            [cpe.util :as u]))

(defonce config (atom nil))

(defn merge-deep [m1 m2]
  (reduce-kv
   (fn [m k v]
     (assoc m k
            (let [v1 (get m k)]
              (if (and (map? v1) (map? v))
                (merge-deep v1 v)
                v))))
   m1 m2))

(defn parse-int [^:String num]
  (let [n (js/parseInt num)]
    (if-not (js/isNaN n) n)))

(defn parse-bool [^:String chk]
  (cond
    (nil? chk) nil
    (re-matches #"(?i)true" chk) true
    (re-matches #"(?i)false" chk) false
    :default nil))

(defn parse-log-level [^:String level]
  (some->> level
          str/trim
          not-empty
          (re-matches #"debug|info|warn|error|fatal")))

(defn parse-env-value [parse-key value]
  (case parse-key
    :int (parse-int value)
    :bool (parse-bool value)
    :log-level (parse-log-level value)
    ;; unknown parse-key, just pass the value as is
    value))

(defn override-config-by-env [config]
  ;; augment the config data with env overrides
  (as-> config config-data
    ;; override by pre-defined envionment variables
    (reduce-kv
     (fn [m k ind]
       (if-let [v (as-> (str "topsoe_" k) $
                    (u/oget-in js/process [:env $])
                    (or $ "")
                    (str/trim $)
                    (not-empty $))]
         (assoc-in m ind
                   (if-let [parse-key (:parse (meta ind))]
                     (parse-env-value parse-key v)
                     v))
         m))
     config-data
     ;; obtain the env vars override map from config
     (:env-vars config-data))

    ;; override if MSI is enabled/disabled?
    (let [disable? (parse-bool
                    (u/oget-in js/process [:env "WEBSITE_DISABLE_MSI"]))]
      (if (nil? disable?) config-data
          (assoc-in config-data [:msi :enable?] (not disable?))))

    ;; add MSI info from hosting environment
    (update config-data :msi assoc
            :endpoint (u/oget-in js/process [:env "MSI_ENDPOINT"])
            :secret (u/oget-in js/process [:env "MSI_SECRET"]))))

(defn init []
  (let [ret-chan (promise-chan)
        common-file "config/common.edn"
        config-file (str "config/"
                         (or (not-empty
                              (u/oget-in js/process
                                         [:env "topsoe_config_file_name"]))
                             "local-dev.edn"))]
    (go
      ;; load common config data
      (let [{:keys [err data]} (<! (u/read-edn-file common-file))]
        (if err
          (put! ret-chan {:err err})

          ;; load specific config data
          (let [config-data data
                {:keys [err data]} (<! (u/read-edn-file config-file))]
            (if err
              (put! ret-chan {:err err})

              ;; override common with specifics and env settings
              (let [config-data (-> (merge-deep config-data data)
                                    (override-config-by-env))

                    ;; some more setup
                    {:keys [workspace-path]} config-data
                    config-data (update-in config-data
                                           [:log :appenders :file :filename]
                                           (fn [file-name]
                                             (str workspace-path file-name)))]
                (reset! config config-data)
                (put! ret-chan {:config config-data})))))))
    ret-chan))
