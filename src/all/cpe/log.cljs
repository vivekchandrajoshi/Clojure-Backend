(ns cpe.log
  (:require [cljs.nodejs :as nodejs]
            [cpe.util :as u]
            [cpe.config :refer [config]]))

(defonce log4js (nodejs/require "log4js"))

(defonce logger (atom nil))


(defn trace [& args]
  (if-let [logger @logger]
    (u/oapply logger :trace args)))

(defn debug [& args]
  (if-let [logger @logger]
    (u/oapply logger :debug args)))

(defn info [& args]
  (if-let [logger @logger]
    (u/oapply logger :info args)))

(defn warn [& args]
  (if-let [logger @logger]
    (u/oapply logger :warn args)))

(defn error [& args]
  (if-let [logger @logger]
    (u/oapply logger :error args)))

(defn fatal [& args]
  (if-let [logger @logger]
    (u/oapply logger :fatal args)))

(defn init []
  (try
    (doto log4js
      (u/ocall :configure (clj->js (:log @config))))
    (reset! logger (u/ocall log4js :getLogger))
    (info "Initialized loggings.")
    {:logger @logger}

    (catch js/Error e
      (println "failed to initialize logger" e)
      {:err e})))
