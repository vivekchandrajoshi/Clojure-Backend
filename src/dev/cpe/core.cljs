(ns cpe.core
  (:require [cljs.nodejs :as nodejs]
            [figwheel.client :as fw]
            [cpe.boot :refer [init]]))

(nodejs/enable-util-print!)

(defn -main []
  (init)
  (fw/start {}))

(set! *main-cli-fn* -main)
