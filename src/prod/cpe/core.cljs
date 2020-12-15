(ns cpe.core
  (:require [cljs.nodejs :as nodejs]
            [cpe.boot :refer [init]]))

 ;; (nodejs/enable-util-print!)

(defn -main []
     (init))

(set! *main-cli-fn* -main)
