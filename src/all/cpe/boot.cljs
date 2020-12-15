(ns cpe.boot
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [<! put! chan promise-chan]]
            [cpe.util :as u]
            [cpe.config :as cfg]
            [cpe.log :as log]
            [cpe.mongodb :as db]
            [cpe.sqldb :as sqldb]
            [cpe.auth :as auth]
            [cpe.app :as app]
            [cpe.vault :as vault]
            [cpe.mail :as mail]
            [cpe.calculations.scheduler :as cs]
            [cpe.calculations.mongo :as cm])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn init []
  (go
    (and ;; stop on first failure
     ;; initializing config
     (-> (<! (cfg/init)) :err not)

     ;; initialize logging
     (-> (log/init) :err not)

      ;; initialize mail
      (-> (mail/init) :err not)

     ;; initialize with vault secrets
     (-> (<! (vault/init)) :err not)

     ;; initialize database connection
     (-> (<! (db/init)) :err not)

     ;; initialize sql database connection
    (-> (<! (sqldb/init)) :err not)

     ;; initalize auth
     (-> (auth/init) :err not)

     ;; initialize database connection
     (-> (<! (cm/init)) :err not)

     ;; scheduled-task
     (-> (cs/scheduled-task) :err not)

     ;; finally - app
     (let [{:keys [err app]} (app/init)]
       (when-not err
         (log/info "App initialization complete.")
         (let [port (or (u/oget-in js/process [:env :port]) 8000)]
           (u/ocall app :listen port)
           (log/info "server started on" port)))))))
