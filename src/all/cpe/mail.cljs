(ns cpe.mail
  (:require [cljs.nodejs :as nodejs]
            [goog.object :as g]
            [cpe.util :as u]
            [cpe.log :as log]
            [cpe.config :refer [config]]))

(defonce request (nodejs/require "request"))
(defonce nodemailer (nodejs/require "nodemailer"))

(defonce state (atom {:transporter nil}))

(defn mail-template [user-name to comment-type plant-name subject comment ]

  #_(str  "<p>A new &nbsp;" comment-type "&nbsp; has made on &nbsp;"
        plant-name "&nbsp; by &nbsp;"user-name ".</br>
           Comment:- &nbsp;" comment "</br> For more info visit.</p>"
        "<a href = '"(str (get @config :portal-uri) "/apps/cpe'")">"
        (str (get @config :portal-uri) "/apps/cpe") "</a>" )

  {:from    "no-reply@topsoe.com"
   :to      (clojure.string/join "," to)
   :subject  (str "Catalyst Performance Evaluation: Comment - on "
            (if (> (count subject) 80)
              (str (first (re-seq #".{1,80}" subject)) " ...")
              subject))
   :html (str  "A new comment has been added on the plant "
           plant-name ".</br>
           Comment:- &nbsp;" comment "</br> For more info visit: "
           "<a href = '"(str (get @config :portal-uri) "/apps/cpe'")">"
           (str (get @config :portal-uri) "/apps/cpe") "</a>" )})

  (defn mail-callback [mail-res]
    (log/info "mail-response" (clj->js mail-res)))

(defn send-mail [opt]
  (let [{:keys [transporter]} @state]
    (log/info  "mail-opt" (clj->js opt) )
    (if (opt :to)
      (u/ocall transporter :sendMail (clj->js opt)
        (fn [err info]
          (mail-callback
            (if err
              {:error err}
              {:message-id  (u/oget info :messageId)
               :response(u/oget info :response)})))) )))

(defn init []
  (let [transporter (u/ocall nodemailer :createTransport
                             #js{:host (get-in @config [:email :host])
                                 :port 25})]
    (swap! state assoc :transporter transporter)
    {:success true}))