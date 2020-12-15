(ns cpe.auth
  (:require [cljs.nodejs :as nodejs]
            [cljs.core.async :refer [<! put! promise-chan]]
            [cljs.reader :as r]
            [clojure.string :as str]
            [cpe.util :as u]
            [cpe.config :refer [config]]
            [cpe.log :as log]
            [cpe.info :as app-info])
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [cpe.macros :refer [defauth]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce request (nodejs/require "request"))

(defonce ^:private app-id "cpe")

(defonce ^:private state (atom {:claims {}}))

(defonce ^:private app-operations (reduce (fn [m {:keys [id] :as op}]
                                            (assoc m id op))
                                          {}
                                          app-info/operations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- in? [item coll]
  (if (and (coll? coll)
           (some #(= item %) coll))
    true
    false))

(defn- now [] (u/ocall (js/Date.) :valueOf))

(defn- not-expired
  "returns back the claims if not expired, else returns nil"
  [claims]
  (if (> (u/ocall (:exp claims) :valueOf) (now))
    claims))

(defn- cleanup-expired-claims [token-claims]
  (let [expired-ts (->> token-claims
                        (remove (comp not-expired val))
                        (map first))]
    (if (empty? expired-ts)
      token-claims ;; nothing to remove
      ;; remove expired tokens
      (apply dissoc token-claims expired-ts))))

(defn- cleanup []
  (log/info "cleaning up tokens cache..")
  (swap! state update :claims cleanup-expired-claims))

(defn init []
  (let [interval (* 60 60 1000)]
    (js/setInterval cleanup interval))

  (log/info "Initalized auth system..")
  {:err false})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; claims string format as received from portal                ;;;;
;;;; --------------------------------------------                ;;;;
;;;; {:id <user email>                                           ;;;;
;;;;  :name <user name>                                          ;;;;
;;;;  :isTopsoe <is topsoe user?>                                ;;;;
;;;;  :isEmailVerified <is email verfied?>                       ;;;;
;;;;  :isAdmin <is portal admin?>                                ;;;;
;;;;  :clientId <client id for external user>                    ;;;;
;;;;  :isClientAdmin <is client admin?>                          ;;;;
;;;;  :apps {<app id> {:features [<feature id>...]               ;;;;
;;;;                   :operations [<operation id>...]           ;;;;
;;;;                   :isAdmin <is app admin? developer>        ;;;;
;;;;                   :isOwner <is app owner? business owner>}} ;;;;
;;;;  :exp <expiry time>}                                        ;;;;
;;;;                                                             ;;;;
;;;; claims map after parsing                                    ;;;;
;;;; ------------------------                                    ;;;;
;;;; {:id <user email>                                           ;;;;
;;;;  :name            <user name>                               ;;;;
;;;;  :topsoe?         <is topsoe user?>                         ;;;;
;;;;  :email-verified? <is email verified?>                      ;;;;
;;;;  :admin?          <is portal admin?>                        ;;;;
;;;;  :client-id       <client id for external user>             ;;;;
;;;;  :client-admin?   <is client admin?>                        ;;;;
;;;;  :features        [<feature id keyword>...]                 ;;;;
;;;;  :operations      [<operation id keyword>...]               ;;;;
;;;;  :app-admin?      <is app admin?>                           ;;;;
;;;;  :app-owner?      <is app owner?>                           ;;;;
;;;;  :exp             <expiry time>}                            ;;;;
;;;;                                                             ;;;;
;;;; NOTE: false or nil values are omitted for brevity           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-claims [claims-edn]
  (if (and claims-edn
           (string? claims-edn)
           (not-empty claims-edn))
    (let [c   (r/read-string claims-edn)
          app (get-in c [:apps app-id])
          fs  (not-empty (mapv keyword (:features app)))
          ops (not-empty (mapv keyword (:operations app)))]
      (cond-> {:id        (:id c)
               :name      (:name c)
               :client-id (:clientId c)
               :exp       (js/Date. (:exp c))}
        (:isTopsoe c)        (assoc :topsoe? true)
        (:isEmailVerified c) (assoc :email-verified? true)
        (:isAdmin c)         (assoc :admin? true)
        (:isClientAdmin c)   (assoc :client-admin? true)
        fs                   (assoc :features fs)
        ops                  (assoc :operations ops)
        (:isAdmin app)       (assoc :app-admin? true)
        (:isOwner app)       (assoc :app-owner? true)))))

(defn- get-claims [token]
  (let [ret-chan (promise-chan)]
    (if-not token
      ;; token not found in request
      (put! ret-chan {:claims nil})
      (if-let [claims (some-> (get-in @state [:claims token])
                              not-expired)]
        ;; found in cache
        (put! ret-chan {:claims claims})
        ;; not in cache, ask portal service
        (u/ocall request :get
                 #js{:url (str (:portal-uri @config) "/auth/token/check")
                     :headers #js {"Accept" "application/edn"
                                   "Authorization" token}
                     :strictSSL true}
                 (fn [err res body]
                   (if (or err (not= 200 (u/oget res :statusCode)))
                     (do
                       (log/debug "couldn't get claims for token!" err)
                       (put! ret-chan {:claims nil}))
                     (let [claims (parse-claims body)]
                       (swap! state assoc-in [:claims token] claims)
                       (put! ret-chan {:claims claims})))))))
    ret-chan))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; basic feature and operation check helpers

(defn- allow-feature?
  "check if given feature (keyword) is allowed for given claims"
  [claims feature]
  (and claims
       (or (:topsoe? claims) ;; featuer restriction not applicable to Topsoe users
           (in? feature (:features claims)))))

(defn- allow-operation?
  "check if given operation (keyword) is allowed for given claims"
  [claims operation]
  (and claims
       (or ((some-fn :admin? :app-admin? :app-owner?) claims)
           (and (:client-admin? claims)
                (not (get-in app-operations [operation :internal?])))
           (in? operation (:operations claims)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; authentication handlers

(defn authenticate
  "Checks if the request has valid authorization header. On successful
  authentication pass on call to next handler by calling next(). It also inserts
  the claims map at req.claims for use by next handler."
  [req res next]
  (go
    (if-let [claims (some-> (u/oget-in req [:headers :authorization])
                            str/trim
                            get-claims
                            <!
                            :claims)]
      (do
        (u/oset req :claims claims)
        (next))
      (u/res-bad res
                 "Unauthorized! Missing or invalid security token."
                 nil
                 401))))



;; access rules to authorize api calls

(defauth self? [claims {:params [userId]}]
  (= (:id claims) userId)
  "Different user.")

(defauth topsoe? [claims]
  (and (:topsoe? claims)
       (allow-operation? claims :view))
  "Not an internal user with view rights.")

(defauth configurator? [claims]
  (and (:topsoe? claims)
       (allow-operation? claims :configurePlant))
  "Not an internal user with configurator rights.")

(defauth view? [claims]
              (allow-operation? claims :view)
         "Not an view rights.")

(defauth pinChart? [claims]
              (allow-operation? claims :pinChart)
         "Not an add and modify Charts rights.")

(defauth uom? [claims]
         (allow-operation? claims :modifyUOMSettings)
         "Not an modify uom rights.")

(defauth upload? [claims]
         (allow-operation? claims :upload)
         "Not an upload rights.")


(defauth export? [claims]
         (allow-operation? claims :export)
         "Not an export rights.")


(defn can-access-client? [claims client-id]
  (and (or (:topsoe? claims)
           (= (:client-id claims) client-id))
       (allow-feature? claims :standard)))

(defauth access-client? [claims {:params [clientId]}]
  (and (can-access-client? claims clientId)
       (allow-operation? claims :view))
  "Access to another client prohibited.")
