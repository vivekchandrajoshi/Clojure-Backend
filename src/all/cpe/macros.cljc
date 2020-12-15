(ns cpe.macros)

(defn- wrap-fn [f]
  (list 'fn '[& args]
        (list 'apply f 'args)))

(defn- wrap-route-fns [routes]
  (mapv (fn [[r & rs]]
          (if (keyword? r)
            (into [r] (mapv wrap-fn rs))
            [r (let [rs (first rs)]
                 (if (symbol? rs) rs (wrap-route-fns rs)))]))
        routes))


(defmacro enable-hot-reload [routes]
  (wrap-route-fns routes))


(defn- param-binding-pairs [req params lookup-fn]
  (mapcat (fn [p]
            (let [k (keyword (name p))]
              (list p (list lookup-fn req [:params k]))))
          params))

;; defauth is meant for nodejs platform, not compatible with clj
(defmacro defauth
  "define authentication middleware for use with nodejs express router."
  [rule [claims {:keys [params]}]
   check-exp message-str]
  (let [req 'req, res 'res, next 'next, allowed 'allowed, message 'message
        req-lookup-fn 'cpe.util/oget
        param-lookup-fn 'cpe.util/oget-in
        res-bad-fn 'cpe.util/res-bad]
    `(defn ~rule [~req ~res ~next]
       (let [~claims  (~req-lookup-fn ~req :claims)
             ~@(param-binding-pairs req params param-lookup-fn)
             ~allowed ~check-exp
             ~message (str "Unauthorized! " ~message-str)]
         (if ~allowed
           (~next)
           (~res-bad-fn ~res ~message nil 401))))))
