(ns cpe.app
  (:require-macros [cpe.macros :refer [enable-hot-reload]])
  (:require [cljs.nodejs :as nodejs]
            [cpe.util :as u]
            [cpe.config :refer [config]]
            [cpe.log :as log]
            [cpe.auth :as a]
            [cpe.handler :as h]
            ))

;; express app
(defonce express (nodejs/require "express"))
(defonce body-parser (nodejs/require "body-parser"))
(defonce app (express))

;; define routes and handlers along with any pre-handlers.
;; see the function create-router for schema of routes specification.
(defonce ^:private routes
         (enable-hot-reload
           [["/section"
             [[:get a/view?  h/get-sections]
              ["/:sectionId" [[:get a/view?  h/get-sections-by-id]]]]]

            ["/sensor"
             [[:get a/view?  h/get-sensor-by-name]
              ["/values"  [[:get a/view?  h/get-sensor-value]]]
              ["/export-excel"  [[:get a/view?  h/export-excel-data]]]]]

            ["/chart"
             [ [:get a/view?  h/get-charts]
              ["/:chartId" [[:get  a/view?  h/get-chart-by-id]]]]]

            ["/user"
             [[:post h/create-user]
              ["/:userId" [[:get  a/self? h/get-user]
                           [:put  a/self? h/update-user]]]]]

            ["/uom"
             [[:get a/view? h/get-uom]]]

            ["/unitSystem"
             [[:get a/view?  h/get-unit-system]
              ["/:name"  [[:get a/view? h/find-unit-system-by-name]]]]]

            ["/misc"
             [[:get a/view?  h/get-misc]
              [:post a/view?  h/create-document]
              ["/:documentName" [[:get a/view?  h/get-document-by-name]
                                 [:put a/view?  h/update-document-by-name]]]]]

            ["/plant"
               [["/:plantId"
                 [["/summary"
                   [[:get a/view? h/get-report-summary]
                    [:post a/view? h/create-report-summary]
                    ["/:summaryId"
                     [[:get a/view? h/get-report-summary-by-id]
                      [:put a/view? h/update-report-summary]
                      [:delete a/view? h/delete-report-summary]
                      ["/publish"
                       [[:put a/view? h/publish-report-summary]]]]]]]
                  ["/comment"
                   [[:get a/view?  h/get-comment-by-plant-id]
                    [:post  a/view?  h/create-comment-by-plant-id]
                    ["/withReply"
                     [[:get a/view? h/get-comments-with-reply]]]
                    ["/includeInReportMulti"
                     [[:put a/topsoe? h/update-include-in-report-multi]]]
                    ["/:commentId" [[:get  a/view?  h/get-comment-by-comment-id]

                                    ["/reply" [[:get  a/view?  h/get-reply-by-comment-id]
                                               ["/commentTime" [[:get a/view?  h/get-reply-by-comment-time]]]
                                               ]]
                                    ["/reply" [[:put a/view? h/update-reply]]]
                                    ["/read" [[:put a/view? h/update-read]]]
                                    ["/includeInReport" [[:put a/topsoe? h/update-include-in-report]]]
                                    ["/modify" [[:put a/view? h/update-comment]]]]]]]]]]]
            ["/client"
             [[:post a/configurator? h/create-client]
              [:get  a/topsoe? h/find-clients]
              ["/search-options"
               [[:get a/topsoe? h/get-client-search-options]]]
              ["/:clientId"
               [[:get a/access-client? h/get-client]
                [:put  a/configurator? h/update-client]
                ["/plant"
                 [[:post a/configurator? h/create-plant]
                  [:get a/topsoe? h/find-plants]
                  ["/:plantId"
                   [[:get a/access-client? h/get-plant]
                    ["/settings"
                     [[:put a/uom? h/update-plant-settings]]]
                    ["/config"
                     [[:put a/configurator? h/update-plant-config]]]]]]]]]]]]))

(defn- create-router
  "Create an Express Router object based on the url-method-handler data.
  *routes* data is a vector of route declarations.
     Each route declaration is a vector like one of following two kinds:
       [<method keyword> handler1 handler2 handler3 ...]
       [<url string> <sub-routes>]
     <method keyword> can be one of :get, :post, :delete, etc.
     <url string> is a string specifying part of url.
     <sub-routes> is defined in the same way as routes.
  Optionally you can provide a list of handlers to be injected before
  handler to do common task like authentication, etc."

  ([routes] (create-router routes nil))
  ([routes pre-handlers]
   (let [router (u/ocall express :Router #js{:mergeParams true})]
     (doseq [[r & rs] routes]
       (if (keyword? r)
         (u/oapply router r "/" (concat pre-handlers rs))
         (u/ocall router :use r (create-router (first rs) pre-handlers))))
     router)))

(defn- download-log [req res]
  (let [i (u/oget-in req [:params :logIndex])
        f (str (get-in @config [:log :appenders :file :filename])
               (if (= i "0") "" (str "." i)))]
    (u/ocall res :setHeader "Content-disposition"
             (str "attachment; filename=log." i))
    (u/ocall res :setHeader "Content-type" "text/plain")
    (u/ocall res :download f)))

(defn- test-api [req res]
  (log/info "test api pinged.")
  (u/ocall res :send "test api ping success"))

(defn- require-https [req res next]
  (if (and
        ;; hosted in azure
        (u/ocall req :get "x-site-deployment-id")
        ;; https related attribute missing in header
        (not (u/ocall req :get "x-arr-ssl")))
    ;; redirect to use HTTPS
    (u/ocall res :redirect (str "https://"
                                (u/ocall req :get "host")
                                (u/oget req :url)))
    ;; already HTTPS, proceed..
    (next)))

(defn- cors [req res next]
  (doseq [[h v] {"Access-Control-Allow-Origin"
                 "*",
                 "Access-Control-Allow-Methods"
                 "GET, PUT, POST, DELETE",
                 "Access-Control-Allow-Headers",
                 "Origin, X-Requested-With, Content-Type, Accept, Authorization"}]
    (u/ocall res :header h v))
  (next))


(defn init []
  (try
    (doto app
      (u/ocall :use require-https)
      (u/ocall :use cors)
      (u/ocall :use (u/ocall body-parser :urlencoded #js{:extended true}))
      (u/ocall :use (u/ocall body-parser :json #js{:limit "1mb"}))

      ;; add routes
      ;(u/ocall :use "/"
      ;         (create-router sf-routes [#(a/authenticate %1 %2 %3)]))
      (u/ocall :use "/api"
               (create-router routes [#(a/authenticate %1 %2 %3)]))
      (u/ocall :get "/test" test-api))
    (log/info "Initialized routing.")

    ;; return the app
    {:app app}

    (catch js/Error e
      (log/error "failed to initialize app router!" e)
      {:err e})))
