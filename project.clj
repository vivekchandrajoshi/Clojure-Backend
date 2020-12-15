(defproject cpe "1.0.4"
  :description "CPE (Catalyst Performance Calculations) backend app"
  :url ""
  :min-lein-version "2.8.1"

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.9.946"] ;1.9.908
                 [org.clojure/core.async  "0.3.443"
                  :exclusions [org.clojure/tools.reader]]
                 [com.andrewmcveigh/cljs-time "0.5.2"]]
  :plugins [[lein-figwheel "0.5.13"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [lein-doo "0.1.7"]]

  :aliases {"doo-once"  ["do"
                         ["clean"]
                         ["doo" "node" "node-test" "once"]]
            "doo-watch" ["do"
                         ["clean"]
                         ["doo" "node" "node-test" "auto"]]
            "build"     ["do"
                         ["clean"]
                         ["cljsbuild" "once" "prod"]]}

  :source-paths ["dev" "src/all"]

  :cljsbuild {:builds
              [{:id           "dev"
                :source-paths ["src/all" "src/dev"]
                :compiler     {:main                 cpe.core
                               :output-to            "main.js"
                               :output-dir           "target/out"
                               :source-map-timestamp true
                               :target               :nodejs}}

               ;; for test with node
               {:id           "node-test"
                :source-paths ["src/all" "src/test"]
                :compiler     {:main        cpe.runner
                               :output-to  "test.js"
                               :output-dir "target/test-out"
                               :target     :nodejs}}

               ;; This next build is an compressed minified build for
               ;; production. You can build this with:
               ;; lein cljsbuild once prod
               {:id           "prod"
                :source-paths ["src/all" "src/prod"]
                :compiler     {:output-to       "main.js"
                               :main             cpe.core
                               :optimizations   :advanced   ;; ensure externs for js interops
                               ;; :optimizations :simple ;; externs not needed
                               :pretty-print    false
                               :target          :nodejs
                               :closure-defines {goog.DEBUG false}
                               :externs         ["externs.js"]}}]}

  ;; setting up nREPL for Figwheel and ClojureScript dev
  ;; Please see:
  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
  :profiles {:dev {:dependencies  [[figwheel-sidecar "0.5.13"]
                                   [com.cemerick/piggieback "0.2.2"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths  ["dev" "src/all"]
                   :repl-options  {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   ;; need to add the compliled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["main.js"
                                                     "target"]}})