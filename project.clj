(defproject cljc-4clojure-solutions "0.1.0-SNAPSHOT"
  :description "4Clojure solutions"
  :url "https://github.com/marcinwaldowski/cljc-4clojure-solutions"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  ;; Required support for reader conditional files (cljc)
  :min-lein-version "2.5.2"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.36"]]

  :plugins [[lein-cljsbuild "1.1.3"]
            [lein-doo "0.1.6"]]

  :clean-targets ^{:protect false} [:target-path "resources/private/js/" "nashorn_code_cache" "out"]

  :cljsbuild
  {:builds
   {:none {:source-paths ["src" "test"]
           :compiler {:main ".test-run"
                      :output-to     "resources/private/js/none/testable.js"
                      :output-dir    "resources/private/js/none"
                      :optimizations :none
                      :pretty-print  true}}
    :whitespace {:source-paths ["src" "test"]
                 :compiler {:main "cljc-4clojure-solutions.test-run"
                            :output-to     "resources/private/js/whitespace/testable.js"
                            :output-dir    "resources/private/js/whitespace"
                            :optimizations :whitespace
                            :pretty-print  true}}
    :simple {:source-paths ["src" "test"]
             :compiler {:main "cljc-4clojure-solutions.test-run"
                        :output-to     "resources/private/js/simple/testable.js"
                        :output-dir    "resources/private/js/simple"
                        :optimizations :simple
                        :pretty-print  true}}
    :advanced {:source-paths ["src" "test"]
               :compiler {:main "cljc-4clojure-solutions.test-run"
                          :output-to     "resources/private/js/advanced/testable.js"
                          :output-dir    "resources/private/js/advanced"
                          :optimizations :advanced
                          :pretty-print  true}}
    :node {:source-paths ["src" "test"]
           :compiler {:main "cljc-4clojure-solutions.test-run"
                      :output-to     "resources/private/js/node/testable.js"
                      :output-dir    "resources/private/js/node"
                      :pretty-print true
                      :target :nodejs}}
    :brepl {:source-paths ["dev" "src" "test"]
            :compiler {:main "brepl.connect"
                       :output-to     "resources/private/js/brepl/testable.js"
                       :output-dir    "resources/private/js/brepl"
                       :asset-path    "js/brepl"
                       :optimizations :none
                       :pretty-print  true}}}}

  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[doo "0.1.6"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [weasel "0.7.0" :exclusions [org.clojure/clojurescript]]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}})
