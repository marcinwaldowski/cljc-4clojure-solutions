(ns cljc-4clojure-solutions.test-run
  (:require [doo.runner :refer-macros [doo-tests]]
            [cljc-4clojure-solutions.core-test]))

(doo-tests 'cljc-4clojure-solutions.core-test)
