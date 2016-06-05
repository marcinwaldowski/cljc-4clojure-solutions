(ns user
  (:require [cemerick.piggieback :as piggieback]
            [weasel.repl.websocket :as weasel]
            [cljs.repl.node :as node]
            [cljs.repl.nashorn :as nashorn]
            [cljs.repl.rhino :as rhino]))

(defn start-weasel [& opts]
  (piggieback/cljs-repl (apply weasel/repl-env opts)))

(defn start-node []
  (piggieback/cljs-repl (node/repl-env)
                        :output-dir "resources/private/js/node"))

(defn start-nashorn []
  (piggieback/cljs-repl (nashorn/repl-env)
                        :output-dir "resources/private/js/none"))

(defn start-rhino []
  (piggieback/cljs-repl (rhino/repl-env)
                        :output-dir "resources/private/js/none"))
