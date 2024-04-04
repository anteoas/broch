#!/usr/bin/env bb

(ns deploy
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [babashka.tasks :as tasks])
  (:import (java.io PushbackReader)))

(def opts (edn/read (PushbackReader. (io/reader ".deploy-opts.edn"))))

(println "Running with opts:")
(prn opts)

(tasks/shell "clj" "-T:build" "clean")
(tasks/shell "clj" "-T:build" "jar")
(let [{:keys [username clojars-token]} opts]
  (tasks/shell "env" (str "CLOJARS_USERNAME=" username) (str "CLOJARS_PASSWORD=" clojars-token)
               "clj" "-X:deploy"))
