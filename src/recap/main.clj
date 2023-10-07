(ns recap.main
  "Entry-point into the application."
  (:refer-clojure :exclude [defn])
  (:require [recap.cli :as cli]
            [utils.common :as c]
            [utils.specin :refer [defn]])
  (:gen-class))


(set! *warn-on-reflection* true) ; for graalvm


(defn -main
  "Entry-point into the application.

  Returns 0 on success, otherwise a positive integer."
  {:ret nat-int?}
  [& args]
  (-> (or args [])
      cli/parse
      cli/run-cmd
      c/exit!))

