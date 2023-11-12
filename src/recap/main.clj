(ns recap.main
  "Entry-point into the application."
  (:refer-clojure :exclude [defn])
  (:require [recap.cli :as cli]
            [recap.utils.common :as u]
            [recap.utils.specin :refer [defn]])
  (:gen-class))


(set! *warn-on-reflection* true) ; for graalvm


(defn -main
  "Entry-point into the application.

  Returns 0 on success, otherwise a positive integer."
  {:ret nat-int?}
  [& args]
  (-> (or args [])
      cli/execute
      u/exit!))
