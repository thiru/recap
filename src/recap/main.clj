(ns recap.main
  "Entry-point into the application."
  (:require [clojure.spec.alpha :as s]
            [recap.cli :as cli]
            [utils.common :as c])
  (:gen-class))

(set! *warn-on-reflection* true) ; for graalvm

(s/fdef -main
        :args (s/cat :args (s/coll-of string?))
        :ret (s/and int? c/non-neg?))

(defn -main
  "Entry-point into the application.

  Returns 0 on success, otherwise a positive integer."
  [& args]
  (-> args cli/parse cli/run-cmd c/exit!))

