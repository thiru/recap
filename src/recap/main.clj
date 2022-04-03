(ns recap.main
  "Entry-point into the application."
  (:require [clojure.spec.alpha :as s]
            [recap.cli :as cli]
            [utils.common :as u])
  (:gen-class))

(set! *warn-on-reflection* true) ; for graalvm

(s/fdef -main
        :args (s/cat :args (s/coll-of string?))
        :ret (s/and int? u/non-neg?))

(defn -main
  "Entry-point into the application.

  Returns 0 on success, otherwise a positive integer."
  [& args]
  (cli/action! (cli/parse args)))
