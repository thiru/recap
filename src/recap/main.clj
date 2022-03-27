(ns recap.main
  "Command-line interface and entry-point into the application."
  (:require [recap.cli :as cli])
  (:gen-class))

(set! *warn-on-reflection* true) ; for graalvm

(defn -main
  "This is the entry-point into the application (e.g. when run from the
   command-line.

   * `args`
     * A list of command-line arguments provided by the user
     * Each argument is a string

  Returns 0 on success, otherwise a positive integer."
  [& args]
  (cli/action (cli/parse args)))
