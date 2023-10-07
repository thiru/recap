(ns utils.printing

  "Various print debugging utilities."

  (:require
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]))

(defmacro PP
  "Convenience macro to pretty-print last evaluated result at the REPL."
  []
  `(puget/cprint *1))

(defn install-expound-printer
  "Replace Clojure Spec's default printer with Expound's printer.

  Ref: https://cljdoc.org/d/expound/expound/0.9.0/doc/faq"
  []
  (alter-var-root #'s/*explain-out* (constantly expound/printer)))
