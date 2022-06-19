(ns user
  "Initial namespace loaded when using a REPL (e.g. using `clj`)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.pprint :refer :all]
            [clojure.reflect :refer :all]
            [clojure.repl :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [expound.alpha :as expound]
            [puget.printer :as puget]
            [reloader.core :as reloader]
            [utils.common :as c]
            [utils.results :as r]
            [recap.cli :as cli]
            [recap.caption :as cap]
            [recap.caption.cue :as cue]
            [recap.main :as main]
            [recap.caption.restitch :as restitch]))

(defonce initialised? (atom false))

(defonce ^{:doc "A short-hand to quit gracefully (otherwise rebel-readline hangs the process)."}
  q (delay (System/exit 0)))

(defmacro PP
  "Convenience macro to pretty-print last evaluated result at the REPL."
  []
  `(puget/cprint *1))

(defn install-expound-printer
  "Replace Clojure Spec's default printer with Expound's printer.

  Ref: https://cljdoc.org/d/expound/expound/0.9.0/doc/faq"
  []
  (alter-var-root #'s/*explain-out* (constantly expound/printer)))

(when (not @initialised?)
  (reset! initialised? true)
  (install-expound-printer)
  (reloader/start ["src" "dev"]))

