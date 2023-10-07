(ns user

  "Initial namespace loaded when using a REPL (e.g. using `clj`)."

  {:clj-kondo/config '{:linters {:unused-namespace {:level :off}
                                 :unused-referred-var {:level :off}}}}
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.java.shell :as sh]
    [clojure.pprint :as pp]
    [clojure.reflect :as reflect]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [rebel-readline.main :as rebel]
    [recap.caption :as cap]
    [recap.caption.copyright :as copyright]
    [recap.caption.cue :as cue]
    [recap.caption.restitch :as restitch]
    [recap.cli :as cli]
    [recap.config :as cfg]
    [recap.main :as main]
    [utils.common :as u]
    [utils.nrepl :as nrepl]
    [utils.printing :as printing :refer [PP]]
    [utils.results :as r]))

(defonce initialised? (atom false))

(when (not @initialised?)
  (reset! initialised? true)
  (printing/install-expound-printer)
  (nrepl/start-server)
  ;; Blocking call:
  (rebel/-main)
  (nrepl/stop-server)
  ;; HACK: rebel-readline causes process to hang and not quit without this:
  (System/exit 0))
