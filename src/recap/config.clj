(ns recap.config
  "Config file loader."
  (:refer-clojure :exclude [defn])
  (:require
    [better-cond.core :as b]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]
    [utils.common :as u]
    [utils.results :as r]
    [utils.specin :refer [defn]]))


(set! *warn-on-reflection* true) ; for graalvm


(declare load-config)


(def active-cfg
  (delay (-> (load-config)
             (update :ends-with-any-punctuation re-pattern)
             (update :ends-with-clause-ending-punctuation re-pattern))))

(def default-config
  "Default config (intentionally loaded at compile-time)."
  (-> "default-config.edn"
      slurp
      edn/read-string))

(defn read-config-file
  {:args (s/cat :file-obj #(instance? java.io.File %))
   :ret map?}
  [file-obj]
  ;(println (format "Found config.edn at '%s'" (.toString file-obj))) ; DEBUG
  (->> file-obj
       u/slurp-file
       edn/read-string
       (u/deep-merge default-config)))

(defn load-config
  "Load config.edn from one of these locations:

  - current working directory
  - user application data directory
    - $XDG_CONFIG_HOME/recap (Linux)
    - %LOCALAPPDATA%/recap (Windows)
    - ~/Library/Preferences/recap (Mac)
  - ~/.config/recap"
  {:ret ::r/result}
  []
  (b/cond
    let [cfg-file (u/load-if-file "config.edn")]

    (r/success? cfg-file)
    (read-config-file (:file cfg-file))

    let [user-home-dir (System/getProperty "user.home")
         app-config-dir (case @u/os
                          :linux (System/getenv "XDG_CONFIG_HOME")
                          :windows (System/getenv "LOCALAPPDATA")
                          :mac (u/join-paths user-home-dir "Library/Preferences")
                          nil)
         cfg-file (u/load-if-file (u/join-paths app-config-dir "recap/config.edn"))]

    (and app-config-dir (r/success? cfg-file))
    (read-config-file (:file cfg-file))

    let [cfg-file (u/load-if-file (u/join-paths user-home-dir ".config/recap/config.edn"))]

    (r/success? cfg-file)
    (read-config-file (:file cfg-file))

    :else
    (do
      (r/print-msg (r/r :warn "No user config.edn found, using defaults")) ; DEBUG
      default-config)))
