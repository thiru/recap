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


(def default-config
  {:absolute-max-chars-per-line 50
   :breakable-clause-ender-min-chars 8
   :breakable-any-punctuation-min-chars 23
   ;; Same as `:ends-in-clause-ending-punctuation` except adds a comma
   :ends-in-any-punctuation #"[,.!?;:\]'\"—–-]['\"]?$"
   :ends-in-clause-ending-punctuation #"[.!?;:\]'\"—–-]['\"]?$"
   :force-new-cue-tolerance-secs 3
   :ideal-max-chars-per-line 38
   :max-lines-per-cue 2})

(defn read-config-file
  {:args (s/cat :file-obj #(instance? java.io.File %))
   :ret map?}
  [file-obj]
  ;(println (format "Found config.edn at '%s'" (.toString file-obj))) ; DEBUG
  (->> file-obj
       u/slurp-file
       edn/read-string
       (merge default-config)))

(defn load-config
  "Load config.edn from one of these locations:

  - current working directory
  - $XDG_CONFIG_HOME/recap
  - $HOME/.config/recap"
  {:ret ::r/result}
  []
  (b/cond
    let [cfg-file (u/load-if-file "config.edn")]

    (r/success? cfg-file)
    (read-config-file (:file cfg-file))

    let [xdg-config-home (System/getenv "XDG_CONFIG_HOME")
         cfg-file (u/load-if-file (u/join-paths xdg-config-home "recap/config.edn"))]

    (and xdg-config-home (r/success? cfg-file))
    (read-config-file (:file cfg-file))

    let [cfg-file (u/load-if-file (u/join-paths (System/getenv "HOME")
                                                ".config/recap/config.edn"))]

    (r/success? cfg-file)
    (read-config-file (:file cfg-file))

    :else
    (do
      (println "No user config.edn found, using defaults")
      default-config)))
