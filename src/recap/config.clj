(ns recap.config
  "Config file loader."
  (:refer-clojure :exclude [defn])
  (:require
    [better-cond.core :as b]
    [clojure.edn :as edn]
    [clojure.spec.alpha :as s]
    [recap.utils.common :as u]
    [recap.utils.results :as r]
    [recap.utils.specin :refer [defn]]))


(set! *warn-on-reflection* true) ; for graalvm


(declare load-config)


(def default-config
  {:absolute-max-chars-per-line 50
   :breakable-clause-ender-min-chars 8
   :breakable-any-punctuation-min-chars 23
   ;; The only difference from `:ends-with-clause-ending-punctuation` is this includes a comma
   :ends-with-any-punctuation "[,.!?;:\\]'\"—–-]['\"]?$"
   :ends-with-clause-ending-punctuation "[.!?;:\\]'\"—–-]['\"]?$"
   :force-new-cue-tolerance-secs 3
   :ideal-max-chars-per-line 38
   :max-lines-per-cue 2
   :sonix
   {:api-key ""
    :json {:url "https://api.sonix.ai/v1/media/%s/transcript.json"}
    :srt {:url "https://api.sonix.ai/v1/media/%s/transcript.srt"
          :opts {:limit_captions true
                 :max_characters 1
                 :max_duration 1
                 :speaker_display "as_typed"
                 :subtitle_lines 1}}
    :vtt {:url "https://api.sonix.ai/v1/media/%s/transcript.vtt"
          :opts {:limit_captions true
                 :max_characters 1
                 :max_duration 1
                 :speaker_display "as_typed"
                 :subtitle_lines 1}}}
   :trint
   {:api-key ""
    :opts {:captions-by-paragraph false
           :max-subtitle-character-length 1
           :highlights-only false
           :enable-speakers true
           :speaker-on-new-line true
           :speaker-uppercase false
           :skip-strikethroughs false}
    :srt-url "https://api.trint.com/export/srt/"
    :vtt-url "https://api.trint.com/export/webvtt/"}})

(def active-cfg
  (delay (-> (load-config)
             (update :ends-with-any-punctuation re-pattern)
             (update :ends-with-clause-ending-punctuation re-pattern))))

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
