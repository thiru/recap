(ns recap.caption
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [recap.caption.utils :as capu]))



(s/def ::prelude (s/coll-of string?))
(s/def ::duration #(re-find #"\d\d:\d\d:\d\d[,.]\d+" %))
(s/def ::start ::duration)
(s/def ::end ::duration)
(s/def ::lines (s/coll-of string?))
(s/def ::cue (s/keys :req-un [::start ::end ::lines]))
(s/def ::cues (s/coll-of ::cue))
(s/def ::caption (s/keys :req-un [::prelude ::cues]))

(s/fdef parse
        :args (s/cat :input string?)
        :ret ::caption)

(defn parse
  "Parse the given captions text into a Clojure data structure.

  We only care about the cues and their timestamps. Other text/metadata is
  passed along as is so this should work equally well for SRT and WebVTT
  formats."
  [input]
  (b/cond
    (str/blank? input)
    []

    let [lines (str/split-lines input)
         num-lines (count lines)]

    (empty? lines)
    []

    :else
    (loop [[line & rest-lines] lines
           captured-prelude? false
           prelude []
           scanning-for :time-range
           cues []]
      (if (and (empty? line) (empty? rest-lines))
        ;; Ignore prelude if empty or is not separated by an empty line from
        ;; the cues
        (let [prelude? (and (not (empty? prelude))
                            (-> prelude last empty?))]
          {:prelude (if prelude? prelude [])
           :cues cues})
        (if (= scanning-for :time-range)
          ;; Scan for line specifying time range:
          (let [time-range (capu/parse-time-range line)]
            (if (empty? time-range)
              (if captured-prelude?
                (recur rest-lines
                       true
                       prelude
                       :time-range
                       cues)
                (recur rest-lines
                       false
                       (conj prelude line)
                       :time-range
                       cues))
              (recur rest-lines
                     true
                     prelude
                     :content
                     (conj cues time-range))))
          ;; Otherwise we scan for content/cues:
          (let [content line]
            (if (str/blank? content)
              (recur rest-lines
                     true
                     prelude
                     :time-range
                     cues)
              (let [total-cues (count cues)
                    cue-to-update (last cues)]
                (recur rest-lines
                       true
                       prelude
                       :content
                       (assoc cues
                              (dec total-cues)
                              (assoc cue-to-update
                                     :lines (conj (or (:lines cue-to-update)
                                                      [])
                                                  line))))))))))))



(def output-formats
  "Supported caption formats that can be output."
  #{:srt :vtt})



(s/fdef to-string
        :args (s/cat :caption ::caption
                     :output-format output-formats
                     :collapse-cue-lines? any?)
        :ret string?)

(defn to-string
  "Convert the given captions to a plain string.

  * `collapse-cue-lines?`
    * Whether to join separate lines in a cue into one (space-delimited)

  Returns a string."
  [caption output-format & {:keys [collapse-cue-lines?]}]
  (loop [[cue & rest-cues] (:cues caption)
         cue-idx 1
         cue-text ""]
    (if (and (empty? cue) (empty? rest-cues))
      (-> (if (= :vtt output-format)
            (str/join "\n" (:prelude caption))
            "")
          (str cue-text)
          str/trim)
      (recur rest-cues
             (inc cue-idx)
             (str cue-text "\n"
                  (if (= :srt output-format)
                    (str cue-idx "\n")
                    "")
                  (:start cue) " --> " (:end cue) "\n"
                  (if (:lines cue)
                    (str/join (if collapse-cue-lines? " " "\n")
                              (:lines cue))
                    (:line cue))
                  "\n")))))
