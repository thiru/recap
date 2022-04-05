(ns recap.caption
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [recap.caption.utils :as capu]
            [utils.common :as u]
            [utils.results :as r]))



(s/def ::prelude (s/coll-of string?))
(s/def ::duration #(re-find #"\d\d:\d\d:\d\d[,.]\d+" %))
(s/def ::start ::duration)
(s/def ::end ::duration)
(s/def ::lines (s/coll-of string?))
(s/def ::cue (s/keys :req-un [::start ::end ::lines]))
(s/def ::cues (s/coll-of ::cue))
(s/def ::caption (s/keys :opt-un [::prelude ::cues]))

(s/fdef parse
        :args (s/cat :input string?)
        :ret ::caption)

(defn parse
  "Parse the given captions text into a Clojure data structure.

  This should work for SRT and WebVTT formats."
  [input]
  (b/cond
    (str/blank? input)
    {}

    let [lines (str/split-lines input)]

    (empty? lines)
    {}

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



(s/fdef to-string
        :args (s/cat :caption ::caption
                     :collapse-cue-lines? any?)
        :ret string?)

(defn to-string
  "Convert the given captions to a plain string.

  * `collapse-cue-lines?`
    * Whether to join separate lines in a cue into one (space-delimited)

  Returns a string."
  [caption & {:keys [collapse-cue-lines?]}]
  (loop [[cue & rest-cues] (:cues caption)
         cue-idx 1
         cue-text ""]
    (if (and (empty? cue) (empty? rest-cues))
      (-> (if (not (empty? (:prelude caption)))
            (str/join "\n" (:prelude caption))
            "")
          (str cue-text)
          str/trim)
      (recur rest-cues
             (inc cue-idx)
             (str cue-text "\n"
                  ;; NOTE: assuming that a `caption` without a prelude is of
                  ;; SRT format, and so requires numbered cues.
                  (if (empty? (:prelude caption))
                    (str cue-idx "\n")
                    "")
                  (:start cue) " --> " (:end cue) "\n"
                  (if (:lines cue)
                    (str/join (if collapse-cue-lines? " " "\n")
                              (:lines cue))
                    (:line cue))
                  "\n")))))



(s/fdef find-overlapping-cues
        :args (s/cat :cues ::cues)
        :ret (s/coll-of int?))

(defn find-overlapping-cues
  "Check if the given cues contain any cases where more than one cue appears on
  screen at once.

  Returns a list of indeces of cues which overlap, if any."
  [cues]
  (if (or (empty? cues)
          (= 1 (count cues)))
    []
    ;; Skip first cue since we're always comparing with previous cues
    (loop [[curr-cue & rest-cues] (rest cues)
           prev-cue (first cues)
           curr-cue-num 2
           overlapping-idxs []]
      (b/cond
        (and (empty? curr-cue) (empty? rest-cues))
        (-> overlapping-idxs distinct sort vec)

        let [prev-cue-end-secs (u/duration->secs (:end prev-cue))]

        (r/failed? prev-cue-end-secs)
        (r/r :error (format "Failed to parse end time of cue %d. %s"
                            curr-cue-num
                            (if (r/result? prev-cue-end-secs)
                              (:message prev-cue-end-secs)
                              "")))

        let [curr-cue-start-secs (u/duration->secs (:start curr-cue))]

        (r/failed? curr-cue-start-secs)
        (r/r :error (format "Failed to parse end time of cue %d. %s"
                            (inc curr-cue-num)
                            (if (r/result? curr-cue-start-secs)
                              (:message curr-cue-start-secs)
                              "")))

        let [overlaps? (> prev-cue-end-secs
                          curr-cue-start-secs)]

        ;do (u/spy [prev-cue-end-secs curr-cue-start-secs overlaps?]) ; DEBUG

        :else
        (recur rest-cues
               curr-cue
               (inc curr-cue-num)
               (if overlaps?
                 (conj overlapping-idxs curr-cue-num (dec curr-cue-num))
                 overlapping-idxs))))))



(s/fdef strip-contiguous-speaker-tags
        :args (s/cat :input string?)
        :ret string?)

(defn strip-contiguous-speaker-tags
  "Remove contiguous same speaker tags from the given captions.

  I.e. only show a speaker tag when there is a change in speaker."
  [input]
  (if (str/blank? input)
    input
    (let [lines (str/split-lines input)]
      (loop [[line & remaining-lines] lines
             last-speaker-tag nil
             stripped []]
        (let [curr-speaker-tag (capu/get-speaker-tag line)
              adjusted-line (if (and curr-speaker-tag
                                     (= last-speaker-tag curr-speaker-tag))
                              (-> line
                                  (subs (count curr-speaker-tag))
                                  str/trim)
                              line)]
          (if (nil? remaining-lines)
            (str/join "\n"
                      (conj stripped adjusted-line))
            (recur remaining-lines
                   (if (and curr-speaker-tag
                            (not= last-speaker-tag curr-speaker-tag))
                     curr-speaker-tag
                     last-speaker-tag)
                   ;; Account for the case where the speaker tag is the only
                   ;; text in the line. We don't want to add a blank line.
                   (if (and curr-speaker-tag
                            (empty? adjusted-line))
                     stripped
                     (conj stripped adjusted-line)))))))))



(comment
  (-> "tmp/captions.vtt" slurp parse)
  (-> "tmp/captions.vtt" slurp parse to-string println)
  (-> "tmp/captions.vtt" slurp parse :cues find-overlapping-cues)
  (-> "tmp/one-word.srt" slurp strip-contiguous-speaker-tags println))
