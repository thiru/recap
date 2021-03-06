(ns recap.caption
  (:refer-clojure :exclude [defn])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [recap.caption.cue :as cue]
            [recap.caption.linger :as linger]
            [recap.caption.restitch :as restitch]
            [recap.caption.data-specs :as dspecs]
            [recap.caption.speaker :as speaker]
            [utils.common :as u]
            [utils.specin :refer [defn]]
            [utils.results :as r]))


(defn empty-caption?
  "Determine whether the given caption has any content (i.e. no cues)."
  {:args (s/cat :caption ::dspecs/caption)
   :ret boolean?}
  [caption]
  (or (nil? caption)
      (empty? caption)
      (empty? (:cues caption))
      (every? cue/empty-cue? (:cues caption))))


(defn parse
  "Parse the given captions text into a Clojure data structure.

  This should work for SRT and WebVTT formats."
  {:args (s/cat :input (s/nilable string?))
   :ret ::dspecs/caption}
  [input]
  (b/cond
    (str/blank? input)
    {}

    let [lines (-> input str/trim str/split-lines)]

    (empty? lines)
    {}

    :else
    (loop [[line & rest-lines] lines
           scanning-for :header
           header []
           cues []]
      (b/cond
        let [eof? (and (empty? line) (empty? rest-lines))]

        eof?
        ;; Ignore header if empty or is not separated by an empty line from
        ;; the cues
        (let [empty-header? (or (empty? header)
                                (empty? (last header)))]
          {:header (if empty-header? [] header)
           :cues cues})

        (= scanning-for :header)
        (let [cue-idx (u/parse-int line :fallback -1)
              header? (and (not (str/blank? line))
                           (not (pos-int? cue-idx)))]
          (if header?
            (recur rest-lines
                   :header
                   (conj header line)
                   cues)
            (recur rest-lines
                   :time-range
                   header
                   cues)))

        let [time-range (cue/parse-time-range line)]

        (= scanning-for :time-range)
        (if (empty? time-range)
          ;; Keep looking for the next time-range. One likely possibility is
          ;; that this line is a cue index (which we ignore)
          (recur rest-lines
                 :time-range
                 header
                 cues)
          (recur rest-lines
                 :content
                 header
                 ;; Start new cue with time-range alone (no content yet):
                 (conj cues time-range)))

        ;; Otherwise we scan for content/cues:
        (= scanning-for :content)
        (if (str/blank? line)
          (recur rest-lines
                 :time-range
                 header
                 cues)
          (recur rest-lines
                 :content
                 header
                 (update cues
                         (dec (count cues))
                         #(assoc % :lines (conj (or (:lines %) [])
                                                line)))))

        :else
        (throw (ex-info "This shouldn't happen (else clause of outter cond)"
                        {:line line}))))))


(defn to-string
  "Convert the given captions to a plain string.

  * `collapse-cue-lines?`
    * Whether to join separate lines in a cue into one (space-delimited)"
  {:args (s/cat :caption ::dspecs/caption
                :kwargs (s/keys* :opt-un []))
   :ret string?}
  [caption & {:keys [collapse-cue-lines?]}]
  (loop [[cue & rest-cues] (:cues caption)
         cue-idx 1
         cue-text ""]
    (if (and (empty? cue) (empty? rest-cues))
      (-> (if (not (empty? (:header caption)))
            (str (str/join "\n" (:header caption))
                 "\n")
            "")
          (str cue-text)
          str/trim)
      (recur rest-cues
             (inc cue-idx)
             (str cue-text "\n"
                  ;; NOTE: assuming that a `caption` without a header is of
                  ;; SRT format, and so requires numbered cues.
                  (if (empty? (:header caption))
                    (str cue-idx "\n")
                    "")
                  (cue/to-string cue :collapse-cue-lines? collapse-cue-lines?)
                  "\n")))))


(defn find-overlapping-cues
  "Check if the given cues contain any cases where more than one cue appears on
  screen at once.

  Returns a list of indeces of cues which overlap, if any."
  {:args (s/cat :cues ::dspecs/cues)
   :ret (s/coll-of nat-int?)}
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


(defn strip-contiguous-speaker-tags
  "Remove contiguous same speaker tags from the given captions.

  I.e. only show a speaker tag when there is a change in speaker."
  {:args (s/cat :input (s/? string?))
   :ret string?}
  [input]
  (if (str/blank? input)
    input
    (let [lines (str/split-lines input)]
      (loop [[line & remaining-lines] lines
             last-speaker-tag nil
             stripped []]
        (let [curr-speaker-tag (speaker/get-speaker-tag line)
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
  (parse nil)
  (parse "")
  (-> "tmp/captions.srt" slurp parse)
  (-> "tmp/captions.vtt" slurp parse)
  (-> "tmp/captions.vtt" slurp parse :cues find-overlapping-cues)
  (-> "tmp/one-word.srt" slurp strip-contiguous-speaker-tags println)
  (-> "tmp/one-word-full.vtt" slurp parse speaker/unique-speaker-tags)
  (->> "tmp/linger-test.vtt" slurp parse linger/linger-cues
       to-string
       (spit "tmp/linger-test-rebuilt.vtt"))
  (def caps (-> "tmp/one-word-full.vtt"
                slurp
                strip-contiguous-speaker-tags
                parse))
  (cue/join-cues (->> "tmp/one-word.srt"
                      slurp
                      strip-contiguous-speaker-tags
                      parse
                      :cues
                      (take 3)))
  (->> caps restitch/restitch linger/linger-cues to-string (spit "tmp/rebuilt.vtt")))
