(ns recap.caption
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [utils.results :as r]))



(declare parse-time-range)



(s/def ::duration #(re-find #"\d\d:\d\d:\d\d[,.]\d+" %))
(s/def ::start ::duration)
(s/def ::end ::duration)
(s/def ::lines (s/coll-of string?))
(s/def ::cue (s/keys :req-un [::start ::end ::lines]))
(s/def ::caption (s/coll-of ::cue))

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
    (loop [line-idx 0
           scanning-for :time-range
           cues []]
      (if (>= line-idx num-lines)
        cues
        (let [line (nth lines line-idx)]
          (if (= scanning-for :time-range)
            ;; Scan for line specifying time range:
            (let [time-range (parse-time-range line)]
              (if (empty? time-range)
                (recur (inc line-idx)
                       :time-range
                       cues)
                (recur (inc line-idx)
                       :content
                       (conj cues time-range))))
            ;; Otherwise we scan for content/cues:
            (let [content line]
              (if (str/blank? content)
                (recur (inc line-idx)
                       :time-range
                       cues)
                (let [total-cues (count cues)
                      cue-to-update (last cues)]
                  (recur (inc line-idx)
                         :content
                         (assoc cues
                                (dec total-cues)
                                (assoc cue-to-update
                                       :lines (conj (or (:lines cue-to-update)
                                                        [])
                                                    line)))))))))))))



(s/fdef parse-time-range
        :args (s/cat :input string?)
        :ret (s/keys :req-un [::start ::end]))

(defn parse-time-range
  [input]
  (b/cond
    (str/blank? input)
    {}

    let [matches (re-matches #"(\d\d:\d\d:\d\d[,.]\d+)\s+-+>\s+(\d\d:\d\d:\d\d[,.]\d+)"
                             input)]

    (or (empty? matches)
        (not= 3 (count matches)))
    {}

    {:start (str/replace (second matches) "," ".")
     :end (str/replace (last matches) "," ".")}))
