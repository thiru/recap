(ns recap.caption
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [utils.results :as r]))



(declare parse-time-range)



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
    (loop [line-idx 0
           captured-prelude? false
           prelude []
           scanning-for :time-range
           cues []]
      (if (>= line-idx num-lines)
        ;; Ignore prelude if empty or is not separated by an empty line from
        ;; the cues
        (let [prelude? (and (not (empty? prelude))
                            (-> prelude last empty?))]
          {:prelude (if prelude? prelude [])
           :cues cues})
        (let [line (nth lines line-idx)]
          (if (= scanning-for :time-range)
            ;; Scan for line specifying time range:
            (let [time-range (parse-time-range line)]
              (if (empty? time-range)
                (if captured-prelude?
                  (recur (inc line-idx)
                         true
                         prelude
                         :time-range
                         cues)
                  (recur (inc line-idx)
                         false
                         (conj prelude line)
                         :time-range
                         cues))
                (recur (inc line-idx)
                       true
                       prelude
                       :content
                       (conj cues time-range))))
            ;; Otherwise we scan for content/cues:
            (let [content line]
              (if (str/blank? content)
                (recur (inc line-idx)
                       true
                       prelude
                       :time-range
                       cues)
                (let [total-cues (count cues)
                      cue-to-update (last cues)]
                  (recur (inc line-idx)
                         true
                         prelude
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
