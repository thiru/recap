(ns recap.caption
  (:require [clojure.string :as str]
            [better-cond.core :as b]
            [utils.results :as r]))

(declare parse-time-range)

(defn parse
  "Parse the given captions text into a Clojure data structure.

  We only care about the cues and their timestamps.

  Returns a vector of maps if successful. An empty vector is returned if the
  input could not be parsed as captions. Each map represents a cue with:
  * `:start`
    * The time the cue appears on screen (e.g. 00:01:20)
  * `:end`
    * The time the cue is removed from screen (e.g. 00:01:30)
  * `:lines`
    * A vector of strings, for each line of text."
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

(defn parse-time-range
  [input]
  (b/cond
    (str/blank? input)
    nil

    let [matches (re-matches #"(\d\d:\d\d:\d\d[,.]\d+)\s+-+>\s+(\d\d:\d\d:\d\d[,.]\d+)"
                             input)]

    (or (empty? matches)
        (not= 3 (count matches)))
    nil

    {:start (str/replace (second matches) "," ".")
     :end (str/replace (last matches) "," ".")}))
