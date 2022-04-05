(ns recap.caption.utils
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]))

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



(s/fdef get-speaker-tag
        :args (s/cat :text string?)
        :ret string?)

(defn get-speaker-tag
  "Find speaker tag in the given text.

  A speaker tag is considered to be the text that indentifies of a speaker. It
  should be some capitalised text followed by a colon, at the start of a cue.

  Returns the speaker tag (including the colon) if found, otherwise `nil`."
  [text]
  (some->> text
           (re-find #"^([A-Z]\w*:)")
           second))



(comment
  (get-speaker-tag "Q1:"))
