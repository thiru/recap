(ns recap.caption.cue
  "Encapsulates a cue in a caption."
  (:require [better-cond.core :as b]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [utils.common :as c]
            [utils.results :as r]))



(s/def ::duration #(re-find #"\d\d:\d\d:\d\d[,.]\d+" %))
(s/def ::start ::duration)
(s/def ::end ::duration)
(s/def ::lines (s/coll-of string?))
(s/def ::cue (s/keys :req-un [::start ::end ::lines]))



(s/fdef empty-cue
        :args (s/cat :cue ::cue)
        :ret boolean?)

(defn empty-cue?
  "Determine whether the given cue has any content (i.e. non-blank content)."
  [cue]
  (or (nil? cue)
      (empty? cue)
      (empty? (:lines cue))
      (every? str/blank? (:lines cue))))



(s/fdef total-secs
        :args (s/cat :cue ::cue)
        :ret (s/or :seconds float? :error-result :r/result))

(defn total-secs
  "Get the total number of seconds spanning the given cue."
  [cue]
  (b/cond
    (empty? cue)
    0

    let [start-dur-r (-> cue :start (str/replace #"," ".") c/duration->secs)]

    (r/failed? start-dur-r)
    (r/prepend-msg start-dur-r (format "Start time of cue (%s) is invalid. "
                                       (:start cue)))

    let [end-dur-r (-> cue :end (str/replace #"," ".") c/duration->secs)]

    (r/failed? end-dur-r)
    (r/prepend-msg end-dur-r (format "End time of cue (%s) is invalid. "
                                     (:end cue)))

    (- end-dur-r start-dur-r)))



(s/fdef to-string
        :args (s/cat :cue ::cue
                     :collapse-cue-lines? boolean?)
        :ret string?)

(defn to-string
  "Convert the given cue to a plain string, suitable to be placed in a caption
  file.

  * `collapse-cue-lines?`
    * Whether to join separate lines in a cue into one (space-delimited)"
  [cue & {:keys [collapse-cue-lines?]}]
  (str (:start cue) " --> " (:end cue) "\n"
       (if (:lines cue)
         (str/join (if collapse-cue-lines? " " "\n")
                   (:lines cue))
         (:line cue))))



(s/fdef join-cues
        :args (s/cat :cues (s/coll-of ::cue))
        :ret ::cue)

(defn join-cues
  "Combine the given cues into one, having just a single line of content."
  [cues]
  {:start (-> cues first :start)
   :end (-> cues last :end)
   :lines [(str/join " " (mapv #(->> % :lines (str/join " "))
                               cues))]})



(s/fdef parse-time-range
        :args (s/cat :input string?)
        :ret (s/or :invalid (s/and empty? map?)
                   :valid (s/keys :req-un [::start ::end])))

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



(comment
  (empty-cue? nil)
  (empty-cue? {})
  (empty-cue? {:lines []})
  (empty-cue? {:lines ["" " "]})
  (empty-cue? {:lines ["a"]})
  (def cue {:start "00:01:00.500"
            :end "00:02:10,700"
            :lines ["first line" "second line"]})
  (total-secs cue)
  (println (to-string cue))
  (println (to-string cue :collapse-cue-lines? true))
  (join-cues [cue {:start "00:02:00"
                   :end "00:03:00"
                   :lines ["cue2 first line" "cue 2 second line"]}]))
