(ns recap.caption.copyright
  (:refer-clojure :exclude [defn])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [recap.caption.data-specs :as dspecs]
            [utils.common :as c]
            [utils.specin :refer [defn]])
  (:import (java.time LocalDate)))


(def defaults {:cue-duration 3.0
               :cue-gap 0.0
               :end-gap 2.0})

(s/def ::seconds (s/and pos? number?))
(s/def ::media-duration ::seconds)

(defn append
  "Appends (copyright) cues to the end of the given captions.

  An empty string in `copyright-lines` indicates a new cue."
  {:args (s/cat :caption ::dspecs/caption
                :media-duration ::media-duration
                :copyright-lines (s/coll-of string?))
   :ret ::dspecs/caption}
  [caption media-duration copyright-lines
   & {:keys [date cue-duration cue-gap end-gap]
      :or {cue-duration (:cue-duration defaults)
           cue-gap (:cue-gap defaults)
           end-gap (:end-gap defaults)}}]
  (let [date (str (or date (-> (LocalDate/now) (.getYear))))]
    (if (empty? copyright-lines)
      caption
      (loop [[cr-line & rest-cr-lines] (reverse copyright-lines)
             wip-cue {:start (-> (- media-duration end-gap cue-duration)
                                 (* 1000)
                                 (c/millis->duration :show-millis? true))
                      :end (-> (- media-duration end-gap)
                               (* 1000)
                               (c/millis->duration :show-millis? true))
                      :lines []}
             copyright-cues []]
        (b/cond
          (and (nil? cr-line) (nil? rest-cr-lines))
          (as-> copyright-cues $
            (conj $ wip-cue)
            (reverse $)
            (mapv #(update % :lines vec)
                  $)
            (update caption :cues #(into % $)))

          (str/blank? cr-line)
          (recur rest-cr-lines
                 (let [end-secs (-> wip-cue
                                    :start
                                    c/duration->secs
                                    (- cue-gap))
                       start-secs (- end-secs cue-duration)]
                   {:start (c/millis->duration (* 1000 start-secs)
                                               :show-millis? true)
                    :end (c/millis->duration (* 1000 end-secs)
                                             :show-millis? true)
                    :lines []})
                 (conj copyright-cues wip-cue))

          :let [cr-line (str/replace cr-line #"\{DATE\}" date)]

          :else
          (recur rest-cr-lines
                 (update wip-cue :lines #(cons cr-line %))
                 copyright-cues))))))


(comment
  (append {:cues [{:start "00:00:00"
                   :end "00:00:01"
                   :lines ["first cue"]}
                  {:start "00:00:01"
                   :end "00:00:02"
                   :lines ["second cue"]}]}
          90
          ["copyright cue 1, line 1"
           "copyright cue 1, line 2"
           ""
           "copyright cue 2, line 1"
           "copyright cue 2, line 2"]))
