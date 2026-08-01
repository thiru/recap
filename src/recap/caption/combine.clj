(ns recap.caption.combine
  (:refer-clojure :exclude [defn])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [recap.caption.data-specs :as dspecs]
            [recap.utils.common :as u]
            [recap.utils.specin :refer [defn]]
            [recap.utils.results :as r]))

(set! *warn-on-reflection* true) ; for graalvm

(defn- divider?
  "Determine whether the given cue is contains a division point."
  {:args (s/cat :cue ::dspecs/cue)
   :ret boolean?}
  [cue]
  (boolean
    (some #(re-find #"xxxxx+$" (-> % str/trim str/lower-case))
          (:lines cue))))

(defn- partition-cues
  "Partitions the cues of each of the given captions."
  {:args (s/cat :caption (s/coll-of ::dspecs/caption))
   :ret (s/or :success (s/coll-of (s/coll-of ::dspecs/cue))
              :failure ::r/result)}
  [captions]
  (loop [idx 0
         curr-caption (first captions)
         last-segment-count nil
         error-msg nil
         segmented-cues-list []]
    (cond
      error-msg
      (r/r :error error-msg)

      (nil? curr-caption)
      segmented-cues-list

      :else
      (let [next-idx (inc idx)
            next-caption (nth captions next-idx nil)
            segmented-cues (u/split-when divider? (:cues curr-caption))
            segment-count (count segmented-cues)]
        (recur next-idx
               next-caption
               segment-count
               (when (and last-segment-count
                          (not= last-segment-count segment-count))
                 (u/fmt+ ["The ~:R file does not have the same number of segments as the "
                          "previous file~:p. The ~:R file has ~d segment~:p, while the other~:p "
                          "have ~d."]
                         (inc idx)
                         (inc idx)
                         segment-count
                         last-segment-count))
               (conj segmented-cues-list segmented-cues))))))

(defn combine
  "Combine team effort captions.
  Takes a number of captions where each caption has a unique segment worked and combine those
  respective segments into one. Segments are divided by convention by a series of x's."
  {:args (s/cat :caption (s/coll-of ::dspecs/caption))
   :ret (s/or :success ::dspecs/caption
              :failure ::r/result)}
  [captions]
  (b/cond
    (empty? captions)
    captions

    let [segmented-cues (partition-cues captions)]

    (r/failed? segmented-cues)
    segmented-cues

    let [combined-cues (loop [idx 0
                              curr-segm-cue (first segmented-cues)
                              final-cues []]
                         (if (nil? curr-segm-cue)
                           final-cues
                           (recur (inc idx)
                                  (nth segmented-cues (inc idx) nil)
                                  (into final-cues (nth curr-segm-cue idx nil)))))]

    :else
    ;; NOTE: assuming the header is the same across all captions so we simply take the first one
    (assoc (first captions)
           :cues combined-cues)))

(comment
  (combine [{:header ["WebVTT"]
             :cues [{:lines ["1aaa"]}
                    {:lines ["1bbb"]}
                    {:lines ["xxxxx"]}
                    {:lines ["1ccc"]}
                    {:lines ["1ddd"]}
                    {:lines ["xxxxx"]}
                    {:lines ["1eee"]}]}
            {:cues [{:lines ["2aaa"]}
                    {:lines ["2bbb"]}
                    {:lines ["xxxxx"]}
                    {:lines ["2ccc"]}
                    {:lines ["2ddd"]}
                    {:lines ["xxxxx"]}
                    {:lines ["2eee"]}]}
            {:cues [{:lines ["3aaa"]}
                    {:lines ["3bbb"]}
                    {:lines ["xxxxx"]}
                    {:lines ["3ccc"]}
                    {:lines ["3ddd"]}
                    {:lines ["xxxxx"]}
                    {:lines ["3eee"]}]}]))
