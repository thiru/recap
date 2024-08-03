(ns recap.caption.restitch
  (:refer-clojure :exclude [defn])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [recap.caption.cue :as cue]
            [recap.caption.data-specs :as dspecs]
            [recap.caption.speaker :as speaker]
            [recap.config :as cfg]
            [recap.utils.common :as u]
            [recap.utils.specin :refer [defn]]
            [recap.utils.results :as r]))


(set! *warn-on-reflection* true) ; for graalvm


(declare
  add-prev-speaker
  group-lines
  start-new-cue?)


(s/def ::force-new-cue-tolerance-secs float?)
(s/def ::max-lines-per-cue int?)


(defn restitch
  "Join cues in the given captions for better readability, based on
  punctuation, quiet gaps, etc."
  {:args (s/cat :caption ::dspecs/caption
                :kwargs (s/keys* :opt-un []))
   :ret ::dspecs/caption}
  [caption & {:keys [opts]
              :or {opts @cfg/active-cfg}}]
  (b/cond
    (empty? (:cues caption))
    caption

    let [cues (mapv cue/join-lines (:cues caption))]

    :else
    (loop [[curr-input-cue & rest-input-cues] (rest cues)
           wip-cue (first cues)
           final-cues []]
      (if (and (empty? curr-input-cue) (empty? rest-input-cues))
        (-> caption
            (assoc :cues (if (cue/empty-cue? wip-cue)
                           final-cues
                           (conj final-cues wip-cue)))
            (group-lines :max-lines-per-cue (:max-lines-per-cue opts))
            (add-prev-speaker))
        (if (start-new-cue? wip-cue curr-input-cue opts)
          (recur rest-input-cues
                 curr-input-cue
                 (if (cue/empty-cue? wip-cue)
                   final-cues
                   (conj final-cues wip-cue)))
          (recur rest-input-cues
                 (cue/join-cues [wip-cue curr-input-cue]
                                :concat-lines? true)
                 final-cues))))))


(defn clause-ender?
  {:args (s/cat :text string?)
   :ret boolean?}
  [text]
  (boolean (re-find (:ends-with-clause-ending-punctuation @cfg/active-cfg) text)))


(defn punctuation-ender?
  {:args (s/cat :text string?)
   :ret boolean?}
  [text]
  (boolean (re-find (:ends-with-any-punctuation @cfg/active-cfg) text)))


(defn has-long-gap?
  "Determine whether there is a long gap between the given cues.

  A gap is considered long if it's greater than
  `force-new-cue-tolerance-secs`."
  {:args (s/cat :cue1 ::dspecs/cue
                :cue2 ::dspecs/cue
                :kwargs (s/keys* :opt-un [::force-new-cue-tolerance-secs]))
   :ret boolean?}
  [cue1 cue2 & {:keys [force-new-cue-tolerance-secs]
                :or {force-new-cue-tolerance-secs
                     (:force-new-cue-tolerance-secs @cfg/active-cfg)}}]
  (let [cue-gap-secs (cue/gap-inbetween cue1 cue2)]
    (if (r/failed? cue-gap-secs)
      (do (r/print-msg cue-gap-secs)
          false)
      (>= cue-gap-secs force-new-cue-tolerance-secs))))


(defn start-new-cue?
  "Determine whether this is an ideal point to create a cue. I.e to start a
  new cue at `wip-cue`."
  {:args (s/cat :wip-cue ::dspecs/cue
                :next-cue ::dspecs/cue
                :opts map?)
   :ret boolean?}
  [wip-cue next-cue opts]
  (b/cond
    let [wip-cue-text (-> wip-cue :lines last (or ""))
         next-cue-text (-> next-cue :lines last (or ""))]

    ;; Always start a new line on speaker tag
    (speaker/get-speaker-tag next-cue-text)
    true

    let [wip-cue-char-count (cue/char-count wip-cue)
         next-cue-char-count (cue/char-count next-cue)]

    ;; Never go above the absolute maximum number of chars allows in a line
    (<= (:absolute-max-chars-per-line opts) (+ wip-cue-char-count
                                               1 ; space between
                                               next-cue-char-count))
    true

    ;; Break line if there's a longish silence
    (has-long-gap? wip-cue next-cue)
    true

    ;; Avoid lines starting with a single word ending in a punctuation mark,
    ;; unless previous line ends in a clause-ending punctuation mark
    (and (punctuation-ender? next-cue-text)
         (not (clause-ender? wip-cue-text)))
    false

    ;; Break line if the current cue ends in a clause-ending punctuation mark
    ;; and the minimum number of chars for this is reached
    (and (>= wip-cue-char-count
             (:breakable-clause-ender-min-chars opts))
         (clause-ender? wip-cue-text))
    true

    ;; Break line if the current cue ends in any punctuation mark and the next
    ;; word does not end in a punctuation mark, while the minimum number of
    ;; chars for this is reached
    (and (>= wip-cue-char-count
             (:breakable-any-punctuation-min-chars opts))
         (punctuation-ender? wip-cue-text)
         (not (punctuation-ender? (-> next-cue :lines last))))
    true

    let [wip-cue-is-opener? (re-find #"^['\[]" wip-cue-text)
         wip-cue-is-closer? (re-find #"['\]],?$" wip-cue-text)]

    ;; Avoid lines starting with a single dangling word that ends a quote or
    ;; bracket:
    (and wip-cue-is-closer? (not wip-cue-is-opener?))
    false

    :else
    (<= (:ideal-max-chars-per-line opts)
        wip-cue-char-count)))


(defn force-new-cue-for-full-stop?
  "Determine whether to force a new cue when there is full stop outside a quoted or bracketed
  phrase."
  {:args (s/cat :cue1 ::dspecs/cue)
   :ret boolean?}
  [cue]
  (let [cue-text (cue/text-only cue)
        cue-text-length (count cue-text)
        last-char (nth cue-text (dec cue-text-length) nil)
        second-last-char (nth cue-text (- cue-text-length 2) nil)]
    (and (>= cue-text-length (:breakable-clause-ender-min-chars @cfg/active-cfg))
         (or (= last-char \.) (= last-char \?) (= last-char \!))
         (not (= second-last-char \.)))))


(defn group-lines
  "Group cues according to `max-lines-per-cue` and respecting longish quiet
  gaps."
  {:args (s/cat :caption ::dspecs/caption
                :kwargs (s/keys* :opt-un [::max-lines-per-cue]))
   :ret ::dspecs/caption}
  [caption & {:keys [max-lines-per-cue]
              :or {max-lines-per-cue (:max-lines-per-cue @cfg/active-cfg)}}]
  (b/cond
    (empty? (:cues caption))
    caption

    (not (every? #(= 1 (count (:lines %)))
                 (:cues caption)))
    (r/r :error (str "In order to group cue lines every cue must contain just "
                     "one line each."))

    :else
    (loop [[curr-input-cue & rest-input-cues] (-> caption :cues rest)
           wip-cue (-> caption :cues first)
           final-cues []]
      (if (and (empty? rest-input-cues) (cue/empty-cue? curr-input-cue))
        (assoc caption :cues (conj final-cues wip-cue))
        (let [append-line? (and (not (has-long-gap? wip-cue curr-input-cue))
                                (not (force-new-cue-for-full-stop? wip-cue))
                                (< (-> wip-cue :lines count)
                                   max-lines-per-cue))]
          (if append-line?
            (recur rest-input-cues
                   (cue/join-cues [wip-cue curr-input-cue])
                   final-cues)
            (recur rest-input-cues
                   curr-input-cue
                   (conj final-cues wip-cue))))))))


(defn add-prev-speaker
  "If there's a speaker change within the same cue add the previous speaker back in."
  {:args (s/cat :caption ::dspecs/caption)
   :ret ::dspecs/caption}
  [caption]
  (loop [[curr-cue & rest-cues] (-> caption :cues)
         last-speaker (-> caption :cues first :lines first speaker/get-speaker-tag (or ""))
         final-cues []]
    (if (and (empty? rest-cues) (cue/empty-cue? curr-cue))
      (assoc caption :cues final-cues)
      (let [new-speaker (->> curr-cue
                             :lines
                             (map speaker/get-speaker-tag)
                             (filter #(not (str/blank? %)))
                             last)
            new-speaker (or new-speaker last-speaker)]
        ;; No need to do anything if there's only 1 line in this cue
        (if (>= 1 (count (:lines curr-cue)))
          (recur rest-cues
                 new-speaker
                 (conj final-cues curr-cue))
          ;; HACK: this is a bit of a hack since it assumes there won't be more than 2 lines
          (let [last-line-speaker (-> curr-cue :lines last speaker/get-speaker-tag)]
            (if (or (str/blank? last-line-speaker)
                    (= last-speaker last-line-speaker))
              (recur rest-cues
                   new-speaker
                   (conj final-cues curr-cue))
              (recur rest-cues
                     new-speaker
                     (conj final-cues (update-in curr-cue [:lines 0] #(str last-speaker " " %)))))))))))

(comment
  (clause-ender? "sdf;")
  (force-new-cue-for-full-stop? {:lines ["the pain that strong pain of -"
                                         "very naturally, of course, no?"]
                                 :start "00:00:00"
                                 :end "00:00:00"})
  (force-new-cue-for-full-stop? {:lines ["the pain that strong pain of -"
                                         "very naturally, of course, no..."]
                                 :start "00:00:00"
                                 :end "00:00:00"}))
