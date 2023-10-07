(ns recap.caption.restitch
  (:refer-clojure :exclude [defn])
  (:require [clojure.spec.alpha :as s]
            [better-cond.core :as b]
            [recap.caption.cue :as cue]
            [recap.caption.data-specs :as dspecs]
            [recap.caption.speaker :as speaker]
            [recap.config :as cfg]
            [utils.common :as u]
            [utils.specin :refer [defn]]
            [utils.results :as r]))


(declare group-lines
         start-new-cue?)


(s/def ::force-new-cue-tolerance-secs float?)
(s/def ::max-lines-per-cue int?)


(def default-opts (cfg/load-config))


(defn restitch
  "Join cues in the given captions for better readability, based on
  punctuation, quiet gaps, etc."
  {:args (s/cat :caption ::dspecs/caption
                :kwargs (s/keys* :opt-un []))
   :ret ::dspecs/caption}
  [caption & {:keys [opts]
              :or {opts default-opts}}]
  (b/cond
    (empty? (:cues caption))
    caption

    let [cues (mapv cue/join-lines (:cues caption))]

    :else
    (loop [[curr-input-cue & rest-input-cues] (rest cues)
           wip-cue (first cues)
           final-cues []]
      (if (empty? rest-input-cues)
        (-> caption
            (assoc :cues
                   (conj final-cues (cue/join-cues [wip-cue curr-input-cue]
                                                   :concat-lines? true)))
            (group-lines :max-lines-per-cue (:max-lines-per-cue opts)))
        (if (start-new-cue? wip-cue curr-input-cue opts)
          (recur rest-input-cues
                 curr-input-cue
                 (conj final-cues wip-cue))
          (recur rest-input-cues
                 (cue/join-cues [wip-cue curr-input-cue]
                                :concat-lines? true)
                 final-cues))))))


(defn clause-ender?
  {:args (s/cat :text string?)
   :ret boolean?}
  [text]
  (boolean (re-find (:ends-with-clause-ending-punctuation default-opts) text)))


(defn punctuation-ender?
  {:args (s/cat :text string?)
   :ret boolean?}
  [text]
  (boolean (re-find (:ends-with-any-punctuation default-opts) text)))


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
                     (:force-new-cue-tolerance-secs default-opts)}}]
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
        last-char (nth cue-text (dec cue-text-length))
        second-last-char (nth cue-text (- cue-text-length 2) nil)]
    (and (>= cue-text-length (:breakable-clause-ender-min-chars default-opts))
         (or (= last-char \.) (= last-char \?) (= last-char \!))
         (not (= second-last-char \.)))))


(defn group-lines
  "Group cues according to `max-lines-per-cue` and respecting longish quiet
  gaps."
  {:args (s/cat :caption ::dspecs/caption
                :kwargs (s/keys* :opt-un [::max-lines-per-cue]))
   :ret ::dspecs/caption}
  [caption & {:keys [max-lines-per-cue]
              :or {max-lines-per-cue (:max-lines-per-cue default-opts)}}]
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
