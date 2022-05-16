(ns recap.caption.restitch
  (:require [clojure.spec.alpha :as s]
            [better-cond.core :as b]
            [recap.caption.cue :as cue]
            [recap.caption.data-specs :as dspecs]
            [recap.caption.speaker :as speaker]
            [utils.common :as u]))


(declare start-new-cue?)


(def default-opts
  {:absolute-max-chars-per-line 65
   :breakable-clause-ender-min-chars 18
   :breakable-any-punctuation-min-chars 23
   ;; Same as `:ends-in-clause-ending-punctuation` except adds a comma
   :ends-in-any-punctuation #"[,.!?;:\]'\"—–-]['\"]?$"
   :ends-in-clause-ending-punctuation #"[.!?;:\]'\"—–-]['\"]?$"
   :force-new-cue-tolerance-seconds 3 ; TODO
   :ideal-max-chars-per-line 35})



(s/fdef restitch
        :args (s/cat :caption ::dspecs/caption)
        :ret ::dspecs/caption)

(defn restitch
  "Join cues in the given captions for better readability, based on
  punctuation, quiet gaps, etc."
  [caption & {:keys [opts]
              :or {opts default-opts}}]
  (if (empty? (:cues caption))
    caption
    (loop [[curr-input-cue & rest-input-cues] (-> caption :cues rest)
           wip-cue (-> caption :cues first)
           final-cues []]
      (if (empty? rest-input-cues)
        (-> caption
            (assoc :cues
                   (conj final-cues (cue/join-cues [wip-cue curr-input-cue]))))
        (if (start-new-cue? wip-cue curr-input-cue opts)
          (recur rest-input-cues
                 curr-input-cue
                 (conj final-cues wip-cue))
          (recur rest-input-cues
                 (cue/join-cues [wip-cue curr-input-cue])
                 final-cues))))))



(s/fdef clause-ender?
        :args (s/cat :text string?)
        :ret boolean?)

(defn clause-ender?
  [text]
  (boolean (re-find (:ends-in-clause-ending-punctuation default-opts) text)))



(s/fdef punctuation-ender?
        :args (s/cat :text string?)
        :ret boolean?)

(defn punctuation-ender?
  [text]
  (boolean (re-find (:ends-in-any-punctuation default-opts) text)))



(s/fdef start-new-cue?
        :args (s/cat :wip-cue ::dspecs/cue :next-cue ::dspecs/cue :opts map?)
        :ret boolean?)

(defn start-new-cue?
  "Determine whether this is an ideal point to create a cue. I.e to start a
  new cue at `wip-cue`."
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


(comment
  (clause-ender? "sdf;"))
