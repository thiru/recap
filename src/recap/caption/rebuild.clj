(ns recap.caption.rebuild
  (:require [clojure.spec.alpha :as s]
            [better-cond.core :as b]
            [recap.caption.cue :as cue]
            [recap.caption.specs :as spec]
            [recap.caption.utils :as capu]
            [utils.common :as u]
            [utils.results :as r]))



(def default-opts
  {:absolute-max-chars-per-line 65
   :breakable-clause-ender-min-chars 18
   :breakable-any-punctuation-min-chars 23
   ;; The maximum number of seconds to extend the time a cue remains on screen.
   ;; This is used to give the reader more time to read the captions. The
   ;; subtitles team does something similar.
   :cue-max-linger-secs 3
   ;; Same as `:ends-in-clause-ending-punctuation` except adds a comma
   :ends-in-any-punctuation #"[,.!?;:\]'\"—–-]['\"]?$"
   :ends-in-clause-ending-punctuation #"[.!?;:\]'\"—–-]['\"]?$"
   :force-new-cue-tolerance-seconds 3 ; TODO
   :ideal-max-chars-per-line 35})






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
        :args (s/cat :wip-cue ::spec/cue :next-cue ::spec/cue :opts map?)
        :ret boolean?)

(defn start-new-cue?
  "Determine whether this is an ideal point to create a cue. I.e to start a
  new cue at `wip-cue`."
  [wip-cue next-cue opts]
  (b/cond
    let [wip-cue-text (-> wip-cue :lines last (or ""))
         next-cue-text (-> next-cue :lines last (or ""))]

    ;; Always start a new line on speaker tag
    (capu/get-speaker-tag next-cue-text)
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


(s/fdef linger-cues
        :args (s/cat :caption ::spec/caption)
        :ret ::spec/caption)

(defn linger-cues
  "Extend the time cues appear on screen, to allow viewers more time to read.

  This elimiates most gaps between cues. There is an upper limit defined by
  `:cue-max-linger-secs`."
  [caption]
  (if (<= (-> caption :cues count) 1)
    caption
    ;; Skip first cue since we're always comparing with previous cues
    (loop [prev-cue (-> caption :cues first)
           [curr-cue & rest-cues] (-> caption :cues rest)
           new-cues []]
      (b/cond
        (and (empty? curr-cue) (empty? rest-cues))
        ;; NOTE: ensure the last cue is on screen for a reasonable amount of
        ;; time. Auto-generated captions (e.g. from Otter) sometimes only shows
        ;; the last cue very briefly.
        (let [last-cue (-> caption :cues last)
              last-cue-total-secs (cue/total-secs last-cue)
              last-cue-end-secs (u/duration->secs (:end last-cue))]
          (cond
            (> last-cue-total-secs (:cue-max-linger-secs default-opts))
            (assoc caption :cues (conj new-cues last-cue))

            (r/failed? last-cue-end-secs)
            (do (r/print-msg
                  (r/prepend-msg last-cue-end-secs
                                 (u/fmt ["Last cue has an invalid end "
                                         "duration: %s. Not adjusting. "
                                         (:end prev-cue)])))
                (assoc caption :cues (conj new-cues last-cue)))

            :else
            (let [new-end-secs (+ last-cue-end-secs
                                  (- (:cue-max-linger-secs default-opts)
                                     last-cue-total-secs))
                  new-end-duration (-> (* 1000 new-end-secs)
                                       (u/millis->duration :show-millis? true))
                  last-cue-extended (assoc last-cue :end new-end-duration)]
              (assoc caption :cues (conj new-cues last-cue-extended)))))

        let [prev-cue-end-secs (u/duration->secs (:end prev-cue))]

        (r/failed? prev-cue-end-secs)
        (do (r/print-msg
              (r/prepend-msg prev-cue-end-secs
                             (u/fmt ["Cue has an invalid end duration: %s. "
                                     "Skipping ahead. "
                                     (:end prev-cue)])))
            (recur curr-cue
                   rest-cues
                   (conj new-cues prev-cue)))

        let [curr-cue-start-secs (u/duration->secs (:start curr-cue))]

        (r/failed? prev-cue-end-secs)
        (do (r/print-msg
              (r/prepend-msg curr-cue-start-secs
                             (u/fmt ["Cue has an invalid start duration: %s. "
                                     "Skipping ahead. "
                                     (:start curr-cue)])))
            (recur curr-cue
                   rest-cues
                   (conj new-cues prev-cue)))

        let [gap-to-curr-cue (- curr-cue-start-secs prev-cue-end-secs)
             has-gap? (pos? gap-to-curr-cue)]

        (not has-gap?)
        (recur curr-cue
               rest-cues
               (conj new-cues prev-cue))

        let [gap-under-max-linger (<= gap-to-curr-cue
                                      (:cue-max-linger-secs default-opts))
             prev-cue-extended (assoc prev-cue :end (:start curr-cue))]

        gap-under-max-linger
        (recur curr-cue
               rest-cues
               (conj new-cues prev-cue-extended))

        :else
        (let [new-end-secs (+ prev-cue-end-secs
                              (:cue-max-linger-secs default-opts))
              new-end-duration (-> (* 1000 new-end-secs)
                                   (u/millis->duration :show-millis? true))
              prev-cue-extended (assoc prev-cue :end new-end-duration)]
          (recur curr-cue
                 rest-cues
                 (conj new-cues prev-cue-extended)))))))



(comment
  (clause-ender? "sdf;"))
