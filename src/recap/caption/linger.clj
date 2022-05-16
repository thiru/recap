(ns recap.caption.linger
  (:require [clojure.spec.alpha :as s]
            [better-cond.core :as b]
            [recap.caption.cue :as cue]
            [recap.caption.data-specs :as dspecs]
            [utils.common :as u]
            [utils.results :as r]))


(s/fdef linger-cues
        :args (s/cat :caption ::dspecs/caption
                     :max-linger-secs int?)
        :ret ::dspecs/caption)

(defn linger-cues
  "Extend the time cues appear on screen, to allow viewers more time to read.

  This elimiates most gaps between cues. An upper limit (in seconds) is defined
  by `max-linger-secs`."
  [caption & {:keys [max-linger-secs]
              :or {max-linger-secs 3}}]
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
            (> last-cue-total-secs max-linger-secs)
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
                                  (- max-linger-secs last-cue-total-secs))
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

        let [gap-to-curr-cue (cue/gap-inbetween prev-cue curr-cue)]

        (r/failed? gap-to-curr-cue)
        (do (r/print-msg
              (r/prepend-msg gap-to-curr-cue
                             (str "Failed to determine gap between cues. "
                                  "Skipping ahead. ")))
            (recur curr-cue
                   rest-cues
                   (conj new-cues prev-cue)))

        let [has-gap? (pos? gap-to-curr-cue)]

        (not has-gap?)
        (recur curr-cue
               rest-cues
               (conj new-cues prev-cue))

        let [gap-under-max-linger (<= gap-to-curr-cue max-linger-secs)
             prev-cue-extended (assoc prev-cue :end (:start curr-cue))]

        gap-under-max-linger
        (recur curr-cue
               rest-cues
               (conj new-cues prev-cue-extended))

        :else
        (let [new-end-secs (+ prev-cue-end-secs max-linger-secs)
              new-end-duration (-> (* 1000 new-end-secs)
                                   (u/millis->duration :show-millis? true))
              prev-cue-extended (assoc prev-cue :end new-end-duration)]
          (recur curr-cue
                 rest-cues
                 (conj new-cues prev-cue-extended)))))))
