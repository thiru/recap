(ns recap.caption.rebuild
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [utils.common :as u]))



(def default-opts {:absolute-max-chars-per-line 65
                   :breakable-clause-ender-min-chars 18
                   :breakable-any-punctuation-min-chars 23
                   ;; Same as `:ends-in-clause-ending-punctuation` but adds a comma
                   :ends-in-any-punctuation #"[,.!?;:\]'\"—–-]['\"]?$"
                   :ends-in-clause-ending-punctuation #"[.!?;:\]'\"—–-]['\"]?$"
                   :force-new-cue-tolerance-seconds 3
                   :ideal-max-chars-per-line 35})



(defn cue-char-count
  "Count the number of characters in the given cue."
  [cue]
  (loop [[line & rest-lines] (:lines cue)
         cnt 0]
    (if (nil? rest-lines)
      (+ cnt (count line)
         ;; Count lines as a newline chars:
         (-> cue :lines count dec))
      (recur rest-lines (+ cnt (count line))))))

(defn clause-ender?
  [text]
  (boolean (re-find (:ends-in-clause-ending-punctuation default-opts) text)))

(defn punctuation-ender?
  [text]
  (boolean (re-find (:ends-in-any-punctuation default-opts) text)))

(defn start-new-cue?
  "Determine whether this is an ideal point to create a cue. I.e to start a
  new cue at `wip-cue`."
  [wip-cue next-cue opts]
  (b/cond
    let [wip-cue-char-count (cue-char-count wip-cue)
         next-cue-char-count (cue-char-count next-cue)]

    ;; Never go above the absolute maximum number of chars allows in a line
    (<= (:absolute-max-chars-per-line opts) (+ wip-cue-char-count
                                               1 ; space between
                                               next-cue-char-count))
    true

    ;; Avoid lines starting with a single word ending in a punctuation mark,
    ;; unless previous line ends in a clause-ending punctuation mark
    (and (punctuation-ender? (-> next-cue :lines last))
         (not (clause-ender? (-> wip-cue :lines last))))
    false

    ;; Break line if the current cue ends in a clause-ending punctuation mark
    ;; and the minimum number of chars for this is reached
    (and (>= wip-cue-char-count
             (:breakable-clause-ender-min-chars opts))
         (clause-ender? (-> wip-cue :lines last)))
    true

    ;; Break line if the current cue ends in any punctuation mark and the next
    ;; word does not end in a punctuation mark, while the minimum number of
    ;; chars for this is reached
    (and (>= wip-cue-char-count
             (:breakable-any-punctuation-min-chars opts))
         (punctuation-ender? (-> wip-cue :lines last))
         (not (punctuation-ender? (-> next-cue :lines last))))
    true

    let [wip-cue-is-opener? (re-find #"^['\[]"
                                     (-> wip-cue :lines first (or "")))
         wip-cue-is-closer? (re-find #"['\]],?$"
                                     (-> wip-cue :lines last (or "")))]

    ;; Avoid lines starting with a single dangling word that ends a quote or
    ;; bracket:
    (and wip-cue-is-closer? (not wip-cue-is-opener?))
    false

    :else
    (<= (:ideal-max-chars-per-line opts)
        wip-cue-char-count)))



(comment
  (cue-char-count {:lines ["abc def"]})
  (clause-ender? "sdf;"))
