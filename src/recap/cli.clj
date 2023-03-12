(ns recap.cli
  "Command-line interface abstraction."
  (:refer-clojure :exclude [defn])
  (:require [clojure.pprint :as pprint]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [puget.printer :as puget]
            [utils.common :as c]
            [utils.specin :refer [defn]]
            [utils.results :as r]
            [recap.caption :as cap]
            [recap.caption.copyright :as copyright]
            [recap.caption.linger :as linger]
            [recap.caption.restitch :as restitch]))


(def version (-> (slurp "VERSION")
                 str/trim))
(def help (-> (slurp "HELP")
              (format version)))

(def primary-commands #{:contiguous :copyright :linger :overlap :parse :restitch :text})

(s/def ::cmd-name keyword?)
(s/def ::cmd-args (s/coll-of string?))
(s/def ::cmd-parse-r (s/keys :req-un [:r/level :r/message
                                      ::cmd-name ::cmd-args]))


(defn parse
  "Parse the given CLI arguments."
  {:args (s/cat :args (s/coll-of string?))
   :ret ::cmd-parse-r}
  [args]
  (if (empty? args)
    (r/r :error "No command specified. Try running: recap --help"
         {:cmd-name nil
          :cmd-args []})

    (let [cmd-name (first args)
          cmd-kw (-> cmd-name str/lower-case keyword)]
      (cond
        (contains? #{:help :--help :-h} cmd-kw)
        (r/r :success ""
             {:cmd-name :help
              :cmd-args []})

        (contains? #{:version :--version} cmd-kw)
        (r/r :success ""
             {:cmd-name :version
              :cmd-args []})

        (contains? primary-commands cmd-kw)
        (r/r :success ""
             {:cmd-name cmd-kw
              :cmd-args (rest args)})

        :else
        (r/r :error (c/fmt ["Unrecognised command/option: '%s'. Try running: "
                            "recap --help"]
                           (first args))
             {:cmd-name cmd-kw
              :cmd-args []})))))


(defn run-cmd
  "Action the specified CLI command."
  {:args (s/cat :cmd-parse-r ::cmd-parse-r)
   :ret ::r/result}
  [cmd-parse-r]
  (when (r/failed? cmd-parse-r)
    (c/exit! cmd-parse-r))

  (case (:cmd-name cmd-parse-r)
    :help
    (r/r :success help)

    :version
    (r/r :success version)

    :contiguous
    (b/cond
      let [slurp-r (c/slurp-file (-> cmd-parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (r/prepend-msg slurp-r "Attempt to read captions file failed: ")

      :else
      (r/r :success (cap/strip-contiguous-speaker-tags slurp-r)))

    :copyright
    (b/cond
      let [slurp-r (c/slurp-file (-> cmd-parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (r/prepend-msg slurp-r "Attempt to read captions file failed: ")

      let [cap-parse-r (cap/parse slurp-r)]

      (r/failed? cap-parse-r)
      cap-parse-r

      let [media-duration-str (-> cmd-parse-r :cmd-args second)
           media-duration-secs (if (str/includes? media-duration-str ":")
                                 (c/duration->secs media-duration-str)
                                 (c/parse-float media-duration-str
                                                :fallback 0))]

      (zero? media-duration-secs)
      (r/r :error (str "Media duration should be a positive number but was: "
                       media-duration-str))

      let [copyright-file-arg (-> cmd-parse-r :cmd-args (nth 2 ""))
           has-copyright-date? (str/includes? copyright-file-arg ":")
           [copyright-file-name copyright-date] (str/split copyright-file-arg #":")
           copyright-file-r (c/slurp-file copyright-file-name)]

      (r/failed? copyright-file-r)
      (r/prepend-msg copyright-file-r
                     "Attempt to read copyright file failed: ")

      (str/blank? copyright-file-r)
      (r/r :warn
           (str "No copyright content found in file: " copyright-file-name))

      let [copyright-lines (-> copyright-file-r
                               str/trim
                               str/split-lines)
           cue-duration (-> cmd-parse-r
                            :cmd-args
                            (nth 3 nil)
                            (or (:cue-duration copyright/defaults)))
           cue-gap (-> cmd-parse-r
                       :cmd-args
                       (nth 3 nil)
                       (or (:cue-gap copyright/defaults)))
           end-gap (-> cmd-parse-r
                       :cmd-args
                       (nth 3 nil)
                       (or (:end-gap copyright/defaults)))]

      :else
      (r/r :success (-> cap-parse-r
                        (copyright/append media-duration-secs
                                          copyright-lines
                                          :date copyright-date
                                          :cue-duration cue-duration
                                          :cue-gap cue-gap
                                          :end-gap end-gap)
                        cap/to-string)))

    :overlap
    (b/cond
      let [slurp-r (c/slurp-file (-> cmd-parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (r/prepend-msg slurp-r "Attempt to read captions file failed: ")

      let [cap-parse-r (cap/parse slurp-r)]

      (r/failed? cap-parse-r)
      cap-parse-r

      :else
      (let [indeces (cap/find-overlapping-cues (:cues cap-parse-r))]
        (if (empty? indeces)
          (r/r :success "No overlapping cues found")
          (r/r :success (c/fmt ["Found %d overlapping cue(s) at the following "
                                "positions:\n%s"]
                               (count indeces)
                               (str/join ", " indeces))))))

    :parse
    (b/cond
      let [slurp-r (c/slurp-file (-> cmd-parse-r :cmd-args first))
           second-arg (-> cmd-parse-r :cmd-args second str str/trim str/lower-case)
           colourise? (and (not (str/blank? second-arg))
                           (not= "false" second-arg))]

      (r/failed? slurp-r)
      (r/prepend-msg slurp-r "Attempt to read file failed: ")

      let [cap-parse-r (cap/parse slurp-r)]

      (r/failed? cap-parse-r)
      cap-parse-r

      :else
      (do
        (if colourise?
          (puget/cprint cap-parse-r)
          (pprint/pprint cap-parse-r))
        (r/r :success "")))

    :linger
    (b/cond
      let [slurp-r (c/slurp-file (-> cmd-parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (r/prepend-msg slurp-r "Attempt to read captions file failed: ")

      let [cap-parse-r (cap/parse slurp-r)]

      (r/failed? cap-parse-r)
      cap-parse-r

      let [linger-secs (-> cmd-parse-r
                           :cmd-args
                           second
                           (c/parse-float :fallback linger/max-linger-secs-default))]

      :else
      (r/r :success (-> cap-parse-r
                        (linger/linger-cues :max-linger-secs linger-secs)
                        cap/to-string)))


    :restitch
    (b/cond
      let [slurp-r (c/slurp-file (-> cmd-parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (r/prepend-msg slurp-r "Attempt to read captions file failed: ")

      let [cap-parse-r (-> slurp-r
                           cap/strip-contiguous-speaker-tags
                           cap/parse)]

      (r/failed? cap-parse-r)
      cap-parse-r

      :else
      (r/r :success (-> cap-parse-r restitch/restitch cap/to-string)))

    :text
    (b/cond
      let [slurp-r (c/slurp-file (-> cmd-parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (r/prepend-msg slurp-r "Attempt to read file failed: ")

      let [cap-parse-r (cap/parse slurp-r)]

      (r/failed? cap-parse-r)
      cap-parse-r

      :else
      (do
        (-> cap-parse-r :cues cap/to-plain-text println)
        (r/r :success "")))))


(comment
  (let [cap-parse-r (-> (c/slurp-file "tmp/one-word.srt")
                        cap/strip-contiguous-speaker-tags
                        cap/parse)]
    (if (r/failed? cap-parse-r)
      cap-parse-r
      (r/r :success (-> cap-parse-r restitch/restitch #_cap/to-string)))))
