(ns recap.cli
  "Command-line interface abstraction."
  (:refer-clojure :exclude [defn])
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [puget.printer :as puget]
            [utils.common :as c]
            [utils.specin :refer [defn]]
            [utils.results :as r]
            [recap.caption :as cap]
            [recap.caption.restitch :as restitch]))


(def version (-> (slurp "VERSION")
                 str/trim))
(def help (-> (slurp "HELP")
              (format version)))

(def primary-commands #{:contiguous :overlap :parse :restitch})

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
      let [slurp-r (c/slurp-file (-> cmd-parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (r/prepend-msg slurp-r "Attempt to read file failed: ")

      let [cap-parse-r (cap/parse slurp-r)]

      (r/failed? cap-parse-r)
      cap-parse-r

      :else
      (puget/cprint cap-parse-r)
      (r/r :success ""))

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
      (r/r :success (-> cap-parse-r restitch/restitch cap/to-string)))))

