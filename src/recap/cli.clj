(ns recap.cli
  "Command-line interface abstraction."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [puget.printer :as puget]
            [utils.common :as c]
            [utils.results :as r]
            [recap.caption :as cap]))



(def version (-> (slurp "VERSION")
                 str/trim))
(def help (-> (slurp "HELP")
              (format version)))



(s/def ::cmd-name keyword?)
(s/def ::cmd-args (s/coll-of string?))
(s/def ::parse-r (s/keys :req-un [:r/level :r/message
                                  ::cmd-name ::cmd-args]))
(s/fdef parse
        :args (s/cat :args (s/coll-of string?))
        :ret ::parse-r)

(defn parse
  "Parse the given CLI arguments."
  [args]
  (if (empty? args)
    (r/r :error "No command specified. Try running: recap --help"
         :cmd-name nil
         :cmd-args [])

    (let [cmd-name (first args)
          cmd-kw (-> cmd-name str/lower-case keyword)]
      (cond
        (contains? #{:help :--help :-h} cmd-kw)
        (r/r :success ""
             :cmd-name :help
             :cmd-args [])

        (contains? #{:version :--version} cmd-kw)
        (r/r :success ""
             :cmd-name :version
             :cmd-args [])

        (contains? #{:contiguous :overlap :parse :rebuild} cmd-kw)
        (r/r :success ""
             :cmd-name cmd-kw
             :cmd-args (rest args))

        :else
        (r/r :error (c/fmt ["Unrecognised command/option: '%s'. Try running: "
                            "recap --help"]
                           (first args))
             :cmd-name cmd-kw
             :cmd-args [])))))



(s/fdef action!
        :args (s/cat :cli-r ::parse-r)
        :ret nil?)

(defn action!
  "Action the specified CLI command."
  [parse-r]
  (when (r/failed? parse-r)
    (c/abort 1 (:message parse-r)))

  (case (:cmd-name parse-r)
    :help (println help)

    :version (println version)

    :contiguous
    (b/cond
      let [slurp-r (c/slurp-file (-> parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed: " (:message slurp-r)))

      :else
      (println (cap/strip-contiguous-speaker-tags slurp-r)))

    :overlap
    (b/cond
      let [slurp-r (c/slurp-file (-> parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed: " (:message slurp-r)))

      let [parse-r (cap/parse slurp-r)]

      (r/failed? parse-r)
      (c/abort 1 (:message parse-r))

      :else
      (let [indeces (cap/find-overlapping-cues (:cues parse-r))]
        (if (empty? indeces)
          (println "No overlapping cues found")
          (println (c/fmt ["Found %d overlapping cue(s) at the following "
                           "positions:\n%s"]
                          (count indeces)
                          (str/join ", " indeces))))))

    :parse
    (b/cond
      let [slurp-r (c/slurp-file (-> parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed: " (:message slurp-r)))

      let [parse-r (cap/parse slurp-r)]

      (r/failed? parse-r)
      (c/abort 1 (:message parse-r))

      :else
      (puget/cprint parse-r))

    :rebuild
    (b/cond
      let [slurp-r (c/slurp-file (-> parse-r :cmd-args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed: " (:message slurp-r)))

      let [parse-r (-> slurp-r
                       cap/strip-contiguous-speaker-tags
                       cap/parse)]

      (r/failed? parse-r)
      (c/abort 1 (:message parse-r))

      :else
      (println (cap/rebuild parse-r)))))

