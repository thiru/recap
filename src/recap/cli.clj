(ns recap.cli
  "Command-line interface abstraction."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [better-cond.core :as b]
            [puget.printer :as puget]
            [utils.common :as c]
            [utils.results :as r]
            [recap.app :as app]
            [recap.caption :as cap]))



(def usage
  (->> [(str app/app-name " " app/version)
        app/author
        ""
        app/description
        ""
        "USAGE: recap <command> [<args>]"
        ""
        "COMMANDS:"
        ""
        "contiguous <FILE>"
        "  Find contiguous same speaker tags and remove them"
        ""
        "help, --help, -h"
        "  Show this help"
        ""
        "parse <FILE>"
        "  Parse the given caption file and output as an EDN map"
        ""
        "overlap <FILE>"
        "  Find overlapping cues in the given caption file"
        ""
        "rebuild <FILE>"
        "  Join cues for better readability (based on punctuation)"
        ""
        "version, --version"
        "  Show current version"]
       (str/join "\n")))



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
    (r/r :error "No command specified"
         :cmd-name nil
         :cmd-args [])

    (let [cmd (-> args first str/lower-case keyword)]
      (cond
        (contains? #{:help :--help :-h} cmd)
        (r/r :success ""
             :cmd-name :help
             :cmd-args [])

        (contains? #{:version :--version} cmd)
        (r/r :success ""
             :cmd-name :version
             :cmd-args [])

        (contains? #{:parse :overlap :contiguous :rebuild} cmd)
        (r/r :success ""
             :cmd-name cmd
             :cmd-args (rest args))

        :else
        (r/r :error (format "Unrecognised command '%s'" (first args))
             :cmd-name cmd
             :cmd-args [])))))



(s/fdef action!
        :args (s/cat :cli-r ::parse-r)
        :ret nil?)

(defn action!
  "Action the specified CLI command."
  [cli-r]
  (when (r/failed? cli-r)
    (c/abort 1 (:message cli-r)))

  (case (:cmd-name cli-r)
    :help (println usage)

    :version (println app/version)

    :parse
    (b/cond
      let [slurp-r (c/slurp-file (-> cli-r :cmd-args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed because of the following error: "
                      (:message slurp-r)))

      let [parse-r (cap/parse slurp-r)]

      (r/failed? parse-r)
      (c/abort 1 (:message parse-r))

      :else
      (puget/cprint parse-r))

    :overlap
    (b/cond
      let [slurp-r (c/slurp-file (-> cli-r :cmd-args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed because of the following error: "
                      (:message slurp-r)))

      let [parse-r (cap/parse slurp-r)]

      (r/failed? parse-r)
      (c/abort 1 (:message parse-r))

      :else
      (let [indeces (cap/find-overlapping-cues (:cues parse-r))]
        (if (empty? indeces)
          (println "No overlapping cues found")
          (println
            (format "Found %d overlapping cue(s) at the following positions:\n%s"
                    (count indeces)
                    (str/join ", " indeces))))))

    :contiguous
    (b/cond
      let [slurp-r (c/slurp-file (-> cli-r :cmd-args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed because of the following error: "
                      (:message slurp-r)))

      :else
      (println (cap/strip-contiguous-speaker-tags slurp-r)))

    :rebuild
    (b/cond
      let [slurp-r (c/slurp-file (-> cli-r :cmd-args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed because of the following error: "
                      (:message slurp-r)))

      let [parse-r (-> slurp-r
                       cap/strip-contiguous-speaker-tags
                       cap/parse)]

      (r/failed? parse-r)
      (c/abort 1 (:message parse-r))

      :else
      (println (cap/rebuild parse-r)))))
