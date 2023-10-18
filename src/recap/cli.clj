(ns recap.cli
  "Command-line interface abstraction."
  (:refer-clojure :exclude [defn])
  (:require [clojure.pprint :as pprint]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [puget.printer :as puget]
            [utils.common :as u]
            [utils.specin :refer [defn]]
            [utils.results :as r]
            [recap.caption :as cap]
            [recap.caption.linger :as linger]
            [recap.caption.restitch :as restitch]
            [recap.trint :as trint]))


(set! *warn-on-reflection* true) ; for graalvm


(def global-opt-cmds
  "A set of arguments that take the form of global options (by convention) but behave like
  sub-commands."
  #{"-h" "--help" "--version"})


(def version (-> (slurp "VERSION") str/trim))
(def help (-> (slurp "HELP") (format version)))


(defn check-args
  {:args (s/cat :args (s/coll-of string?))
   :ret ::r/result}
  [args]
  (if (or (empty? args)
          (str/blank? (first args)))
    (r/r :error "No sub-command specified. Try running: recap --help"
         {:args args})
    (r/r :success "Some arguments found"
         {:args args})))

(defn extract-global-opts
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [args] :as _result}]
  (loop [rest-args args
         curr-arg (first rest-args)
         global-opts []]
    (if (and curr-arg (str/starts-with? curr-arg "-"))
      (recur (rest rest-args)
             (first (rest rest-args))
             (conj global-opts curr-arg))
      (r/r :success (format "Extracted %d global option(s)" (count global-opts))
           {:args rest-args
            :global-opts global-opts}))))

(defn extract-sub-cmd-and-args
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [global-opts args] :as _result}]
  (if-let [sub-cmd  (u/find-first global-opt-cmds global-opts)]
    (r/r :success (format "Extracted global option, '%s' as sub-command" sub-cmd)
         {:global-opts global-opts
          :sub-cmd sub-cmd
          :args []})
    (r/r :success (format "Extracted sub-command '%s'" (first args))
         {:global-opts global-opts
          :sub-cmd (first args)
          :args (rest args)})))

(defn parse-cli-args
  "Parse the given CLI arguments."
  [args]
  {:args (s/cat :args (s/coll-of string?))
   :ret ::r/result}
  (r/while-success-> (check-args args)
                     extract-global-opts
                     extract-sub-cmd-and-args))

(defn contiguous-sub-cmd
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [args] :as _result}]
  (r/while-success->> (u/slurp-file (first args))
                      cap/strip-contiguous-speaker-tags
                      (r/r :success)))

(defn linger-sub-cmd
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [args] :as _result}]
  (let [linger-secs (u/parse-float (second args) :fallback linger/max-linger-secs-default)
        apply-linger #(linger/linger-cues % :max-linger-secs linger-secs)]
    (r/while-success->> (u/slurp-file (first args))
                        cap/parse
                        apply-linger
                        cap/to-string
                        (r/r :success))))

(defn overlap-sub-cmd
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [args] :as _result}]
  (letfn [(indeces->result [indeces]
            (if (empty? indeces)
              (r/r :success "No overlapping cues found")
              (r/r :success (format "Found %d overlapping cue(s) at the following positions:\n%s"
                                    (count indeces)
                                    (str/join ", " indeces)))))]
    (r/while-success-> (u/slurp-file (first args))
                       cap/parse
                       :cues
                       cap/find-overlapping-cues
                       indeces->result)))

(defn parse-sub-cmd
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [args] :as _result}]
  (let [no-colour? (-> args second (or "") str/trim str/lower-case (= "false"))
        print-maybe-coloured #(do
                                (if no-colour?
                                  (pprint/pprint %)
                                  (puget/cprint %))
                                "")]
    (r/while-success->> (u/slurp-file (first args))
                        cap/parse
                        print-maybe-coloured
                        (r/r :success))))

(defn restitch-sub-cmd
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [args] :as _result}]
  (r/while-success->> (u/slurp-file (first args))
                      cap/strip-contiguous-speaker-tags
                      cap/parse
                      restitch/restitch
                      cap/to-string
                      (r/r :success)))

(defn text-sub-cmd
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [args] :as _result}]
  (r/while-success->> (u/slurp-file (first args))
                      cap/parse
                      :cues
                      cap/to-plain-text
                      (r/r :success)))

(defn trint-dl-sub-cmd
  {:args (s/cat :_result ::r/result)
   :ret ::r/result}
  [{:keys [args] :as _result}]
  (r/while-success->> (trint/get-document-captions (-> args first keyword)
                                                   (second args))
                      (r/r :success)))

(defn run-sub-cmd
  {:args (s/cat :result ::r/result)
   :ret ::r/result}
  [{:keys [sub-cmd] :as result}]
  (case sub-cmd
    ("-h" "--help")
    (r/r :success help)

    "--version"
    (r/r :success version)

    "contiguous"
    (contiguous-sub-cmd result)

    "linger"
    (linger-sub-cmd result)

    "overlap"
    (overlap-sub-cmd result)

    "parse"
    (parse-sub-cmd result)

    "restitch"
    (restitch-sub-cmd result)

    "text"
    (text-sub-cmd result)

    "trint-dl"
    (trint-dl-sub-cmd result)

    (assoc result
           :level :error
           :message "Unrecognised sub-command. Try running: recap --help")))
