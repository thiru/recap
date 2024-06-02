(ns recap.cli
  "Command-line interface abstraction."
  (:refer-clojure :exclude [defn])
  (:require [clojure.pprint :as pprint]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [puget.printer :as puget]
            [recap.caption :as cap]
            [recap.caption.linger :as linger]
            [recap.caption.restitch :as restitch]
            [recap.fixup :as fixup]
            [recap.sonix :as sonix]
            [recap.trint :as trint]
            [recap.utils.common :as u]
            [recap.utils.specin :refer [defn]]
            [recap.utils.results :as r]))


(set! *warn-on-reflection* true) ; for graalvm


(s/def ::cli-args (s/coll-of string?))
(s/def ::global-opt #(str/starts-with? % "-"))
(s/def ::global-opts (s/coll-of ::global-opt))
(s/def ::sub-cmd (s/nilable string?))
(s/def ::stdin (s/nilable string?))
(s/def ::cli-r (s/and ::r/result
                      (s/keys :opt-un [::cli-args ::global-opts ::sub-cmd ::stdin])))


(def global-opt-cmds
  "A set of arguments that take the form of global options (by convention) but behave like
  sub-commands."
  #{"-h" "--help" "--version"})


(def version (-> (slurp "VERSION") str/trim))
(def help (-> (slurp "HELP") (format version)))


(defn check-args
  {:args (s/cat :args ::cli-args)
   :ret ::cli-r}
  [args]
  (if (or (empty? args)
          (str/blank? (first args)))
    (r/r :error "No sub-command specified. Try running: recap --help"
         {:args args})
    (r/r :success "Some arguments found"
         {:args args})))

(defn extract-global-opts
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args] :as _cli-r}]
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
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [global-opts args] :as _cli-r}]
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
  {:args (s/cat :args ::cli-args)
   :ret ::cli-r}
  (r/while-success-> (check-args args)
                     extract-global-opts
                     extract-sub-cmd-and-args))

(defn contiguous-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (r/while-success->> (or stdin (u/slurp-file (first args)))
                      cap/strip-contiguous-speaker-tags
                      (r/r :success)))

(defn essay-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (let [max-chars (-> args second
                      (u/parse-int :fallback cap/default-max-chars-per-para))
        to-essay-form #(cap/to-essay-form % :max-chars-per-para max-chars)]
    (r/while-success->> (or stdin (u/slurp-file (first args)))
                        cap/parse
                        :cues
                        to-essay-form
                        (r/r :success))))

(defn fixup-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (letfn [(clean-cue-lines [caption]
            (update caption
                    :cues
                    (fn [cues]
                      (mapv (fn [cue]
                              (update cue :lines #(mapv fixup/fixup %)))
                            cues))))]
    (r/while-success->> (or stdin (u/slurp-file (first args)))
                        cap/parse
                        clean-cue-lines
                        cap/to-string
                        (r/r :success))))

(defn linger-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (let [linger-secs (u/parse-float (second args) :fallback linger/max-linger-secs-default)
        apply-linger #(linger/linger-cues % :max-linger-secs linger-secs)]
    (r/while-success->> (or stdin (u/slurp-file (first args)))
                        cap/parse
                        apply-linger
                        cap/to-string
                        (r/r :success))))

(defn overlap-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (letfn [(indeces->result [indeces]
            (if (empty? indeces)
              (r/r :success "No overlapping cues found")
              (r/r :success (format "Found %d overlapping cue(s) at the following positions:\n%s"
                                    (count indeces)
                                    (str/join ", " indeces)))))]
    (r/while-success-> (or stdin (u/slurp-file (first args)))
                       cap/parse
                       :cues
                       cap/find-overlapping-cues
                       indeces->result)))

(defn parse-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (let [no-colour? (-> args second (or "") str/trim str/lower-case (= "false"))
        print-maybe-coloured #(do
                                (if no-colour?
                                  (pprint/pprint %)
                                  (puget/cprint %))
                                "")]
    (r/while-success->> (or stdin (u/slurp-file (first args)))
                        cap/parse
                        print-maybe-coloured
                        (r/r :success))))

(defn restitch-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (r/while-success->> (or stdin (u/slurp-file (first args)))
                      cap/strip-contiguous-speaker-tags
                      cap/parse
                      restitch/restitch
                      cap/to-string
                      (r/r :success)))

(defn text-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (r/while-success->> (or stdin (u/slurp-file (first args)))
                      cap/parse
                      :cues
                      cap/to-plain-text
                      (r/r :success)))

(defn sonix-dl-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (let [captions-format (-> args first keyword)
        id (or stdin (second args))]
    (r/while-success->> (sonix/get-captions captions-format id)
                        (r/r :success))))

(defn trint-dl-sub-cmd
  {:args (s/cat :_cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [args stdin] :as _cli-r}]
  (let [captions-format (-> args first keyword)
        id (or stdin (second args))]
    (r/while-success->> (trint/get-document-captions captions-format id)
                        (r/r :success))))

(defn read-stdin-maybe
  "Read text from stdin if requested by user."
  {:args (s/cat :cli-r ::cli-r)
   :ret (s/nilable string?)}
  [{:keys [global-opts] :as cli-r}]
  (let [stdin (when (= "-i" (first global-opts))
                (u/read-stdin))]
    (assoc cli-r :stdin stdin)))

(defn run-sub-cmd
  {:args (s/cat :cli-r ::cli-r)
   :ret ::cli-r}
  [{:keys [sub-cmd] :as cli-r}]
  (case sub-cmd
    ("-h" "--help")
    (r/r :success help)

    "--version"
    (r/r :success version)

    "contiguous"
    (contiguous-sub-cmd cli-r)

    "essay"
    (essay-sub-cmd cli-r)

    "fixup"
    (fixup-sub-cmd cli-r)

    "linger"
    (linger-sub-cmd cli-r)

    "overlap"
    (overlap-sub-cmd cli-r)

    "parse"
    (parse-sub-cmd cli-r)

    "restitch"
    (restitch-sub-cmd cli-r)

    "text"
    (text-sub-cmd cli-r)

    "sonix-dl"
    (sonix-dl-sub-cmd cli-r)

    "trint-dl"
    (trint-dl-sub-cmd cli-r)

    (assoc cli-r
           :level :error
           :message "Unrecognised sub-command. Try running: recap --help")))

(defn execute
  "Execute the command specified by the given arguments."
  {:args (s/cat :args ::cli-args)
   :ret ::cli-r}
  [args]
  (-> (parse-cli-args args)
      read-stdin-maybe
      run-sub-cmd))


(comment
  (execute ["--version"])
  (execute ["text" "tmp/short.vtt"]))
