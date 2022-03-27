(ns recap.cli
  (:require [clojure.string :as str]
            [better-cond.core :as b]
            [utils.common :as c]
            [utils.results :as r]
            [recap.caption :as cap]))

(def usage "Usage: recap <command> [<args>]")

(defn parse
  "Parse the given CLI arguments."
  [args]
  (if (empty? args)
    (r/r :error "No command specified"
         :command nil
         :args [])

    (let [cmd (-> args first str/lower-case keyword)]
      (cond
        (or (= :help cmd) (= :-h cmd) (= :--help cmd))
        (r/r :success ""
             :command :help
             :args [])

        (= :parse cmd)
        (r/r :success ""
             :command cmd
             :args (rest args))

        :else
        (r/r :error (format "Unrecognised command '%s'" (first args))
             :command cmd
             :args [])))))

(defn action
  "Action the specified CLI command & args."
  [cli-r]
  (when (r/failed? cli-r)
    (c/abort 1 (:message cli-r)))

  (case (:command cli-r)
    :help (println usage)

    :parse
    (b/cond
      let [slurp-r (c/slurp-file (-> cli-r :args first))]

      (r/failed? slurp-r)
      (c/abort 1 (str "Attempt to parse captions failed because of the following error:\n"
                      (:message slurp-r)))

      let [parse-r (cap/parse slurp-r)]

      (r/failed? parse-r)
      (c/abort 1 (:message parse-r))

      :else
      (println parse-r))))
