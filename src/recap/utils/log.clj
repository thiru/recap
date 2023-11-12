(ns recap.utils.log
  "Simple/naive logging."
  {:clj-kondo/config '{:linters {:unresolved-var {:exclude [jansi-clj.core]}}}}
  (:refer-clojure :exclude [defn])
  (:require
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [jansi-clj.core :as jansi]
            [recap.utils.log :as log]
            [recap.utils.results :as r]
            [recap.utils.specin :refer [defn]]))


(set! *warn-on-reflection* true) ; for graalvm


(defn colourise
  "Colourise the given `text` according to `level`."
  {:args (s/cat :level ::r/level
                :text string?)
   :ret string?}
  [level text]
  (condp = level
    :success
    (jansi/green text)

    :info
    (jansi/default text)

    :trace
    (jansi/cyan text)

    :debug
    (jansi/magenta text)

    :warn
    (jansi/yellow text)

    :error
    (jansi/red text)

    :fatal
    (jansi/bold (jansi/red text))

    :else
    (jansi/default text)))

(defn log
  "Log the given result map to stdout."
  {:args (s/cat :result ::r/result)
   :ret ::r/result}
  [result]
  (println (format "%s: %s" (->> result :level name str/upper-case (colourise (:level result)))
                   (:message result))))

(defn test-all-levels
  []
  (log (r/r :success "success message"))
  (log (r/r :info "info message"))
  (log (r/r :trace "trace message"))
  (log (r/r :debug "debug message"))
  (log (r/r :warn "warn message"))
  (log (r/r :error "error message"))
  (log (r/r :fatal "fatal message")))

(comment
  (test-all-levels))
