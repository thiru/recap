(ns utils.common
  "Common/generic utilities."
  (:require [better-cond.core :as b]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [puget.printer :as puget]
            [utils.results :as r]))



(defmacro spy
  "A simpler version of Timbre's spy which simply pretty-prints to stdout
  and returns the eval'd expression."
  [expr]
  `(let [evaled# ~expr]
     (print (str '~expr " => "))
     (puget/cprint evaled#)
     evaled#))



(s/fdef non-neg?
        :args (s/cat :n number?)
        :ret boolean?)

(defn non-neg?
  "Determine whether the given number is non-negative."
  [n]
  (<= 0 n))



(s/fdef exit!
        :args (s/cat :result ::r/result)
        :ret nil?)

(defn exit!
  "Exit app with a success/failure exit code based on the given result.
  The result's message is also printed to stdout or stderr as appropriate."
  [result]
  (r/print-msg result)
  (System/exit (case (:level result)
                 (:success :trace :info :warn :debug) 0
                 1)))



(s/fdef fmt
        :args (s/cat :formatter string?
                     :args (s/coll-of string?))
        :ret string?)

(defn fmt
  "A convenience function to create a formatted string (via `format`).

  The first argument is the format control string. If it's a list it will be
  concatenated together. This makes it easier to format long strings."
  [formatter & args]
  (if (sequential? formatter)
    (apply format (str/join "" formatter) args)
    (apply format formatter args)))



(s/fdef slurp-file
        :args (s/cat :file-path string?)
        :ret (s/or :content string?
                   :error-result :r/result))

(defn slurp-file
  "Read all contents of the given file.

  Returns the contents of the string if successfully read, otherwise a
  `r/result`."
  [file-path]
  (if (str/blank? file-path)
    (r/r :error "No file was provided")
    (let [file (io/file file-path)]
      (if (not (.exists file))
        (r/r :error
             (format "File '%s' was not found or inaccessible" file-path))
        (slurp file)))))



(s/fdef parse-int
        :args (s/cat :input string?
                     :fallback int?)
        :ret int?)

(defn parse-int
  "Exception-free integer parsing.

   Returns the parsed integer if successful, otherwise fallback."
  [input & {:keys [fallback]
            :or {fallback 0}}]
  (try
   (Integer/parseInt input)
   (catch Exception _
     fallback)))



(s/fdef parse-float
        :args (s/cat :input string?
                     :fallback float?)
        :ret float?)

(defn parse-float
  "Exception-free float parsing.

   Returns the parsed float if successful, otherwise fallback."
  [input & {:keys [fallback]
            :or {fallback 0.0}}]
  (try
   (Float/parseFloat input)
   (catch Exception _
     fallback)))


(s/fdef duration->secs
        :args (s/cat :duration string?)
        :ret (s/or :seconds float? :error-result :r/result))

(defn duration->secs
  "Parse the given duration string to a total number of seconds."
  [duration]
  (b/cond
    (str/blank? duration)
    0

    let [segs (str/split duration #":")]

    (< 3 (count segs))
    (r/r :error (format (str "Expected a maximum of 3 time segments (e.g. "
                             "'01:02:03') but found %d")
                    (count segs)))

    let [reverse-segs (reverse segs)
         hours (let [hours (nth reverse-segs 2 nil)]
                 (if hours
                   (parse-int hours :fallback -1)
                   0))
         minutes (-> (second reverse-segs)
                     (parse-int :fallback -1))
         seconds (-> (first reverse-segs)
                     (parse-float :fallback -1.0))]

    (or (neg? hours)
        (neg? minutes)
        (neg? seconds))
    (r/r :error
         "Invalid duration. Could not parse each time segment as a number.")

    let [total-secs (+ seconds
                       (* minutes 60)
                       (* hours 3600))]

    :else
    total-secs))



(s/fdef millis->duration
        :args (s/cat :millis int?
                     :show-millis? boolean?)
        :ret (s/or :duration string?))

(defn millis->duration
  "Convert the given milliseconds to a duration."
  [millis & {:keys [show-millis?]}]
  (when (not (number? millis))
    (throw (ex-info "Input must be a number specifying milliseconds"
                    {:millis millis})))

  (if (or (nil? millis) (zero? millis))
    "00:00:00"
    (let [duration (java.time.Duration/ofMillis millis)
          hours (.toHoursPart duration)
          minutes (.toMinutesPart duration)
          seconds (.toSecondsPart duration)
          milliseconds (.toMillisPart duration)]
      (str (format "%02d:%02d:%02d"
               hours
               minutes
               seconds)
           (if show-millis?
             (format ".%03d" milliseconds)
             "")))))

