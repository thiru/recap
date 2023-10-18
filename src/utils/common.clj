(ns utils.common
  "Common/generic utilities."
  (:refer-clojure :exclude [defn])
  (:require [better-cond.core :as b]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [puget.printer :as puget]
            [utils.specin :refer [defn]]
            [utils.results :as r]))


(set! *warn-on-reflection* true) ; for graalvm


(s/def ::file (s/or :file-path (s/nilable string?)
                    :file-obj #(instance? java.io.File %)))


(def os
  "Get the current OS as a keyword."
  (delay (let [os-str (str/lower-case (System/getProperty "os.name"))]
           (cond
             (<= 0 (or (str/index-of os-str "linux") -1))
             :linux

             (<= 0 (or (str/index-of os-str "windows") -1))
             :windows

             (<= 0 (or (str/index-of os-str "mac") -1))
             :mac

             :else
             (keyword (str "unknown-" os-str))))))


(defn find-first
  "Get the first element in `coll` where `pred` returns truthy when applied to it."
  [pred coll]
  (some #(when (pred %) %) coll))


(defn java-class-name=>clj-fn-name [fn]
  (let [kebabified (str/replace (str fn) "_" "-")
        seg1 (str/split kebabified #"\$")
        seg2 (str/split (second seg1) #"\@")]
    (str (first seg1) \/ (first seg2))))

(defn as-clj-stack-info
  [^java.lang.StackTraceElement ste]
  {:fn (-> ste .getClassName java-class-name=>clj-fn-name)
   :line (-> ste .getLineNumber)})

(defn ignored-stack-element?
  [^java.lang.StackTraceElement ste]
  (let [ignored #{"get_all_callers" "get_last_caller" "dbg" "clojure.lang" "clojure.spec" "swank" "nrepl" "eval"}
        class-name (.getClassName ste)]
    (some #(re-find (re-pattern %) class-name)
          ignored)))

(defn get-all-callers []
  (-> (Throwable.) .fillInStackTrace .getStackTrace))

(defn get-last-caller []
  (->> (get-all-callers)
       vec
       (find-first (comp not ignored-stack-element?))
       as-clj-stack-info))

(defmacro spy
  "A simpler version of Timbre's spy which simply pretty-prints to stdout
  and returns the eval'd expression."
  [expr]
  `(let [evaled# ~expr
         caller# (get-last-caller)]
     (print (format "[%s:%d] %s => " (:fn caller#) (:line caller#) '~expr))
     (puget/cprint evaled#)
     evaled#))


(defn exit!
  "Exit app with a success/failure exit code based on the given result.
  The result's message is also printed to stdout or stderr as appropriate."
  {:args (s/cat :result ::r/result)
   :ret nil?}
  [result]
  (r/print-msg result)
  (System/exit (case (:level result)
                 (:success :trace :info :warn :debug) 0
                 1)))


(defn fmt
  "A convenience function to create a formatted string (via `format`).

  The first argument is the format control string. If it's a list it will be
  concatenated together. This makes it easier to format long strings."
  {:args (s/cat :formatter (s/or :whole-string string?
                                 :segmented-string (s/coll-of string?))
                :args (s/* any?))
   :ret string?}
  [formatter & args]
  (if (sequential? formatter)
    (apply format (str/join "" formatter) args)
    (apply format formatter args)))


(defn join-paths
  "Join the given path segments.

  If any of the segments are `nil` it will be ignored."
  {:args (s/cat :segments (s/* (s/nilable string?)))
   :ret string?}
  [& segments]
  (.toString ^java.io.File (apply io/file (map #(or % "") segments))))


(defn load-if-file
  "Load the file at the given path if it exists."
  {:args (s/cat :file ::file)
   :ret (s/and ::r/result (s/keys :opt-un [::file]))}
  [file]
  (let [^java.io.File file-obj (io/as-file file)]
    (if (and file-obj
             (.exists ^java.io.File file-obj)) ; type hint needed for GraalVM image
      (r/r :success "File successfully loaded" {:file file-obj})
      (r/r :error (format "File '%s' was not found or is inaccessible"
                          (or file ""))))))


(defn slurp-file
  "Read all contents of the given file."
  {:args (s/cat :file ::file)
   :ret (s/or :content string?
              :error-result ::r/result)}
  [file]
  (let [file-r (load-if-file file)]
    (if (r/success? file-r)
      (slurp (:file file-r))
      file-r)))


(defn parse-int
  "Exception-free integer parsing.

   Returns the parsed integer if successful, otherwise `fallback`."
  {:args (s/cat :input (s/nilable string?)
                :kwargs (s/keys* :opt-un []))
   :ret int?}
  [input & {:keys [fallback]
            :or {fallback 0}}]
  (try
   (Integer/parseInt input)
   (catch Exception _ fallback)))


(defn parse-float
  "Exception-free float parsing.

   Returns the parsed float if successful, otherwise `fallback`."
  {:args (s/cat :input (s/nilable string?)
                :kwargs (s/keys* :opt-un []))
   :ret float?}
  [input & {:keys [fallback]
            :or {fallback 0.0}}]
  (try
   (Float/parseFloat input)
   (catch Exception _ fallback)))


(defn duration->secs
  "Parse the given duration string to a total number of seconds."
  {:args (s/cat :duration (s/nilable string?))
   :ret (s/or :seconds float?
              :error-result :r/result)}
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


(defn millis->duration
  "Convert the given milliseconds to a duration."
  {:args (s/cat :millis number?
                :kwargs (s/keys* :opt-un []))
   :ret (s/or :duration string?)}
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
