(ns utils.common
  "Common/generic utilities."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [utils.results :as r]))



(s/fdef non-neg?
        :args (s/cat :n number?)
        :ret boolean?)

(defn non-neg?
  "Determine whether the given number is non-negative."
  [n]
  (<= 0 n))



(s/fdef abort
        :args (s/cat :exit-code (s/and int? non-neg?)
                     :msg any?)
        :ret nil?)

(defn abort
  "Abort app with the given exit code and message."
  [exit-code msg]
  (if (zero? exit-code)
    (println msg)
    (binding [*out* *err*]
      (println msg)))
  (System/exit exit-code))



(s/fdef slurp-file
        :args (s/cat :file-path string?)
        :ret (s/or :string string?
                   :result :r/result))

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
