(ns utils.common
  "Common/generic utilities."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [utils.results :as r]))

(defn abort
  "Abort app with the given exit code and message."
  [exit-code msg]
  (if (zero? exit-code)
    (println msg)
    (binding [*out* *err*]
      (println msg)))
  (System/exit exit-code))

(defn slurp-file
  "Read all contents of the given file."
  [file-path]
  (if (str/blank? file-path)
    (r/r :error "No file was specified")
    (let [file (io/file file-path)]
      (if (not (.exists file))
        (r/r :error
             (format "File '%s' was not found or inaccessible" file-path))
        (slurp file)))))
