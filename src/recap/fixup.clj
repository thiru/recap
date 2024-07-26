(ns recap.fixup
  "Fixup some common mistakes and follow certain captioning conventions."
  (:require
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [recap.utils.common :as u]))


(set! *warn-on-reflection* true) ; for graalvm


(defn fixup
  "Fixup some common mistakes and follow certain captioning conventions."
  {:args (s/cat :text string?)
   :ret string?}
  [text]
  (if (str/blank? text)
    ""
    (-> text
        ;; Replace parentheses with brackets
        (str/replace #"\(" "[")
        (str/replace #"\)" "]")
        ;; Replace HTML-encoded space with a regular space
        (str/replace #"&nbsp;" " ")
        ;; Unencode angle brackets
        (str/replace #"&lt;" "<")
        (str/replace #"&gt;" ">")
        ;; Remove extraneous ellipses
        (str/replace #"\.\.\.\.+" "...")
        ;; Ellipsis should be exactly three periods without preceding spaces
        ;; (unless it follows a colon or semicolon)
        (str/replace #"([^:;]) +\.{2,}" "$1...")
        ;; Replace 'ok' with 'okay' (preserving capitalisation)
        (str/replace #"\b([oO])[kK]\b" "$1kay")
        ;; Replace a single hyphen that's not a word separator with m-dash
        ;; and remove surround spaces (but not the preceding whitespace if it
        ;; follows a colon or semicolon)
        (str/replace #"([^:;])\s+-\s" "$1—")
        (str/replace #"([:;]\s+)-\s" "$1—")
        ;; Replace contiguous hyphens with m-dash
        (str/replace #"--+" "—")
        ;; Replace contiguous colons with just one
        (str/replace #"::+" ":")
        ;; Remove whitespace before certain punctuation marks
        (str/replace #"[ \t]+([.!?:;,])" "$1")
        ;; Replace contiguous spaces with just one
        (str/replace #"  +" " ")
        ;; Trim spaces and tabs at beginning/end:
        (str/replace #"^[ \t]+" "")
        (str/replace #"[ \t]+$" ""))))


(comment
  (fixup " Q :: This &lt;i&gt;is&lt;/i&gt; some -- &nbsp; sample---test input . OK ; ok , okay .... Hi ! Huh ? To :  Good--&nbsp;"))
