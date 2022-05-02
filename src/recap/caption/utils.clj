(ns recap.caption.utils
  (:require [clojure.spec.alpha :as s]))



(s/fdef get-speaker-tag
        :args (s/cat :text string?)
        :ret string?)

(defn get-speaker-tag
  "Find speaker tag in the given text.

  A speaker tag is taken to be the text that indentifies a speaker.

  It should be some capitalised text followed by a colon, at the beginning of a
  line with at most one space.

  Returns the speaker tag (including the colon) if found, otherwise `nil`."
  [text]
  (some->> text
           (re-find #"^([A-Z][\w-]*)(\s+[A-Z][\w-]*)?(:)")
           first))



(comment
  (get-speaker-tag "Q1: hello")
  (get-speaker-tag "Bob: hello")
  (get-speaker-tag "Bob-Tim: hello")
  (get-speaker-tag "Bob Tim: hello")
  (get-speaker-tag "Bob Tim Li: hello")
  (get-speaker-tag "Hi, Bob: hello"))

