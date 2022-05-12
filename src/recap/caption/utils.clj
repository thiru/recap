(ns recap.caption.utils
  (:require [clojure.spec.alpha :as s]))



(declare get-speaker-tag-srt
         get-speaker-tag-webvtt)

(s/fdef get-speaker-tag
        :args (s/cat :text string?)
        :ret (s/or :found string?
                   :not-found nil?))

(defn get-speaker-tag
  "Find speaker tag in the given text.

  A speaker tag is taken to be the text that indentifies a speaker.

  It should be some capitalised text followed by a colon, at the beginning of a
  line with at most one space.

  Returns the speaker tag (including the colon) if found, otherwise `nil`."
  [text]
  (or (get-speaker-tag-webvtt text)
      (get-speaker-tag-srt text)))


(defn get-speaker-tag-srt
  [text]
  (->> text
       (re-find #"^([A-Z][\w-]*)(\s+[\w-]*)?(:)")
       first))

(defn get-speaker-tag-webvtt
  [text]
  (when text
    (re-find #"^<v\s+[^>]+>" text)))


(comment
  (get-speaker-tag "Q1: hello")
  (get-speaker-tag "Al: hello")
  (get-speaker-tag "Al-Bob: hello")
  (get-speaker-tag "Al Bob: hello")
  (get-speaker-tag "Al bob: hello")
  (get-speaker-tag "Al bob cate: hello")
  (get-speaker-tag "<v Al bob cate:> hello")
  (get-speaker-tag "Hi, Al: hello"))

