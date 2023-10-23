(ns recap.caption.speaker
  (:refer-clojure :exclude [defn])
  (:require [clojure.spec.alpha :as s]
            [recap.caption.data-specs :as dspecs]
            [utils.common :as u]
            [utils.specin :refer [defn]]))


(set! *warn-on-reflection* true) ; for graalvm


(declare get-speaker-tag-srt
         get-speaker-tag-webvtt)


(defn get-speaker-tag
  "Find the speaker tag in the given text, if present.

  A speaker tag is taken to be the text that indentifies a speaker."
  {:args (s/cat :text string?)
   :ret (s/or :found string?
              :not-found nil?)}
  [text]
  (or (get-speaker-tag-webvtt text)
      (get-speaker-tag-srt text)))


(defn get-speaker-tag-srt
  "SRT files don't have special notation for speaker tags so we try our best to match on some basic
  assumptions on how one is typically formatted. I.e. it should be one or more capitalised words
  followed by a colon and at the beginning of the line.

  An example speaker tag: `Bob:`."
  [text]
  (->> text
       (re-find #"^([A-Z][\w-]*)(\s+[A-Z0-9][\w-]*)*(:+)")
       first))

(defn get-speaker-tag-webvtt
  "WebVTT files define a specific notation for speaker tags.

  An example speaker tag: `<v Bob>`."
  [text]
  (when text
    (re-find #"^<v\s+[^>]+>" text)))


(defn unique-speaker-tags
  "Find unique speaker tags in the given captions."
  {:args (s/cat :caption ::dspecs/caption)
   :ret (s/or :found (s/coll-of string?)
              :not-found nil?)}
  [caption]
  (some->> caption
           :cues
           (mapv :lines)
           flatten
           (mapv get-speaker-tag)
           (remove nil?)
           distinct))


(comment
  (get-speaker-tag "Q1: hello")
  (get-speaker-tag "Q1:: hello")
  (get-speaker-tag "Speaker 1: hello")
  (get-speaker-tag "Al: hello")
  (get-speaker-tag "Al-Bob: hello")
  (get-speaker-tag "Al Bob: hello")
  (get-speaker-tag "Al bob: hello")
  (get-speaker-tag "Al Bob Cate: hello")
  (get-speaker-tag "Al bob cate: hello")
  (get-speaker-tag "<v Al bob cate:> hello")
  (get-speaker-tag "Hi, Al: hello"))
