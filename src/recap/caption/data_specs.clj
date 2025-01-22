(ns recap.caption.data-specs
  "Core data-specs around captions."
  (:require [clojure.spec.alpha :as s]))


(set! *warn-on-reflection* true) ; for graalvm


(s/def ::duration #(re-find #"(\d\d:)+\d\d[,.]?\d*" %))

(s/def ::start ::duration)
(s/def ::end ::duration)
(s/def ::lines (s/coll-of string?))

(s/def ::cue (s/keys :opt-un [::start ::end ::lines]))
(s/def ::cues (s/coll-of ::cue))

(s/def ::header (s/coll-of string?))

(s/def ::caption (s/keys :opt-un [::header]
                         :req-un [::cues]))
