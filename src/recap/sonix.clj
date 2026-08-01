(ns recap.sonix
  "Interop with Sonix's web API."
  (:refer-clojure :exclude [defn])
  (:require
    [better-cond.core :as b]
    [babashka.http-client :as http]
    [cheshire.core :as json]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [recap.caption :refer [fix-overlapping-cues fixup-cues]]
    [recap.caption.data-specs :as dspecs]
    [recap.caption.combine :refer [combine]]
    [recap.caption.linger :refer [linger-cues]]
    [recap.caption.restitch :refer [restitch]]
    [recap.config :as cfg]
    [recap.utils.log :refer [log]]
    [recap.utils.common :as u]
    [recap.utils.results :as r]
    [recap.utils.specin :refer [defn]]))

(set! *warn-on-reflection* true) ; for graalvm

(declare
  api-key
  api-opts
  base-url
  fix-broken-words
  http-get
  normalise-words
  remove-contiguous-speaker-tags
  safe-parse-json
  secs->duration
  speaker-section->cues
  split-multi-words
  team-media?)

;; NOTE: The following spec is based on the JSON response for retrieving a Sonix transcript.
;; See: https://sonix.ai/docs/api#get_json

(s/def ::doc-id string?)
(s/def ::folder-id string?)

(s/def ::name string?)
(s/def ::quality_score (s/nilable string?))

(s/def ::speaker (s/nilable string?))
(s/def ::start_time float?)
(s/def ::end_time float?)

(s/def ::highlight boolean?)
(s/def ::highlight_color string?)
(s/def ::strikethrough boolean?)

(s/def ::word (s/keys :req-un [::text ::start_time ::end_time]
                      :opt-un [::highlight ::highlight_color ::strikethrough]))
(s/def ::words (s/coll-of ::word))
(s/def ::speaker-section (s/keys :req-un [::speaker ::words]
                                 :opt-un [::start_time ::end_time]))

(s/def ::transcript (s/coll-of ::speaker-section))
(s/def ::xscript-api-res (s/keys :req-un [::name ::transcript]
                                 :opt-un [::quality_score]))

(def captions-formats
  "Supported captions formats."
  #{:srt :vtt})

(defn get-media-status
  "Get the status and metadata of the specified document id.
  See the following link for more details:
  https://sonix.ai/docs/api#media_status"
  {:args (s/cat :id ::doc-id)
   :ret (s/or :success map?
              :failure ::r/result)}
  [id]
  (b/cond
    (str/blank? id)
    (r/r :error "No document id provided")

    let [api-res (http-get (format (-> @cfg/active-cfg :sonix :media-status :url) id))]

    (r/failed? api-res)
    api-res

    :else
    api-res))

(defn list-media-files
  "Get list of media files in the specified folder.
  See the following link for more details:
  https://sonix.ai/docs/api#list_media"
  {:args (s/cat :id ::folder-id)
   :ret (s/or :success map?
              :failure ::r/result)}
  [id]
  (b/cond
    (str/blank? id)
    (r/r :error "No folder id provided")

    let [opts {:form-params (-> @cfg/active-cfg :sonix :list-media-files :opts
                                (assoc :folder_id id))}
         api-res (http-get (format (-> @cfg/active-cfg :sonix :list-media-files :url) id)
                           :opts opts)]

    (r/failed? api-res)
    api-res

    :else
    api-res))

(defn get-transcript
  "Get the transcript of the document with the specified id."
  {:args (s/cat :id ::doc-id)
   :ret (s/or :success ::xscript-api-res
              :failure ::r/result)}
  [id]
  (b/cond
    (str/blank? id)
    (r/r :error "No document id provided")

    let [api-res (http-get (format (base-url :json) id))]

    (r/failed? api-res)
    api-res

    :else
    api-res))

(defn xscript->captions
  "Convert the Sonix-specific transcript document into our standard captions data structure."
  {:args (s/cat :xscript-api-res ::xscript-api-res)
   :ret (s/or :success ::dspecs/caption
              :failure ::r/result)}
  [xscript-api-res]
  (b/cond
    (nil? xscript-api-res)
    (r/r :error "No Sonix transcript document provided")

    let [captions {:header ["WebVTT"]
                   :cues []}]

    let [transcript (-> xscript-api-res :transcript)]

    (empty? transcript)
    captions

    let [transcript (->> (remove-contiguous-speaker-tags transcript)
                         (map #(update % :words normalise-words))
                         (map #(update % :words fix-broken-words))
                         (map #(update % :words split-multi-words))
                         (mapcat speaker-section->cues))]

    :else
    (assoc captions :cues transcript)))

(defn get-captions
  "Get the captions (SRT or WebVTT) of the document with the specified id.
  NOTE: There's a bug in Sonix that causes words to appear broken up and spaces before
  punctuation marks at times. So, it may be best to avoid using this function and use
  `get-transcript` with `xscript->captions` instead."
  {:args (s/cat :captions-format captions-formats
                :id ::doc-id)
   :ret (s/or :success string?
              :failure ::r/result)}
  [captions-format id]
  (b/cond
    (nil? captions-format)
    (r/r :error "No captions format provided")

    (nil? (captions-formats captions-format))
    (r/r :error (format "Unsupported captions format '%s'" (name captions-format)))

    (str/blank? id)
    (r/r :error "No document id provided")

    :else
    (http-get (format (base-url captions-format) id)
              :opts {:form-params (api-opts captions-format)})))

(defn find-team-medias
  "Find all media files that a team of individuals may be working on, where one of those files
  is the given id."
  {:args (s/cat :id ::doc-id)
   :ret (s/or :success map?
              :failure ::r/result)}
  [id]
  (b/cond
    (str/blank? id)
    (r/r :error "No document id provided")

    let [media-status (get-media-status id)]

    (r/failed? media-status)
    media-status

    let [my-folder-id (-> media-status :folder :id)]

    (str/blank? my-folder-id)
    (r/r :error (str "Failed to get folder id of the given media id: " id))

    let [medias-res (list-media-files my-folder-id)]

    (r/failed? medias-res)
    medias-res

    let [team-medias (->> medias-res
                          :media
                          (filter #(team-media? media-status %)))]

    :else
    team-medias))

(defn process-single-captions
  "Download a transcript and fully process as a captions file."
  {:args (s/cat :id ::doc-id)
   :ret (s/or :success (s/merge ::r/result
                                (s/keys :req-un [::dspecs/caption]))
              :failure ::r/result)}
  [id]
  (b/cond
    (str/blank? id)
    (r/r :error "No document id provided")

    let [transcript (get-transcript id)]

    (r/failed? transcript)
    transcript

    :else
    (r/while-success->
      (xscript->captions transcript)
      (restitch)
      (fix-overlapping-cues)
      (fixup-cues)
      (linger-cues)
      (as-> $
        (r/r :success
             (u/fmt+ "Successfully converted Sonix transcript to captions"
                     (:name transcript))
             {:caption $
              :source-medias [(:name transcript)]})))))

(defn process-team-captions
  "Find all documents belonging to a team effort (based on the specified document id) and combine
  them to produce a single, fully processed captions file."
  {:args (s/cat :id ::doc-id)
   :ret (s/or :success (s/merge ::r/result
                                (s/keys :req-un [::dspecs/caption]))
              :failure ::r/result)}
  [id]
  (b/cond
    (str/blank? id)
    (r/r :error "No document id provided")

    let [team-medias (find-team-medias id)]

    (r/failed? team-medias)
    team-medias

    (= 1 (count team-medias))
    (r/r :error
         (str "Could not find other documents that appear to be part of a team effort "
              "with this one."))

    do (log (r/r :info (u/fmt+ "Found ~d team document~:p associated with ~s: ~s"
                               (count team-medias) id
                               (->> team-medias
                                    (map :id)
                                    (str/join ", ")))))

    let [sorted-medias (sort-by :name team-medias)
         transcripts (mapv #(get-transcript (:id %)) sorted-medias)
         failed-transcript (some r/failed? transcripts)]

    failed-transcript
    failed-transcript

    let [captions (mapv xscript->captions transcripts)
         failed-caption (some r/failed? captions)]

    failed-caption
    failed-caption

    :else
    (r/while-success->
      (combine captions)
      (restitch)
      (fix-overlapping-cues)
      (fixup-cues)
      (linger-cues)
      (as-> $
        (r/r :success
             (u/fmt+ ["Successfully converted ~d Sonix transcript~:p to captions"]
                     (count sorted-medias))
             {:caption $
              :source-medias (map :name sorted-medias)})))))

(defn base-url [captions-format]
  (-> @cfg/active-cfg :sonix captions-format :url))

(defn api-key []
  (-> @cfg/active-cfg :sonix :api-key))

(defn api-opts [captions-format]
  (-> @cfg/active-cfg :sonix captions-format :opts))

(defn safe-parse-json
  "Attempt to parse the given string as JSON. If it fails an exception is
  caught and logged while the original string is returned."
  [s]
  (try
    (json/parse-string s true)
    (catch Exception _ex
      ;(println "Failed to parse string as JSON" ex) ; DEBUG
      s)))

(defn http-get
  "Perform an HTTP GET.

  Returns a result on failure, otherwise the response body which may be a
  simple string or map (in case of a JSON response)."
  [url & {:keys [opts]}]
  ;(u/spy [url opts]) ; DEBUG
  (b/cond
    (str/blank? (api-key))
    (r/r :error "Sonix API key is missing in config.edn")

    let [opts (merge {:headers {"Authorization" (str "Bearer " (api-key))}
                      :throw false}
                     opts)
         response (http/get url opts)]

    let [body (if (str/includes? (get-in response [:headers "content-type"])
                                 "application/json")
                (safe-parse-json (:body response))
                (:body response))]

    (not (<= 200 (:status response) 299))
    ;; NOTE: Sonix seems to always return error responses in JSON
    (let [json-body (safe-parse-json body)]
      (r/r :error
           (format "Request to '%s' failed (HTTP status %d)\n%s"
                   url
                   (:status response)
                   (:error json-body))
           {:res-body json-body
            :res-status (:status response)}))

    :else
    body))

(defn remove-contiguous-speaker-tags
  "Remove contiguous same speaker tags from the given transcript object."
  {:args (s/cat :transcript ::transcript)
   :ret ::transcript}
  [transcript]
  ;; Starting with the second speaker section as there's no need to modify the first one
  (loop [[curr-speaker-section & rest-speaker-sections] (rest transcript)
         prev-speaker (-> transcript first :speaker)
         updated-transcript [(first transcript)]]
    (if (and (empty? curr-speaker-section) (empty? rest-speaker-sections))
      updated-transcript
      (let [curr-speaker (if (str/blank? (:speaker curr-speaker-section))
                          prev-speaker
                          (:speaker curr-speaker-section))
            updated-speaker-section (if (= curr-speaker prev-speaker)
                                      (assoc curr-speaker-section :speaker nil)
                                      curr-speaker-section)]
        (recur rest-speaker-sections
               curr-speaker
               (conj updated-transcript updated-speaker-section))))))

(defn normalise-words
  "Make it easier to work with the given words."
  {:args (s/cat :words ::words)
   :ret ::words}
  [words]
  (map (fn [word]
         (update word :text
                 #(-> %
                      ;; Replace contiguous whitespace with a single space
                      (str/replace #"\s+" " ")
                      ;; Replace hyphen with m-dash
                      (str/replace #" - " "— ")
                      ;; Replace contiguous hyphens with m-dash
                      (str/replace #"--+" "—"))))
       words))

(defn fix-broken-words
  "Correct broken words or punctuation that have been split into two word objects back into one.
  Some observed examples
  - 'stepping'  -> 's' and 'tepping'
  - 'beingness' -> 'being' and 'ness'
  Sometimes punctuation marks like periods, question marks, commas and single quotes are also
  contained in a separate word object."
  {:args (s/cat :words ::words)
   :ret ::words}
  [words]
  ;; Starting with the second word since the first word won't have any issues
  (loop [[curr-word & rest-words] (rest words)
         prev-word (first words)
         fixed-words [(first words)]]
    (if (and (empty? curr-word) (empty? rest-words))
      fixed-words
      (b/cond
        ;; Normal case where the word starts with a space
        (str/starts-with? (:text curr-word) " ")
        (let [updated-word (update curr-word :text str/triml)]
          (recur rest-words
                 updated-word
                 (conj fixed-words updated-word)))

        let [disjoint-punct (second (re-find #"^([,.!?;:\]'\"—–-]+) " (:text curr-word)))]

        ;; An abnormal case where a punctuation mark is attached to the next word
        ;; (e.g. "end" and ". Start" should be "end." and "Start")
        disjoint-punct
        (let [new-prev-word (update prev-word :text #(str % disjoint-punct))
              new-curr-word (update curr-word :text #(-> %
                                                         (subs (count disjoint-punct))
                                                         (str/triml)))]
          (recur rest-words
                 new-curr-word
                 (-> fixed-words
                     butlast
                     vec
                     (conj new-prev-word new-curr-word))))

        ;; An abnormal case where the current word doesn't start with a space and the previous
        ;; word ends in a punctuation mark or space. In this case we don't want to join the words.
        (or (str/ends-with? (:text prev-word) " ")
            (re-find (:ends-with-any-punctuation @cfg/active-cfg) (:text prev-word)))
        (recur rest-words
               curr-word
               (conj fixed-words curr-word))

        ;; An abnormal case where a word object does not start with a space. This is considered
        ;; to be a bug where a word may be broken up or a punctuation mark is dijoint from the
        ;; word it should be attached to and is now all alone. In this case we join it with the
        ;; previous word.
        :else
        (let [updated-word (assoc prev-word
                                  :text (str (:text prev-word) (:text curr-word))
                                  :end_time (:end_time curr-word))]
          (recur rest-words
                 updated-word
                 (-> fixed-words
                     butlast
                     vec
                     (conj updated-word))))))))

(defn split-multi-words
  "Split word objects containing multiple words."
  {:args (s/cat :words ::words)
   :ret ::words}
  [words]
  (loop [[word & rest-words] words
         new-words []]
    (if (and (empty? word) (empty? rest-words))
      new-words
      (b/cond
        let [text (:text word)]

        (or (str/blank? text)
            (>= 1 (count text)))
        (recur rest-words
               (conj new-words word))

        let [space-idx (str/index-of text " " 1)
             text-length (count text)]

        (or (nil? space-idx)
            (= space-idx (dec text-length)))
        (recur rest-words
               (conj new-words word))

        let [total-duration (-> (- (:end_time word) (:start_time word))
                                (max 0.0))
             part1-text (subs text 0 space-idx)
             part2-text (subs text space-idx)
             part1-percent (float (/ (count part1-text) text-length))
             split-time (- (:end_time word)
                           (* part1-percent total-duration))
             part1-word (assoc word
                               :text part1-text
                               :end_time split-time)
             part2-word (assoc word
                               :text part2-text
                               :start_time split-time)]

        :else
        ;; Put the second part of the split word back onto the queue in case it has more words
        ;; that need to be split
        (recur (cons part2-word rest-words)
               (conj new-words part1-word))))))

(defn speaker-section->cues
  {:args (s/cat :speaker-section ::speaker-section)
   :ret ::dspecs/cues}
  [speaker-section]
  (map-indexed
    (fn [idx word]
      {:lines [(if (and (zero? idx)
                        (not (str/blank? (:speaker speaker-section))))
                 (str (:speaker speaker-section) ": " (:text word))
                 (:text word))]
       :start (secs->duration (:start_time word))
       :end (secs->duration (:end_time word))})
    (:words speaker-section)))

(defn secs->duration
  {:args (s/cat :secs float?)
   :ret ::dspecs/duration}
  [secs]
  (-> secs
      (or 0.0) ; null guard
      (* 1000) ; seconds -> milliseconds
      (u/millis->duration :show-millis? true)))

(defn team-media?
  "Determine whether the two medias appear to be part of a team effort."
  {:args (s/cat :ref-media map?
                :other-media map?)
   :ret boolean?}
  [ref-media other-media]
  (and
    ;; Starts with a number
    (re-find #"^\d+" (:name other-media))
    ;; Duration diff is at most 2 (seconds)
    (> 2
       (abs (- (:duration other-media)
               (:duration ref-media))))
    ;; Last 10 characters are the same
    (str/ends-with? (:name other-media)
                    (subs (:name ref-media)
                          (- (count (:name other-media)) 10)))))

(comment
  (-> {:name "Normal transcript"
       :transcript [{:speaker "M"
                     :words [{:text "One" :start_time 0 :end_time 1}
                             {:text " two" :start_time 1 :end_time 2}
                             {:text " three" :start_time 1 :end_time 2}]}]}
      (xscript->captions))
  (-> {:name "Broken words"
       :transcript [{:speaker "M"
                     :words [{:text "One" :start_time 0 :end_time 1}
                             {:text " t" :start_time 1 :end_time 2}
                             {:text "wo" :start_time 1 :end_time 2}
                             {:text " three" :start_time 1 :end_time 2}]}]}
      (xscript->captions))

  (process-single-captions "invalid-id")
  (process-team-captions "invalid-id")
  (find-team-medias "invalid-id")
  (list-media-files "invalid-id")
  (get-media-status "invalid-id")
  (get-captions :vtt "invalid-id")
  (get-transcript "invalid-id")
  (-> (get-transcript "invalid-id")
      (xscript->captions)))
