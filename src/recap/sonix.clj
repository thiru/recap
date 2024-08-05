(ns recap.sonix
  "Interop with Sonix's web API."
  (:require
            [better-cond.core :as b]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [recap.caption.data-specs :as dspecs]
            [recap.config :as cfg]
            [recap.utils.common :as u]
            [recap.utils.results :as r]))


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
  split-multi-words)


;; NOTE: The following spec is based on the JSON response for retrieving a Sonix transcript.
;; See: https://sonix.ai/docs/api#get_json

(s/def ::doc-id string?)

(s/def ::name string?)
(s/def ::quality_score (s/nilable string?))

(s/def ::speaker string?)
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


(s/fdef get-transcript
        :args (s/cat :id ::doc-id)
        :ret (s/or :success ::xscript-api-res
                   :failure ::r/result))

(defn get-transcript
  "Get the transcript of the document with the specified id."
  [id]
  (b/cond
    (str/blank? id)
    (r/r :error "No document id provided")

    let [api-res (http-get (format (base-url :json) id))]

    (r/failed? api-res)
    api-res

    :else
    api-res))


(s/fdef xscript->captions
        :args (s/cat :xscript-api-res ::xscript-api-res)
        :ret (s/or :success ::dspecs/caption
                   :failure ::r/result))

(defn xscript->captions
  "Convert the Sonix-specific transcript document into our standard captions data structure."
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


(s/fdef get-captions
        :args (s/cat :captions-format captions-formats
                     :id ::doc-id)
        :ret (s/or :success string?
                   :failure ::r/result))

(defn get-captions
  "Get the captions (SRT or WebVTT) of the document with the specified id.
  NOTE: There's a bug in Sonix that causes words to appear broken up and spaces before
  punctuation marks at times. So, it may be best to avoid using this function and use
  `get-transcript` with `xscript->captions` instead."
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


(s/fdef remove-contiguous-speaker-tags
        :args (s/cat :transcript ::transcript)
        :ret ::transcript)
(defn remove-contiguous-speaker-tags
  "Remove contiguous same speaker tags from the given transcript object."
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


(s/fdef normalise-words
        :args (s/cat :words ::words)
        :ret ::words)
(defn normalise-words
  "Make it easier to work with the given words."
  [words]
  (map (fn [word]
         (update word :text
                 #(-> %
                      ;; Replace contiguous whitespace with a single space
                      (str/replace #"\s+" " ")
                      ;; Replace hyphen with m-dash
                      (str/replace #" - " "—")
                      ;; Replace contiguous hyphens with m-dash
                      (str/replace #"--+" "—"))))
       words))


(s/fdef fix-broken-words
        :args (s/cat :words ::words)
        :ret ::words)
(defn fix-broken-words
  "Correct broken words or punctuation that have been split into two word objects back into one.
  Some observed examples
  - 'stepping'  -> 's' and 'tepping'
  - 'beingness' -> 'being' and 'ness'
  Sometimes punctuation marks like periods, question marks, commas and single quotes are also
  contained in a separate word object."
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


(s/fdef split-multi-words
        :args (s/cat :words ::words)
        :ret ::words)
(defn split-multi-words
  "Split word objects containing multiple words."
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

(s/fdef speaker-section->cues
        :args (s/cat :speaker-section ::speaker-section)
        :ret ::dspecs/cues)
(defn speaker-section->cues
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

(s/fdef secs->duration
        :args (s/cat :secs float?)
        :ret ::dspecs/duration)
(defn secs->duration
  [secs]
  (-> secs
      (or 0.0) ; null guard
      (* 1000) ; seconds -> milliseconds
      (u/millis->duration :show-millis? true)))


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

  (get-captions :vtt "invalid-id")
  (get-transcript "invalid-id")
  (-> (get-transcript "invalid-id")
      (xscript->captions)))
