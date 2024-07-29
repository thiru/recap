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
  safe-parse-json
  secs->duration
  speaker-section->cues)


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

    let [speakers (-> xscript-api-res :transcript)]

    (empty? speakers)
    captions

    let [speakers (mapv #(update % :words fix-broken-words)
                        speakers)]

    let [cues (mapcat speaker-section->cues speakers)]

    :else
    (assoc captions :cues cues)))


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
      ;; There are two cases to handle here (doesn't apply to the first word):
      ;; - The normal case where a word object starts with a space.
      ;; - The abnormal case where a word object does not start a space. This is considered a bug
      ;;   where a word may be broken up or a punctuation mark which is not attached to the
      ;;   previous word. In this case we join it with the previous word.
      (if (str/starts-with? (:text curr-word) " ")
        (let [updated-word (update curr-word :text str/triml)]
          (recur rest-words
                 updated-word
                 (conj fixed-words updated-word)))
        (let [updated-word (assoc prev-word
                                  :text (str (:text prev-word) (:text curr-word))
                                  :end_time (:end_time curr-word))]
          (recur rest-words
                 updated-word
                 (-> fixed-words
                     butlast
                     vec
                     (conj updated-word))))))))

(s/fdef speaker-section->cues
        :args (s/cat :speaker-section ::speaker-section)
        :ret ::dspecs/cues)
(defn speaker-section->cues
  [speaker-section]
  (map-indexed
    (fn [idx word]
      {:lines [(if (zero? idx)
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
