(ns recap.sonix
  "Interop with Sonix's web API."
  (:require
            [better-cond.core :as b]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [recap.config :as cfg]
            [recap.utils.common :as u]
            [recap.utils.results :as r]))


(declare api-key
         base-url
         safe-parse-json
         api-opts
         http-get)


(def captions-formats
  "Supported captions formats."
  #{:srt :vtt})


(s/fdef get-captions
        :args (s/cat :captions-format captions-formats
                     :id :transcript/id)
        :ret (s/or :text string?
                   :result ::r/result))

(defn get-captions
  "Get the captions (SRT or WebVTT) of the document with the specified id."
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
              :opts {:form-params (api-opts)})))

(defn base-url [captions-format]
  (if (= :vtt captions-format)
    (-> @cfg/active-cfg :sonix :webvtt-url)
    (-> @cfg/active-cfg :sonix :srt-url)))

(defn api-key []
  (-> @cfg/active-cfg :sonix :api-key))

(defn api-opts []
  (-> @cfg/active-cfg :sonix :opts))

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

    let [body (if (= :json (:accept opts))
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
