(ns recap.trint
  "Interop with Trint's web API."
  (:require
            [better-cond.core :as b]
            [cheshire.core :as json]
            [babashka.http-client :as http]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [recap.config :as cfg]
            [utils.common :as u]
            [utils.results :as r]))


(declare _
         api-key
         base-url
         safe-parse-json
         api-opts
         http-get)


(def captions-formats
  "Supported captions formats."
  #{:srt :vtt})


(s/def :folder/_id string?)
(s/def :folder/name string?)
(s/def ::folder (s/keys :req-un [:folder/_id :folder/name]))
(s/fdef list-folders
        :ret (s/coll-of ::folder))


(defn list-folders
  "List all folders in the account."
  []
  (http-get "https://api.trint.com/folders" :opts {:accept :json}))



(s/def :transcript/id string?)
(s/def :transcript/text string?)
(s/fdef get-document-text
        :args (s/cat :id :transcript/id)
        :ret (s/or :text :transcript/text
                   :result ::r/result))

(defn get-document-text
  "Get the document with the specified id as plain text."
  [id]
  (b/cond
    (str/blank? id)
    (r/r :error "No document id provided")

    let [json (http-get (str "https://api.trint.com/export/text/" id)
                        :opts {:accept :json})]

    (r/failed? json)
    json

    (= 403 (:res-status json))
    (r/r :error (format "Failed to retrieve transcript. Maybe the id '%s' is incorrect?"
                        id))

    let [aws-url (:url json)]

    (str/blank? aws-url)
    (r/r :error "URL to download transcript not provided"
         {:response-body json})

    :else
    (http-get aws-url)))



(s/fdef get-document-captions
        :args (s/cat :captions-format captions-formats
                     :id :transcript/id)
        :ret (s/or :text string?
                   :result ::r/result))

(defn get-document-captions
  "Get the captions (SRT or WebVTT) of the document with the specified id."
  [captions-format id]
  (b/cond
    (nil? captions-format)
    (r/r :error "No captions format provided")

    (nil? (captions-formats captions-format))
    (r/r :error (format "Unsupported captions format '%s'" (name captions-format)))

    (str/blank? id)
    (r/r :error "No document id provided")

    let [json (http-get (str (base-url captions-format) id)
                        :opts {:accept :json
                               :query-params (api-opts)})]

    (= 403 (:res-status json))
    (r/r :error (format "Failed to retrieve captions. Maybe the id '%s' is incorrect?"
                        id))

    (r/failed? json)
    json

    let [aws-url (:url json)]

    (str/blank? aws-url)
    (r/r :error "URL to download document not provided"
         {:response-body json})

    :else
    (http-get aws-url)))


(defn base-url [captions-format]
  (if (= :vtt captions-format)
    (-> @cfg/active-cfg :trint :webvtt-url)
    (-> @cfg/active-cfg :trint :srt-url)))

(defn api-key []
  (-> @cfg/active-cfg :trint :api-key))

(defn api-opts []
  (-> @cfg/active-cfg :trint :opts))

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
  (b/cond
    (str/blank? (api-key))
    (r/r :error "Trint API key is missing in config.edn")

    let [opts (merge {:headers {:api-key (api-key)}
                      :throw false}
                 opts)
         response (http/get url opts)]

    let [body (if (= :json (:accept opts))
                (safe-parse-json (:body response))
                (:body response))]

    (not (<= 200 (:status response) 299))
    (r/r :error
         (format "Request to '%s' failed (%d%s)"
                 url
                 (:status response)
                 (if (empty? (:reason-phrase response))
                   ""
                   (str " - " (:reason-phrase response))))
         {:res-body body
          :res-status (:status response)})

    body))
