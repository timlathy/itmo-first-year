(ns lab3-client.http
  (:require [lab3-client.auth :as auth]
            [clojure.string :as str]
            [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.core.match :refer [match]]))

(def host "http://localhost:8080")

(declare client-data parse-body)

(defn get-json [endpoint]
  (->
    (str host endpoint)
    (client/get (client-data "" (auth/jwt-big-brother)))
    parse-body))

(defmacro def-http-method [fun-name method]
  (let [json (symbol 'json)
        endpoint (symbol 'endpoint)
        token (symbol 'token)]
    `(defn ~fun-name [~json ~endpoint ~token]
       (-> 
         (str host ~endpoint)
         (~method (client-data ~json ~token))
         parse-body))))

(def-http-method post-json client/post)
(def-http-method patch-json client/patch)

; Internal

(defn client-data [json token]
  {:body (json/generate-string json)
   :headers (auth/http-headers token)
   :content-type :json
   :accept :json})

(defn parse-body [req]
  (-> req :body (json/parse-string true)))

