(ns lab3-client.auth
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [cheshire.core :as json]
            [clj-jwt.core :refer [jwt sign to-str]]
            [clojure.core.match :refer [match]]))

(def jwt-secret "RandomSecretSharedWithTheAuthService")

(defn http-headers [jwt]
  {"Authorization" (str "Bearer " jwt)})

(defn gen-jwt [sub]
  (-> {:sub sub} jwt (sign :HS256 jwt-secret) to-str))

(defn jwt-big-brother []
  (gen-jwt "BIG_BROTHER"))

(defn jwt-device [owner]
  (->> owner (str "DEVICE_OF ") gen-jwt))
