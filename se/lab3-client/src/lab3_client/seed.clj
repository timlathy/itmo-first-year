(ns lab3-client.seed
  (:require [lab3-client.auth :as auth]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [cheshire.core :as json]
            [clj-jwt.core :refer [jwt sign to-str]]
            [clojure.core.match :refer [match]]))

(declare post-json)

(defn post-data []
  (-> {:name "Незнайка"} (post-json "/people"
                                    (auth/jwt-big-brother)) :id)
  (-> {:name "Козлик"} (post-json "/people"
                                  (auth/jwt-big-brother)) :id)
  (-> {:balance 10
       :name "Individual Незнайка"
       :ownerName "Незнайка"} (post-json "/accounts"
                                         (auth/jwt-big-brother)))
  (-> {:balance 10
       :name "Individual Козлик"
       :ownerName "Козлик"} (post-json "/accounts"
                                       (auth/jwt-big-brother)))
  (-> {:newLocation "Downtown"
       :means "By foot"
       :date "2028-06-12'T'08:17:30"} (post-json "/actions/locationChanges"
                                                 (auth/jwt-device "Незнайка")))
  (-> {:newLocation "Downtown"
       :means "By foot"
       :date "2028-06-12'T'08:18:30"} (post-json "/actions/locationChanges"
                                                 (auth/jwt-device "Козлик")))
  )

; Internal

(defn post-json [json endpoint token]
  (->
    (str "http://localhost:8080" endpoint)
    (client/post
      {:body (json/generate-string json)
       :headers (auth/http-headers token)
       :content-type :json
       :accept :json})
    :body
    (json/parse-string true)))

