(ns lab3-client.core
  (:gen-class)
  (:require [lab3-client.http :as http]
            [lab3-client.seed :as seed]
            [clojure.string :as str]
            [clj-http.client :as client]
            [cheshire.core :as json]))

(declare prepare-people-report prepare-report
         person-report describe-action describe-transaction describe-conversation
         describe-employment-report)

(defn -main [& args]
  (println "Seeding the app...")
  (seed/post-data)
  (println "---")
  (run! println (prepare-people-report))
  (run! println (prepare-report)))

(defn prepare-people-report []
  (map person-report '("Незнайка" "Козлик" "Богач Билли")))

(defn prepare-employment-report [person]
  (map describe-employment-report (http/get-json
    (str "/employment/requests/" person))))

(defn prepare-report []
  (let [actions (map describe-action
                     (concat (http/get-json "/actions/Незнайка")
                             (http/get-json "/actions/Козлик")))
        transactions (map describe-transaction
                        (distinct (concat (http/get-json "/transactions/drawer/Незнайка")
                                          (http/get-json "/transactions/drawer/Козлик")
                                          (http/get-json "/transactions/drawee/Незнайка")
                                          (http/get-json "/transactions/drawee/Козлик"))))
        conversations (map describe-conversation
                         (distinct (concat (http/get-json "/conversations/Незнайка")
                                           (http/get-json "/conversations/Козлик"))))]
  (->>
    (concat actions transactions conversations)
    (sort-by :date)
    (map :description)
    (concat (prepare-employment-report "Незнайка") (prepare-employment-report "Козлик")))))

(defn person-report [person-name]
  (->> (http/get-json (str "/people/" person-name)) :socialClass (str person-name " is ")))

(defn parse-date [ds]
  (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd'T'HH:mm:ss") ds))

(defn describe-transaction [t]
  {:description (str (-> t :drawee) " transferred "
                     (-> t :amount) " currency units to "
                     (-> t :drawer))
   :date (-> t :date parse-date)})

(defn describe-conversation [c]
  {:description (str "A conversation has been observed between "
                      (str/join ", " (map :name (:participants c)))
                     " and decoded to \"" (:recognizedContent c) "\"")
   :date (-> c :date parse-date)})

(defn describe-action [a]
  {:description (str "A location change has been observed for "
                     (-> a :actor)
                     ": new location is " (:newLocation a)
                     ", detected means of transporation: " (:means a))
   :date (-> a :date parse-date)}) 

(defn describe-employment-report [report]
  (let [person (:applicant report)
        status (:status report)
        details (:details report)]
    (str person " requested an employment; the application status is now " status "; additional details are: \"" details "\"")))
