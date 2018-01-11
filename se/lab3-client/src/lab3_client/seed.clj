(ns lab3-client.seed
  (:require [lab3-client.auth :as auth]
            [lab3-client.http :as http]
            [clojure.string :as str]
            [clojure.core.match :refer [match]]))

(defn create-person [person account-balance]
  (-> {:name person}
      (http/post-json "/people" (auth/jwt-big-brother)))
  (-> {:ownerName person
       :name (str "Individual " person)
       :balance account-balance}
      (http/post-json "/accounts" (auth/jwt-big-brother))))

(defn create-conversation [person other-participants content date]
  (-> {:participantNames other-participants
       :content content
       :date date}
      (http/post-json "/conversations" (auth/jwt-device person))))

(defn create-personal-transaction [drawee drawer amount date]
  (-> {:personName drawer
       :amount amount
       :date date}
      (http/post-json "/transactions" (auth/jwt-device drawee))))

(defn create-business-transaction [drawee drawer amount date]
  (-> {:businessName drawer
       :amount amount
       :date date}
      (http/post-json "/transactions" (auth/jwt-device drawee))))

(defn create-loc-change [person new-location means date]
  (-> {:newLocation new-location
       :means means
       :date date}
      (http/post-json "/actions/locationChanges" (auth/jwt-device person))))

(defn create-business-and-owner [business-name owner-name owner-balance]
  (create-person owner-name owner-balance)
  (-> {:name business-name
       :ownerName owner-name}
      (http/post-json "/businesses" (auth/jwt-big-brother))))

(defn create-employment-request [person status details]
  (let [id ((http/post-json {}
              "/employment/requests/"
              (auth/jwt-device person)) :id)]
    (-> {:status status
         :details details}
        (http/patch-json
          (str "/employment/requests/" id)
          (auth/jwt-big-brother)))))

(defn create-employment [person-name business-name title]
  (-> {:employeeName person-name
       :employerName business-name
       :title title}
      (http/post-json "/employment" (auth/jwt-big-brother))))

(defn post-data []
  ; Main characters
  (create-person "Незнайка" 10)
  (create-person "Козлик" 10)
  ; Rich people
  (create-person "Богач Билли" 8192)
  (create-person "Скряга Сэм" 16384)
  (create-person "Толстосум Том" 32768)
  ; Employment history
  (create-employment-request "Незнайка" "Rejected" "No suitable positions found. The applicant lacks education, has poor appearance and no prior job experience.")
  (create-employment-request "Козлик" "Rejected" "No suitable positions found. The applicant lacks education, has poor appearance and no prior job experience.")
  ; A day in the life...
  (create-loc-change "Незнайка" "Downtown" "By foot" "2028-06-12T08:17:40")
  (create-loc-change "Козлик" "Downtown" "By foot" "2028-06-12T08:18:00")
  ; First encounter
  (create-conversation "Незнайка" '("Козлик" "Богач Билли") 
                       "позвольте ... пожалуйста ... спасибо спасибо"
                       "2028-06-12T09:32:00")
  (create-conversation "Козлик" '("Незнайка" "Богач Билли")
                       "какие тяжелые ... помочь ... вот сюда ... отлично"
                       "2028-06-12T09:49:20")
  (create-personal-transaction "Богач Билли" "Незнайка" 12 "2028-06-12T09:52:31")
  ; Eating
  (create-conversation "Незнайка" '("Козлик")
                       "придется ... одноразовое питание ... лучше всего питаться вечером перед сном ... если проешь свои денежки днем или утром к вечеру ... проголодаешься и ночью не сможешь заснуть"
                       "2028-06-12T19:19:20")
  ;;; Умберто's
  (create-business-and-owner "Умберто's" "Упитанный Умберто" 982)
  (create-person "Повар Педро" 92)
  (create-employment "Повар Педро" "Умберто's" "Кокинеро")
  ;;; Transaction at Умберто's
  (create-business-transaction "Незнайка" "Умберто's" 45 "2028-06-12T19:27:10")
  ; Returning to a cheaper area
  (create-loc-change "Козлик" "Outskirts" "By foot" "2028-06-12T21:01:00")
  (create-loc-change "Незнайка" "Outskirts" "By foot" "2028-06-12T21:01:10")
  ; Taking a "good" night's sleep
  (create-business-and-owner "Ночлежка Грега" "Грязный Грег" 376)
  (create-business-transaction "Незнайка" "Ночлежка Грега" 45 "2028-06-12T21:12:10"))
