(ns ccas.core
  (:require [buddy.auth.accessrules :refer [restrict]]
            [buddy.auth.backends.session :refer [session-backend]]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
            [buddy.hashers :as hashers]
            [buddy.auth :refer [authenticated? throw-unauthorized]]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [compojure.core :refer [defroutes context GET POST]]
            [compojure.route :refer [not-found]]
            [integrant.core :as ig]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.util.response :refer [response redirect]]))


(defonce system (atom {}))

(def config {:db/couch {:opts {:basic-auth [(System/getenv "CAL_USR") (System/getenv "CAL_PWD")]
                               ;; :body "{\"json\": \"input\"}"
                               :headers {"X-Api-Version" "2"}
                               :content-type :json
                               :socket-timeout 1000      ;; in milliseconds
                               :connection-timeout 1000  ;; in milliseconds
                               :accept :json
                               :query-params {}
                               :pool {:threads 1 :default-per-route 1}}
                        :prot "http"
                        :host "localhost"
                        :port 5984}

             :get/register {:db (ig/ref :db/couch)
                            :path "vl_db/_design/ccas/register.html"}

             :post/register {:db (ig/ref :db/couch)
                             :pwd-opts {:min-length 3}
                             :allowed-users "vl_db/000_MAINTAINERS"}
             
             :get/login {:db (ig/ref :db/couch)
                         :path "vl_db/_design/ccas/login.html"}
             
             :get/index {:db (ig/ref :db/couch)
                         :path "vl_db/_design/ccas/login.html"}
             
             :routes/app {:get-register (ig/ref :get/register)
                          :post-register (ig/ref :post/register)
                          :get-login (ig/ref :get/login)
                          :get-index (ig/ref :get/index)}

             :server/app {:routes (ig/ref :routes/app)
                          :backend (session-backend)}
             
             :server/jetty {:opts {:port 8080
                                   :join? false}
                            
                            :app (ig/ref :server/app)}})

;; https://github.com/adambard/buddy-test/blob/master/src/buddy_test/app.clj

(defn uuid [] (java.util.UUID/randomUUID))

(def userstore (atom {})) ;; <- couchdb

;; create new user in couchdb
;; curl -X PUT http://localhost:5984/_users/org.couchdb.user:jan \
;;      -H "Accept: application/json" \
;;      -H "Content-Type: application/json" \
;;      -d '{"name": "jan", "password": "apple", "roles": [], "type": "user"}'


(defn create-user! [user]
  (let [password (:password user)
        user-id (uuid)]
    (-> user
        (assoc :id user-id :password-hash (hashers/encrypt password))
        (dissoc :password)
        (->> (swap! userstore assoc user-id)))))

(defn get-user [user-id]
  (get @userstore user-id))

(defn get-user-by-username-and-password [username password]
  (reduce (fn [_ user]
            (if (and (= (:username user) username)
                     (hashers/check password (:password-hash user)))
              (reduced user))) (vals @userstore)))

(defn post-login [{{username "username" password "password"} :form-params session :session :as req}]
  (if-let [user (get-user-by-username-and-password username password)]
    (assoc (redirect "/index/")
           :session (assoc session :identity (:id user)))
    (redirect "/login/")))

(defn post-logout [{session :session}]
  (assoc (redirect "/login/")
         :session (dissoc session :identity)))

(defn wrap-user [handler]
  (fn [{user-id :identity :as req}]
    (handler (assoc req :user (get-user user-id)))))

(defn get-allowed-users [{:keys [srv opts] :as db} path]
  (-> (http/get (str srv path) opts)
      :body
      (json/read-str :key-fn keyword)
      :Maintainers))

(defn user-allowed? [vec-of-maps key value]
  (->> vec-of-maps
       (filterv (fn [m] (= value (key m))))
       count
       pos?))

(defn passwds-ok? [pwd1 pwd2 {:keys [min-length] :as pwd-opts}]
  (and (= pwd1 pwd2)
       (<= min-length (count pwd1))))

;; ## system up

(defmethod ig/init-key :db/couch [_ {:keys [prot host port] :as conf}]
  (assoc conf :srv (str prot "://" host ":" port "/")))

(defmethod ig/init-key :get/login [_ {:keys [path db]}]
  (fn [req]
    (let [{srv :srv opts :opts} db]
      (-> (http/get (str srv path) opts) :body))))

(defmethod ig/init-key :get/register [_ {:keys [path db]}]
  (fn [req]
    (let [{srv :srv opts :opts} db]
      (-> (http/get (str srv path) opts) :body))))

(defmethod ig/init-key :post/register [_ {:keys [db allowed-users pwd-opts]}]
  (fn [{{email "email" pwd1 "password1" pwd2 "password2"} :form-params session :session :as req}]
    (let [allowed-users (get-allowed-users db allowed-users)]
      (if (and (user-allowd? allowed-users :email email)
               (pwds-ok? pwd1 pwd2 pwd-opts))
        "yey"
        "ney"))))

(defmethod ig/init-key :get/index [_ {:keys [path db]}]
  (fn [req]
    (if-not (authenticated? req)
      (throw-unauthorized)
      (str "<h1>hello auth</h1>" path))))

(defmethod ig/init-key :routes/app [_ {:keys [get-login get-index get-register post-register] :as conf}]
  (defroutes all-routes
    (GET "/index/" [] get-index)
    (GET "/login/" [] get-login)
    (GET "/register/" [] get-register)
    (POST "/register/" [] post-register)
    (POST "/login/" [] post-login)
    (POST "/logout/" [] post-logout)
    (not-found "<h1>not found</h1>")))

(defmethod ig/init-key :server/app [_ {:keys [backend routes]}]
  (-> routes
      (wrap-user)
      (wrap-authentication backend)
      (wrap-authorization backend)
      (wrap-session)
      (wrap-params)))

;; ## system down
(defmethod ig/init-key :server/jetty [_ {:keys [opts app]}]
  (run-jetty app  opts))

(defmethod ig/halt-key! :server/jetty [_ server]
  (.stop server))

;; ## start stop
(defn start []
  (keys (reset! system (ig/init config))))

(defn stop []
  (ig/halt! @system)
  (reset! system {}))

;; ## helper functions

(defn app [] (-> system deref :server/app))


;; ## playground
(comment
  ((app) {:request-method :post :uri "/login/" :body "username=admin&password=1234"})
  ((app) {:request-method :get :uri "/admin/"
          :headers {"cookie" "ring-session=5c39d06a-156d-401f-ae1c-f86a2ca717d6"}}))

(comment
  (create-user! {:username "admin" :password "1234"}))

(comment
  (def vec-of-maps
    (get-allowed-users (:db/couch @system) "vl_db/000_MAINTAINERS"))
  (user-allowed? vec-of-maps :email "Thomas.Bock@ptb.de"))

(comment
  (passwds-ok? "123" "123" {:min-length 3}))
