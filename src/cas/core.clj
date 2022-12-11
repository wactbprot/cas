(ns cas.core
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

;; # System

;; The entire system is stored in an atom filled
;; by [integrant](https://github.com/weavejester/integrant)
(defonce system (atom {}))

;; System `config`uration map.
(def config {:db/couch {:opts {:basic-auth [(System/getenv "CAL_USR")
                                            (System/getenv "CAL_PWD")]
                               ;; :body "{\"json\": \"input\"}"
                               ;; :headers {"Accept" "application/json"
                               ;;          "Content-Type" "application/json"}
                               ;; :query-params {}
                               :content-type :json
                               :socket-timeout 1000
                               :connection-timeout 1000
                               :accept :json
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


;; ## utils and helper functions

;; some utils   
(defn uuid [] (java.util.UUID/randomUUID))

(defn sys-map [] (-> system deref))

(defn app [] (-> (sys-map) :server/app))

(defn db [] (->  (sys-map) :db/couch))

(comment
  (app)
  (db))

;; The application base is/was:
;; [github.com/adambard/buddy-test](https://github.com/adambard/buddy-test/blob/master/src/buddy_test/app.clj)

;; ## register
(def userstore (atom {}))

(defn get-allowed-users-doc [{:keys [srv opts] :as db} path]
  (-> (http/get (str srv path) opts)
      :body
      (json/read-str :key-fn keyword)))

(defn get-allowed-users [db path]
  (-> (get-allowed-users-doc db path)
      :Maintainers))

(comment
  (defn create-user! [user]
    (let [password (:password user)
          user-id (uuid)]
      (-> user
          (assoc :id user-id :password-hash (hashers/encrypt password))
          (dissoc :password)
          (->> (swap! userstore assoc user-id))))))

;; The register process generates a user in the CouchDB `_users` database.
                                        
;; <pre>
;; curl -X PUT http://localhost:5984/_users/org.couchdb.user:jan 
;;       -H "Accept: application/json" 
;;       -H "Content-Type: application/json" 
;;       -d '{"name": "jan", "password": "apple", "roles": [], "type": "user"}'
;; </pre>

;; The response looks like this:
;; <pre>
;; {:cached nil,
;;  :request-time 381,
;;  :repeatable? false,
;;  :protocol-version {:name "HTTP", :major 1, :minor 1},
;;  :streaming? true,
;;  :chunked? false,
;;  :reason-phrase "Created",
;;  :headers
;;  {"X-CouchDB-Body-Time" "0",
;;   "X-Couch-Request-ID" "27182a3f7c",
;;   "Location" "http://localhost:5984/_users/org.couchdb.user:wwwww",
;;   "Date" "Sun, 11 Dec 2022 10:53:37 GMT",
;;   "ETag" "\"1-07fd1aae324a51caaa14f91beec78c50\"",
;;   "Cache-Control" "must-revalidate",
;;   "Content-Length" "85",
;;   "Server" "CouchDB/3.2.2 (Erlang OTP/23)",
;;   "Content-Type" "application/json",
;;   "Connection" "close"},
;;  :orig-content-encoding nil,
;;  :status 201,
;;  :length 85,
;;  :body
;;  "{\"ok\":true,\"id\":\"org.couchdb.user:wwwww\",\"rev\":\"1-07fd1aae324a51caaa14f91beec78c50\"}\n",
;;  :trace-redirects []}
;; </pre>

;; <pre>
;; {
;;   _id: org.couchdb.user:wactbprot@gmail.com,
;;   _rev: 1-991b0fa43605bdd2d7e9679400d0d366,
;;   name: wactbprot@gmail.com,
;;   roles: [],
;;   type: user,
;;   password_scheme: pbkdf2,
;;   iterations: 10,
;;   derived_key: 79908b31d412a87240bd7e733225f6d93c5125d8,
;;   salt: 297ab114fcd91ac3ffd3d88a79189bae
;;  }
;; </pre>

(defn create-user [{srv :srv opts :opts :as db} email pwd]
  (let [url (str srv "_users/org.couchdb.user:" email)
        body {:name email :password pwd :roles [] :type "user"}
        opts (assoc opts :body (json/write-str body))
         res (http/put url opts)]
    (prn res)
    ))
    
(defn get-user [user-id]
  (get @userstore user-id))

  (defn get-user-by-username-and-password [username password]
    (reduce (fn [_ user]
              (if (and (= (:username user) username)
                       (hashers/check password (:password-hash user)))
                (reduced user))) (vals @userstore)))

;; The register methode provides the opportunity to allow certain
;; users (see [[user-allowed?]] and [[get-allowed-users]])

(defn user-allowed? [vec-of-maps key value]
  (->> vec-of-maps
       (filterv (fn [m] (= value (key m))))
       count
       pos?))

;; The password checks can be done on the client side but must be done
;; on the server side anyway (never trust user input). The `pwd-opts`
;; can/should be worked out (e.g. regular expression) 
(defn passwds-ok? [pwd1 pwd2 {:keys [min-length] :as pwd-opts}]
  (and (= pwd1 pwd2)
       (<= min-length (count pwd1))))

(defn preconditions-ok? [{{email "email" pwd1 "password1" pwd2 "password2"} :form-params} pwd-opts] 
  (and
   #_(user-allowed? (get-allowed-users db allowed-users) :email email)
   (passwds-ok? pwd1 pwd2 pwd-opts)))

;; ## login

;; Redirect to `/` (success) or `/login/` (fail) after login data are posted.
(defn post-login [{{email "email" password "password"} :form-params session :session :as req}]
  (if-let [user (get-user-by-username-and-password email password)]
    (assoc (redirect "/")
           :session (assoc session :identity (:id user)))
    (redirect "/login/")))

(defn post-logout [{session :session}]
  (assoc (redirect "/login/")
         :session (dissoc session :identity)))

(defn wrap-user [handler]
  (fn [{user-id :identity :as req}]
    (handler (assoc req :user (get-user user-id)))))

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
  (fn [{{email "email" pwd "password1"} :form-params  :as req}]
    (if (preconditions-ok? req pwd-opts)
      (create-user db email pwd)
      (redirect "/login/"))))

(defmethod ig/init-key :get/index [_ {:keys [path db]}]
  (fn [req]
    (if-not (authenticated? req)
      (throw-unauthorized)
      (str "<h1>hello app</h1>" path))))

(defmethod ig/init-key :routes/app [_ {:keys [get-login get-index get-register post-register] :as conf}]
  (defroutes all-routes
    (GET "/" [] get-index)
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
  (run-jetty app opts))

(defmethod ig/halt-key! :server/jetty [_ server]
  (.stop server))

;; ## start stop

(defn start [] (keys (reset! system (ig/init config))))

(defn stop []
  (ig/halt! @system)
  (reset! system {}))

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
