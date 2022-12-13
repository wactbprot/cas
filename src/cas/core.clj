(ns cas.core
  (:require [buddy.auth.accessrules :refer [restrict]]
            [buddy.auth.backends.session :refer [session-backend]]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
            [buddy.hashers :as hashers]
            [buddy.auth :refer [authenticated? throw-unauthorized]]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [compojure.core :refer [defroutes context GET POST]]
            [compojure.route :refer [not-found]]
            [integrant.core :as ig]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.util.response :refer [response redirect]]
            [ring.util.codec :refer [url-encode]]))

;; # System

;; The entire system is stored in an atom filled
;; by [integrant](https://github.com/weavejester/integrant)
(defonce system (atom {}))

;; System `config`uration map.
(def config {:db/couch {:opts {:basic-auth [(System/getenv "CAL_USR")
                                            (System/getenv "CAL_PWD")]
                               :content-type :json
                               :socket-timeout 1000
                               :connection-timeout 1000
                               :accept :json
                               :pool {:threads 1 :default-per-route 1}}
                        :prot "http"
                        :host "localhost"
                        :port 5984
                        :usr-path "_users/org.couchdb.user:"
                        :usr-map {:roles [] :type "user"}
                        :member-path "vl_db/_security"
                        :session-path "/_session" }

             :get/register {:db (ig/ref :db/couch)
                            :path "vl_db/_design/cas/register.html"}

             :post/register {:db (ig/ref :db/couch)
                             :pwd-opts {:min-length 3}
                             :allowed-users "vl_db/000_MAINTAINERS"}
             
             :get/login {:db (ig/ref :db/couch)
                         :path "vl_db/_design/cas/login.html"}

             :post/login {:db (ig/ref :db/couch)}

             :get/index {:db (ig/ref :db/couch)
                         :path "vl_db/_design/cas/index.html"}
             
             :routes/app {:get-register (ig/ref :get/register)
                          :post-register (ig/ref :post/register)
                          :get-login (ig/ref :get/login)
                          :post-login (ig/ref :post/login)
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

;; The application base is/was:
;; [github.com/adambard/buddy-test](https://github.com/adambard/buddy-test/blob/master/src/buddy_test/app.clj)

;; ## register
(def userstore (atom {}))

(defn get-allowed-users-doc [{:keys [srv opts] :as db} path]
  (-> (http/get (str srv path) opts)
      :body
      (json/read-str :key-fn keyword)))

(defn get-members-doc [{:keys [member-url opts] :as db}]
  (-> (http/get member-url opts)
      :body  
      (json/read-str :key-fn keyword)))

(defn get-allowed-users [db path]
  (-> (get-allowed-users-doc db path)
      :Maintainers))

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

(defn create-user [{:keys [usr-url-fn usr-map opts] :as db} usr pwd]
  (let [url (usr-url-fn usr)
        body (assoc usr-map :name usr :password pwd)
        opts (assoc opts :body (json/write-str body))]
    (http/put url opts)))

(defn make-usr-member [{:keys [member-url opts] :as db} usr]
  (let [members (get-members-doc db)
        members (update-in members [:members :names] conj usr)
        opts (assoc opts :body (json/write-str members))]
    (http/put member-url opts)))


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

(defn usr-exist? [{:keys [usr-url-fn opts] :as db} usr]
  (try
    (http/head (usr-url-fn usr) opts)
    true
    (catch Exception e
      false)))

(defn check-preconditions [{{email "email" pwd1 "password1" pwd2 "password2"} :form-params} db pwd-opts] 
  (cond-> {}
    #_#_(not (user-allowed? (get-allowed-users db allowed-users) :email email)) (assoc :error true
                                                                                       :reason "user not allowed")
    (usr-exist? db email) (assoc :error true
                                 :reason "already registered, goto <a href='/login/'>login</a>")
    (not (passwds-ok? pwd1 pwd2 pwd-opts)) (assoc :error true
                                                  :reason "passwords dont match, try again <a href='/register/'>register</a>"))) 

;; ## login

;; CouchDB provides [Cookie Authentication](https://docs.couchdb.org/en/3.2.2-docs/api/server/authn.html#cookie-authentication)
;; The request:
;;
;; <pre>
;; POST /_session HTTP/1.1
;; Accept: application/json
;; Content-Length: 37
;; Content-Type: application/json
;; Host: localhost:5984
;; 
;; {
;;     "name": "root",
;;     "password": "relax"
;;  }
;; </pre>
;; will be anwered with
;;
;; <pre>
;; HTTP/1.1 200 OK
;; Cache-Control: must-revalidate
;; Content-Length: 43
;; Content-Type: application/json
;; Date: Mon, 03 Dec 2012 01:23:14 GMT
;; Server: CouchDB (Erlang/OTP)
;; Set-Cookie: AuthSession=cm9vdDo1MEJCRkYwMjq0LO0ylOIwShrgt8y-UkhI-c6BGw; Version=1; Path=/; HttpOnly
;; 
;; {"ok":true,"name":"root","roles":["_admin"]}
;; </pre>
;; Redirect to `/` (success) or `/login/` (fail) after login data are posted.

(defn get-cookie [{:keys [srv session-path opts] :as db} usr pwd]
  (let [opts (assoc opts :body (json/write-str {:name usr :password pwd}))]
    (-> (http/post (str srv session-path) opts) :Set-Cookie)))


;; ## system up

(defmethod ig/init-key :db/couch [_ {:keys [prot host port usr-path member-path] :as conf}]
  (let [srv (str prot "://" host ":" port "/")]
    (assoc conf
           :srv srv
           :usr-url-fn (fn [usr] (str srv usr-path usr))
           :member-url (str srv member-path))))

(defmethod ig/init-key :get/login [_ {:keys [path db]}]
  (fn [req]
    (let [{srv :srv opts :opts} db]
      (-> (http/get (str srv path) opts) :body))))

(defmethod ig/init-key :post/login [_ {:keys [db]}]
  (fn [{{email "email" pwd "password"} :form-params  :as req}]
    (if-let [cookie (get-cookie db email pwd)]
      (assoc (redirect "/") :Set-Cookie cookie)
      (redirect "/login/"))))

(defmethod ig/init-key :get/register [_ {:keys [path db]}]
  (fn [req]
    (let [{srv :srv opts :opts} db]
      (-> (http/get (str srv path) opts) :body))))

(defmethod ig/init-key :post/register [_ {:keys [db allowed-users pwd-opts]}]
  (fn [{{email "email" pwd "password1"} :form-params  :as req}]
    (let [{error :error
           reason :reason} (check-preconditions req db pwd-opts)]
      (if-not error
        (let [{status :status} (create-user db email pwd)]
          (if (< status 400)
            (let [{status :status} (make-usr-member db email)]
              (if (< status 400)
                (redirect "/login/")
                (str "status " status)))
            (str "status " status)))
        (str "precondition failed " reason) ))))

(defmethod ig/init-key :get/index [_ {:keys [path db]}]
  (fn [req]
    (if-not (authenticated? req)
      (redirect "/login/")
      (str "<h1>hello app</h1>" path))))

(defmethod ig/init-key :routes/app [_ {:keys [get-login post-login get-index get-register post-register] :as conf}]
  (defroutes all-routes
    (GET "/" [] get-index)
    (GET "/login/" [] get-login)
    (GET "/register/" [] get-register)
    (POST "/register/" [] post-register)
    (POST "/login/" [] post-login)
    (not-found "<h1>not found</h1>")))

(defmethod ig/init-key :server/app [_ {:keys [backend routes]}]
  (-> routes
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
  (app)
  (db))

(comment
  (usr-exist? (db) "www"); => false
  (usr-exist? (db) "thomas.bock@ptb.de")) ; => true

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


(comment
  (defn create-user! [user]
    (let [password (:password user)
          user-id (uuid)]
      (-> user
          (assoc :id user-id :password-hash (hashers/encrypt password))
          (dissoc :password)
          (->> (swap! userstore assoc user-id))))))
