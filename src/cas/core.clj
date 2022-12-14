(ns cas.core
  ^{:author "Thomas Bock <thomas.bock@ptb.de>"}
  (:require
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
(def config {:db/couch {:opts {:content-type :json
                               :socket-timeout 1000
                               :connection-timeout 1000
                               :throw-exceptions false
                               :accept :json
                               :pool {:threads 1 :default-per-route 1}}
                        :prot "http"
                        :host "localhost"
                        :port 5984
                        :admin-usr (System/getenv "CAL_USR")
                        :admin-pwd (System/getenv "CAL_PWD")
                        :usr-path "_users/org.couchdb.user:"
                        :usr-map {:roles [] :type "user"}
                        :member-path "vl_db/_security"
                        :allowed-users-path "vl_db/000_MAINTAINERS"
                        :session-path "/_session" }

             :get/register {:db (ig/ref :db/couch)
                            :path "vl_db/_design/cas/register.html"}

             :post/register {:db (ig/ref :db/couch)
                             :pwd-opts {:min-length 3}}
             
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

;; ## register
(defn status-ok? [{:keys [status body] :as res}] (< status 400))

(defn res->body [{:keys [status body] :as res}] (when (status-ok? res) body))

(defn res->json [res]
  (when-let [body (res->body res)] (json/read-str body :key-fn keyword)))

(defn admin-opts [{:keys [admin-usr admin-pwd] :as db}]
  (-> db :opts (assoc :basic-auth  [admin-usr admin-pwd])))

(defn get-allowed-users-doc [{:keys [srv allowed-users-path] :as db}]
  (-> (http/get (str srv allowed-users-path) (admin-opts db))
      res->json))

(defn get-allowed-users [db]
  (-> (get-allowed-users-doc db)
      :Maintainers))

;; The register process creates a user in the CouchDB `_users` database.
(defn create-user [{:keys [usr-url-fn usr-map] :as db} usr pwd]
  (let [url (usr-url-fn usr)
        body (assoc usr-map :name usr :password pwd)
        opts (admin-opts db)
        opts (assoc opts :body (json/write-str body))]
    (res->json (http/put url opts))))


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

(defn usr-exist? [{:keys [usr-url-fn] :as db} usr]
  (status-ok? (http/head (usr-url-fn usr) (admin-opts db))))

(defn check-preconditions [{{email "email" pwd1 "password1" pwd2 "password2"} :form-params} db pwd-opts] 
  (cond-> {}
    #_#_(not (user-allowed? (get-allowed-users db allowed-users) :email email)) (assoc :error true
                                                                                       :reason "user not allowed")
    (usr-exist? db email) (assoc :error true
                                 :reason "already registered <a href='/login/'>login</a>")
    (not (passwds-ok? pwd1 pwd2 pwd-opts)) (assoc :error true
                                                  :reason "passwords mismatch <a href='/register/'>register</a>"))) 

;; ## login

;; Redirect to `/` (success) or `/login/` (fail) after login data are posted.

(defn make-usr-member [{:keys [member-url] :as db} usr]
  (let [opts (admin-opts db)
        members (res->json (http/get member-url opts))
        members (update-in members [:members :names] conj usr)
        opts (assoc opts :body (json/write-str members))]
    (res->json (http/put member-url opts))))

(defn res->cookie [res]
  (when (status-ok? res)
    (-> res :cookies (get "AuthSession") :value)))

(defn get-cookie [{:keys [srv session-path opts] :as db} usr pwd]
  (let [opts (assoc opts :body (json/write-str {:name usr :password pwd}))]
    (-> (http/post (str srv session-path) opts)
        res->cookie)))

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
      (-> (http/get (str srv path) (admin-opts db))
          res->body))))

(defmethod ig/init-key :post/login [_ {:keys [db]}]
  (fn [{{email "email" pwd "password"} :form-params  :as req}]
    (if-let [cookie (get-cookie db email pwd)]
      (if (make-usr-member db email)
        (assoc (redirect "/") :Set-Cookie cookie)
        (str "failed to make user a database member"))
      (redirect "/login/"))))

(defmethod ig/init-key :get/register [_ {:keys [path db]}]
  (fn [req]
    (let [{srv :srv} db]
      (-> (http/get (str srv path) (admin-opts db))
          res->body))))

(defmethod ig/init-key :post/register [_ {:keys [db allowed-users pwd-opts]}]
  (fn [{{email "email" pwd "password1"} :form-params  :as req}]
    (let [{error :error reason :reason} (check-preconditions req db pwd-opts)]
      (if-not error
        (if (create-user db email pwd)
          (redirect "/login/")
          (str "failed to create user "))
        (str "precondition failed " reason)))))

(defmethod ig/init-key :get/index [_ {:keys [path db]}]
  (fn [req]
    (let [{srv :srv opts :opts} db
          opts (assoc-in opts [:headers :Cookie] (get-in req [:headers "cookie"]))
          res (http/get (str srv path) opts)]
      (if (status-ok? res)
        (res->body res)
        (redirect "/login/")))))

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
  (create-user! {:username "admin" :password "1234"}))

(comment
  (passwds-ok? "123" "123" {:min-length 3}))
