(ns cas.auth-handler
  ^{:author "Thomas Bock <thomas.bock@ptb.de>"}
  (:require
   [clj-http.client :as http]
   [clojure.data.json :as json]
   [ring.util.response :refer [redirect]]))

;; ## utils and helper functions

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

(defn pass-cookie [req opts]
  (assoc-in opts [:headers :Cookie] (get-in req [:headers "cookie"])))

;; ## handler functions 

;; The register login process works as follows.

;; ### get register

;; Get the configured register page with admin creds. 
(defn get-register [{:keys [path db]}]
  (fn [req]
    (let [{srv :srv} db]
      (-> (http/get (str srv path) (admin-opts db))
          res->body))))

;; ### get register

;; Username (email address) and password are submited (plain) -> switch on https.
;; If the preconditions are ok a user is created and redirected to `/login`.
(defn post-register [{:keys [db allowed-users pwd-opts]}]
  (fn [{{email "email" pwd "password1"} :form-params  :as req}]
    (let [{error :error reason :reason} (check-preconditions req db pwd-opts)]
      (if-not error
        (if (create-user db email pwd)
          (redirect "/login/")
          (str "failed to create user "))
        (str "precondition failed " reason)))))


;; ### get login

;; Get the configured login page with admin creds. 
(defn get-login [{:keys [path db]}]
  (fn [req]
    (let [{srv :srv opts :opts} db]
      (-> (http/get (str srv path) (admin-opts db))
          res->body))))

;; ### post login

;; Redirect to `/` and add cookie (success) or `/login/` (fail) after
;; login data are posted.
(defn post-login [{:keys [db]}]
  (fn [{{email "email" pwd "password"} :form-params  :as req}]
    (if (make-usr-member db email)
      (if-let [cookie (get-cookie db email pwd)]
        (assoc (redirect "/") :Set-Cookie cookie)
        (redirect "/login/"))
      (redirect "/login/"))))

;; ### get index

;; The request to index `/` is authorised by the session cookie
;; passed. The `data-trans-fn` enables the transformation of the data
;; received from the database.
(defn get-index [{:keys [path db data-trans-fn]}]
  (fn [req]
    (let [{srv :srv opts :opts} db
          opts (pass-cookie req opts)
          res (http/get (str srv path) opts)]
      (if (status-ok? res)
        (data-trans-fn (res->body res))
        (redirect "/login/")))))
