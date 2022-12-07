(ns ccas.core
  (:require [buddy.auth.accessrules :refer [restrict]]
            [buddy.auth.backends.session :refer [session-backend]]
            [buddy.auth.middleware :refer [wrap-authentication wrap-authorization]]
            [buddy.hashers :as hashers]
            [clj-http.client :as http]
            [clojure.java.io :as io]
            [compojure.core :refer [defroutes context GET POST]]
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

             :get/login {:db (ig/ref :db/couch)
                         :path "vl_db/_design/ccas/login.html"}
             
             :routes/all {:get-login (ig/ref :get/login)}

             :server/app {:routes (ig/ref :routes/all)
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

(defn get-index [req]
  (slurp (io/resource "public/index.html")))

(defn get-admin [req]
  (slurp (io/resource "public/admin.html")))

(defn post-login [{{username "username" password "password"} :form-params
                   session :session :as req}]
  (if-let [user (get-user-by-username-and-password username password)]
    (assoc (redirect "/")
           :session (assoc session :identity (:id user)))
    (redirect "/login/")))

(defn post-logout [{session :session}]
  (assoc (redirect "/login/")
         :session (dissoc session :identity)))

(defn is-authenticated [{user :user :as req}]
  (not (nil? user)))

(defn wrap-user [handler]
  (fn [{user-id :identity :as req}]
    (handler (assoc req :user (get-user user-id)))))

(defroutes admin-routes
  (GET "/" [] get-admin))


(defmethod ig/init-key :db/couch [_ {:keys [prot host port] :as conf}]
  (assoc conf :srv (str prot "://" host ":" port "/")))


(defmethod ig/init-key :get/login [_ {:keys [path db]}]
  (fn [req]
    (prn "call")
    (let [{srv :srv opts :opts} db]
      (-> (http/get (str srv path) opts) :body))))

(defmethod ig/init-key :routes/all [_ {:keys [ini get-login] :as conf}]
  (defroutes all-routes
    (context "/admin" []
             (restrict admin-routes {:handler identity}))
    (GET "/" [] get-index)
    (GET "/login/" [] get-login)
    (POST "/login/" [] post-login)
    (POST "/logout/" [] post-logout)))

(defmethod ig/init-key :server/app [_ {:keys [backend routes]}]
  (-> routes
      (wrap-user)
      (wrap-authentication backend)
      (wrap-authorization backend)
      (wrap-session)
      (wrap-params)))

(defmethod ig/init-key :server/jetty [_ {:keys [opts app]}]
  (run-jetty app  opts))

(defmethod ig/halt-key! :server/jetty [_ server]
  (.stop server))

(defn start []
  (keys (reset! system (ig/init config))))

(defn stop []
  (ig/halt! @system)
  (reset! system {}))

(defn app [] (-> system deref :server/app))

(comment
  ((app) {:request-method :post :uri "/login/" :body "username=admin&password=1234"})
  ((app) {:request-method :get :uri "/admin/"
           :headers {"cookie" "ring-session=5c39d06a-156d-401f-ae1c-f86a2ca717d6"}
           }))

(comment
  (defn -main []
                                        ; Init admin user
    (create-user! {:username "admin" :password "1234"})
    ( my-app {:port 8080})))
