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

(def config {:db/couch {:opts
                        {:basic-auth [(System/getenv "CAL_USR") (System/getenv "CAL_PWD")]
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

             :server/jetty {:opts {:port 8080
                                   :join? false}
                            :backend (session-backend)}})

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

(defn get-login [req]
  ;; http://localhost:5984/vl_db/_design/ccas/login.html
  (slurp (io/resource "public/login.html")))

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

(defroutes all-routes
  (context "/admin" []
    (restrict admin-routes {:handler identity}))
  (GET "/" [] get-index)
  (GET "/login/" [] get-login)
  (POST "/login/" [] post-login)
  (POST "/logout/" [] post-logout))

(comment
  (defn -main []
                                        ; Init admin user
    (create-user! {:username "admin" :password "1234"})
    ( my-app {:port 8080})))


(defmethod ig/init-key :db/couch [_ {:keys [prot host port] :as conf}]
  (assoc conf :url (str prot "://" host ":" port "/")))

(defmethod ig/init-key :server/jetty [_ {:keys [opts backend]}]
  (run-jetty (-> #'all-routes
                 (wrap-user)
                 (wrap-authentication backend)
                 (wrap-authorization backend)
                 (wrap-session)
                 (wrap-params))
             opts))

(defmethod ig/halt-key! :server/jetty [_ server]
  (.stop server))

(defn start []
  (keys (reset! system (ig/init config))))

(defn stop []
  (ig/halt! @system)
  (reset! system {}))


(comment

  (run-jetty #'my-app {:port 8080 :join? false})
  (my-app {:request-method :post :uri "/login/" :body "username=admin&password=1234"})

  (my-app {:request-method :get :uri "/admin/"
           :headers {"cookie" "ring-session=5c39d06a-156d-401f-ae1c-f86a2ca717d6"}
           }))
