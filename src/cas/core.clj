(ns cas.core
  ^{:author "Thomas Bock <thomas.bock@ptb.de>"}
  (:require
   [cas.auth-handler :as ah]
   [clj-http.client :as http]
   [clojure.string :as string]
   [compojure.core :refer [defroutes context GET POST]]
   [compojure.route :refer [not-found]]
   [integrant.core :as ig]
   [ring.adapter.jetty :refer [run-jetty]]
   [ring.middleware.session :refer [wrap-session]]
   [ring.middleware.cookies :refer [wrap-cookies]]
   [ring.middleware.params :refer [wrap-params]]
   [ring.util.codec :refer [url-encode]]))


;; ## System configuration map

(def config {:db/couch {:opts {:content-type :json
                               :socket-timeout 1000
                               :connection-timeout 1000
                               :throw-exceptions false
                               :accept :json
                               :pool {:threads 1 :default-per-route 1}}
                        :prot "http"
                        :host "localhost"
                        :name "vl_db"
                        :port 5984
                        :admin-usr (System/getenv "CAL_USR")
                        :admin-pwd (System/getenv "CAL_PWD")
                        :usr-path "_users/org.couchdb.user:"
                        :usr-map {:roles [] :type "user"}
                        :js-path "_design/cas/js%2F"
                        :css-path "_design/cas/css%2F"
                        :member-path "_security"
                        :allowed-users-path "000_MAINTAINERS"
                        :session-path "/_session" }

             :get/register {:db (ig/ref :db/couch)
                            :header "_design/cas/header.html"
                            :footer "_design/cas/footer.html"
                            :content "_design/cas/register.html"
                            :data-trans-fn identity}

             :post/register {:db (ig/ref :db/couch)
                             :pwd-opts {:min-length 3}}
             
             :get/login {:db (ig/ref :db/couch)
                            :header "_design/cas/header.html"
                            :footer "_design/cas/footer.html"
                            :content "_design/cas/login.html"
                            :data-trans-fn identity}

             :post/login {:db (ig/ref :db/couch)}

             :get/index {:db (ig/ref :db/couch)
                         :header "_design/cas/header.html"
                         :footer "_design/cas/footer.html"
                         :content "_design/cas/index.html"
                         :data-trans-fn identity}

             :get/js {:db (ig/ref :db/couch)}

             :get/css {:db (ig/ref :db/couch)}
             
             :routes/app {:db (ig/ref :db/couch)
                          :get-js (ig/ref :get/js)
                          :get-css (ig/ref :get/css)
                          :get-register (ig/ref :get/register)
                          :post-register (ig/ref :post/register)
                          :get-login (ig/ref :get/login)
                          :post-login (ig/ref :post/login)
                          :get-index (ig/ref :get/index)}

             :server/app {:routes (ig/ref :routes/app)}
             
             :server/jetty {:opts {:port 8080
                                   :join? false}
                            
                            :app (ig/ref :server/app)}})

;; # System

;; The entire system is stored in an atom filled
;; by [integrant](https://github.com/weavejester/integrant)
(defonce system (atom {}))

;; Some helper functions.
(defn sys-map [] (-> system deref))
(defn app [] (-> (sys-map) :server/app))
(defn db [] (->  (sys-map) :db/couch))

;; ## system up

(defmethod ig/init-key :get/login [_ conf]
  (ah/get-login conf))

(defmethod ig/init-key :post/login [_ conf]
  (ah/post-login conf))

(defmethod ig/init-key :get/register [_ conf]
  (ah/get-register conf))

(defmethod ig/init-key :post/register [_ conf]
  (ah/post-register conf))

(defmethod ig/init-key :get/index [_ conf]
  (ah/get-index conf))

(defmethod ig/init-key :get/js [_ conf]
  (ah/get-js conf))

(defmethod ig/init-key :get/css [_ conf]
  (ah/get-css conf))

(defmethod ig/init-key :db/couch [_ {:keys [prot host port name usr-path allowed-users-path member-path js-path css-path] :as conf}]
  (let [srv (str prot "://" host ":" port "/")
        db-url (str srv name "/")]
    (assoc conf
           :srv srv
           :db-url db-url
           :allowed-users-url (str db-url "/" allowed-users-path)
           :usr-url-fn (fn [usr] (str srv usr-path usr))
           :member-url (str srv member-path)
           :js-url (str db-url "/" js-path)
           :css-url (str db-url "/" css-path))))

(defmethod ig/init-key :routes/app [_ {:keys [get-login post-login get-index get-register post-register get-js get-css db] :as conf}]
  (defroutes all-routes
 
    (GET "/js/:file" [file] get-js)
    (GET "/css/:file" [file] get-css)
    (GET "/login/" [] get-login)
    (GET "/register/" [] get-register)
    (POST "/register/" [] post-register)
    (POST "/login/" [] post-login)
    (GET "/" [] get-index)
    (not-found "<h1>not found</h1>")))

(defmethod ig/init-key :server/app [_ {:keys [backend routes]}]
  (-> routes
      wrap-cookies
      wrap-params))

(defmethod ig/init-key :server/jetty [_ {:keys [opts app]}]
  (run-jetty app opts))

;; ## system down

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
