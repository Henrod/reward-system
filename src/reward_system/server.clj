(ns reward-system.server
	(:require [ring.adapter.jetty :refer [run-jetty]]
    		[compojure.core :refer [defroutes GET POST]]
    		[ring.middleware.params :refer [wrap-params]]
    		[ring.middleware.reload :refer [wrap-reload]]
    		[reward-system.core :refer :all]
    		[ring.util.response :as res]))

(def invites (atom []))

(defn path [file-name]
	(str "test/reward_system/" file-name))

(defn handler [{params :params} command]
	(res/response
		(case command
			:rank (if-let [file (params "file")]
				           (let [graph (-> file path build-from-file (add-nodes @invites))]
	                       (-> graph rank seq str))
				           "Specify file name")
			:add   (if-let [src (params "src")]
				         (if-let [dst (params "dst")]
				         	(if (= src dst)
				         		"One can't invite himself/herself"
					           (str (swap! invites conj (read-string src) (read-string dst))))
				           "Specify invited person")
				         "Specify invitee person"))))

(defroutes routes
	(POST "/rank" request (handler request :rank))
	(POST "/add"  request (handler request :add)))

(def app (-> routes
		           (wrap-params)
		           (wrap-reload)))

(defn -main []
	(run-jetty app {:port 3000}))