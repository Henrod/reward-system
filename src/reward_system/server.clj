(ns reward-system.server
	(:require [ring.adapter.jetty :refer [run-jetty]]
    		[compojure.core :refer [defroutes GET POST]]
    		[ring.middleware.params :refer [wrap-params]]
    		[reward-system.core :refer :all]
    		[clojure.data.json :as json]))

(defroutes routes
	(GET "/" [] {:status 200
		            :headers {"Content-Type" "application/json"}
		            :body (-> (input-from-file "test/reward_system/input.txt")
		            		build
		           			parse-result)})
	(GET "/little" [] {:status 200
			          :headers {"Content-Type" "application/json"}
			          :body (-> (input-from-file "test/reward_system/little_input.txt")
		            			build
		           				parse-result)})
	(POST "/" req
		(let [	obj (json/read-str (get-in req [:params "obj"] "{}") :key-fn keyword)
			input (mapcat (fn [[f s]] (list f (-> s str keyword))) obj)]
			{:status 200
			  :headers {"Content-Type" "application/json"}
			  :body (-> (apply add-to-input (input-from-file "test/reward_system/little_input.txt") input)
			  		build
			  		parse-result)})))

(def app (wrap-params routes))

(defn -main []
	(println "Running on port 3000")
	(run-jetty app {:port 3000}))