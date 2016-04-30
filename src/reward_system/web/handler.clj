(ns reward-system.web.handler
  (:require [compojure.core :refer :all]
			[reward-system.web.views :as views]
            [compojure.route :as route]
			[compojure.route :refer [resources not-found]]
			[compojure.handler :as handler]
			[ring.adapter.jetty :as jetty])
  (:gen-class))

(defroutes app-routes
	(GET "/" [] views/home-page)
	(GET "/add-customer" [] (views/add-customer-page))
	(POST "/add-customer"
        {params :params}
        (views/add-customer-results-page params))
	(GET "/all-customers" [] (views/all-customers-page))
	(resources "/")
	(route/not-found "Not Found"))

(def app
  (handler/site app-routes))

(defn -main
	[& [port]]
	(let [port (Integer. (or port
                         (System/getenv "PORT")
                         5000))]
					(jetty/run-jetty #'app {:port  port
                          :join? false})))
