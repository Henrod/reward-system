(defproject reward-system "0.1.0"
  :description "System that represents a company that rewards its customers."
  :url "https://github.com/Henrod/reward-system"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [compojure "1.4.0"]
                 [ring/ring-defaults "0.1.5"]
				 [hiccup "1.0.2"]
				 [ring/ring-jetty-adapter "1.4.0"]]
	:plugins [[lein-ring "0.9.7"]]
	:ring {:handler reward-system.web.handler/app}
	:profiles
	{:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
							[ring/ring-mock "0.3.0"]]}}
	:main reward-system.web.handler)
