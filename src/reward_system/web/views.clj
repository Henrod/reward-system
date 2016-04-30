(ns reward-system.web.views
	(:require [clojure.string :as str]
        	[hiccup.page :as hic-p]
			[reward-system.system.core :as core]
			[reward-system.system.graph :as graph]
			))

; File directory
(def input-file "resources/files/test.txt")

; Graph that represent the company, its customers and the invitations (connections)
(def company (core/build-graph (graph/->Graph {}) input-file))
(println company)

(defn gen-page-head
  [title]
  [:head
   [:title (str "Reward System: " title)]
   (hic-p/include-css "/css/styles.css")])

(def header-links
  [:div#header-links
   "[ "
   [:a {:href "/"} "Home"]
   " | "
   [:a {:href "/add-customer"} "Add a Customer"]
   " | "
   [:a {:href "/all-customers"} "View All Customers"]
   " ]"])

(defn home-page
	[x]
	(hic-p/html5
		(gen-page-head "Home")
		header-links
		[:h1 "Reward System"]
		[:h2 "Customers sequence of invitation from file:"]
		[:h3 (core/input-html input-file)]))

(defn add-customer-page
	[]
	(hic-p/html5
		(gen-page-head "New customer")
		header-links
		[:h1 "Add a Customer"]
		[:form {:action "/add-customer" :method "POST"}
		[:p "src value: " [:input {:type "text" :name "src"}]]
		[:p "dst value: " [:input {:type "text" :name "dst"}]]
		[:p [:input {:type "submit" :value "Submit a customer"}]]]))

(defn add-customer-results-page
[{:keys [src dst]}]
	(def company (core/add-customer company src dst))
	(hic-p/html5
   	(gen-page-head "New customer")
   	header-links
   	[:h1 "Added a Customer"]
   	[:p "Added [" src ", " dst "] "]))

(defn all-customers-page
	[]
	(hic-p/html5
    (gen-page-head "Rank")
    	header-links
    	[:h1 "Rank of all Customers in company by Score"]
    	[:h3 (core/rank-html company)]))
