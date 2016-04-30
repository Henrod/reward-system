(ns reward-system.system.core
	(:require [reward-system.system.graph :as graph])
	(:use clojure.java.io)
	(:require [clojure.string :as str]))

; Read inputs from file and build the customers list and the graph
(defn build-graph [company input-file]
	(def tmp-company company)
	(with-open [rdr (reader input-file)]
		(let [first-line (first (line-seq rdr))]
			(if (= nil first-line)
				(println "Empty file")
				(do
					(let [link (str/split first-line #"\s")]
						(def tmp-company (graph/add-node tmp-company
							(graph/make-node nil (link 0))))

						(def tmp-company (graph/add-node tmp-company
							(graph/make-node (link 0) (link 1))))
				(doseq [line (line-seq rdr)]
					(let [link (str/split line #"\s")]
						(let [src (link 0) dst (link 1)]
							(def tmp-company (graph/add-node tmp-company (graph/make-node src dst)))))))))))
	tmp-company)

(defn input-html
	"Return string in of the inputs of the program in the html format."
	[input-file]
	(let [input-list (atom "")]
	(with-open [rdr (reader input-file)]
		(doseq [line (line-seq rdr)]
			(swap! input-list #(println-str %1 line "</br>"))))
			@input-list))

(defn rank-html
	"Return string in of the rank of the company."
	[company]
	(graph/html-json company))

(defn add-customer
	"Receive customer source and customer destination from POST and add them to graph."
	[company src dst]
	(graph/add-node company (graph/make-node src dst)))

(defn graph-html
	"Graph in html format"
	[company]
	(graph/html-graph company))
