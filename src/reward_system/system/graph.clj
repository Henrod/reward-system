(ns reward-system.system.graph
	(:require [reward-system.system.customerslist :as cl]
			  [reward-system.system.util :as util]
			  [reward-system.system.node :as node]
			  [clojure.string :as str])
	(:use clojure.java.io))

(defprotocol GraphOperations
	"Protocol with all methods that a graph must have"
	(build-graph [this input-file] "Read inputs from file and build the customers list and the graph")
	(add-customer [this src dst] "Add a new connection to the parent node. new-node: instance of type Node")
	(print! [this] "Print all nodes in the graph and their links.")
	(update-parents [this start-node] "Update parents score after node added.")
	(print-json [this] "Print the rank in JSON format.")
	(rank-html [this] "Return string in of the rank of the company.")
	(graph-html [this] "Returns the graph values to be printed in html."))

; Graph is a map with all nodes in the graph. Each node has a list of nodes linked to itself.
; Customers is a list of nodes that are already int the graph
(defrecord Graph [graph-map customers]
	GraphOperations
	(build-graph [this input-file]
		(let [tmp-this (atom this)]
		(with-open [rdr (reader input-file)]
			(let [first-line (first (line-seq rdr))]
				(if (= nil first-line)
					(println "Empty file")
					(do
						(let [link (str/split first-line #"\s")]
							(swap! tmp-this #(add-customer %1 nil (link 0)))
							(swap! tmp-this #(add-customer %1 (link 0) (link 1)))
					(doseq [line (line-seq rdr)]
						(let [link (str/split line #"\s")]
							(let [src (link 0) dst (link 1)]
								(swap! tmp-this #(add-customer %1 src dst))))))))))
		@tmp-this))

	(add-customer [this src dst]
		(let [new-graph (atom this) new-node (node/build-node src dst)]
			(let [parent-node (.parent new-node)]
			(if (nil? parent-node)
				(do
					(if (cl/not-has? (.customers @new-graph) (.value new-node))
							(swap! new-graph
								#(->Graph
									(.graph-map %1)
									(cl/add (.customers %1) (.value new-node)))))
					(swap! new-graph #(->Graph (assoc (.graph-map %1) (keyword (str (.value new-node))) new-node) (.customers %1))))

				(do
					(let [parent ((.graph-map @new-graph) (keyword (str parent-node)))]
						(swap! new-graph
							#(->Graph
								(assoc (.graph-map %1) (keyword (str parent-node)) (node/add-link parent new-node))
								(.customers %1)))

						(if (cl/not-has? (.customers @new-graph) (.value new-node))
							(do
								(swap! new-graph
									#(->Graph
										(assoc (.graph-map %1) (keyword (str (.value new-node))) new-node)
										(cl/add (.customers %1) (.value new-node)))))))))
			(swap! new-graph #(update-parents %1 new-node))
			@new-graph)))

	(print! [this]
		(cl/print! (.customers this))
		(doseq [[value node] (.graph-map this)]
			(node/print! node))
		(println ""))

	(update-parents [this base]
		(let [new-graph (atom this)]
			(if (or
					(= nil base)
					(= nil (.parent base))
					(> (count (.links ((keyword (str (.parent base))) (.graph-map @new-graph)))) 1))
				@new-graph
				(loop [base-value (.value base), parent ((keyword (str (.parent base))), (.graph-map @new-graph)) factor 1]
					(when (not= nil parent)
					(if (= nil (.parent parent))
						@new-graph
						(let [next-parent ((keyword (str (.parent parent))) (.graph-map @new-graph))]
							(let [new-parent
								(node/->Node
									(.value next-parent)
									(.parent next-parent)
									(+ factor (.score next-parent))
									(.links next-parent))]
								(swap! new-graph #(->Graph (assoc (.graph-map %1) (keyword (str (.value new-parent))) new-parent)
									(.customers %1)))
								(recur
									(.value parent)
									((keyword (str (.parent parent))) (.graph-map @new-graph))
									(* factor 0.5))))))))
		@new-graph))

	(print-json [this]
		(let [tmp-list (atom [])]
			(doseq [[value node] (.graph-map this)]
				(swap! tmp-list #(conj %1 node)))
			(println "{")
			(def itr-count 1)
			(let [size (count @tmp-list)]
				(doseq [node (sort-by :score > @tmp-list)]
					(print (str "\t\"" (.value node) "\": " (.score node)))
					(if (= itr-count size)
						(println "")
						(println ","))
					(def itr-count (inc itr-count))))
			(println "}")))

	(rank-html [this]
		(let [tmp-list (atom [])]
			(doseq [[value node] (.graph-map this)]
				(swap! tmp-list #(conj %1 node)))
			(let [json (atom "")]
				(swap! json #(str %1 "{</br>" ))
				(let [itr-count (atom 1) size (count @tmp-list)]
					(doseq [node (sort-by :score > @tmp-list)]
						(swap! json #(str %1 "\"" (.value node) "\": " (.score node)))
						(if (= @itr-count size)
							(swap! json #(str %1 "</br>"))
							(swap! json #(str %1 ",</br>")))
						(swap! itr-count #(inc %1))))
				(swap! json #(str %1 "}"))
				@json)))

	(graph-html [this]
		(let [html (atom "")]
		(doseq [[value node] (.graph-map this)]
			(swap! html #(node/html-node node %1)))
		@html)))
