(ns reward-system.system.graph
	(:require [reward-system.system.customerslist :as cl])
	(:require [reward-system.system.util :as util]))

; List to hold all customers in the company
(def customers (cl/->customers-list ()))

(defprotocol NodeOperations
	"A protocol to represent customer as nodes in a graph"
	(add-node-link [this new-node] "Add new node to linked nodes")
	(print-node [this] "Print the attributes of a node")
	(html-node [this data] "Returns attributes of a node in html format"))

; Customer record.
	; value: number representing customer in the network
	; parent: first person who invited this customer
	; score: reward
	; links:  list of nodes connected (customers this invited)
(defrecord Node [value parent score links]
	NodeOperations
	(add-node-link [this new-node]
		(->Node
			(.value this)
			(.parent this)
			(.score this)
			(cons new-node (.links this))))

	(print-node [this]
		(print "value:" (.value this) ", parent:" (.parent this) ", score:" (.score this) ", links: " )
		(doseq [linked-node (.links this)]
			(print (.value linked-node) " "))
		(println ""))

	(html-node [this data]
		(let [html (atom data)]
			(swap! html #(str %1 "value: " (.value this) "    ,    parent: " (.parent this) "    ,    score: " (.score this) "    ,    links: "))
			(doseq [linked-node (.links this)]
				(swap! html #(str %1 (.value linked-node) " ")))
			(swap! html #(str %1 "</br>"))
			@html)))

(defn make-node [parent value]
	"Create node with default values. Parent is only the integer representing its value."
	(if (= parent nil)
		(->Node (util/parse-int value) nil 0 ())
		(->Node (util/parse-int value) (util/parse-int parent) 0 ())))

(defprotocol GraphOperations
	"Protocol with all methods that a graph must have"
	(add-node [this new-node] "Add a new connection to the parent node.
		new-node: instance of type Node")
	(print! [this] "Print all nodes in the graph and their links.")
	(update-parents [this start-node] "Update parents score after node added.")
	(print-json [this] "Print the rank in JSON format.")
	(html-json [this] "Returns the rank in JSON format to be printed in html.")
	(html-graph [this] "Returns the graph values to be printed in html."))

; Graph is a map with all nodes in the graph. Each node has a list of nodes linked to itself.
(defrecord Graph [graph-map]
	GraphOperations
	(add-node [this new-node]
		(def new-graph this)

		(let [parent-node (.parent new-node)]
		(if (= parent-node nil)
			(do
				(if (cl/not-has? customers (.value new-node))
						(def customers (cl/add customers (.value new-node))))
				(def new-graph (->Graph (assoc (.graph-map new-graph) (keyword (str (.value new-node))) new-node))))
			(do
				(let [parent ((.graph-map new-graph) (keyword (str parent-node)))]
					(def new-graph (->Graph
						(assoc (.graph-map new-graph)
							(keyword (str parent-node)) (add-node-link parent new-node))))
					(if (cl/not-has? customers (.value new-node))
						(do
							(def customers (cl/add customers (.value new-node)))
							(def new-graph (->Graph
								(assoc (.graph-map new-graph)
									(keyword (str (.value new-node))) new-node)))))))))
		(def new-graph (update-parents new-graph new-node))
		new-graph)

	(print! [this]
		(cl/print! customers)
		(doseq [[value node] (.graph-map this)]
			(print-node node))
		(println ""))

	(update-parents [this base]
		(def new-graph this)
		(if (or
				(= nil base)
				(= nil (.parent base))
				(> (count (.links ((keyword (str (.parent base))) (.graph-map new-graph)))) 1))
			new-graph
			(loop [base-value (.value base), parent ((keyword (str (.parent base))), (.graph-map new-graph)) factor 1]
				(when (not= nil parent)
				(if (= nil (.parent parent))
					new-graph
					(let [next-parent ((keyword (str (.parent parent))) (.graph-map new-graph))]
						(let [new-parent
							(->Node
								(.value next-parent)
								(.parent next-parent)
								(+ factor (.score next-parent))
								(.links next-parent))]
							(def new-graph (->Graph (assoc (.graph-map new-graph) (keyword (str (.value new-parent))) new-parent)))
							(recur
								(.value parent)
								((keyword (str (.parent parent))) (.graph-map new-graph))
								(* factor 0.5))))))))
		new-graph)

		(print-json [this]
			(def tmp-list [])
			(doseq [[value node] (.graph-map this)]
				(def tmp-list (conj tmp-list node)))
			(println "{")
			(def itr-count 1)
			(let [size (count tmp-list)]
				(doseq [node (sort-by :score > tmp-list)]
					(print (str "\t\"" (.value node) "\": " (.score node)))
					(if (= itr-count size)
						(println "")
						(println ","))
					(def itr-count (inc itr-count))))
			(println "}"))

		(html-json [this]
			(def tmp-list [])
			(doseq [[value node] (.graph-map this)]
				(def tmp-list (conj tmp-list node)))
			(let [json (atom "")]
				(swap! json #(str %1 "{</br>" ))
				(let [itr-count (atom 1) size (count tmp-list)]
					(doseq [node (sort-by :score > tmp-list)]
						(swap! json #(str %1 "\"" (.value node) "\": " (.score node)))
						(if (= @itr-count size)
							(swap! json #(str %1 "</br>"))
							(swap! json #(str %1 ",</br>")))
						(swap! itr-count #(inc %1))))
				(swap! json #(str %1 "}"))
				@json))

		(html-graph [this]
			(let [html (atom "")]
			(doseq [[value node] (.graph-map this)]
				(swap! html #(html-node node %1)))
			@html))
	)
