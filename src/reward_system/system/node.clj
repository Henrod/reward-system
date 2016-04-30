(ns reward-system.system.node
	(:require [reward-system.system.util :as util]))

(defprotocol NodeOperations
	"A protocol to represent customer as nodes in a graph"
	(add-link [this new-node] "Add new node to linked nodes")
	(print! [this] "Print the attributes of a node")
	(html-node [this data] "Returns attributes of a node in html format"))

; Customer record.
	; value: number representing customer in the network
	; parent: first person who invited this customer
	; score: reward
	; links:  list of nodes connected (customers this invited)
(defrecord Node [value parent score links]
	NodeOperations
	(add-link [this new-node]
		(->Node
			(.value this)
			(.parent this)
			(.score this)
			(cons new-node (.links this))))

	(print! [this]
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

(defn build-node [parent value]
	"Create node with default values. Parent is only the integer representing its value."
	(if (= parent nil)
		(->Node (util/parse-int value) nil 0 ())
		(->Node (util/parse-int value) (util/parse-int parent) 0 ())))
