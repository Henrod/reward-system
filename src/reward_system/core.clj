(ns reward-system.core)

(defn self-inviting [graph node parent]
	(= node parent))

(defn repeated-invitation [graph node parent]
	(contains? (:adj graph) node))

(defn invitee-not-in-network [graph node parent]
	(not (contains? graph parent)))

(defn not-respect-constraints [graph node parent]
	(some true? ((juxt 
                              self-inviting
                              invitee-not-in-network
		                       repeated-invitation) graph node parent)))

(defn update-points [parent graph]
	(if (-> parent graph :adj empty? not)
		graph
		(reduce
			(fn [m [k v]] (update-in m [k :pts] + v))
			graph
			(map
				vector
				(take-while integer? (iterate #(-> % graph :par) (-> parent graph :par)))
				(iterate #(/ % 2) 1)))))

(defn add-node [graph node parent]
	(if (not-respect-constraints graph node parent)
		graph
		(update-in
			(update-points 
				parent
				(if (contains? graph node)
				  graph
				  (assoc graph node {:par parent :pts 0M :adj #{}})))
			[parent :adj]
			conj
			node)))

(defn read-file [file]
	(let [input (slurp file)]
		(map read-string (filter #(not (empty? %)) (clojure.string/split input #"\s|\r\n")))))

(defn build-from-file [file]
	(let [input (partition 2 (read-file file))
		     root (-> input first first)
		     graph {root {:par nil :pts 0M :adj #{}}}]
	(reduce (fn [m [par son]] (add-node m son par)) graph input)))

(defn rank [graph]
	(let [p (fn [k] (-> k graph :pts))
		     r (reverse (sort-by p (keys graph)))]
		(map list r (map #(-> % graph :pts) r))))