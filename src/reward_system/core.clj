(ns reward-system.core)

(defn self-inviting 
	"A person can't invite himself/herself"
	[network node parent]
	(= node parent))

(defn repeated-invitation 
   "Repeat an old invitation can't generate further points"
	[network node parent]
	(contains? (-> parent network :adj) node))

(defn invitee-not-in-network 
    "A person outside the network can't invite anyone"
	[network node parent]
	(not (contains? network parent)))

(defn not-respect-constraints 
   "Return an array of booleans. True for each constraint it doesn't respect"
	[network node parent]
	(some true? ((juxt 
                              self-inviting
                              invitee-not-in-network
		                       repeated-invitation) network node parent)))

(defn update-points 
   "Update the points of a parent and backwards until the root. Return the network"
	[parent network]
	(if (-> parent network :adj empty? not)
		network
		(reduce
			(fn [m [k v]] (update-in m [k :pts] + v))
			network
			(map
				vector
				(take-while integer? (iterate #(-> % network :par) (-> parent network :par)))
				(iterate #(/ % 2) 1)))))

(defn add-node 
   "Add a new node to the network if the constraints are respected. Return the network"
	[network node parent]
	(if (not-respect-constraints network node parent)
		network
		(update-in
			(update-points 
				parent
				(if (contains? network node)
				  network
				  (assoc network node {:par parent :pts 0M :adj #{}})))
			[parent :adj]
			conj
			node)))

(defn add-nodes 
    "Repeatedly add nodes from a list to the network. Return the network"
	[network nps]
	{:pre [(even? (count nps))]}
	(let [lls (partition 2 nps)]
		(reduce (fn [m [s d]] (add-node m d s)) network lls)))

(defn read-file 
   "Reads a file and returns a lazy sequence with node and parent repeated"
	[file]
	(let [input (slurp file)]
		(pmap read-string (filter #(not (empty? %)) (clojure.string/split input #"\s|\r\n")))))

(defn build-from-file 
   "Read a file and build the network from it"
	[file]
	(let [input (read-file file)
		     root (first input)
		     network {root {:par nil :pts 0M :adj #{}}}]
	(add-nodes network input)))

(defn rank 
   "Return a list of lists with the rank and the points of each member"
	[network]
	(let [p (fn [k] (-> k network :pts))
		     r (reverse (sort-by p (keys network)))]
		(pmap list r (pmap #(-> % network :pts) r))))