(ns reward-system.core
	(:require
		[clojure.math.numeric-tower :as math]
		[clojure.java.io :as io]
		[clojure.data.json :as json])
	(:use 
		[ring.adapter.jetty :only [run-jetty]]
    		[compojure.core :only [defroutes GET POST]]
    		[ring.middleware.params :only [wrap-params]]))

(defn add-to-input
	[input & pairs]
	(apply conj input pairs))

(defn- get-parent
	[src obj]
	(get-in obj [src :parent 0]))

(defn update-points
	[obj src k]
	(if (nil? src)
		obj
		(if (neg? k)
			(update-points  obj  (get-parent src obj)  (inc k))
			(update-points 
				(update-in obj [src :point] (partial + (math/expt 0.5M k)))
				(get-parent src obj) (inc k)))))

(defn nor [a b] (not (or a b)))

(defn update-obj
	[obj src dst]
	(cond (or (= src dst) (nil? dst) (nil? src) 
		 	(and (contains? obj src) (some #{dst} (get-in obj [src :neighbors]))) 
		 	(nor (contains? obj src) (empty? obj)))
				obj
		(and (not (contains? obj src)) (empty? obj))
			(merge  obj 
				{src {:neighbors [dst] :parent [] :point 0}
				dst {:neighbors [] :parent [src] :point 0}})
		:else (let [ new-obj (update-in obj [src :neighbors] #(conj % dst))
				dst-in-graph (contains? obj dst)
				src-is-leaf (empty? (get-in obj [src :neighbors]))]
					(cond 
						(and dst-in-graph src-is-leaf) 
							(update-points (update-in new-obj [dst :parent] #(conj % src)) src -1)
						(and dst-in-graph (not src-is-leaf)) 
							new-obj
						(and (not dst-in-graph) src-is-leaf) 
							(update-points (merge new-obj {dst {:neighbors [] :parent [src] :point 0}}) src  -1)
						:else 
							(merge new-obj {dst {:neighbors [] :parent [src] :point 0}})))))

(defn build
	[input]
	(reduce (fn [obj [src dst]]  (update-obj obj src dst))  {}  (partition 2 input)))

(defn input-from-file
	[file-path]
	(let [input (ref []) to-key (comp keyword str)]
		(with-open [rdr (io/reader file-path)]
			(doseq [line (line-seq rdr)]
				(let [[src dst] (map to-key (clojure.string/split line #" "))]
					(dosync (alter input conj src dst)))))
		@input))

(defn parse-value
	[value]
	(cond
		(zero? value) 
			"0"
		(= 0M (rem value 1)) 
			(str (int value) ".0")
		(= \0 (last (str value)))
			(->> value 
				str 
				(re-seq #"(\d+.(0*[1-9]+)*)(0*)$") 
				first 
				second)
		:else 
			(str value)))

(defn parse-result
	[obj]
	(json/write-str (zipmap (keys obj) (map (comp parse-value :point) (vals obj)))))

; Server Configuration
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
			input (reduce (fn [res [f s]] (conj res f (-> s str keyword))) [] obj)]
			{:status 200
			  :headers {"Content-Type" "application/json"}
			  :body (-> (apply add-to-input (input-from-file "test/reward_system/little_input.txt") input)
			  		build
			  		parse-result)})))

(def app (wrap-params routes))

(defn -main []
	(println "Running on port 3000")
	(run-jetty app {:port 3000}))