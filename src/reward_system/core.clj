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
	(into [] (concat input (map #(into [] %) (partition 2 pairs)))))

(defn- get-parent
	[src obj]
	(if (empty? (:parent (src obj)))
		nil 
		((:parent (src obj)) 0)))

(defn update-points
	[obj src k]
	(if (nil? src)
		obj
		(if (< k 0)
			(update-points  obj  (get-parent src obj)  (inc k))
			(update-points 
				(update-in obj [src :point] #(+ % (math/expt 0.5 k)))
				(get-parent src obj) (inc k)))))

(defn update-obj
	[obj [src dst]]
	(if (= src dst)
		obj
		(if (contains? obj src)
			(if (some #{dst} (:neighbors (src obj)))
				obj
				(let [new-obj (update-in obj [src :neighbors] #(conj % dst))]
					(if (contains? obj dst)
						(if (empty? (:neighbors (src obj)))
							(update-points (update-in new-obj [dst :parent] #(conj % src)) src -1)
							new-obj)
						(if (empty? (:neighbors (src obj)))
							(update-points
								(assoc 
									new-obj
									dst {:neighbors [] :parent [src] :point 0})
								src -1)
							(assoc 
								new-obj
								dst {:neighbors [] :parent [src] :point 0})))))
			(assoc 
				(assoc  obj src {:neighbors [dst] :parent [] :point 0})
				dst {:neighbors [] :parent [src] :point 0}))))

(defn build
	[input]
	(loop [obj {} [pair & input#] input]
		(if (nil? pair)
			obj
			(recur 
				(update-obj obj pair)
				input#))))

(defn input-from-file
	[file-path]
	(let [input (ref [])]
		(with-open [rdr (io/reader file-path)]
			(doseq [line (line-seq rdr)]
				(let [numbers (clojure.string/split line #" ") src (keyword (str (first numbers))) dst (keyword (str (second numbers)))]
					(dosync (alter input conj [src dst])))))
		@input))

(defn parse-result
	[obj]
	(loop [[[key value] & obj#] (seq obj) result {}]
		(if (nil? key)
			(json/write-str result)
			(recur obj# (assoc result key (:point value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SERVER CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defroutes routes
	(GET "/" [] {:status 200
				 :headers {"Content-Type" "application/json"}
				 :body (parse-result (build (input-from-file "test/reward_system/input.txt")))})
	(POST "/" req 
		(let [obj (json/read-str ((:params req) "obj")  :key-fn keyword)
			  input (reduce #(concat %1 (map (fn [elm] (if (keyword? elm) elm (keyword (str elm)))) %2)) [] (seq obj))]
			  {:status 200
				 :headers {"Content-Type" "application/json"}
				 :body (parse-result (build (apply add-to-input (input-from-file "test/reward_system/little_input.txt") input)))})))

(def app (wrap-params routes))

(defn -main []
	(println "Running on port 3000")
	(run-jetty app {:port 3000}))