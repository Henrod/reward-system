  (ns reward-system.core-test
  (:require [clojure.test :refer :all]
            [reward-system.core :refer :all]))

(def number-of-tests 1000)
(def max-number (* number-of-tests 10))

(defn- rand-number-keyword
	([] (keyword (str (rand-int max-number))))
	([but] (loop [found (rand-number-keyword)]
			(if (= found but)
				(recur (rand-number-keyword))
				found))))

(deftest add-to-input-test
	(testing "Adding new users to input"
		(loop [	n 0 
         			result [] 
            			src (rand-number-keyword) 
               		dst (rand-number-keyword)]
			(when (< n number-of-tests)
				(let [new-result (add-to-input result src dst)]
					(is (= (inc (count result)) (count new-result)))
					(is (= (last new-result) [src dst]))
					(recur (inc n) new-result (rand-number-keyword) (rand-number-keyword)))))))

(deftest update-obj-test
	(testing "Update one pair of users to input"
    		(loop [	src (rand-number-keyword)
                		dst (rand-number-keyword src)
                		result (update-obj {} [src dst])
                		n 0]
                	(when (< n number-of-tests)
	                	(let [new-result (update-obj result [src dst])
	                		  get-parent (fn [k m] 
	                		  				(if (empty? (:parent (k m))) 
	                		  					nil 
	                		  					((:parent (k m)) 0)))
	                		  parent (get-parent src new-result)]
	                		  (if (not (nil? parent))
		                		  (if (not (empty? (:neighbors (src result))))
		                			(is (= (parent result) (parent new-result)))
		                			(loop [gparent parent k 1.0]
		                				(when (not (nil? gparent))
			                				(is (= (:point (gparent new-result)) (:point (update (gparent result) :point #(+ k %)))))
			                				(recur (get-parent gparent result) (/ k 2))))))
	                	(let [next-src ((into [] (keys new-result)) (rand-int (count new-result)))
	                		 next-dst (rand-number-keyword next-src)]
	                		(recur next-src next-dst new-result  (inc n))))))))

(defn- build-input
	[size]
	(let [src (rand-number-keyword) dst (rand-number-keyword src)]
		(loop [i 0 result [[src dst]]]
			(if (< i size)
				(let [new-src ((result (rand-int (count result))) (rand-int 2))
					  new-dst (rand-number-keyword new-src)]
					(recur (inc i) (conj result [new-src new-dst])))
				result))))

(defn inverseBFS
	[input]
	(let [result (build input)
		  users (distinct (into [] (map second input)))]
	  	(loop [[user & users#] users
	  			parents (:parent (result user)) 
	  			count (reduce #(assoc %1 %2 0) {} (keys result)) 
	  			been-invited #{}]
	  		(if (nil? user)
	  			(for [c count]
	  				(is (= (second c) (:point (result (first c))))))
	  			(recur 
	  				users#
	  				(:parent (result (first users#)))
	  				(loop [k 1.0 [level-count level-parents been-invited] [count parents been-invited]]
	  					(if (empty? level-parents)
	  						level-count
		  					(recur 
		  						(/ k 2) 
			  					(loop [[parent & parents#] level-parents
			  							inner-count level-count 
			  							next-parents [] 
			  							next-been-invited been-invited]
			  						(if (nil? parent)
			  							[inner-count next-parents been-invited]
					  					(if (or (empty? (:neighbors (result user))) (contains? next-been-invited user))
					  						(recur 
					  							parents# 
					  							inner-count 
					  							(concat next-parents (:parent (result parent))) 
					  							(conj next-been-invited user))
					  						(recur
					  							parents#
					  							(update inner-count parent  #(+ % k))
					  							(concat next-parents (:parent (result parent))) 
					  							(conj next-been-invited user))))))))
	  				(conj been-invited user))))))

(deftest build-test
	(testing "Build from list and test points"
		(inverseBFS (build-input number-of-tests))))

(deftest build-test-from-file
	(testing "Build from list and test points from little file"
		(inverseBFS (input-from-file "test/reward_system/little_input.txt")))
	(testing "Build from list and test points from file"
		(inverseBFS (input-from-file "test/reward_system/input.txt"))))

(deftest parse-result-test
	(testing "parsing result"
		(let [result (build (build-input number-of-tests))
			  parse (parse-result result)]
			(for [[key points] parse]
				(println key points)))))