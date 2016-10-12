  (ns reward-system.core-test
  (:require [clojure.test :refer :all]
            [reward-system.core :refer :all]))

(def number-of-tests 10)
(def max-number (* number-of-tests 10))

(defn- rand-number-keyword
	([] (keyword (str (rand-int max-number))))
	([but] (loop [found (rand-number-keyword)]
			(if (= found but)
				(recur (rand-number-keyword))
				found)))
 	([min key] 
   		(if (= key :after)
	     		(+ min (rand-int max-number)))))

(defn- build-input
	[size]
	(let [src (rand-number-keyword) dst (rand-number-keyword src)]
		(loop [i 0 result [[src dst]]]
			(if (< i size)
				(let [	new-src ((result (rand-int (count result))) (rand-int 2))
					new-dst (rand-number-keyword new-src)]
						(recur (inc i) (conj result [new-src new-dst])))
				result))))

(deftest test-add-to-input
	(testing "Adding new users to input"
		(loop [n 0 result [] src (rand-number-keyword) dst (rand-number-keyword)]
			(if (< n number-of-tests)
				(let [new-result (add-to-input result src dst)]
					(if (and 
							(is (= (+ 2 (count result)) (count new-result)))
							(is (= (take-last 2 new-result) [src dst])))
						(recur (inc n) new-result (rand-number-keyword) (rand-number-keyword))))))))

(deftest test-update-points
	(testing "adding invites and updating points")
		)

(deftest test-update-obj
	(testing "empty obj adding a new connection"
		(for [n (range number-of-tests)]
			(let [src (rand-number-keyword)
			         dst (rand-number-keyword src)
			         obj (update-obj {}  src dst)]
				(and 
					(is (= src (get-in obj [dst :parent 0])))
					(is (= dst (get-in obj [src :neighbors 0])))))))

	(testing "empty obj, first user can't invite himself/herself"
		(let [src (rand-number-keyword) 
		          obj (update-obj {}  src src)]
			(is (empty? obj))))

	(testing "non empty obj, someone inside obj inviting someone outside"
		(for [n (range number-of-tests)]
			(let [input [:1 :2, :1 :3, :3 :4, :2 :4, :4 :5, :4 :6]
			         src (let [users (distinct input)] (get users (rand-int (count users))))
            		         dst (rand-number-keyword 6 :after)
            		         obj (update-obj (build input) src dst)]
				(and
          					(is (= (get-in obj [dst :parent 0]) src))
               				(is (= (last (get-in obj [src :neighbors])) dst))))))
 
 	(testing "non empty obj, someone inside obj inviting someone inside"
		(for [n (range number-of-tests)]
			(let [input [:1 :2, :1 :3, :3 :4, :2 :4, :4 :5, :4 :6]
			         src (let [users (distinct input)] (get users (rand-int (count users))))
            		         dst (let [users (remove #{src} (distinct input))] (get users (rand-int (count users))))
            		         obj (update-obj (build input) src dst)]
				(and
          					(is (= (last (get-in obj [dst :parent])) src))
               				(is (= (last (get-in obj [src :neighbors])) dst))))))
  
  	(testing "non empty obj, multiple invites from src to dst doesn't change anything"
     		(let [src (rand-number-keyword) dst (rand-number-keyword src)]
			(loop [n number-of-tests obj (update-obj {} src dst)]
     				(if (and 
               				(pos? n) 
                   				(is (= obj 
                              				{src {:parent [] :neighbors [dst] :point 0}
                                   				  dst {:parent [src] :neighbors [] :point 0}})))
           					(recur (dec n) (update-obj obj src dst)))))))

(deftest test-build-result-for-little-input
	(testing "build result"
		(let [	input [:1 :2, :1 :3, :3 :4, :2 :4, :4 :5, :4 :6] 
			result (build input)
			count (reduce (fn [m [k v]] (assoc m k (:point v))) {} result)
			result# {:1 2.5M, :2 0, :3 1M, :4 0, :5 0, :6 0}]
				(is (= count result#))))

	(testing "build result and add users"
		(let [	input (add-to-input [:1 :2 :1 :3 :3 :4 :2 :4 :4 :5 :4 :6]  :5 :7 :6 :8)
			result (build input)
			count (reduce (fn [m [k v]] (assoc m k (:point v))) {} result)
			result# {:1 3.0M, :2 0, :3 2.0M, :4 2.0M, :5 0, :6 0, :7 0, :8 0}]
				(is (= count result#))))

	(testing "someone outside can't invite"
		(let [	input (add-to-input [:1 :2 :1 :3 :3 :4 :2 :4 :4 :5 :4 :6]  :100 :5)
			result (build input)
			count (reduce (fn [m [k v]] (assoc m k (:point v))) {} result)
			result# {:1 2.5M, :2 0, :3 1.0M, :4 0, :5 0, :6 0}]
				(is (= count result#))))

	(testing "nil invites someone, nothing changes"
		(let [	input (add-to-input [:1 :2 :1 :3 :3 :4 :2 :4 :4 :5 :4 :6]  nil :5)
			result (build input)
			count (reduce (fn [m [k v]] (assoc m k (:point v))) {} result)
			result# {:1 2.5M, :2 0, :3 1.0M, :4 0, :5 0, :6 0}]
				(is (= count result#))))

	(testing "someone invites nil, nothing changes"
		(let [	input (add-to-input [:1 :2 :1 :3 :3 :4 :2 :4 :4 :5 :4 :6]  :1 nil)
			result (build input)
			count (reduce (fn [m [k v]] (assoc m k (:point v))) {} result)
			result# {:1 2.5M, :2 0, :3 1.0M, :4 0, :5 0, :6 0}]
				(is (= count result#))))

	(testing "someone invites nil, nothing changes [2]"
		(let [	input (add-to-input [:1 :2 :1 :3 :3 :4 :2 :4 :4 :5 :4 :6]  :1)
			result (build input)
			count (reduce (fn [m [k v]] (assoc m k (:point v))) {} result)
			result# {:1 2.5M, :2 0, :3 1.0M, :4 0, :5 0, :6 0}]
				(is (= count result#))))

	(testing "someone invites himself/herself, nothing changes"
		(let [	input (add-to-input [:1 :2 :1 :3 :3 :4 :2 :4 :4 :5 :4 :6]  :5 :5)
			result (build input)
			count (reduce (fn [m [k v]] (assoc m k (:point v))) {} result)
			result# {:1 2.5M, :2 0, :3 1.0M, :4 0, :5 0, :6 0}]
				(is (= count result#)))))

(deftest test-input-from-file
	(testing "read little input"
		(let [	input (input-from-file "test/reward_system/little_input.txt")
			expected [:1 :2, :1 :3, :3 :4, :2 :4, :4 :5, :4 :6]]
				(is (= input expected))))
	(testing "read big input"
		(let [	input (input-from-file "test/reward_system/input.txt")
			expected [:1 :2, :2 :3, :3 :4, :4 :5, :5 :6, :6 :7, :7 :8, :8 :9, :9 :10, :10 :11, :11 :12, :12 :13, :13 :14, :14 :15, :15 :16, :16 :17, :17 :18, :18 :19, :19 :20, :20 :21, :21 :22, :22 :23, :23 :24, :24 :25, :25 :26, :26 :27, :27 :28, :28 :29, :29 :30, :30 :31, :31 :32, :32 :33, :33 :34, :34 :35, :35 :36, :36 :37, :37 :38, :38 :39, :39 :40, :40 :41, :41 :42, :42 :43, :43 :44, :44 :45, :45 :46, :46 :47, :47 :48, :48 :49, :49 :50, :50 :51, :51 :101, :50 :52, :52 :102, :50 :53, :53 :103, :50 :54, :54 :104, :50 :55, :55 :105, :50 :56, :56 :106, :50 :57, :57 :107, :50 :58, :58 :108, :50 :59, :59 :109, :50 :60, :60 :110, :50 :61, :61 :111, :50 :62, :62 :112, :50 :63, :63 :113, :50 :64, :64 :114, :50 :65, :65 :115, :50 :66, :66 :116, :50 :67, :67 :117, :50 :68, :68 :118, :50 :69, :69 :119, :50 :70, :70 :120, :50 :71, :71 :121, :50 :72, :72 :122, :50 :73, :73 :123, :50 :74, :74 :124, :50 :75, :75 :125, :50 :76, :76 :126, :50 :77, :77 :127, :50 :78, :78 :128, :50 :79, :79 :129, :50 :80, :80 :130, :50 :81, :81 :131, :50 :82, :82 :132, :50 :83, :83 :133, :50 :84, :84 :134, :50 :85, :85 :135, :50 :86, :86 :136, :50 :87, :87 :137, :50 :88, :88 :138, :50 :89, :89 :139, :50 :90, :90 :140, :50 :91, :91 :141, :50 :92, :92 :142, :50 :93, :93 :143, :50 :94, :94 :144, :50 :95, :95 :145, :50 :96, :96 :146, :50 :97, :97 :147, :50 :98, :98 :148, :50 :99, :99 :149, :50 :100, :100 :150, :69 :70, :70 :53, :52 :54, :59 :21, :86 :9, :94 :43, :59 :96, :15 :69, :15 :40, :93 :2, :77 :52, :74 :66, :39 :81, :8 :18, :61 :68, :63 :15, :51 :50, :93 :77, :28 :91, :23 :22, :42 :1, :95 :96, :53 :53, :1 :46, :10 :49, :34 :26, :80 :21, :88 :94, :11 :10, :80 :24, :10 :100, :20 :110, :30 :120, :30 :130, :40 :140, :50 :150, :60 :160, :80 :100, :90 :100, :56 :100, :12 :10, :10 :12, :100 :56, :100 :90, :40 :140, :10 :150, :11 :180, :10 :190, :24 :48, :25 :49, :16 :48]]
				(is (= input expected)))))

(deftest test-parse-result
	(testing "parsing result"
		(let [result (build (build-input number-of-tests)) parse (parse-result result)]
			(for [[key points] parse]
				(is (= points (get-in result [key :point])))))))