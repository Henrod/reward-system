  (ns reward-system.core-test
  (:require 	[clojure.test :refer :all]
            	[reward-system.core :refer :all]
            	[ring.mock.request :as mock]
            	[clojure.data.json :as json]))

(def number-of-tests 100)
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

(defn build-input
	[size]
	(reduce into [] (for [i (range size)]
				(let [src (rand-number-keyword) dst (rand-number-keyword src)]
					[src dst]))))

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
	(testing "first invite"
		(let [	src (rand-number-keyword) 
        			dst (rand-number-keyword src)
        			obj {src {:parent [] :neighbors [dst] :point 0}, dst {:parent [src] :neighbors [] :point 0}}
		        	res (update-points obj src -1)]
		        	(is (= res obj))))
	(testing "second invite"
		(let [	obj {  :1 {:parent [] :neighbors [:2] :point 0}
				:2 {:parent [:1] :neighbors [:3] :point 0}
				:3 {:parent [:2] :neighbors [] :point 0}}
			exp { :1 {:parent [] :neighbors [:2] :point 1M}
				:2 {:parent [:1] :neighbors [:3] :point 0}
				:3 {:parent [:2] :neighbors [] :point 0}}
		        	res (update-points obj :2 -1)]
		        	(is (= res exp))))
	(testing "second invite"
		(let [	obj {  :1 {:parent [] :neighbors [:2] :point 1M}
				:2 {:parent [:1] :neighbors [:3] :point 0}
				:3 {:parent [:2] :neighbors [:4] :point 0}
				:4 {:parent [:3] :neighbors [] :point 0}}
			exp {  :1 {:parent [] :neighbors [:2] :point 1.5M}
				:2 {:parent [:1] :neighbors [:3] :point 1M}
				:3 {:parent [:2] :neighbors [:4] :point 0}
				:4 {:parent [:3] :neighbors [] :point 0}}
		        	res (update-points obj :3 -1)]
		        	(is (= res exp))))
	(testing "when 2 invites 5, new points are computed"
		(let [	obj {  :1 {:parent [] :neighbors [:2] :point 1.5M}
				:2 {:parent [:1] :neighbors [:3 :5] :point 1M}
				:3 {:parent [:2] :neighbors [:4] :point 0}
				:4 {:parent [:3] :neighbors [] :point 0}
				:5 {:parent [:2] :neighbors [] :point 0}}
			exp {  :1 {:parent [] :neighbors [:2] :point 2.5M}
				:2 {:parent [:1] :neighbors [:3 :5] :point 1M}
				:3 {:parent [:2] :neighbors [:4] :point 0}
				:4 {:parent [:3] :neighbors [] :point 0}
				:5 {:parent [:2] :neighbors [] :point 0}}
		        	res (update-points obj :2 -1)]
		        	(is (= res exp)))))

(deftest test-update-obj
	(testing "empty obj adding a new connection"
		(seq 
			(for [n (range number-of-tests)]
				(let [src (rand-number-keyword)
				         dst (rand-number-keyword src)
				         obj (update-obj {}  src dst)]
					(and 
						(is (= src (get-in obj [dst :parent 0])))
						(is (= dst (get-in obj [src :neighbors 0]))))))))

	(testing "empty obj, first user can't invite himself/herself"
		(let [src (rand-number-keyword) 
		          obj (update-obj {}  src src)]
			(is (empty? obj))))

	(testing "non empty obj, someone inside obj inviting someone outside"
		(seq
			(for [n (range number-of-tests)]
				(let [input [:1 :2, :1 :3, :3 :4, :2 :4, :4 :5, :4 :6]
				         src (let [users (vec (distinct input))] (get users (rand-int (count users))))
	            		         dst (rand-number-keyword 6 :after)
	            		         obj (update-obj (build input) src dst)]
					(and
	          					(is (= (get-in obj [dst :parent 0]) src))
	               				(is (= (last (get-in obj [src :neighbors])) dst)))))))
 
 	(testing "non empty obj, someone inside obj inviting someone inside"
 		(seq
			(for [n (range number-of-tests)]
				(let [input [:1 :2, :1 :3, :3 :4, :2 :4, :4 :5, :4 :6]
				         src (let [users (distinct input)] (get users (rand-int (count users))))
	            		         dst (let [users (remove #{src} (distinct input))] (get users (rand-int (count users))))
	            		         obj (update-obj (build input) src dst)]
					(and
	          					(is (= (last (get-in obj [dst :parent])) src))
	               				(is (= (last (get-in obj [src :neighbors])) dst)))))))
  
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

(deftest test-parse-value
	(testing "zero"
		(is (= "0" (parse-value 0))))
	(testing "BigDecimal zero"
		(is (= "0" (parse-value 0M))))
	(testing "Integer"
		(is (= "1.0" (parse-value 1M))))
	(testing "Another integer"
		(is (= "3.0" (parse-value 3M))))
	(testing "BigDecimal float"
		(is (= "3.5" (parse-value 3.5M))))
	(testing "BigDecimal float trailing zeroes 3.50M"
		(is (= "3.5" (parse-value 3.50M))))
	(testing "BigDecimal float trailing zeroes 3.500M"
		(is (= "3.5" (parse-value 3.500M))))
	(testing "BigDecimal float trailing zeroes 3.500000000000M"
		(is (= "3.5" (parse-value 3.500000000000M))))
	(testing "BigDecimal float trailing zeroes 3.500550055005500550055055M"
		(is (= "3.500550055005500550055055" (parse-value 3.500550055005500550055055M))))
	(testing "BigDecimal float trailing zeroes 3.500550055005500550055055000000000M"
		(is (= "3.500550055005500550055055" (parse-value 3.500550055005500550055055000000000M)))))

#_(deftest test-parse-result
	(testing "parsing result"
		(let [result (build (build-input number-of-tests)) parse (parse-result result)]
			(seq
				(for [[key points] parse]
					(is (= points (get-in result [key :point]))))))))

(deftest test-get
	(testing "get result from little input"
		(is (= (routes (mock/request :get "/little"))
			{:status 200
			  :headers {"Content-Type" "application/json"}
			  :body "{\"1\":\"2.5\",\"2\":\"0\",\"3\":\"1.0\",\"4\":\"0\",\"5\":\"0\",\"6\":\"0\"}"})))
	(testing "get result from big input"
		(is (= (routes (mock/request :get "/"))
			{:status 200
			  :headers {"Content-Type" "application/json"}
			  :body "{\"60\":\"0\",\"72\":\"0\",\"146\":\"0\",\"14\":\"2.00000000069849193096160888671875\",\"120\":\"0\",\"109\":\"0\",\"88\":\"0\",\"39\":\"2.0234375\",\"89\":\"0\",\"37\":\"2.005859375\",\"130\":\"0\",\"59\":\"0\",\"108\":\"0\",\"96\":\"0\",\"64\":\"0\",\"27\":\"2.0000057220458984375\",\"138\":\"0\",\"42\":\"2.1875\",\"45\":\"3.5\",\"114\":\"0\",\"149\":\"0\",\"77\":\"0\",\"83\":\"0\",\"123\":\"0\",\"31\":\"2.000091552734375\",\"65\":\"0\",\"40\":\"2.046875\",\"18\":\"2.0000000111758708953857421875\",\"52\":\"0\",\"190\":\"0\",\"12\":\"2.0000000001746229827404022216796875\",\"115\":\"0\",\"11\":\"2.00000000008731149137020111083984375\",\"24\":\"2.0000007152557373046875\",\"76\":\"0\",\"75\":\"0\",\"10\":\"2.000000000043655745685100555419921875\",\"21\":\"2.0000000894069671630859375\",\"125\":\"0\",\"117\":\"0\",\"56\":\"0\",\"70\":\"0\",\"110\":\"0\",\"73\":\"0\",\"118\":\"0\",\"61\":\"0\",\"142\":\"0\",\"23\":\"2.00000035762786865234375\",\"143\":\"0\",\"13\":\"2.000000000349245965480804443359375\",\"122\":\"0\",\"66\":\"0\",\"58\":\"0\",\"126\":\"0\",\"140\":\"0\",\"119\":\"0\",\"133\":\"0\",\"30\":\"2.0000457763671875\",\"38\":\"2.01171875\",\"71\":\"0\",\"53\":\"0\",\"4\":\"2.000000000000682121026329696178436279296875\",\"43\":\"2.375\",\"57\":\"0\",\"26\":\"2.00000286102294921875\",\"79\":\"0\",\"95\":\"0\",\"98\":\"0\",\"106\":\"0\",\"16\":\"2.000000002793967723846435546875\",\"131\":\"0\",\"139\":\"0\",\"44\":\"2.75\",\"87\":\"0\",\"85\":\"0\",\"100\":\"0\",\"7\":\"2.000000000005456968210637569427490234375\",\"91\":\"0\",\"105\":\"0\",\"35\":\"2.00146484375\",\"81\":\"0\",\"55\":\"0\",\"1\":\"2.000000000000085265128291212022304534912109375\",\"78\":\"0\",\"147\":\"0\",\"160\":\"0\",\"50\":\"50.0\",\"8\":\"2.00000000001091393642127513885498046875\",\"113\":\"0\",\"62\":\"0\",\"36\":\"2.0029296875\",\"22\":\"2.000000178813934326171875\",\"124\":\"0\",\"47\":\"8.0\",\"67\":\"0\",\"74\":\"0\",\"25\":\"2.000001430511474609375\",\"9\":\"2.0000000000218278728425502777099609375\",\"141\":\"0\",\"20\":\"2.00000004470348358154296875\",\"86\":\"0\",\"135\":\"0\",\"17\":\"2.00000000558793544769287109375\",\"46\":\"5.0\",\"102\":\"0\",\"144\":\"0\",\"32\":\"2.00018310546875\",\"49\":\"26.0\",\"112\":\"0\",\"28\":\"2.000011444091796875\",\"48\":\"14.0\",\"19\":\"2.000000022351741790771484375\",\"69\":\"0\",\"94\":\"0\",\"2\":\"2.00000000000017053025658242404460906982421875\",\"103\":\"0\",\"148\":\"0\",\"5\":\"2.00000000000136424205265939235687255859375\",\"41\":\"2.09375\",\"121\":\"0\",\"90\":\"0\",\"107\":\"0\",\"137\":\"0\",\"101\":\"0\",\"111\":\"0\",\"15\":\"2.0000000013969838619232177734375\",\"145\":\"0\",\"99\":\"0\",\"97\":\"0\",\"3\":\"2.0000000000003410605131648480892181396484375\",\"93\":\"0\",\"127\":\"0\",\"6\":\"2.0000000000027284841053187847137451171875\",\"33\":\"2.0003662109375\",\"150\":\"0\",\"51\":\"0\",\"63\":\"0\",\"104\":\"0\",\"54\":\"0\",\"116\":\"0\",\"82\":\"0\",\"84\":\"0\",\"92\":\"0\",\"80\":\"0\",\"29\":\"2.00002288818359375\",\"134\":\"0\",\"132\":\"0\",\"34\":\"2.000732421875\",\"129\":\"0\",\"128\":\"0\",\"180\":\"0\",\"68\":\"0\",\"136\":\"0\"}"}))))

(deftest test-post
	(testing "post with no new invites"
		(is (= (routes (mock/request :post "/"))
			{:status 200
			  :headers {"Content-Type" "application/json"}
			  :body "{\"1\":\"2.5\",\"2\":\"0\",\"3\":\"1.0\",\"4\":\"0\",\"5\":\"0\",\"6\":\"0\"}"})))
	(testing "post with user inviting nil"
		(is (= (routes (assoc (mock/request :post "/") :5 nil))
			{:status 200
			  :headers {"Content-Type" "application/json"}
			  :body "{\"1\":\"2.5\",\"2\":\"0\",\"3\":\"1.0\",\"4\":\"0\",\"5\":\"0\",\"6\":\"0\"}"})))
	(testing "post with nil inviting nil"
		(is (= (routes (assoc (mock/request :post "/") nil nil))
			{:status 200
			  :headers {"Content-Type" "application/json"}
			  :body "{\"1\":\"2.5\",\"2\":\"0\",\"3\":\"1.0\",\"4\":\"0\",\"5\":\"0\",\"6\":\"0\"}"})))
	(testing "adding new invites to little input"
		(is (= (routes (assoc (mock/request :post "/") :params {"obj" "{\"5\": 7, \"6\": 8}"}))
			{:status 200
			  :headers {"Content-Type" "application/json"}
			  :body "{\"1\":\"3.0\",\"2\":\"0\",\"3\":\"2.0\",\"4\":\"2.0\",\"5\":\"0\",\"6\":\"0\",\"7\":\"0\",\"8\":\"0\"}"}))))