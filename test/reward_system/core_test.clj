(ns reward-system.core-test
  (:require 	[clojure.test :refer :all]
            	[reward-system.core :refer :all]
            	[ring.mock.request :as mock]))

(def little-network {1 {:par nil, :adj #{2 3}, :pts 2.5M}
	                                  2 {:par 1 ,  :adj #{4},     :pts 0M}
	                                  3 {:par 1,   :adj #{4},     :pts 1M}
	                                  4 {:par 3,   :adj #{5 6}, :pts 0M}
	                                  5 {:par 4,   :adj #{},       :pts 0M}
	                                  6 {:par 4,   :adj #{},       :pts 0M}})

(deftest test-constraints
	(testing "Self inviting"
	   (is (true? (not-respect-constraints little-network 1 1))))

	(testing "Repeated invite"
		(is (true? (not-respect-constraints little-network 2 1))))

	(testing "Invitee not in the network"
		(is (true? (not-respect-constraints little-network 1 1000)))))

(deftest test-add-node
	(testing "Add invitation from 1 to 2"
		(is (= (add-node {1 {:par nil, :adj #{}, :pts 0M}} 2 1)
                 {1 {:par nil, :adj #{2}, :pts 0M}, 
                   2 {:par 1, :adj #{}, :pts 0M}})))

	(testing "Add invitation from 1 to 3"
		(is (= (add-node {1 {:par nil, :adj #{2}, :pts 0M}, 
			                             2 {:par 1, :adj #{}, :pts 0M}} 
			                            3 1)
                 {1 {:par nil, :adj #{2 3}, :pts 0M}, 
                   2 {:par 1, :adj #{}, :pts 0M}, 
                   3 {:par 1, :adj #{}, :pts 0M}})))

	(testing "Add invitation from 3 to 4"
		(is (= (add-node {1 {:par nil, :adj #{2 3}, :pts 0M}, 
                                        2 {:par 1, :adj #{}, :pts 0M}, 
                                        3 {:par 1, :adj #{}, :pts 0M}}
			                            4 3)
                 {1 {:par nil, :adj #{2 3}, :pts 1M}, 
                   2 {:par 1, :adj #{}, :pts 0M}, 
                   3 {:par 1, :adj #{4}, :pts 0M}
                   4 {:par 3, :adj #{}, :pts 0M}})))

	(testing "Add invitation from 2 to 4"
		(is (= (add-node {1 {:par nil, :adj #{2 3}, :pts 1M}, 
                                        2 {:par 1, :adj #{}, :pts 0M}, 
                                        3 {:par 1, :adj #{4}, :pts 0M}
                                        4 {:par 3, :adj #{}, :pts 0M}}
			                            4 2)
                 {1 {:par nil, :adj #{2 3}, :pts 2M}, 
                   2 {:par 1, :adj #{4}, :pts 0M}, 
                   3 {:par 1, :adj #{4}, :pts 0M}
                   4 {:par 3, :adj #{}, :pts 0M}})))

	(testing "Add invitation from 4 to 5"
		(is (= (add-node {1 {:par nil, :adj #{2 3}, :pts 2M}, 
                                        2 {:par 1, :adj #{4}, :pts 0M}, 
                                        3 {:par 1, :adj #{4}, :pts 0M}
                                        4 {:par 3, :adj #{}, :pts 0M}}
			                            5 4)
                 {1 {:par nil, :adj #{2 3}, :pts 2.5M}, 
                   2 {:par 1, :adj #{4}, :pts 0M}, 
                   3 {:par 1, :adj #{4}, :pts 1M}
                   4 {:par 3, :adj #{5}, :pts 0M}
                   5 {:par 4, :adj #{}, :pts 0M}})))

	(testing "Add invitation from 4 to 6"
		(is (= (add-node {1 {:par nil, :adj #{2 3}, :pts 2.5M}, 
                                        2 {:par 1, :adj #{4}, :pts 0M}, 
                                        3 {:par 1, :adj #{4}, :pts 1M}
                                        4 {:par 3, :adj #{5}, :pts 0M}
                                        5 {:par 4, :adj #{}, :pts 0M}}
			                            6 4)
                 {1 {:par nil, :adj #{2 3}, :pts 2.5M}, 
                   2 {:par 1, :adj #{4}, :pts 0M}, 
                   3 {:par 1, :adj #{4}, :pts 1M}
                   4 {:par 3, :adj #{5 6}, :pts 0M}
                   5 {:par 4, :adj #{}, :pts 0M}
                   6 {:par 4, :adj #{}, :pts 0M}}))))

(deftest test-build-from-file
	(is (= (build-from-file "test/reward_system/little_input.txt")
		        little-network)))

(deftest test-add-nodes
	(testing "Normal case to build little-network"
	  (is (= (add-nodes {1 {:par nil, :adj #{}, :pts 0M}} [1 2 1 3 3 4 2 4 4 5 4 6])
		         little-network)))

  (testing "Self invitation"
    (is (= (add-nodes {1 {:par nil, :adj #{}, :pts 0M}} [1 2 1 3 3 4 2 4 1 1 4 5 4 6])
		         little-network)))

  (testing "Repeated invitation"
    (is (= (add-nodes {1 {:par nil, :adj #{}, :pts 0M}} [1 2 1 2 1 2 1 3 3 4 2 4 4 5 4 6])
		         little-network)))

  (testing "Outsider inviting"
    (is (= (add-nodes {1 {:par nil, :adj #{}, :pts 0M}} [1 2 10 1 1 3 10 11 3 4 2 4 4 5 4 6])
		         little-network)))

	(testing "Case where there are odd number of inputs"
	  (is (thrown? AssertionError (add-nodes {1 {:par nil, :adj #{}, :pts 0M}} 
	  	                                                                         [1 2 1 3 3 4 2 4 4 5 4 6 1])))))

(deftest test-rank
	(testing "for little inputs"
		(is (< (map second (rank little-network)))))

	(testing "for normal inputs"
		(is (< (map second (rank (build-from-file "test/reward_system/input.txt")))))))