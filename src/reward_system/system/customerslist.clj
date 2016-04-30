(ns reward-system.system.customerslist
	(:require [reward-system.system.util :as util]))

(defprotocol List
	"A simple list with methods to concatenate and verify if element exists"
	(add [this value] "Add a element in the list")
	(has? [this value] "Returns true if element is in the list")
	(not-has? [this value] "Returns true if element is NOT in the list")
	(print! [this] "Print the elements of the list"))

; Used to hold all customers current in the company
(defrecord customers-list [cust-list]
	List
	(add [this value]
		(->customers-list (cons value (.cust-list this))))
	(has? [this value]
		(not (nil? (some #{value} (.cust-list this)))))
	(not-has? [this value]
		(nil? (some #{value} (.cust-list this))))
	(print! [this]
		(print "Customers list: ")
		(prn (.cust-list this)))
	)
