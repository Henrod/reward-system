(ns reward-system.system.util)

(defn parse-int [s]
	"Convert a number in string to integer"
	(Integer/parseInt (re-find #"\d+" s)))
