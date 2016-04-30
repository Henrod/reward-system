(ns reward-system.system.util
	(:use clojure.java.io))

(defn parse-int [s]
	"Convert a number in string to integer"
	(Integer/parseInt (re-find #"\d+" s)))

(defn input-html
	"Return string in of the inputs of the program in the html format."
	[input-file]
	(let [input-list (atom "")]
	(with-open [rdr (reader input-file)]
		(doseq [line (line-seq rdr)]
			(swap! input-list #(str %1 line "</br>"))))
			@input-list))
