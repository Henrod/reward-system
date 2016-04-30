(ns reward-system.system.util
	(:use clojure.java.io))

(defn parse-int [s]
	"Convert a number in string to integer"
	(Integer/parseInt s));(re-find #"\d+" s)))

(defn input-html
	"Return string in of the inputs of the program in the html format."
	[input-file]
	(let [input-list (atom "")]
	(with-open [rdr (reader input-file)]
		(doseq [line (line-seq rdr)]
			(swap! input-list #(str %1 line "</br>"))))
			@input-list))

(defn not-has?
	"Returns true if a list of nodes does not contain a value"
	[some-list value]
	(println "__DEBUG__")
	(println value)
	(println some-list)
	(let [h (atom true)]
		(doseq [node-tmp some-list]
			(swap! h #(and %1 (not (= (.value node-tmp) value)))))
	@h))
