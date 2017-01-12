

(deffunction collect-positive-class-names ($?condition)
)

(deffunction collect-positive-class-names-one ($?cond-elem)
	(if (eq (nth$ 2 $?cond-elem) not)
	   then
	   	(create$)
	   else
	   	(if (or	(eq (nth$ 2 $?cond-elem) or)
   			(eq (nth$ 2 $?cond-elem) and))
   		   then
   		   	(collect-positive-class-names (subseq$ $?cond-elem 3 (- (length$ $?cond-elem) 1)))
   		   else
   		   	(if (eq (nth$ 2 $?cond-elem) <-)
			   then
				(if (eq (nth$ 4 $?cond-elem) object)
			   	   then
			   		(bind ?class (nth$ (+ (member$ is-a $?cond-elem) 1) $?cond-elem))
			   	   else
			   	   	(bind ?class (nth$ 4 $?cond-elem))
			   	)
			   else
				(if (eq (nth$ 2 $?cond-elem) object)
			   	   then
			   		(bind ?class (nth$ (+ (member$ is-a $?cond-elem) 1) $?cond-elem))
			   	   else
			   	   	(bind ?class (nth$ 2 $?cond-elem))
			   	)
			)
		   	(if (is_derived ?class)
		   	   then
				?class
			   else
			   	(create$)
			)
		)
	)
)

(deffunction collect-positive-class-names ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
		(bind ?p2 (get-token $?condition))
		;(bind $?first-cond-elem (subseq$ $?condition 1 ?p2))
		(bind $?result (create$ $?result (collect-positive-class-names-one (subseq$ $?condition 1 ?p2))))
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	$?result
)

(deffunction collect-negative-class-names ($?condition)
)

(deffunction collect-negative-class-names-one ($?cond-elem)
	(if (eq (nth$ 2 $?cond-elem) not)
	   then
   		(collect-positive-class-names (subseq$ $?cond-elem 3 (- (length$ $?cond-elem) 1)))
	   else
	   	(if (or	(eq (nth$ 2 $?cond-elem) or)
   			(eq (nth$ 2 $?cond-elem) and))
   		   then
   		   	(collect-negative-class-names (subseq$ $?cond-elem 3 (- (length$ $?cond-elem) 1)))
   		   else
		   	(create$)
		)
	)
)

(deffunction collect-negative-class-names ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
		(bind ?p2 (get-token $?condition))
		;(bind $?first-cond-elem (subseq$ $?condition 1 ?p2))
		(bind $?result (create$ $?result (collect-negative-class-names-one (subseq$ $?condition 1 ?p2))))
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	$?result
)


(deffunction calc-positive-stratum (?no-of-derived-classes ?current-stratum $?positive-condition-classes)
	(if (> ?current-stratum ?no-of-derived-classes)
	   then
	   	-1
	   else
	   	(if (= (length$ $?positive-condition-classes) 0)
	   	   then
	   	   	?current-stratum
	   	   else
	   	   	;(bind ?pos-class (nth$ 1 $?positive-condition-classes))
	   	   	;(bind ?pos-class-id (nth$ 1 (get-specific-facts derived-class name (nth$ 1 $?positive-condition-classes))))
	   	   	(bind ?body-class-stratum (fact-slot-value (nth$ 1 (get-specific-facts derived-class name (nth$ 1 $?positive-condition-classes))) stratum))
	   	   	(if (< ?current-stratum ?body-class-stratum)
	   	   	   then
	   	   	   	(calc-positive-stratum ?no-of-derived-classes ?body-class-stratum (rest$ $?positive-condition-classes))
	   	   	   else
	   	   	   	(calc-positive-stratum ?no-of-derived-classes ?current-stratum (rest$ $?positive-condition-classes))
	   	   	)
	   	)
	)
)

(deffunction calc-negative-stratum (?no-of-derived-classes ?current-stratum $?negative-condition-classes)
	(if (> ?current-stratum ?no-of-derived-classes)
	   then
	   	-1
	   else
	   	(if (= (length$ $?negative-condition-classes) 0)
	   	   then
	   	   	?current-stratum
	   	   else
	   	   	(bind ?body-class-stratum (fact-slot-value (nth$ 1 (get-specific-facts derived-class name (nth$ 1 $?negative-condition-classes))) stratum))
	   	   	(if (<= ?current-stratum ?body-class-stratum)
	   	   	   then
	   	   	   	(calc-negative-stratum ?no-of-derived-classes (+ ?body-class-stratum 1) (rest$ $?negative-condition-classes))
	   	   	   else
	   	   	   	(calc-negative-stratum ?no-of-derived-classes ?current-stratum (rest$ $?negative-condition-classes))
	   	   	)
	   	)
	)
)

(deffunction calc-stratum (?derived-class $?condition)
	;(bind $?positive-condition-classes (remove-duplicates$ (collect-positive-class-names $?condition)))
	;(bind $?negative-condition-classes (remove-duplicates$ (collect-negative-class-names $?condition)))
	(bind ?no-of-derived-classes (length$ (get-template-specific-facts derived-class (get-fact-list))))
	(bind ?derived-class-index (nth$ 1 (get-specific-facts derived-class name ?derived-class)))
	(if (eq ?derived-class-index nil)
	   then
	   	1
	   else
		;(bind ?current-stratum (fact-slot-value ?derived-class-index stratum))
		;(bind ?next-stratum (calc-positive-stratum ?no-of-derived-classes (fact-slot-value ?derived-class-index stratum) (remove-duplicates$ (collect-positive-class-names $?condition))))
		(calc-negative-stratum ?no-of-derived-classes (calc-positive-stratum ?no-of-derived-classes (fact-slot-value ?derived-class-index stratum) (remove-duplicates$ (collect-positive-class-names $?condition))) (remove-duplicates$ (collect-negative-class-names $?condition)))
	)
)



(deffunction calc-stratum-afterwards (?production-rule-condition ?class)
	;(bind $?pr (explode$ ?production-rule))
	;(bind ?imp_pos (member$ => $?pr))
	;(bind $?condition (subseq$ $?pr 17 (- ?imp_pos 1)))   ;avoid initial stuff
	;(bind $?condition (explode$ ?production-rule-condition))
	(bind ?stratum (calc-stratum ?class (my-explode$ ?production-rule-condition)))
	(if (= ?stratum -1)
	   then
	   	(printout t "Rules are not stratified!" crlf)
	   	(halt)
	   else
	   	(if (> ?stratum 1)
	   	   then
	   		(bind ?derived-class-index (nth$ 1 (get-specific-facts derived-class name ?class)))
	   		(modify ?derived-class-index (stratum ?stratum))
	   	)
	)
)

(deffunction calc-salience (?class)
	;(bind ?derived-class-index (nth$ 1 (get-specific-facts derived-class name ?class)))
	;(bind ?class-stratum (fact-slot-value (nth$ 1 (get-specific-facts derived-class name ?class)) stratum))
	(- 1000 (fact-slot-value (nth$ 1 (get-specific-facts derived-class name ?class)) stratum))
)
