(deffunction discover-slot-name (?pos $?cond-elem)
	(bind ?pos (- ?pos 1))
	(while (neq (nth$ ?pos $?cond-elem) "(")
	   do
	   	(bind ?pos (- ?pos 1))
	)
	(nth$ (+ ?pos 1) $?cond-elem)
)

(deffunction discover-type (?var $?condition)
	(if (= (length$ $?condition) 0)
	   then
	   	(create$)
	   else
		(bind ?p2 (get-token $?condition))
		(bind $?cond-elem (subseq$ $?condition 1 ?p2))
		;(bind $?rest-cond (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	   	(if (or 
	   		(eq (nth$ 2 $?cond-elem) not)
	   		(eq (nth$ 2 $?cond-elem) test))
	   	   then
	   	   	(discover-type ?var (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	   	   else
	   	   	(if (or 
	   			(eq (nth$ 2 $?cond-elem) and)
	   			(eq (nth$ 2 $?cond-elem) or))
	   		   then
	   		   	(bind ?type (discover-type ?var (subseq$ $?cond-elem 3 (- (length$ $?cond-elem) 1))))
	   		   	(if (not ?type)
	   		   	   then
	   		   	   	(discover-type ?var (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	   		   	   else
	   		   	   	?type
	   		   	)
	   		   else
	   		   	(if (eq (nth$ 2 $?cond-elem) <-)
	   		   	   then
	   		   	   	(bind $?cond-elem (subseq$ $?cond-elem 3 (length$ $?cond-elem)))
	   		   	   else
	   		   	   	(if (eq (nth$ 4 $?cond-elem) name)
	   		   	   	   then
	   		   	   	   	(bind $?cond-elem (delete$ $?cond-elem 3 6))
	   		   	   	)
	   		   	)
	   		   	(bind ?pos (member$ ?var $?cond-elem))
				(if (not ?pos)
				   then
	   		   	   	(discover-type ?var (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	   		   	   else
	   		   	   	(bind ?slot (discover-slot-name ?pos $?cond-elem))
	   		   	   	(if (eq (nth$ 2 $?cond-elem) object)
	   		   	   	   then
	   		   	   		(bind ?class (nth$ (+ (member$ is-a $?cond-elem) 1) $?cond-elem))
	   		   	   	   else
	   		   	   	   	(bind ?class (nth$ 2 $?cond-elem))
	   		   	   	)
	   		   	   	(if (and (class-existp ?class) (slot-existp ?class ?slot))
	   		   	   	   then
	   		   	   	   	(bind $?slot-types (slot-types ?class ?slot))
	   		   	   		(if (eq (nth$ 1 $?slot-types) INSTANCE-NAME)
	   		   	   		   then
	   		   	   		   	(create$ INSTANCE-NAME (get-type-of ?class ?slot))
	   		   	   		   else
	   		   	   		   	$?slot-types
	   		   	   		)
	   		   	   	   else
	   		   	   	   	(if (and (eq (sub-string 1 3 ?class) "gen") (eq (sub-string (- (length ?slot) 3) (length ?slot) ?slot) "_obj"))
	   		   	   	   	   then
	   		   	   	   	   	(create$ INSTANCE-NAME USER)
	   		   	   	   	   else
	   		   	   	   		(create$)
	   		   	   	   	)
	   		   	   	)
	   		   	)
	   		)
		)
	)
)

(deffunction discover-ref-type (?var $?condition)
	(if (= (length$ $?condition) 0)
	   then
	   	(create$)
	   else
		(bind ?p2 (get-token $?condition))
		(bind $?cond-elem (subseq$ $?condition 1 ?p2))
		;(bind $?rest-cond (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	   	(if (or 
	   		(eq (nth$ 2 $?cond-elem) not)
	   		(eq (nth$ 2 $?cond-elem) test))
	   	   then
	   	   	(discover-ref-type ?var (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	   	   else
	   	   	(if (or 
	   			(eq (nth$ 2 $?cond-elem) and)
	   			(eq (nth$ 2 $?cond-elem) or))
	   		   then
	   		   	(bind $?type (discover-ref-type ?var (subseq$ $?cond-elem 3 (- (length$ $?cond-elem) 1))))
	   		   	(if (= (length$ $?type) 0)
	   		   	   then
	   		   	   	(discover-ref-type ?var (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	   		   	   else
	   		   	   	$?type
	   		   	)
	   		   else
	   		   	(if (or
	   		   		(and 
	   		   			(eq (nth$ 1 $?cond-elem) ?var)
	   		   			(eq (nth$ 2 $?cond-elem) <-))
	   		   	   	(and
	   		   	   		(eq (nth$ 4 $?cond-elem) name)
	   		   	   		(eq (nth$ 5 $?cond-elem) ?var)))
	   		   	   then
	   		   	   	;(bind ?class (nth$ (+ (member$ is-a $?cond-elem) 1) $?cond-elem))
	   		   	   	(create$ INSTANCE-NAME (nth$ (+ (member$ is-a $?cond-elem) 1) $?cond-elem))
	   		   	   else
	   		   	   	(discover-ref-type ?var (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	   		   	)
	   		)
		)
	)
)

(deffunction guess-slot-def ($?condition-and-slot)
	;(verbose "guess-slot-def - $?condition-and-slot: " $?condition-and-slot crlf)
	(bind $?condition (subseq$ $?condition-and-slot 1 (- (member$ $$$ $?condition-and-slot) 1)))
	(bind $?slot (subseq$ $?condition-and-slot (+ (member$ $$$ $?condition-and-slot) 1) (length$ $?condition-and-slot)))
	(bind $?value-expr (subseq$ $?slot 3 (- (length$ $?slot) 1)))
	(if (= (length$ $?value-expr) 1)
	   then
	   	(bind ?value (nth$ 1 $?value-expr))
	   	(if (is-singlevar ?value)
	   	   then
	   	   	(bind ?slot-field slot)
	   	   else
	   	   	(if (is-multivar ?value)
	   	   	   then
	   	   	   	(bind ?slot-field multislot)
	   	   	   else
	   	   	   	(bind ?slot-field slot)
	   	   	)
	   	)
	   else
	   	(if (eq (nth$ 1 $?value-expr) "(")
	   	   then
	   	   	(if (and
	   	   		(is-aggregate-function (nth$ 2 $?value-expr))
	   	   		(is-var (nth$ 3 $?value-expr)))
	   	   	   then
	   	   		(bind ?slot-field multislot)
	   	   		(bind ?value (nth$ 3 $?value-expr))
	   	   	   else
	   	   		(bind ?slot-field multislot)
	   	   		(bind ?value (nth$ 2 $?value-expr))
	   	   	)
	   	)
	)
	(if (floatp ?value)
	   then
	   	(bind $?type FLOAT)
	   	(bind $?ref-type (create$))
	   else
	   	(if (integerp ?value)
	   	   then
	   	   	(bind $?type INTEGER)
	   	   	(bind $?ref-type (create$))
	   	   else
	   	   	(if (symbolp ?value)
	   	   	   then
	   	   	   	(bind $?type SYMBOL)
	   	   	   	(bind $?ref-type (create$))
	   	   	   else
	   	   	   	(if (is-var ?value)
	   	   	   	   then
	   	   	   	   	(bind $?type (discover-type ?value $?condition))
	   	   	   	   	(if (= (length$ $?type) 0)
	   	   	   	   	   then
	   	   	   	   	   	(bind $?type (discover-ref-type ?value $?condition))
	   	   	   	   	)
	   	   	   	   	(if (= (length$ $?type) 0)
	   	   	   	   	   then
	   	   	   	   	   	(bind $?type "?VARIABLE")
	   	   	   	   	   	(bind $?ref-type (create$))
	   	   	   	   	   else
	   	   	   	   		(if (eq (nth$ 1 $?type) INSTANCE-NAME)
	   	   	   	   		   then
	   	   	   				;(bind ?class (nth$ 2 $?type))
	   	   	   				;(bind ?slot-name (nth$ 2 $?slot))
	   	   	   	   		   	(bind $?ref-type (create$ (nth$ 2 $?slot) (nth$ 2 $?type)))
	   	   	   	   		   	(bind $?type INSTANCE-NAME)
	   	   	   	   		   else
	   	   	   	   		   	(bind $?ref-type (create$))
	   	   	   	   		)
	   	   	   	   	)
	   	   	   	   else
	   	   	   	   	(bind $?type STRING)
	   	   	   	   	(bind $?ref-type (create$))
	   	   	   	)
	   	   	)
	   	)
	)
	(create$ 
		"(" ?slot-field (nth$ 2 $?slot) "(" type $?type ")" ")"
		;(insert$ (replace$ $?slot 3 3 (create$ "(" type $?type ")")) 2 ?slot-field)
		$$$
		$?ref-type
	)
)

(deffunction guess-slot-defs ($?condition-and-slots)
	(bind $?condition (subseq$ $?condition-and-slots 1 (- (member$ $$$ $?condition-and-slots) 1)))
	(bind $?slots (subseq$ $?condition-and-slots (+ (member$ $$$ $?condition-and-slots) 1) (length$ $?condition-and-slots)))
	(if (= (length$ $?slots) 0)
	   then
	   	(create$ $$$)
	   else
	   	;(bind ?p1 (member$ "(" $?slots))
	   	;(bind ?p2 (member$ ")" $?slots))
	   	(bind ?p2 (get-token $?slots))
		;(bind $?slot (subseq$ $?slots (member$ "(" $?slots) ?p2))
		(bind $?new-slot-and-ref (guess-slot-def (create$ $?condition $$$ (subseq$ $?slots (member$ "(" $?slots) ?p2))))
		;(bind $?new-slot (subseq$ $?new-slot-and-ref 1 (- (member$ $$$ $?new-slot-and-ref) 1)))
		;(bind $?new-reference-type (subseq$ $?new-slot-and-ref (+ (member$ $$$ $?new-slot-and-ref) 1) (length$ $?new-slot-and-ref)))
		;(bind $?rest-slots (subseq$ $?slots (+ ?p2 1) (length$ $?slots)))
		(bind $?new-rest-slots-and-refs (guess-slot-defs (create$ $?condition $$$ (subseq$ $?slots (+ ?p2 1) (length$ $?slots)))))
		;(bind $?new-rest-slots (subseq$ $?new-rest-slots-and-refs 1 (- (member$ $$$ $?new-rest-slots-and-refs) 1)))
		;(bind $?new-rest-reference-types (subseq$ $?new-rest-slots-and-refs (+ (member$ $$$ $?new-rest-slots-and-refs) 1) (length$ $?new-rest-slots-and-refs)))
		(create$ (subseq$ $?new-slot-and-ref 1 (- (member$ $$$ $?new-slot-and-ref) 1)) (subseq$ $?new-rest-slots-and-refs 1 (- (member$ $$$ $?new-rest-slots-and-refs) 1)) $$$ (subseq$ $?new-slot-and-ref (+ (member$ $$$ $?new-slot-and-ref) 1) (length$ $?new-slot-and-ref)) (subseq$ $?new-rest-slots-and-refs (+ (member$ $$$ $?new-rest-slots-and-refs) 1) (length$ $?new-rest-slots-and-refs)))
	)
)

(deffunction discover-class-of-var (?class-expr $?condition)
	(bind ?pos (member$ ?class-expr $?condition))
	(if (integerp ?pos)
	   then
	   	(if (eq (nth$ (- ?pos 2) $?condition) <-)
	   	   then
	   	   	(bind ?oid (nth$ (- ?pos 3) $?condition))
	   	   	(if (not (is-var ?oid))
	   	   	   then
	   	   		(if (instance-existp (symbol-to-instance-name ?oid))
	   	   		   then
	   	   	   		(return (class (symbol-to-instance-name ?oid)))
	   	   	   	   else
	   	   	   	   	(return ?class-expr)
	   	   	   	)
	   	   	   else
	   	   	   	(bind $?types (discover-type ?oid $?condition))
	   	   	   	(if (eq (nth$ 1 $?types) INSTANCE-NAME)
	   	   	   	   then
	   	   	   	   	(return (nth$ 2 $?types))
	   	   	   	   else
	   	   	   	   	(return ?class-expr)
	   	   	   	)
	   	   	)
	   	   else
	   	   	(return ?class-expr)
	   	)
	   else
	   	(return ?class-expr)
	)
)
