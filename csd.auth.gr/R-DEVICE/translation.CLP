(deffunction transform-cond-elem ($?cond-element)
)

(deffunction transform-one-cond-elem ($?cond-element)
	(if (or 
		(eq (nth$ 2 $?cond-element) not)
   		(eq (nth$ 2 $?cond-element) or)
   		(eq (nth$ 2 $?cond-element) and))
   	   then
   	   	(create$ "(" (nth$ 2 $?cond-element)
   	   			(transform-cond-elem (subseq$ $?cond-element 3 (- (length$ $?cond-element) 1)))
   	   		")"
   	   	)
	   else
		(if (eq (nth$ 2 $?cond-element) <-)
		   then
		   	(if (is-var (nth$ 1 $?cond-element))
		   	   then
		   	   	(bind ?object-id (nth$ 1 $?cond-element))
		   	   else
		   	   	(bind ?object-id (str-cat "[" (nth$ 1 $?cond-element) "]"))
		   	)
			(if (eq (nth$ 4 $?cond-element) object)
		   	   then
		   		;$?cond-element
		   		(insert$ (subseq$ $?cond-element 3 (length$ $?cond-element)) 3 "(" name ?object-id ")")
		   	   else
		   	   	;(replace$ $?cond-element 4 4 object "(" is-a (nth$ 4 $?cond-element) ")")
		   	   	(replace$ (subseq$ $?cond-element 3 (length$ $?cond-element)) 2 2 object "(" name ?object-id ")" "(" is-a (nth$ 4 $?cond-element) ")")
		   	)
		   else
			(if (eq (nth$ 2 $?cond-element) object)
   			   then
				$?cond-element
		   	   else
		   		(replace$ $?cond-element 2 2 object "(" is-a (nth$ 2 $?cond-element) ")")
		   	)
		)
	)
)


(deffunction transform-cond-elem ($?cond-element)
	(bind $?result (create$))
	(while (> (length$ $?cond-element) 0)
	   do
	   	(bind ?p2 (get-token $?cond-element))
		(bind $?result (create$ $?result (transform-one-cond-elem (subseq$ $?cond-element 1 ?p2))))
		(bind $?cond-element (subseq$ $?cond-element (+ ?p2 1) (length$ $?cond-element)))
   	)
	$?result
)


(deffunction path-transform-one-slot-vector-aux (?previous-var ?current-class $?path-list)
	(if (= (length$ $?path-list) 0)
	   then
	   	(create$)
	   else
		(bind ?current-slot (nth$ 1 $?path-list))
	   	(bind ?next-var (str-cat "?" (gensym)))
		(if (is-multislot ?current-class ?current-slot)
		   then
		   	(bind $?next-var-expr (create$ "$?" ?next-var "$?"))
		   else
		   	(bind $?next-var-expr (create$ ?next-var))
		)
		;(bind ?next-class (get-type-of ?current-class ?current-slot))
	   	;(bind $?current-cond-elem (create$ ?previous-var <- "(" object "(" is-a ?current-class ")" "(" ?current-slot $?next-var-expr ")" ")"))
;	   	(bind $?rest-cond-elems (path-transform-one-slot-vector-aux ?next-var (get-type-of ?current-class ?current-slot) (rest$ $?path-list)))
;	   	(create$ (path-transform-one-slot-vector-aux ?next-var (get-type-of ?current-class ?current-slot) (rest$ $?path-list)) (create$ ?previous-var <- "(" object "(" is-a ?current-class ")" "(" ?current-slot $?next-var-expr ")" ")"))
	   	(create$ (path-transform-one-slot-vector-aux ?next-var (get-type-of ?current-class ?current-slot) (rest$ $?path-list)) (create$ "(" object "(" name ?previous-var ")" "(" is-a ?current-class ")" "(" ?current-slot $?next-var-expr ")" ")"))
;	   	(create$ $?current-cond-elem $?rest-cond-elems)
	)
)

(deffunction path-transform-one-slot-vector (?class $?slot-vector)
	(bind ?p2 (get-token (rest$ $?slot-vector)))
	(bind $?path-list (reverse$ (subseq$ $?slot-vector 3 ?p2)))
	;(bind $?rest-slot-vector (subseq$ $?slot-vector (+ ?p2 2) (- (length$ $?slot-vector) 1)))
	(if (> (length$ $?path-list) 1)
	   then
		(bind ?first-var (str-cat "?" (gensym)))
		(bind ?first-slot (nth$ 1 $?path-list))
		(if (is-multislot ?class ?first-slot)
		   then
		   	(bind $?first-var-expr (create$ "$?" ?first-var "$?"))
		   else
		   	(bind $?first-var-expr (create$ ?first-var))
		)
		;(bind $?new-slot-vector (create$ "(" ?first-slot ?first-var-expr ")"))
		;(bind ?next-class (get-type-of ?class ?first-slot))
		;(bind $?new-cond-elems1 (path-transform-one-slot-vector-aux ?first-var (get-type-of ?class ?first-slot) (rest$ $?path-list)))
;		(bind $?new-cond-elems (replace$ $?new-cond-elems1 (- (length$ $?new-cond-elems1) 2) (- (length$ $?new-cond-elems1) 2) $?rest-slot-vector))
;		(bind $?new-cond-elems (replace$ (path-transform-one-slot-vector-aux ?first-var (get-type-of ?class ?first-slot) (rest$ $?path-list)) 11 11 $?rest-slot-vector))
;		(create$ (replace$ (path-transform-one-slot-vector-aux ?first-var (get-type-of ?class ?first-slot) (rest$ $?path-list)) 11 11 (subseq$ $?slot-vector (+ ?p2 2) (- (length$ $?slot-vector) 1))) $$$ (create$ "(" ?first-slot ?first-var-expr ")"))
		(create$ (replace$ (path-transform-one-slot-vector-aux ?first-var (get-type-of ?class ?first-slot) (rest$ $?path-list)) 13 13 (subseq$ $?slot-vector (+ ?p2 2) (- (length$ $?slot-vector) 1))) $$$ (create$ "(" ?first-slot ?first-var-expr ")"))
	   else
	   	(create$ $$$ "(" $?path-list (subseq$ $?slot-vector (+ ?p2 2) (- (length$ $?slot-vector) 1)) ")" )
	)
)

(deffunction path-transform-slot-vectors (?class $?slot-vectors)
	(if (= (length$ $?slot-vectors) 0)
	   then
	   	(create$)
	   else
	   	(bind ?p2 (get-token $?slot-vectors))
	   	(bind $?first-slot-vector (subseq$ $?slot-vectors 1 ?p2))
	   	;(bind $?rest-slot-vectors (subseq$ $?slot-vectors (+ ?p2 1) (length$ $?slot-vectors)))
	   	(if (eq (nth$ 2 $?first-slot-vector) "(")
	   	   then
	   	   	(bind $?altered-first-slot-vector (path-transform-one-slot-vector ?class $?first-slot-vector))
	   	   	(bind ?split-pos (member$ $$$ $?altered-first-slot-vector))
			(if (numberp ?split-pos)
			   then
		   		(bind $?new-first-slot-vector (subseq$ $?altered-first-slot-vector (+ ?split-pos 1) (length$ $?altered-first-slot-vector)))
				(bind $?new-first-cond-elems (subseq$ $?altered-first-slot-vector 1 (- ?split-pos 1)))
			   else
		   		(bind $?new-first-slot-vector $?altered-first-slot-vector)
		   		(bind $?new-first-cond-elems (create$))
			)
	   	   else
	   	   	(bind $?new-first-slot-vector $?first-slot-vector)
	   	   	(bind $?new-first-cond-elems (create$))
		)
	   	(bind $?altered-rest-slot-vectors (path-transform-slot-vectors ?class (subseq$ $?slot-vectors (+ ?p2 1) (length$ $?slot-vectors))))
		(bind ?split-pos (member$ $$$ $?altered-rest-slot-vectors))
		(if (numberp ?split-pos)
		   then
		   	(bind $?new-rest-slot-vectors (subseq$ $?altered-rest-slot-vectors (+ ?split-pos 1) (length$ $?altered-rest-slot-vectors)))
			(bind $?new-rest-cond-elems (subseq$ $?altered-rest-slot-vectors 1 (- ?split-pos 1)))
		   else
		   	(bind $?new-rest-slot-vectors $?altered-rest-slot-vectors)
		   	(bind $?new-rest-cond-elems (create$))
		)
		(create$ $?new-first-cond-elems $?new-rest-cond-elems $$$ $?new-first-slot-vector $?new-rest-slot-vectors)
;		(create$ $?new-rest-cond-elems $?new-first-cond-elems $$$ $?new-first-slot-vector $?new-rest-slot-vectors)
	)
)
	   	
(deffunction path-transform ($?cond-element)
)

(deffunction path-transform-one ($?cond-element)
	(if (or 
		(eq (nth$ 2 $?cond-element) not)
   		(eq (nth$ 2 $?cond-element) or)
   		(eq (nth$ 2 $?cond-element) and))
   	   then
   	   	(create$ "(" (nth$ 2 $?cond-element)
   	   			(path-transform (subseq$ $?cond-element 3 (- (length$ $?cond-element) 1)))
   	   		")"
   	   	)
	   else
;		(if (eq (nth$ 2 $?cond-element) <-)
		(if (eq (nth$ 4 $?cond-element) name)
		   then
;		   	(bind $?slot-vectors (subseq$ $?cond-element 9 (- (length$ $?cond-element) 1)))
		   	(bind $?slot-vectors (subseq$ $?cond-element 11 (- (length$ $?cond-element) 1)))
;		   	(bind $?init-stuff (subseq$ $?cond-element 1 8))
		   	(bind $?init-stuff (subseq$ $?cond-element 1 10))
;		   	(bind ?original-class (nth$ 7 $?cond-element))
		   	(bind ?original-class (nth$ 9 $?cond-element))
		   else
		   	(bind $?slot-vectors (subseq$ $?cond-element 7 (- (length$ $?cond-element) 1)))
		   	(bind $?init-stuff (subseq$ $?cond-element 1 6))
		   	(bind ?original-class (nth$ 5 $?cond-element))
		)
		(bind $?altered-slot-vectors (path-transform-slot-vectors ?original-class $?slot-vectors))
		(bind ?split-pos (member$ $$$ $?altered-slot-vectors))
		(if (numberp ?split-pos)
		   then
		   	(bind $?new-slot-vectors (subseq$ $?altered-slot-vectors (+ ?split-pos 1) (length$ $?altered-slot-vectors)))
			(bind $?new-cond-elems (subseq$ $?altered-slot-vectors 1 (- ?split-pos 1)))
		   else
		   	(bind $?new-slot-vectors $?altered-slot-vectors)
		   	(bind $?new-cond-elems (create$))
		)
		; This may change in the future, $?new-cond-elems could go back!
		(create$ $?new-cond-elems $?init-stuff $?new-slot-vectors ")" )
	)
)


(deffunction path-transform ($?cond-element)
	(bind $?result (create$))
	(while (> (length$ $?cond-element) 0)
	   do
	   	(bind ?p2 (get-token $?cond-element))
		(bind $?result (create$ $?result (path-transform-one (subseq$ $?cond-element 1 ?p2))))
		(bind $?cond-element (subseq$ $?cond-element (+ ?p2 1) (length$ $?cond-element)))
   	)
	$?result
)

(deffunction object-index-transform ($?cond-element)
)

(deffunction object-index-transform-one ($?cond-element)
	(if (or 
   		(eq (nth$ 2 $?cond-element) or)
   		(eq (nth$ 2 $?cond-element) and))
   	   then
   	   	(create$ "(" (nth$ 2 $?cond-element)
   	   			(object-index-transform (subseq$ $?cond-element 3 (- (length$ $?cond-element) 1)))
   	   		")"
   	   	)
	   else
		(if (or 
   			(eq (nth$ 2 $?cond-element) test)
   			(eq (nth$ 2 $?cond-element) not)
;			(eq (nth$ 2 $?cond-element) <-)
			(eq (nth$ 4 $?cond-element) name))
   		   then
   		   	$?cond-element
   		   else
		   	;(bind ?varname (gensym))
			;(bind ?var (str-cat "?" (gensym)))
;			(create$ (str-cat "?" (gensym)) <- $?cond-element)
			(insert$ $?cond-element 3 "(" name (str-cat "?" (gensym)) ")" )
		)
	)
)

(deffunction object-index-transform ($?cond-element)
	(bind $?result (create$))
	(while (> (length$ $?cond-element) 0)
	   do
	   	(bind ?p2 (get-token $?cond-element))
		(bind $?result (create$ $?result (object-index-transform-one (subseq$ $?cond-element 1 ?p2))))
		(bind $?cond-element (subseq$ $?cond-element (+ ?p2 1) (length$ $?cond-element)))
   	)
	$?result
)


(deffunction translate-cond-element ($?cond-element)
	(if (eq (nth$ 2 $?cond-element) test)
	   then
	   	$?cond-element
   	   else
   	   	(bind $?non-existent-classes (check-non-existent-classes $?cond-element))
   	   	(if (> (length$ $?non-existent-classes) 0)
   	   	   then
   	   		(assert (non-existent-classes $?non-existent-classes))
   	   	)
   	   	;(printout t "Orig cond-elem: " $?cond-element crlf)
   	   	;(bind $?new-cond-elem1 (transform-cond-elem $?cond-element))
   	   	;(printout t "cond-elem after transform-cond-elem: " $?new-cond-elem1 crlf)
   	   	;(bind $?new-cond-elem (path-transform $?new-cond-elem1 ))
   	   	;(printout t "cond-elem after path-transform: " $?new-cond-elem crlf)
   	   	;(bind $?result (object-index-transform $?new-cond-elem))
   	   	;(printout t "cond-elem after object-index-transform: " $?result crlf)
   	   	;$?result
   	   	(object-index-transform (path-transform (transform-cond-elem $?cond-element)))
	)
)


(deffunction translate-condition ($?condition)
	;
	(bind $?result (create$))
	;   	(printout t "condition: " $?condition crlf)
	;   	(printout t "result: " $?result crlf)
	(while (> (length$ $?condition) 0)
	   do
	;   	(printout t "condition: " $?condition crlf)
	;   	(printout t "result: " $?result crlf)
		(bind ?p2 (get-token $?condition))
		;(bind $?cond-elem (subseq$ $?condition 1 ?p2))
		;(bind $?new-cond-elem (translate-cond-element $?cond-elem))
		;(bind $?new-cond-elem (translate-cond-element (subseq$ $?condition 1 ?p2)))
		;(bind $?result (create$ $?result $?new-cond-elem))
		(bind $?result (create$ $?result (translate-cond-element (subseq$ $?condition 1 ?p2))))
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	$?result
)

(deffunction create-namespace (?class-namespace)
	(bind ?pos (str-index / ?class-namespace))
	(if (eq ?pos FALSE)
	   then
	   	nil
	   else
	   	(bind ?class (string-to-field (sub-string 1 (- ?pos 1) ?class-namespace)))
	   	(bind ?namespace (string-to-field (sub-string (+ ?pos 1) (str-length ?class-namespace) ?class-namespace)))
	   	(if (not (is_namespace ?namespace))
	   	   then
	   		(assert (namespace (name ?namespace) (classes ?class)))
	   	   else
	   	   	(bind ?namespace-index (nth$ 1 (get-specific-facts namespace name ?namespace)))
	   		(bind $?old-classes (fact-slot-value ?namespace-index classes))
	   		(if (not (member$ ?class $?old-classes))
	   		   then
	   			(modify ?namespace-index (classes (create$ ?class $?old-classes)))
	   		)
	   	)
	   	?namespace
	)
)

(deffunction create-template (?rule-name $?condition-and-conclusion)
	;(bind $?condition (subseq$ $?condition-and-conclusion 1 (- (member$ $$$ $?condition-and-conclusion) 1)))
	(bind $?conclusion (subseq$ $?condition-and-conclusion (+ (member$ $$$ $?condition-and-conclusion) 1) (length$ $?condition-and-conclusion)))
	(bind ?class (nth$ 2 $?conclusion))
	(bind $?slot-and-ref-defs (guess-slot-defs (create$ (subseq$ $?condition-and-conclusion 1 (- (member$ $$$ $?condition-and-conclusion) 1)) $$$ (subseq$ $?conclusion 3 (- (length$ $?conclusion) 1)))))
	(bind $?slot-defs (subseq$ $?slot-and-ref-defs 1 (- (member$ $$$ $?slot-and-ref-defs) 1)))
	(bind $?reference-types (subseq$ $?slot-and-ref-defs (+ (member$ $$$ $?slot-and-ref-defs) 1) (length$ $?slot-and-ref-defs)))
	(if (not (is_derived ?class))
	   then
		(bind ?namespace (create-namespace ?class))
	   	(assert (derived-class (name ?class) (deductive-rules ?rule-name)))
	   	(my-build (str-cat$ (create$ 
	   			"(" defclass ?class 
	   				"(" is-a DERIVED-CLASS ")"
	   				$?slot-defs 
					"(" multislot class-refs 
						"(" source composite ")"
						"(" default $?reference-types ")"
					")"
					(if (eq ?namespace nil)
					   then
					   	(create$)
					   else
					   	(create$ "(" slot namespace
								"(" source composite ")"
								"(" default ?namespace ")"
							 ")"
						)
					)
	   			")"
	   		)
	   	))
	   else
	   	(bind ?derived-class-index (nth$ 1 (get-specific-facts derived-class name ?class)))
	   	;(bind $?old-rules (fact-slot-value ?derived-class-index deductive-rules))
	   	(modify ?derived-class-index (deductive-rules (create$ ?rule-name (fact-slot-value ?derived-class-index deductive-rules))))
	   	(bind $?new-slot-defs (needs-redefinition ?class $?slot-defs))
	   	(if (neq $?new-slot-defs FALSE)
	   	   then
	   	   	;(verbose "For class: " ?class crlf)
	   	   	;(verbose "in new slot-defs: " $?slot-defs crlf)
	   	   	;(verbose "out new slot-defs: " $?new-slot-defs crlf)
	   	   	(re-define-class ?class $?new-slot-defs)
	   	)
	)
	?class
)

(deffunction collect-slot-values ($?slot-vectors)
	(bind $?result (create$))
	(while (> (length$ $?slot-vectors) 0)
	   do
	   	(bind ?p2 (get-token $?slot-vectors))
	   	(bind $?first-slot-vector (subseq$ $?slot-vectors 1 ?p2))
	   	;(bind $?first-slot-value (subseq$ $?first-slot-vector 3 (- (length$ $?first-slot-vector) 1)))
	   	;(bind $?result (create$ $?result "(" make-symbol (subseq$ $?first-slot-vector 3 (- (length$ $?first-slot-vector) 1)) ")" ))
	   	(bind $?result (create$ $?result (subseq$ $?first-slot-vector 3 (- (length$ $?first-slot-vector) 1)) ))
	   	(bind $?slot-vectors (subseq$ $?slot-vectors (+ ?p2 1) (length$ $?slot-vectors)))
	)
	$?result
)



;(deffunction get-object-address-vars ($?condition)
;	(bind $?result (create$))
;	(while (> (length$ $?condition) 0)
;	   do
;		;(bind ?p2 (get-token $?condition))
;		(if (neq (nth$ 1 $?condition) "(")
;		   then
;			(bind $?result (create$ $?result "(" instance-name (nth$ 1 $?condition) ")" ))
;		)
;		(bind $?condition (subseq$ $?condition (+ (get-token $?condition) 1) (length$ $?condition)))
;	)
;	$?result
;)

(deffunction get-object-address-vars ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
		;(bind ?p2 (get-token $?condition))
		; Should cater for and/or conditions
		(if (and 
			(eq (nth$ 2 $?condition) object)
			(eq (nth$ 4 $?condition) name))
		   then
			(bind $?result (create$ $?result (nth$ 5 $?condition)))
		)
		(bind $?condition (subseq$ $?condition (+ (get-token $?condition) 1) (length$ $?condition)))
	)
	$?result
)

(deffunction get-object-names-and-classes ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
		;(bind ?p2 (get-token $?condition))
		; Should cater for and/or conditions
		(if (and 
			(eq (nth$ 2 $?condition) object)
			(eq (nth$ 4 $?condition) name))
		   then
			(bind $?result (create$ $?result (subseq$ $?condition 1 10) ")" ))
		)
		(bind $?condition (subseq$ $?condition (+ (get-token $?condition) 1) (length$ $?condition)))
	)
	$?result
)

(deffunction is-function ($?slot)
	(and
		(eq (nth$ 3 $?slot) "(")
	   	(is-aggregate-function (nth$ 4 $?slot))
	   	(is-var (nth$ 5 $?slot))
	)
)

(deffunction extract-functions ($?conclusion)
	(bind $?slots (subseq$ $?conclusion 3 (- (length$ $?conclusion) 1)))
	(bind $?result (create$))
	(while (> (length$ $?slots) 0)
	   do
	   	(bind ?p2 (get-token $?slots))
	   	(bind $?first-slot (subseq$ $?slots 1 ?p2))
	   	(if (is-function $?first-slot)
	   	   then
	   	   	(bind $?result (create$ $?result $?first-slot))
	   	   	(break)
	   	)
	   	(bind $?slots (subseq$ $?slots (+ ?p2 1) (length$ $?slots)))
	)
	(if (> (length$ $?result) 0)
	   then
		(return (create$ "(" (nth$ 2 $?conclusion) $?result ")"))
	   else
	   	(return (create$))
	)
)

(deffunction remove-functions ($?conclusion)
	(bind $?slots (subseq$ $?conclusion 3 (- (length$ $?conclusion) 1)))
	(bind $?result (create$))
	(while (> (length$ $?slots) 0)
	   do
	   	(bind ?p2 (get-token $?slots))
	   	(bind $?first-slot (subseq$ $?slots 1 ?p2))
	   	(if (not (is-function $?first-slot))
	   	   then
	   	   	(bind $?result (create$ $?result $?first-slot))
	   	)
	   	(bind $?slots (subseq$ $?slots (+ ?p2 1) (length$ $?slots)))
	)
	(return (create$ "(" (nth$ 2 $?conclusion) $?result ")"))
)

(deffunction translate-deductive-rule (?rule-index ?deductive-rule)
	(bind $?dr (my-explode$ ?deductive-rule))
	(bind ?imp_pos (member$ => $?dr))
	(bind $?condition (subseq$ $?dr 1 (- ?imp_pos 1)))
	(bind $?conclusion (subseq$ $?dr (+ ?imp_pos 1) (length$ $?dr)))
	(bind $?new-condition (translate-condition $?condition))
	;(verbose "new-condition: "$?new-condition crlf)
	(bind ?p2 (get-token $?conclusion))
	(if (eq (nth$ 2 (subseq$ $?conclusion (member "(" $?conclusion) ?p2)) calc)
	   then
	   	(bind $?init-concl (subseq$ $?conclusion 3 (- ?p2 1)))
		(bind $?rest-concl (subseq$ $?conclusion (+ ?p2 1) (length$ $?conclusion)))
	   else
	   	(bind $?init-concl (create$))
		(bind $?rest-concl $?conclusion )
	)
	;(bind $?conclusion-values (collect-slot-values (subseq$ $?rest-concl 3 (- (length$ $?rest-concl) 1))))
	(bind ?rule-name (gensym))
	(bind ?class (create-template ?rule-name (create$ $?new-condition $$$ $?rest-concl)))
	;(verbose "rest-concl (before): " $?rest-concl crlf)
	(bind $?function-concl (extract-functions $?rest-concl))
	;(verbose "function-concl: " $?function-concl crlf)
	(bind $?rest-concl (remove-functions $?rest-concl))
	;(verbose "rest-concl (after): " $?rest-concl crlf)
	(if (> (length$ $?function-concl) 0)
	   then
		(bind ?new-var (str-cat "?" (gensym)))
		(assert (aggregateattrule (str-cat$
			$?condition
			?new-var <- $?rest-concl
			  =>
			?new-var <- $?function-concl
		)))
		(bind ?*untranslated_rules* (+ ?*untranslated_rules* 1))
	)
	(bind $?inst (create$ "(" symbol-to-instance-name "(" sym-cat ?class (collect-slot-values (subseq$ $?rest-concl 3 (- (length$ $?rest-concl) 1))) ")" ")" ))
	(bind $?object-address-vars (get-object-address-vars $?new-condition))
	;(verbose "object-address-vars: " $?object-address-vars crlf)
	(bind $?object-names-and-classes (get-object-names-and-classes $?new-condition))
	(bind $?derived-slots (subseq$ $?rest-concl 3 (- (length$ $?rest-concl) 1)))
	(bind ?production-rule (str-cat$ (create$ 
		"(" defrule ?rule-name 
			"(" declare "(" salience "(" calc-salience ?class ")" ")" ")"
			"(" run-deductive-rules ")"
			$?new-condition 
			"(" not 
				"(" object 
					"(" name "?DO" "&" ":(" eq "?DO" $?inst ")" ")"
					"(" is-a ?class ")"
					$?derived-slots
			;		"(" counter "?old-counter" ")" 
			;		"(" derivators "$?" "?derivator" "$?" ")"
;					"(" derivators "$?" +++ $?object-address-vars +++ "$?" ")"
					"(" derivators "$?" +++ "?" $?object-address-vars +++ "$?" ")"
				")"
			")"

;			"(" test
;				"(" or 
;					"(" not "(" instance-existp $?inst ")" ")"
;					"(" not "(" member$ "(" implode$ "(" create$ $?object-address-vars ")" ")" "(" send $?inst get-derivators ")" ")" ")"
;					"(" neq "(" subseq-pos "(" create$ +++ $?object-address-vars  +++ $$$ "(" send $?inst get-derivators ")" ")" ")" FALSE ")"
;				")"
;			")"
		  => 
			$?init-concl
			"(" bind "?oid" $?inst ")"
			"(" if "(" instance-existp "?oid" ")"
			   	then
;			   		"(" slot-insert$ "?oid" derivators 1 "(" implode$ "(" create$ $?object-address-vars ")" ")" ")"
;			   		"(" slot-insert$ "?oid" derivators 1 +++ $?object-address-vars +++ ")"
			   		"(" slot-insert$ "?oid" derivators 1 +++ ?rule-name $?object-address-vars +++ ")"
			   		"(" send "?oid" put-counter "(" + "(" send "?oid" get-counter ")" 1 ")" ")"
			   	else		
					(insert$ 
						(insert$ 
							$?rest-concl 
							(length$ $?rest-concl)
;							"(" derivators "(" implode$ "(" create$ $?object-address-vars ")" ")" ")"
;							"(" derivators +++ $?object-address-vars +++ ")"
							"(" derivators +++ ?rule-name $?object-address-vars +++ ")"
						)
						2 
						make-instance "?oid" of
					)
			")"
			;"(" send "?oid" print ")"
		")"
	)))
	;(printout t "production-rule: " ?production-rule crlf)
	(bind ?del-rule-name (gensym))
	(bind ?delete-production-rule (str-cat$ (create$ 
		"(" defrule ?del-rule-name 
			"(" declare "(" salience 1000 ")" ")"
			"(" run-deductive-rules ")"
			;"?derived-object" <- 
				"(" object 
					"(" name "?derived-object" ")"
					"(" is-a ?class ")"
					$?derived-slots
					"(" counter "?old-counter" ")" 
;					"(" derivators "$?" "?derivator" "$?" ")"
;					"(" derivators "$?DER-B" +++ $?object-address-vars +++ "$?DER-A" ")"
					"(" derivators "$?DER-B" +++ ?rule-name $?object-address-vars +++ "$?DER-A" ")"
				")"

			"(" or
				"(" test 
					"(" not
						;"("all-instance-existp "(" explode$ "?derivator" ")" ")"
						"("all-instance-existp "(" create$ $?object-address-vars ")" ")"
					")"
				")"
;				(build-queries (create$ $?object-address-vars +++ $?new-condition $$$ $?conclusion))
				"(" and
					$?object-names-and-classes
					"(" not 
						(if (> (token-length $?new-condition) 1)
						   then
							(create$ "(" and $?new-condition ")" )
						   else
						   	$?new-condition
						)
					")"
				")"
			")"
		  => 
		  	;"(" printout t "derivators: " $?object-address-vars crlf ")"
			"(" bind "?new-counter" "(" - "?old-counter" 1 ")" ")"
			"(" if "(" = "?new-counter" 0 ")"
				then
					"(" send "?derived-object" delete ")"
				else
					"(" modify-instance "?derived-object" 
						"(" counter "?new-counter" ")" 
						"(" derivators  "$?DER-B" "$?DER-A" ")"
;						"(" derivators "(" delete-member$ "(" send "?derived-object" get-derivators ")" "?derivator" ")" ")"
					")"
			")"
		")"
	)))
	;(printout t "delete-production-rule: " ?delete-production-rule crlf)
	(modify ?rule-index 
		(name ?rule-name) 
		(del-name ?del-rule-name) 
		;(production-rule ?production-rule) 
		(production-rule (str-cat$ $?new-condition))
		;(delete-production-rule ?delete-production-rule) 
		(derived-class ?class))
	(bind $?non-existent-class-indices (get-template-specific-facts non-existent-classes (get-fact-list)))
	(if (= (length$ $?non-existent-class-indices) 0)
	   then
		(my-build ?production-rule)
		(my-build ?delete-production-rule)
	   else	   
		(bind ?idx (nth$ 1 $?non-existent-class-indices))
		(bind $?non-existent-classes (fact-slot-value ?idx implied))
		(assert (pending-rule 
				(production-rule ?production-rule) 
				(delete-production-rule ?delete-production-rule)
				(non-existent-classes (fact-slot-value ?idx implied))))
		(retract ?idx)
	)
)

(deffunction nullify-slots ($?slots)
	(bind $?result (create$))
	(while (> (length$ $?slots) 0)
	   do
	   	(bind ?p2 (get-token $?slots))
		;(bind $?first-slot (subseq$ $?slots 1 ?p2))
		(bind $?result (create$ $?result (replace$ (subseq$ $?slots 1 ?p2) 3 3 nil)))
		(bind $?slots (subseq$ $?slots (+ ?p2 1) (length$ $?slots)))
	)
	$?result
)

(deffunction not-nil-slots ($?slots)
	(bind $?result (create$))
	(while (> (length$ $?slots) 0)
	   do
	   	(bind ?p2 (get-token $?slots))
		;(bind $?first-slot (subseq$ $?slots 1 ?p2))
		(bind $?result (create$ $?result "(" neq (nth$ 3 (subseq$ $?slots 1 ?p2)) nil ")"))
		(bind $?slots (subseq$ $?slots (+ ?p2 1) (length$ $?slots)))
	)
	$?result
)

(deffunction collect-condition-slots ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
	   	(bind ?p2 (get-token $?condition))
		(bind $?first-cond-elem (subseq$ $?condition 1 ?p2))
;		(bind $?result (create$ $?result (subseq$ $?first-cond-elem 9 (- (length$ $?first-cond-elem) 1))))
		(bind $?result (create$ $?result (subseq$ $?first-cond-elem 11 (- (length$ $?first-cond-elem) 1))))
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	$?result
)


;(deffunction translate-derived-attribute-rule (?rule-index ?derived-attribute-rule)
(deffunction translate-derived-attribute-rule (?derived-attribute-rule $?d-classes)
	(verbose "Derived attribute rule: " ?derived-attribute-rule crlf)
	(bind $?dar (my-explode$ ?derived-attribute-rule))
	(bind ?imp_pos (member$ => $?dar))
	;(bind $?condition (subseq$ $?dar 1 (- ?imp_pos 1)))
	(bind $?conclusion (subseq$ $?dar (+ ?imp_pos 1) (length$ $?dar)))
	(bind $?new-condition (translate-condition (subseq$ $?dar 1 (- ?imp_pos 1))))
	(bind ?implies-class (nth$ 1 $?d-classes))
	(bind $?depends-classes (rest$ $?d-classes))
	;(bind ?p1 (member "(" $?conclusion))
	(bind ?p2 (get-token $?conclusion))
	;(bind $?first-concl-elem (subseq$ $?conclusion (member "(" $?conclusion) ?p2))
	(if (eq (nth$ 2 (subseq$ $?conclusion (member "(" $?conclusion) ?p2)) calc)
	   then
	   	(bind $?init-concl (subseq$ $?conclusion 3 (- ?p2 1)))
		(bind $?rest-concl (subseq$ $?conclusion (+ ?p2 1) (length$ $?conclusion)))
	   else
	   	(bind $?init-concl (create$))
		(bind $?rest-concl $?conclusion )
	)
	(bind ?derived-object (nth$ 1 $?rest-concl))
	(bind $?derived-slots (subseq$ $?rest-concl 5 (- (length$ $?rest-concl) 1)))
	(bind ?rule-name (gensym))
	(bind ?del-rule-name (gensym))
	(bind $?concl-as-cond (transform-one-cond-elem $?rest-concl))
	(bind $?var-class-pairs (collect-var-class-pairs $?concl-as-cond))
	;(bind $?slot-predicates (collect-slot-predicates $?concl-as-cond))
	(bind $?encaps-slots (create$ $?derived-slots (inverse-brackets (reverse$ $?var-class-pairs))))
	(bind $?encaps-preds (create$ $?init-concl (collect-slot-predicates $?concl-as-cond)))
	(while (> (length$ $?encaps-slots) 0)
	   do
	   	(bind ?p2 (get-token $?encaps-slots))
		;(bind $?first-slot (subseq$ $?encaps-slots 1 ?p2))
		;(bind ?current-value (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)))
		(bind $?encaps-preds (collapse-vars (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)) $?encaps-preds))
		(bind $?encaps-slots (subseq$ $?encaps-slots (+ ?p2 1) (length$ $?encaps-slots)))
	)
	;(bind $?copy-var-class-pairs $?var-class-pairs)
	;(while (> (length$ $?copy-var-class-pairs) 0)
	;   do
	;   	(bind ?p2 (get-token $?copy-var-class-pairs))
		;(bind $?first-pair (subseq$ $?copy-var-class-pairs 1 ?p2))
	;	(bind ?one-class-var (nth$ 2 (subseq$ $?copy-var-class-pairs 1 ?p2)))
	;	(bind $?encaps-preds (replace-member$ $?encaps-preds (create$ "(" instance-address ?one-class-var ")") ?one-class-var))
	;	(bind $?copy-var-class-pairs (subseq$ $?copy-var-class-pairs (+ ?p2 1) (length$ $?copy-var-class-pairs)))
	;)
	;(bind $?all-condition-slots (collect-condition-slots $?new-condition))
	(bind $?not-nil-slot-tests (not-nil-slots (collect-condition-slots $?new-condition)))
	(bind ?production-rule (str-cat$ (create$ 
		"(" defrule ?rule-name 
			"(" declare "(" salience 2000 ")" ")"
			"(" run-deductive-rules ")"
			$?new-condition
			"(" test 
				(if (= (token-length $?not-nil-slot-tests) 1)
				   then
				   	(create$ $?not-nil-slot-tests)
				   else
					(create$ "(" and $?not-nil-slot-tests ")" )
				)
			")"
			"(" not 
				"(" test 
					"(" any-instancep 
						"(" $?var-class-pairs ")" 
						(if (= (token-length $?encaps-preds) 1)
						   then
						   	(create$ $?encaps-preds)
						   else
						   	(create$ "(" and $?encaps-preds ")")
						)
					")" 
				")"
			")"
			"(" test "(" instance-existp ?derived-object ")" ")"
		  => 
			$?init-concl 
			"(" modify-instance ?derived-object $?derived-slots ")"
		")"
	)))
	(bind $?var-class-pairs (collect-var-class-pairs $?new-condition))
	;(bind $?slot-predicates (collect-slot-predicates $?new-condition))
	(bind $?encaps-slots (create$ $?derived-slots (inverse-brackets (reverse$ $?var-class-pairs))))
	(bind $?encaps-preds (create$ $?init-concl $?not-nil-slot-tests (collect-slot-predicates $?new-condition)))
	(while (> (length$ $?encaps-slots) 0)
	   do
	   	(bind ?p2 (get-token $?encaps-slots))
		;(bind $?first-slot (subseq$ $?encaps-slots 1 ?p2))
		;(bind ?current-value (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)))
		(bind $?encaps-preds (collapse-vars (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)) $?encaps-preds))
		(bind $?encaps-slots (subseq$ $?encaps-slots (+ ?p2 1) (length$ $?encaps-slots)))
	)
;	(bind $?copy-var-class-pairs $?var-class-pairs)
;	(while (> (length$ $?copy-var-class-pairs) 0)
;	   do
;	   	(bind ?p2 (get-token $?copy-var-class-pairs))
;		;(bind $?first-pair (subseq$ $?copy-var-class-pairs 1 ?p2))
;		(bind ?one-class-var (nth$ 2 (subseq$ $?copy-var-class-pairs 1 ?p2)))
;		(bind $?encaps-preds (replace-member$ $?encaps-preds (create$ "(" instance-address ?one-class-var ")") ?one-class-var))
;		(bind $?copy-var-class-pairs (subseq$ $?copy-var-class-pairs (+ ?p2 1) (length$ $?copy-var-class-pairs)))
;	)
	(bind $?not-nil-slot-tests (not-nil-slots $?derived-slots))
	(bind ?delete-production-rule (str-cat$ (create$ 
		"(" defrule ?del-rule-name 
			"(" declare "(" salience 1999 ")" ")"
			"(" run-deductive-rules ")"
			$?concl-as-cond
			"(" test 
				(if (= (token-length $?not-nil-slot-tests) 1)
				   then
				   	(create$ $?not-nil-slot-tests)
				   else
					(create$ "(" and $?not-nil-slot-tests ")" )
				)
			")"
			"(" not 
				"(" test 
					"(" any-instancep 
						"(" $?var-class-pairs ")" 
						(if (= (token-length $?encaps-preds) 1)
						   then
						   	(create$ $?encaps-preds)
						   else
						   	(create$ "(" and $?encaps-preds ")")
						)
					")"
				")"
			")"
			"(" test "(" instance-existp ?derived-object ")" ")"
		  => 
			"(" modify-instance ?derived-object (nullify-slots $?derived-slots) ")"
		")"
	)))
	(assert 
		(derived-attribute-rule 
			(derived-attribute-rule ?derived-attribute-rule)
			(name ?rule-name) 
			(del-name ?del-rule-name) 
			(implies ?implies-class)
			(depends-on $?depends-classes)
			;(production-rule ?production-rule) 
			;(delete-production-rule ?delete-production-rule)
		)
	)
	;(modify ?rule-index 
	;	(name ?rule-name) 
	;	(del-name ?del-rule-name) 
	;	(production-rule ?production-rule) 
	;	(delete-production-rule ?delete-production-rule) 
	;)
	(my-build ?production-rule)
	(my-build ?delete-production-rule)
)

(deffunction find-object-of-value (?value-var $?condition)
	(while (> (length$ $?condition) 0)
	   do
	   	(bind ?p2 (get-token $?condition))
		;(bind $?first-cond-elem (subseq$ $?condition 1 ?p2))
		(if (member$ ?value-var (subseq$ $?condition 1 ?p2))
		   then
		   	;(return (nth$ 1 $?condition))
		   	(return (nth$ 5 $?condition))
		)
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	FALSE
)

(deffunction find-depend-var-aux ($?expression)
	(bind ?end (length$ $?expression))
	(loop-for-count (?n 1 ?end)
	   do
		(if (is-var (nth$ ?n $?expression))
		   then
		   	(return (nth$ ?n $?expression))
		)
	)
	FALSE
)

(deffunction find-depend-var (?var $?code)
	(while (> (length$ $?code) 0)
	   do
	   	(bind ?p2 (get-token $?code))
		(bind $?first-code-elem (subseq$ $?code 1 ?p2))
		(if (and
			(eq (nth$ 2 $?first-code-elem) bind)
			(eq (nth$ 3 $?first-code-elem) ?var))
		   then
			(return (find-depend-var-aux (subseq$ $?first-code-elem 4 (- (length$ $?first-code-elem) 1))))
		)
		(bind $?code (subseq$ $?code (+ ?p2 1) (length$ $?code)))
	)
	FALSE
)


;(deffunction translate-aggregate-attribute-rule (?rule-index ?aggregate-attribute-rule)
(deffunction translate-aggregate-attribute-rule (?aggregate-attribute-rule $?d-classes)
	(verbose "Aggregate attribute rule: " ?aggregate-attribute-rule crlf)
	(bind $?aar (my-explode$ ?aggregate-attribute-rule))
	(bind ?imp_pos (member$ => $?aar))
	;(bind $?condition (subseq$ $?aar 1 (- ?imp_pos 1)))
	(bind $?conclusion (subseq$ $?aar (+ ?imp_pos 1) (length$ $?aar)))
	(bind $?new-condition (translate-condition (subseq$ $?aar 1 (- ?imp_pos 1))))
	(bind ?implies-class (nth$ 1 $?d-classes))
	(bind $?depends-classes (rest$ $?d-classes))
	;(bind ?p1 (member "(" $?conclusion))
	(bind ?p2 (get-token $?conclusion))
	;(bind $?first-concl-elem (subseq$ $?conclusion (member "(" $?conclusion) ?p2))
	(if (eq (nth$ 2 (subseq$ $?conclusion (member "(" $?conclusion) ?p2)) calc)
	   then
	   	(bind $?init-concl (subseq$ $?conclusion 3 (- ?p2 1)))
		(bind $?rest-concl (subseq$ $?conclusion (+ ?p2 1) (length$ $?conclusion)))
	   else
	   	(bind $?init-concl (create$))
		(bind $?rest-concl $?conclusion )
	)
	(bind ?rule-name (gensym))
	(bind ?del-rule-name (gensym))
	(bind ?derived-object (nth$ 1 $?rest-concl))
	(bind ?class (nth$ 4 $?rest-concl))
	(bind ?att (nth$ 6 $?rest-concl))
	(bind ?aggregate-fun (nth$ 8 $?rest-concl))
	(bind ?current-value (nth$ 9 $?rest-concl))
	(if (member$ ?current-value $?new-condition)
	   then
		(bind ?object-of-value (find-object-of-value ?current-value $?new-condition))
	   else
	   	;(bind ?search-value (find-depend-var ?current-value $?init-concl))
	   	(bind ?object-of-value (find-object-of-value (find-depend-var ?current-value $?init-concl) $?new-condition))
	)
	;(bind $?all-condition-slots (collect-condition-slots $?new-condition))
	(bind $?not-nil-slot-tests (not-nil-slots (collect-condition-slots $?new-condition)))
	(bind $?check-old-att	
		(create$
			;"?aggr-fun-ind" <- 
				"(" object "(" name "?aggr-fun-ind" ")" "(" is-a ?aggregate-fun ")" "(" class ?class ")" "(" attribute ?att ")" "(" instance ?derived-object ")" "(" values "$?old"  ")" "(" objects "$?old-list"  ")" ")"
		)
	)
	(bind $?var-class-pairs (collect-var-class-pairs $?check-old-att))
	;(bind $?slot-predicates (collect-slot-predicates $?check-old-att))
	(bind $?encaps-slots (create$ "(" instance ?derived-object ")" (inverse-brackets (reverse$ $?var-class-pairs))))
	(bind $?encaps-preds 
		(create$ 
			$?init-concl 
			"(" or 
				"(" neq "(" member$ ?object-of-value "$?old-list" ")" 
					"(" member$ ?current-value "$?old" ")" 
				")"
				"(" eq	"(" member$ ?object-of-value "$?old-list" ")" 
					"(" member$ ?current-value "$?old" ")" 
					FALSE
				")"
			")"
			(collect-slot-predicates $?check-old-att)
		)
	)
	(while (> (length$ $?encaps-slots) 0)
	   do
	   	(bind ?p2 (get-token $?encaps-slots))
		;(bind $?first-slot (subseq$ $?encaps-slots 1 ?p2))
		;(bind ?c-value (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)))
		(bind $?encaps-preds (collapse-vars (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)) $?encaps-preds))
		(bind $?encaps-slots (subseq$ $?encaps-slots (+ ?p2 1) (length$ $?encaps-slots)))
	)
;	(bind $?copy-var-class-pairs $?var-class-pairs)
;	(while (> (length$ $?copy-var-class-pairs) 0)
;	   do
;	   	(bind ?p2 (get-token $?copy-var-class-pairs))
		;(bind $?first-pair (subseq$ $?copy-var-class-pairs 1 ?p2))
;		(bind ?one-class-var (nth$ 2 (subseq$ $?copy-var-class-pairs 1 ?p2)))
;		(bind $?encaps-preds (replace-member$ $?encaps-preds (create$ "(" instance-address ?one-class-var ")") ?one-class-var))
;		(bind $?copy-var-class-pairs (subseq$ $?copy-var-class-pairs (+ ?p2 1) (length$ $?copy-var-class-pairs)))
;	)
	(bind ?production-rule (str-cat$ (create$ 
		"(" defrule ?rule-name 
			"(" declare "(" salience 2000 ")" ")"
			"(" run-deductive-rules ")"
			$?new-condition
			"(" test 
				(if (= (token-length $?not-nil-slot-tests) 1)
				   then
				   	(create$ $?not-nil-slot-tests)
				   else
					(create$ "(" and $?not-nil-slot-tests ")" )
				)
			")"
			"(" test 
				"(" or
					"(" not
						"(" any-instancep
							"(" "(" "?AF" ?aggregate-fun ")" ")" 
							"(" and 
								"(" eq "?AF:class" ?class ")" 
								"(" eq "?AF:instance" ?derived-object ")" 
								"(" eq "?AF:attribute" ?att ")" 
							")" 
						")" 
					")"
					"(" any-instancep 
						"(" $?var-class-pairs ")" 
						(if (= (token-length $?encaps-preds) 1)
						   then
						   	(create$ $?encaps-preds)
						   else
						   	(create$ "(" and $?encaps-preds ")")
						)
					")"
				")"
			")"
			"(" test "(" instance-existp ?derived-object ")" ")"
		  => 
			$?init-concl 
			"(" bind "$?aggr-fun-ind1" 
				"(" find-instance 
					"(" "(" "?AF" ?aggregate-fun ")" ")" 
					"(" and 
						"(" eq "?AF:class" ?class ")" 
						"(" eq "?AF:instance" ?derived-object ")" 
						"(" eq "?AF:attribute" ?att ")" 
					")" 
				")" 
			")"
			"(" if "(" = "(" length$ "$?aggr-fun-ind1" ")" 0 ")"
			   then
			   	"(" bind "?aggr-fun-ind" 
			   		;"(" instance-address 
			   			"(" make-instance of ?aggregate-fun 
			   				"(" class ?class ")" 
			   				"(" instance ?derived-object ")" 
			   				"(" attribute ?att ")" 
			   			")" 
			   		;")" 
			   	")"
			   else
			   	"(" bind "?aggr-fun-ind" "(" nth$ 1 "$?aggr-fun-ind1" ")" ")"
			")"
			;"(" bind "$?old" "(" send "?aggr-fun-ind" get-values ")" ")"
			;"(" bind "$?old-list" "(" send "?aggr-fun-ind" get-objects ")" ")"
			"(" bind "$?new" "(" create$ "(" send "?aggr-fun-ind" get-values ")" ?current-value ")" ")"
			;"(" bind "$?new-list" "(" create$ "(" send "?aggr-fun-ind" get-objects ")" ?object-of-value ")" ")"
			;"(" bind "$?result" "(" send "?aggr-fun-ind" calc-result "$?new" ")" ")"
			"(" modify-instance ?derived-object "(" ?att "(" send "?aggr-fun-ind" calc-result "$?new" ")" ")" ")"
			"(" modify-instance "?aggr-fun-ind" "(" values "$?new" ")" "(" objects "(" create$ "(" send "?aggr-fun-ind" get-objects ")" ?object-of-value ")" ")" ")"
		")"
	)))
	(bind $?var-class-pairs (collect-var-class-pairs $?new-condition))
	;(bind $?slot-predicates (collect-slot-predicates $?new-condition))
	(bind $?encaps-slots (create$ "(" ?att ?current-value ")" (inverse-brackets (reverse$ $?var-class-pairs))))
	(bind $?encaps-preds (create$ $?not-nil-slot-tests $?init-concl (collect-slot-predicates $?new-condition)))
	(while (> (length$ $?encaps-slots) 0)
	   do
	   	(bind ?p2 (get-token $?encaps-slots))
		;(bind $?first-slot (subseq$ $?encaps-slots 1 ?p2))
		;(bind ?c-value (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)))
		(bind $?encaps-preds (collapse-vars (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)) $?encaps-preds))
		(bind $?encaps-slots (subseq$ $?encaps-slots (+ ?p2 1) (length$ $?encaps-slots)))
	)
;	(bind $?copy-var-class-pairs $?var-class-pairs)
;	(while (> (length$ $?copy-var-class-pairs) 0)
;	   do
;	   	(bind ?p2 (get-token $?copy-var-class-pairs))
		;(bind $?first-pair (subseq$ $?copy-var-class-pairs 1 ?p2))
;		(bind ?one-class-var (nth$ 2 (subseq$ $?copy-var-class-pairs 1 ?p2)))
;		(bind $?encaps-preds (replace-member$ $?encaps-preds (create$ "(" instance-address ?one-class-var ")") ?one-class-var))
;		(bind $?copy-var-class-pairs (subseq$ $?copy-var-class-pairs (+ ?p2 1) (length$ $?copy-var-class-pairs)))
;	)
	(bind ?c-value-var-list1 (sym-cat "$?" (gensym)))
	(bind ?o-value-var-list1 (sym-cat "$?" (gensym)))
	(bind ?c-value-var-list2 (sym-cat "$?" (gensym)))
	(bind ?o-value-var-list2 (sym-cat "$?" (gensym)))
	(bind ?delete-production-rule (str-cat$ (create$ 
		"(" defrule ?del-rule-name 
			"(" declare "(" salience 1999 ")" ")"
			"(" run-deductive-rules ")"
			;"?aggr-fun-ind" <- 
				"(" object 
					"(" name "?aggr-fun-ind" ")"
					"(" is-a ?aggregate-fun ")" 
					"(" class ?class ")" "(" attribute ?att ")" "(" instance ?derived-object ")" 
					"(" values ?c-value-var-list1 ?current-value ?c-value-var-list2  ")" 
					"(" objects ?o-value-var-list1 "&:(" eq "(" length$ ?c-value-var-list1 ")" "(" length$ ?o-value-var-list1 ")" ")" ?object-of-value ?o-value-var-list2  ")" ")"
			"(" not 
				"(" test 
					"(" any-instancep 
						"(" $?var-class-pairs ")" 
						(if (= (token-length $?encaps-preds) 1)
						   then
						   	(create$ $?encaps-preds)
						   else
						   	(create$ "(" and $?encaps-preds ")")
						)
					")"
				")"
			")"
		  => 
			"(" bind "$?new" "(" create$ ?c-value-var-list1 ?c-value-var-list2 ")" ")"
			;"(" bind "$?new-list" "(" create$ ?o-value-var-list1 ?o-value-var-list2 ")" ")"
			;"(" bind "$?result" "(" send "?aggr-fun-ind" calc-result "$?new" ")" ")"
			"(" modify-instance ?derived-object "(" ?att "(" send "?aggr-fun-ind" calc-result "$?new" ")" ")" ")"
			"(" modify-instance "?aggr-fun-ind" "(" values "$?new" ")" "(" objects "(" create$ ?o-value-var-list1 ?o-value-var-list2 ")" ")" ")"
		")"
	)))
	(assert 
		(aggregate-attribute-rule 
			(aggregate-attribute-rule ?aggregate-attribute-rule)
			(name ?rule-name) 
			(del-name ?del-rule-name)
			(implies ?implies-class)
			(depends-on $?depends-classes)
			;(production-rule ?production-rule) 
			;(delete-production-rule ?delete-production-rule)
		)
	)
;	(modify ?rule-index 
;		(name ?rule-name) 
;		(del-name ?del-rule-name) 
;		(production-rule ?production-rule) 
;		(delete-production-rule ?delete-production-rule) 
;	)
	(my-build ?production-rule)
	(my-build ?delete-production-rule)
)


(deffunction collect-class-names ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
		(bind ?p2 (get-token $?condition))
		(bind $?cond-elem (subseq$ $?condition 1 ?p2))
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
   		(if (or 
   			(eq (nth$ 2 $?cond-elem) not)
   			(eq (nth$ 2 $?cond-elem) or)
   			(eq (nth$ 2 $?cond-elem) and))
   		   then
   		   	(bind $?result (create$ $?result (collect-class-names (subseq$ $?cond-elem 3 (- (length$ $?cond-elem) 1)))))
   		   else
   		   	(if (eq (nth$ 2 $?cond-elem) <-)
   		   	   then
   		   	   	(bind ?class (nth$ 4 $?cond-elem))
   		   	   else
   		   	   	(bind ?class (nth$ 2 $?cond-elem))
   		   	)
   		   	(if (neq ?class test)
   		   	   then
   		   		(bind $?result (create$ $?result ?class))
   		   	)
   		)
   	)
   	$?result
)
		

(deffunction build-dependency-network (?deductive-rule)
	(bind $?dr (my-explode$ ?deductive-rule))
	(bind ?imp_pos (member$ => $?dr))
	(bind $?condition (subseq$ $?dr 1 (- ?imp_pos 1)))
	(bind $?conclusion (subseq$ $?dr (+ ?imp_pos 1) (length$ $?dr)))
	;(bind $?depends-on-classes (collect-class-names (subseq$ $?dr 1 (- ?imp_pos 1))))
	;(verbose "conclusion (before): " $?conclusion crlf)
	(if (eq (nth$ 2 $?conclusion) <-)
	   then
	   	(bind $?conclusion (subseq$ $?conclusion 3 (length$ $?conclusion)))
	)
	;(verbose "conclusion (after): " $?conclusion crlf)
	(bind ?p2 (get-token $?conclusion))
	(if (eq (nth$ 2 (subseq$ $?conclusion (member "(" $?conclusion) ?p2)) calc)
	   then
		(bind ?implies-class (nth$ 2 (subseq$ $?conclusion (+ ?p2 1) (length$ $?conclusion))))
	   else
		(bind ?implies-class (nth$ 2 $?conclusion))
	)
	;(verbose "condition: " $?condition crlf)
	(create$ ?implies-class (collect-class-names $?condition))
	;(modify ?idx (depends-on $?depends-on-classes) (implies ?implies-class))
)
	
(deffunction insert-pending-rule (?rule ?del-rule $?classes)
	(if (= (length$ (exist-classes $?classes)) 0)
	   then
	   	(my-build ?rule)
	   	(my-build ?del-rule)
	   else
	 	(printout t "Rule: " crlf ?rule crlf "cannot be created because classes " (exist-classes $?classes) " do not exist!" crlf) 
	)
)


(deffunction pre-compile-deductive-rule (?rule-string1)
   	(bind ?rule-string (replace-anonymous-variables ?rule-string1))
	(bind $?results-2nd-order (analyze-2nd-order ?rule-string))
	(if (neq $?results-2nd-order FALSE)
	   then
	   	(verbose "2nd order rule: " ?rule-string " auxiliary data: " $?results-2nd-order crlf)
	   	(assert (2nd-order-rule ?rule-string $?results-2nd-order))
   		(bind ?*untranslated_rules* (+ ?*untranslated_rules* 1))
		;(translate-2nd-order-rule ?rule $?results-2nd-order)
		;(printout t "ok!" crlf)
	)
	(if (or
		(eq $?results-2nd-order FALSE)
		(eq (nth$ 1 $?results-2nd-order) aliased-slot))
	   then
	   	(verbose "deductive rule: " ?rule-string crlf)
		(bind $?dependencies (build-dependency-network ?rule-string))
		(assert 
			(deductive-rule 
				(deductive-rule ?rule-string) 
				(depends-on (rest$ $?dependencies))
				(implies (nth$ 1 $?dependencies))
			)
		)
	)
)



(deffunction translate-device-rules ()
	(while (> ?*untranslated_rules* 0)
	   do
		(run-goal translate-derived-attribute-rules)
		(run-goal translate-aggregate-attribute-rules)
		(run-goal pre-compile-deductive-rules)
		(run-goal translate-deductive-rules)
		(run-goal translate-2nd-order-rules)
	)
	(run-goal insert-pending-rules)
	(run-goal calc-stratum-for-all)
	TRUE
)
