(deffunction extract-var (?var $?code)
	(bind $?result (create$))
	(while (> (length$ $?code) 0)
	   do
	   	(bind ?p2 (get-token $?code))
		(bind $?first-code-elem (subseq$ $?code 1 ?p2))
		(if (and 
			(eq (nth$ 2 $?first-code-elem) bind) 
			(eq (nth$ 3 $?first-code-elem) ?var))
		   then
		   	(return $?first-code-elem)
		)
		(bind $?code (subseq$ $?code (+ ?p2 1) (length$ $?code)))
	)
	$?result
)

(deffunction remove-var (?var $?code)
	(bind $?result (create$))
	(while (> (length$ $?code) 0)
	   do
	   	(bind ?p2 (get-token $?code))
		(bind $?first-code-elem (subseq$ $?code 1 ?p2))
		(if (not (and 
			(eq (nth$ 2 $?first-code-elem) bind) 
			(eq (nth$ 3 $?first-code-elem) ?var)))
		   then
		   	(bind $?result (create$ $?result $?first-code-elem))
		   else
		   	(bind $?result (create$ $?result (subseq$ $?code (+ ?p2 1) (length$ $?code))))
		   	(break)
		)
		(bind $?code (subseq$ $?code (+ ?p2 1) (length$ $?code)))
	)
	$?result
)

(deffunction collapse-vars (?var $?code)
	(bind $?target-code (extract-var ?var $?code))
	(bind $?encaps-code (remove-var ?var $?code))
	(bind $?left-code (create$))
	(while (> (length$ $?encaps-code) 0)
	   do
	   	(bind ?p2 (get-token $?encaps-code))
		(bind $?first-code-elem (subseq$ $?encaps-code 1 ?p2))
		(bind $?encaps-code (subseq$ $?encaps-code (+ ?p2 1) (length$ $?encaps-code)))
		(if (eq (nth$ 2 $?first-code-elem) bind)
		   then
			(bind ?encaps-var (nth$ 3 $?first-code-elem))
			(bind $?encaps-var-code (subseq$ $?first-code-elem 4 (- (length$ $?first-code-elem) 1)))
			(if (or (member$ ?encaps-var $?encaps-code)
				(member$ ?encaps-var $?left-code))
			   then
		   		(bind $?encaps-code (replace-member$ $?encaps-code $?encaps-var-code ?encaps-var))
		   		(bind $?target-code (replace-member$ $?target-code $?encaps-var-code ?encaps-var))
		   		(bind $?left-code (replace-member$ $?left-code $?encaps-var-code ?encaps-var))
		   	   else
		   	   	(bind $?left-code (create$ $?left-code $?first-code-elem))
		   	)
		   else
		   	(bind $?left-code (create$ $?left-code $?first-code-elem))
		)
	)
	(if (< (length$ $?target-code) 5)
	   then
	   	$?left-code
	   else
		(create$ 
			$?left-code 
			"(" or 
				"(" eq ?var (subseq$ $?target-code 4 (- (length$ $?target-code) 1)) ")"
				"(" member$ ?var "(" create$ (subseq$ $?target-code 4 (- (length$ $?target-code) 1)) ")" ")"
			")"
		)
	)
)

(deffunction collect-negative-conditions ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
	   	(bind ?p2 (get-token $?condition))
		(bind $?first-cond-elem (subseq$ $?condition 1 ?p2))
		(if (eq (nth$ 2 $?first-cond-elem) not)
		   then
			(bind $?result (create$ $?result (subseq$ $?first-cond-elem 3 (- (length$ $?first-cond-elem) 1))))
		)
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	$?result
)

(deffunction collect-positive-conditions ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
	   	(bind ?p2 (get-token $?condition))
		(bind $?first-cond-elem (subseq$ $?condition 1 ?p2))
		(if (neq (nth$ 2 $?first-cond-elem) not)
		   then
			(bind $?result (create$ $?result $?first-cond-elem))
		)
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	$?result
)

(deffunction collect-var-class-pairs ($?condition)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
	   	(bind ?p2 (get-token $?condition))
		(bind $?first-cond-elem (subseq$ $?condition 1 ?p2))
		(if (and 
			(neq (nth$ 2 $?first-cond-elem) test)
			(neq (nth$ 2 $?first-cond-elem) not)
		    )
		   then
		   	(if (or 
		   		(eq (nth$ 2 $?first-cond-elem) and)
		   		(eq (nth$ 2 $?first-cond-elem) or))
		   	   then
		   	   	(bind $?result (create$ $?result (collect-var-class-pairs (subseq$ $?first-cond-elem 3 (- (length$ $?first-cond-elem) 1)))))
		   	   else
;				(bind $?result (create$ $?result "(" (nth$ 1 $?first-cond-elem) (nth$ 7 $?first-cond-elem) ")"))
				(bind $?result (create$ $?result "(" (nth$ 5 $?first-cond-elem) (nth$ 9 $?first-cond-elem) ")"))
			)
		)
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	$?result
)



; This function needs imporovements
; First, connectives are not handled
; second, other combinations of variables for multifield slots are not handled, only ($? ?x $?) and
; of course the trivial $?x
(deffunction collect-slot-predicates-aux (?class-var $?slot-pairs-and-svars)
	(bind ?pos (member$ $$$ $?slot-pairs-and-svars))
   	(bind $?svars (subseq$ $?slot-pairs-and-svars 1 (- ?pos 1)))
   	(bind $?slot-pairs (subseq$ $?slot-pairs-and-svars (+ ?pos 1) (length$ $?slot-pairs-and-svars)))
	(bind $?result (create$))
	(while (> (length$ $?slot-pairs) 0)
	   do
	   	(bind ?p2 (get-token $?slot-pairs))
		(bind $?first-slot-pair (subseq$ $?slot-pairs 1 ?p2))
		(bind $?value-expr (subseq$ $?first-slot-pair 3 (- (length$ $?first-slot-pair) 1)))
		(if (and 
			(= (length$ $?value-expr) 1) 
			(is-var (nth$ 1 $?value-expr))
			(not (member$ (nth$ 1 $?value-expr) $?svars))
		    )
		   then
			(bind $?result (create$ $?result "(" bind (nth$ 1 $?value-expr) (sym-cat ?class-var : (nth$ 2 $?first-slot-pair)) ")"))
		   else 
		   	(if (and 
		   		(= (length$ $?value-expr) 3) 
		   		(is-var (nth$ 2 $?value-expr))
				(not (member$ (nth$ 2 $?value-expr) $?svars))
		   	    )
		   	   then
				(bind $?result (create$ $?result "(" bind (nth$ 2 $?value-expr) (sym-cat ?class-var : (nth$ 2 $?first-slot-pair)) ")"))
		  	   else
		  	   	(if (and 
		  	   		(not (and (= (length$ $?value-expr) 1) (is-var (nth$ 1 $?value-expr)) (member$ (nth$ 1 $?value-expr) $?svars)))
		  	   		(not (and (= (length$ $?value-expr) 3) (is-var (nth$ 2 $?value-expr)) (member$ (nth$ 2 $?value-expr) $?svars)))
			  	    )
			  	   then
					(bind $?result (create$ $?result "(" eq $?value-expr (sym-cat ?class-var : (nth$ 2 $?first-slot-pair)) ")"))
				)
			)
		)
		(bind $?slot-pairs (subseq$ $?slot-pairs (+ ?p2 1) (length$ $?slot-pairs)))
	)
	$?result
)

(deffunction collect-slot-predicates ($?condition-and-svars)
	(bind ?pos (member$ $$$ $?condition-and-svars))
	(if (eq ?pos FALSE)
	   then
	   	(bind $?svars (create$))
	   	(bind $?condition $?condition-and-svars)
	   else
	   	(bind $?svars (subseq$ $?condition-and-svars 1 (- ?pos 1)))
	   	(bind $?condition (subseq$ $?condition-and-svars (+ ?pos 1) (length$ $?condition-and-svars)))
	)
	(bind $?result (create$))
	(while (> (length$ $?condition) 0)
	   do
	   	(bind ?p2 (get-token $?condition))
		(bind $?first-cond-elem (subseq$ $?condition 1 ?p2))
		(if (and 
			(neq (nth$ 2 $?first-cond-elem) test)
			(neq (nth$ 2 $?first-cond-elem) not)
		    )
		   then
		   	(if (or
		   		(eq (nth$ 2 $?first-cond-elem) and)
		   		(eq (nth$ 2 $?first-cond-elem) or))
		   	   then
		   	   	(bind $?result (create$ $?result "(" (nth$ 2 $?first-cond-elem) (collect-slot-predicates (create$ $?svars $$$ (subseq$ $?first-cond-elem 3 (- (length$ $?first-cond-elem) 1)))) ")"))
		   	   else
				;(bind ?class-var (nth$ 1 $?first-cond-elem))
				;(bind $?result (create$ $?result (collect-slot-predicates-aux (nth$ 1 $?first-cond-elem) (create$ $?svars $$$ (subseq$ $?first-cond-elem 9 (- (length$ $?first-cond-elem) 1))))))
				(bind $?result (create$ $?result (collect-slot-predicates-aux (nth$ 5 $?first-cond-elem) (create$ $?svars $$$ (subseq$ $?first-cond-elem 11 (- (length$ $?first-cond-elem) 1))))))
			)
		)
		(bind $?condition (subseq$ $?condition (+ ?p2 1) (length$ $?condition)))
	)
	$?result
)

(deffunction create-new-vars ($?var-class-pairs)
	(bind $?result (create$))
	(while (> (length$ $?var-class-pairs) 0)
	   do
	   	(bind ?p2 (get-token $?var-class-pairs))
		;(bind $?first-pair (subseq$ $?var-class-pairs 1 ?p2))
		(bind $?result (create$ $?result "(" (str-cat "?" (gensym)) (nth$ 2 (subseq$ $?var-class-pairs 1 ?p2)) ")"))
		(bind $?var-class-pairs (subseq$ $?var-class-pairs (+ ?p2 1) (length$ $?var-class-pairs)))
	)
	$?result
)

(deffunction replace-vars ($?double-list)	
	(bind $?new-and-old-vars (subseq$ $?double-list 1 (- (member$ $$$ $?double-list) 1)))
	;(bind $?code (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list)))
	(bind ?code-string (str-cat$ (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list))))
	(while (> (length$ $?new-and-old-vars) 0)
	   do
		(bind ?p2 (get-token $?new-and-old-vars))
		(bind $?first-pair (subseq$ $?new-and-old-vars 1 ?p2))
		(bind $?new-and-old-vars (subseq$ $?new-and-old-vars (+ ?p2 1) (length$ $?new-and-old-vars)))
		(bind ?code-string (str-replace ?code-string (nth$ 2 $?first-pair) (nth$ 3 $?first-pair)))
	)
	(my-explode$ ?code-string)
)


(deffunction create-new-var-list ($?new-and-old-vars)
	(bind $?result (create$))
	(while (> (length$ $?new-and-old-vars) 0)
	   do
		(bind ?p2 (get-token $?new-and-old-vars))
		;(bind $?first-pair (subseq$ $?new-and-old-vars 1 ?p2))
		(bind $?result (create$ $?result (nth$ 2 (subseq$ $?new-and-old-vars 1 ?p2))))
		(bind $?new-and-old-vars (subseq$ $?new-and-old-vars (+ ?p2 1) (length$ $?new-and-old-vars)))
	)
	$?result
)

(deffunction collect-vars ($?code)
	(bind $?result (create$))
	(bind ?end (length$ $?code))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (and
	   		(is-var (nth$ ?n $?code))
	   		(neq (nth$ ?n $?code) "$?"))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ ?n $?code)))
	   	)
	)
	$?result
)

(deffunction object-index-transform ($?cond-element)
)

(deffunction build-queries ($?triple-list)
	(bind ?pos1 (member$ +++ $?triple-list))
	(bind $?object-address-vars (subseq$ $?triple-list 1 (- ?pos1 1)))
	(bind ?pos2 (member$ $$$ $?triple-list))
	(bind $?new-condition (subseq$ $?triple-list (+ ?pos1 1) (- ?pos2 1)))
	(bind $?conclusion (subseq$ $?triple-list (+ ?pos2 1) (length$ $?triple-list)))
	(bind ?p2 (get-token $?conclusion))
	(if (eq (nth$ 2 (subseq$ $?conclusion (member "(" $?conclusion) ?p2)) calc)
	   then
	   	(bind $?init-concl (subseq$ $?conclusion 3 (- ?p2 1)))
		(bind $?rest-concl (subseq$ $?conclusion (+ ?p2 1) (length$ $?conclusion)))
	   else
	   	(bind $?init-concl (create$))
		(bind $?rest-concl $?conclusion )
	)
	(bind $?derived-slots (subseq$ $?rest-concl 3 (- (length$ $?rest-concl) 1)))
	(bind $?var-class-pairs (collect-var-class-pairs $?new-condition))
	(bind $?positive-conditions (collect-positive-conditions $?new-condition))
	(bind $?singleton-vars (difference$ (create$ (collect-singletons$ (create$ (collect-vars $?positive-conditions) (collect-vars $?conclusion))) $$$ $?var-class-pairs)))
	(bind $?encaps-slots (create$ $?derived-slots (inverse-brackets (reverse$ $?var-class-pairs))))
	(bind $?encaps-preds (create$ $?init-concl (collect-slot-predicates (create$ $?singleton-vars $$$ $?new-condition))))
	(while (> (length$ $?encaps-slots) 0)
	   do
	   	(bind ?p2 (get-token $?encaps-slots))
		(bind $?encaps-preds (collapse-vars (nth$ 3 (subseq$ $?encaps-slots 1 ?p2)) $?encaps-preds))
		(bind $?encaps-slots (subseq$ $?encaps-slots (+ ?p2 1) (length$ $?encaps-slots)))
	)
	(bind $?encaps-preds (replace-member$ $?encaps-preds eq bind))
	;(bind $?copy-var-class-pairs $?var-class-pairs)
	;(while (> (length$ $?copy-var-class-pairs) 0)
	;   do
	;   	(bind ?p2 (get-token $?copy-var-class-pairs))
	;	(bind ?one-class-var (nth$ 2 (subseq$ $?copy-var-class-pairs 1 ?p2)))
		;(bind $?encaps-preds (replace-member$ $?encaps-preds (create$ "(" instance-address ?one-class-var ")") ?one-class-var))
	;	(bind $?copy-var-class-pairs (subseq$ $?copy-var-class-pairs (+ ?p2 1) (length$ $?copy-var-class-pairs)))
	;)
	(bind $?new-and-old-vars (create-new-vars $?var-class-pairs))
	(bind $?new-var-class-pairs (replace-vars (create$ $?new-and-old-vars $$$ $?var-class-pairs)))
	(bind $?new-encaps-preds (replace-vars (create$ $?new-and-old-vars $$$ $?encaps-preds)))
	(bind $?new-var-list (create-new-var-list $?new-and-old-vars))
	(bind $?negative-conditions (collect-negative-conditions $?new-condition))
	; The following lines build an object query out of the negative conditions
	(if (> (length$ $?negative-conditions) 0)
	   then
		(bind $?negative-conditions (object-index-transform $?negative-conditions))
		(bind $?neg-var-class-pairs (create$ $?var-class-pairs (collect-var-class-pairs $?negative-conditions)))
		;(bind $?neg-slot-predicates (collect-slot-predicates $?negative-conditions))
		;(bind $?neg-encaps-preds (replace-member$ (collect-slot-predicates $?negative-conditions) eq bind))
		(bind $?neg-encaps-slots (create$ $?derived-slots (inverse-brackets (reverse$ $?neg-var-class-pairs))))
		(bind $?neg-encaps-preds (create$ $?init-concl (collect-slot-predicates (create$ $?positive-conditions $?negative-conditions))))
		(while (> (length$ $?neg-encaps-slots) 0)
		   do
		   	(bind ?p2 (get-token $?neg-encaps-slots))
			(bind $?neg-encaps-preds (collapse-vars (nth$ 3 (subseq$ $?neg-encaps-slots 1 ?p2)) $?neg-encaps-preds))
			(bind $?neg-encaps-slots (subseq$ $?neg-encaps-slots (+ ?p2 1) (length$ $?neg-encaps-slots)))
		)
		(bind $?neg-encaps-preds (replace-member$ $?neg-encaps-preds eq bind))
		;(bind $?copy-var-class-pairs $?neg-var-class-pairs)
		;(while (> (length$ $?copy-var-class-pairs) 0)
		;   do
		;   	(bind ?p2 (get-token $?copy-var-class-pairs))
		;	(bind ?one-class-var (nth$ 2 (subseq$ $?copy-var-class-pairs 1 ?p2)))
			;(bind $?neg-encaps-preds (replace-member$ $?neg-encaps-preds (create$ "(" instance-address ?one-class-var ")") ?one-class-var))
		;	(bind $?copy-var-class-pairs (subseq$ $?copy-var-class-pairs (+ ?p2 1) (length$ $?copy-var-class-pairs)))
		;)
		(bind $?neg-new-and-old-vars (create-new-vars $?neg-var-class-pairs))
		(bind $?neg-new-var-class-pairs (replace-vars (create$ $?neg-new-and-old-vars $$$ $?neg-var-class-pairs)))
		(bind $?neg-new-encaps-preds (replace-vars (create$ $?neg-new-and-old-vars $$$ $?neg-encaps-preds)))
		;(bind ?no-of-pos-class-var (token-length $?var-class-pairs))
		(bind $?neg-new-var-list (subseq$ (create-new-var-list $?neg-new-and-old-vars) 1 (token-length $?var-class-pairs)))
		;(printout t "neg-new-var-list: " $?neg-new-var-list crlf)
	)
	(create$
		"(" test
			"(" any-instancep 
				"(" $?new-var-class-pairs ")" 
				"(" and 
;					"(" eq "(" create$ $?new-var-list ")" "(" explode$ "?derivator" ")" ")" 
					"(" eq "(" create$ $?new-var-list ")" "(" create$ $?object-address-vars ")" ")" 
					"(" not 
						(if (= (token-length $?new-encaps-preds) 1)
						   then
						   	(create$ $?new-encaps-preds)
						   else
						   	(create$ "(" and $?new-encaps-preds ")")
						)
					")"
				")"
			")"
		")"
		(if (> (length$ $?negative-conditions) 0)
		   then
			(create$
				"(" test 
					"(" any-instancep 
						"(" $?neg-new-var-class-pairs ")" 
						"(" and 
;							"(" eq "(" create$ $?neg-new-var-list ")" "(" explode$ "?derivator" ")" ")" 
							"(" eq "(" create$ $?neg-new-var-list ")" "(" create$ $?object-address-vars ")"  ")" 
							$?neg-new-encaps-preds
						")"
					")"
				")"
			)
		   else
		   	(create$)
		)
	)
)
