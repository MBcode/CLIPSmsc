;;; ######################################################################################
;;; These are the rules that dynamically generate inference rules based on the
;;; semantics of the OWL language. The rules that are generated are stored in the
;;; file ?*rule-file* after executing the function save-o-device()
;;; ######################################################################################

;;; if an object belongs to a class that has a has-value restriction, then the 
;;; object should have the corresponding value in the restricted slot
(defrule owl:hasValue-necessary
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c)(subclass $? ?r $?))
	(test (is-hasValue-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(if (member$ ?p (class-slots ?c inherit)) then 
		(bind ?value (get-restriction-value ?r))
		(if (and (is-object-property ?p) (not (instancep ?value))) then
			(bind ?value (symbol-to-instance-name ?value)))
		(bind ?r-name (str-cat "$nes_hasValue$-" ?c "-" ?p "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " ?o <- (object (is-a " ?c ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $?values &: (not (member$ " (instance-string ?value) " $?values)))) => "))
		(bind ?rule (str-cat ?rule " (owl-insert-value ?o " ?p " " (instance-string ?value) "))"))
		(assert (rule (name ?r-name)(type hasValue)(code ?rule))))
)

;;; all the objects that satisfy the has-value restriction should belong to 
;;; the class where the restriction is defined
(defrule owl:hasValue-sufficient
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c)(equivalent $? ?r $?))
	(test (is-hasValue-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(if (member$ ?p (class-slots ?c inherit))	then 
		(bind ?value (get-restriction-value ?r))
		(if (and (is-object-property ?p) (not (instancep ?value))) then
			(bind ?value (symbol-to-instance-name ?value)))
		(bind ?r-name (str-cat "$suf_hasValue$-" ?c "-" ?p "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " ?obj <- (object (is-a " ?*owl:Thing* ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $?values &: (member$ " (instance-string ?value) " $?values)))"))
		(bind ?rule (str-cat ?rule " (test (not (instance-has-class-type ?obj " ?c "))) =>"))
		(bind ?rule (str-cat ?rule " (owl-make-instance ?obj " ?c "))"))
		(assert (rule (name ?r-name)(type hasValue)(code ?rule))))
)

;;; if an object belongs to a class that is restricted with an all-values-from 
;;; restriction, then the object should contain in the restricted slot
;;; values only from the restriction filler (or no values)
(defrule owl:allValuesFrom-necessary
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c)(subclass $? ?r $?))
	(test (is-allValuesFrom-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(if (member$ ?p (class-slots ?c inherit))	then 
		(bind ?value (get-restriction-value ?r))
		(bind ?r-name (str-cat "$nes_allValuesFrom$-" ?c "-" ?p "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " (object (is-a " ?c ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $? ?value $?))"))
		(bind ?rule (str-cat ?rule " (object (name ?value &:(not (instance-has-class-type ?value " ?value ")))) => "))
		(bind ?rule (str-cat ?rule " (owl-make-instance ?value " ?value "))"))
		(assert (rule (name ?r-name)(type allValuesFrom)(code ?rule))))
)

;;; all the objects that satisfy the all-values-from restriction should belong to 
;;; the class where the restriction is defined
(defrule owl:allValuesFrom-sufficient
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c)(equivalent $? ?r $?))
	(test (is-allValuesFrom-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(if (member$ ?p (class-slots ?c inherit)) then 
		(bind ?value (get-restriction-value ?r))
		(bind ?r-name (str-cat "$suf_allValuesFrom$-" ?c "-" ?p "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " ?obj <- (object (is-a " ?*owl:Thing* ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $?values &:(all-values-are-of-type $?values " ?value ")))"))
		(bind ?rule (str-cat ?rule " (test (not (instance-has-class-type ?obj " ?c "))) => "))
		(bind ?rule (str-cat ?rule " (owl-make-instance ?obj " ?c "))"))
		(assert (rule (name ?r-name)(type allValuesFrom)(code ?rule))))
)

;;; all the objects that satisfy the some-values-form restriction should belong to 
;;; the class where the restriction is defined
(defrule owl:someValuesFrom-sufficient
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c) (equivalent $? ?r $?))
	(test (is-someValuesFrom-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(if (member$ ?p (class-slots ?c inherit))	then 
		(bind ?value (get-restriction-value ?r))
		(bind ?r-name (str-cat "$suf_someValuesFrom$-" ?c "-" ?p "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " ?obj <- (object (is-a " ?*owl:Thing* ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $?values &:(some-values-are-of-type $?values " ?value ")))"))
		(bind ?rule (str-cat ?rule " (test (not (instance-has-class-type ?obj " ?c "))) => "))
		(bind ?rule (str-cat ?rule " (owl-make-instance ?obj " ?c "))"))
		(assert (rule (name ?r-name)(type someValuesFrom)(code ?rule))))
)

;;; checks if an object that belongs to a class with a cardinality restriction
;;; violates the cardinality value
(defrule cardinality-dynamic
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c)(subclass $? ?r $?))
	(test (is-cardinality-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(if (member$ ?p (class-slots ?c inherit))	then 
		(bind ?value (get-restriction-value ?r))
		(bind ?r-name (str-cat "$cardinality$-" ?c "-" ?p "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " ?obj <- (object (is-a " ?c ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $?values &:(> (length$ $?values) " ?value "))) => "))
		(bind ?rule (str-cat ?rule " (debug warn \"The object \" (instance-name ?obj) \" has more than \"" ?value "\" values in \"" ?p "))"))
		(assert (rule (name ?r-name)(type cardinality)(code ?rule))))
)

;;; checks if an object that belongs to a class with a qualified cardinality restriction
;;; violates the cardinality value
(defrule qualifiedCardinality-dynamic
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c)(subclass $? ?r $?))
	(test (is-qualifiedCardinality-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(bind ?onClass (get-restriction-onClass ?r))
	(bind ?value (get-restriction-value ?r))
	(if (member$ ?p (class-slots ?c inherit))	then 
		(bind ?r-name (str-cat "$qualifiedCardinality$-" ?c "-" ?p "-" ?onClass "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " ?obj <- (object (is-a " ?c ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $?values &:(more-qualified-values $?values " ?onClass " " ?value "))) => "))
		(bind ?rule (str-cat ?rule " (debug warn \"The object \" (instance-name ?obj) \" has more than \"" ?value "\" values of type \"" ?onClass "\" in \"" ?p "))"))
		(assert (rule (name ?r-name)(type qualifiedCardinality)(code ?rule))))
)

;;; checks if an object that belongs to a class with a max cardinality restriction
;;; violates the max cardinality value 
(defrule maxCardinality-dynamic
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c)(subclass $? ?r $?))
	(test (is-maxCardinality-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(if (member$ ?p (class-slots ?c inherit))	then 
		(bind ?value (get-restriction-value ?r))
		(bind ?r-name (str-cat "$maxCardinality$-" ?c "-" ?p "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " ?obj <- (object (is-a " ?c ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $?values &:(> (length$ $?values) " ?value "))) => "))
		(bind ?rule (str-cat ?rule " (debug warn \"The object \" (instance-name ?obj) \" has more than \"" ?value "\" values in \"" ?p "))"))
		(assert (rule (name ?r-name)(type maxCardinality)(code ?rule))))
)

;;; checks if an object that belongs to a class with a max qualified cardinality restriction
;;; violates the cardinality value
(defrule maxQualifiedCardinality-dynamic
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c)(subclass $? ?r $?))
	(test (is-maxQualifiedCardinality-restriction ?r))
	=>
	(bind ?p (get-restriction-property ?r))
	(bind ?onClass (get-restriction-onClass ?r))
	(bind ?value (get-restriction-value ?r))
	(if (member$ ?p (class-slots ?c inherit))	then 
		(bind ?r-name (str-cat "$maxQualifiedCardinality$-" ?c "-" ?p "-" ?onClass "-" ?value))
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(bind ?rule (str-cat ?rule " ?obj <- (object (is-a " ?c ")"))
		(bind ?rule (str-cat ?rule " (" ?p " $?values &:(more-qualified-values $?values " ?onClass " " ?value "))) => "))
		(bind ?rule (str-cat ?rule " (debug warn \"The object \" (instance-name ?obj) \" has more than \"" ?value "\" values of type \"" ?onClass "\" in \"" ?p "))"))
		(assert (rule (name ?r-name)(type maxQualifiedCardinality)(code ?rule))))
)

;;; if an object satisfies all the classes of an intersection, then the object
;;; should belong to the intersection class
(defrule intersection-sufficient
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c) (intersection $?intersection &: (neq (length$ $?intersection) 0)))
	=>
	(bind $?restrictions (get-restriction-classes $?intersection))
	(bind $?named-classes (remove-restriction-classes $?intersection))
	(bind $?slots (create$))	
	(bind ?len-restrictions (length$ $?restrictions))
	(loop-for-count (?n 1 ?len-restrictions) do
		(bind $?slots (remove-duplicates$ (create$ $?slots (get-restriction-property (nth$ ?n $?restrictions))))))
	(bind ?len-slots (length$ $?slots))
	(bind ?r-name (str-cat "$intersection$-" ?c))
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))			
	(bind ?len-named-classes (length$ $?named-classes))
	(loop-for-count (?n 1 ?len-named-classes)	do
		(bind ?cl (nth$ ?n $?named-classes))
		(bind ?rule (str-cat ?rule " (object (name ?obj)(is-a " ?cl ")"))
		(loop-for-count (?m 1 ?len-slots) do
			(bind ?slot (nth$ ?m $?slots))
			(if (slot-existp ?c ?slot inherit) then
					(bind ?rule (str-cat ?rule " (" ?slot " $?" (gensym*)")"))))
		(bind ?rule (str-cat ?rule ")")))
	(if (eq ?len-named-classes 0) then
		(bind ?rule (str-cat ?rule " (object (name ?obj)(is-a " ?*owl:Thing* ")"))
		(loop-for-count (?m 1 ?len-slots) do
			(bind ?slot (nth$ ?m $?slots))
			(bind ?rule (str-cat ?rule " (" ?slot " $?" (gensym*)")")))
		(bind ?rule (str-cat ?rule ")")))
	(loop-for-count (?n 1 ?len-restrictions) do
		(bind ?res (nth$ ?n $?restrictions))
		(bind ?rule (str-cat ?rule "(test (satisfy-restriction ?obj " ?res "))")))		
	(bind ?rule (str-cat ?rule " (test (not (instance-has-class-type ?obj " ?c "))) => "))
	(bind ?rule (str-cat ?rule " (owl-make-instance ?obj " ?c "))"))
	(assert (rule (name ?r-name)(type intersection)(code ?rule)))
)

;;; all the objects of the non-delegator classes should belong to the 
;;; delegators
(defrule equivalent-classification
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c) (delegator TRUE) (equivalent $? ?equiv &: (not (is-restriction-class ?equiv)) $?))
	=>
	(bind ?r-name (str-cat "$equivalent$-" ?c "-" (gensym*)))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "?obj <- (object (is-a " ?equiv "))"))
	(bind ?rule (str-cat ?rule " (test (not (instance-has-class-type ?obj " ?c "))) => "))
	(bind ?rule (str-cat ?rule " (owl-make-instance ?obj " ?c "))"))
	(assert (rule (name ?r-name)(type equivalent)(code ?rule)))
)

;;; check the consistency of the objects in terms of disjoint axioms.
;;; An object cannot belong simultaneously to disjoint classes.
(defrule disjoint-dynamic
	(declare (salience 9880))
	(goal (name delegators-defined))
	(CLASS (name ?c) (disjoint $? ?disjoint $?))
	=>
	(bind ?r-name (str-cat "$disjoint$-" ?c "-" (gensym*)))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule " (object (name ?o1)(is-a " ?c "))"))
	(bind ?rule (str-cat ?rule " (object (name ?o1)(is-a " ?disjoint ")) => "))
	(bind ?rule (str-cat ?rule " (debug error \"The object \" ?o1 \" belongs to disjoint classes.\"))"))
	(assert (rule (name ?r-name)(type disjoint)(code ?rule)))
)

;property chains
(defrule proeprtyChain-dynamic
	(declare (salience 9880))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p &: (is-object-property ?p)) (propertyChain $? ?chain $?))
	=>
		(bind $?values (collect-list-elements ?chain))
		(bind ?r-name (str-cat "$propertyChain$-" ?p "-" (gensym*)))	
		(bind ?rule (str-cat " (defrule " ?r-name))
		(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
		(progn$ (?value $?values)
			(bind ?domain (nth$ 1 (get-property-domain ?value)))
			(bind ?rule (str-cat ?rule " (object (is-a " ?domain ")"))
			(bind ?rule (str-cat ?rule " (name ?obj" ?value-index ")"))
			(bind ?rule (str-cat ?rule " (" ?value " $? ?obj" (+ ?value-index 1) " $?))")))
		(bind ?rule (str-cat ?rule " (test (not (member$ ?obj" (+ (length$ $?values) 1) " (send ?obj1 get-" ?p ")))) => ")) 
		(bind ?rule (str-cat ?rule " (owl-insert-value ?obj1 " ?p " ?obj" (+ (length$ $?values) 1)"))"))
		(assert (rule (name ?r-name)(type propertyChain)(code ?rule)))
)


;;; The slot values should comply with the range restrictions (object properties)
(defrule range-dynamic
	(declare (salience 9880))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p &: (is-object-property ?p)) (range $? ?d $?))
	=>
	(bind ?r-name (str-cat "$range$-" ?p))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "(object (is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?v $?))"))
	(bind ?rule (str-cat ?rule "(object (name ?v &: (not (instance-has-class-type ?v " ?d ")))) => " ))
	(bind ?rule (str-cat ?rule " (owl-make-instance ?v " ?d "))"))
	(assert (rule (name ?r-name)(type range)(code ?rule)))
)
	
;;; handle the values of symmetric properties
(defrule symmetric-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p) (type $? symmetric $?))
	=>
	(bind ?r-name (str-cat "$symmetric$-" ?p))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "(object (name ?obj)(is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?v $?))"))
	(bind ?rule (str-cat ?rule "(object (name ?v)"))
	(bind ?rule (str-cat ?rule "(" ?p " $?values2 &: (not (member$ ?obj $?values2)))) =>"))
	(bind ?rule (str-cat ?rule " (owl-insert-value ?v " ?p " ?obj))"))
	(assert (rule (name ?r-name)(type symmetric)(code ?rule)))
)

;;; handle the values of transitive properties
(defrule transitive-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p) (type $? transitive $?))
	=>
	(bind ?r-name (str-cat "$transitive$-" ?p))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "(object (name ?o1)(is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?o2 $?))"))
	(bind ?rule (str-cat ?rule "(object (name ?o2 &~ ?o1)(is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?o3 &~ ?o1 $?))"))
	(bind ?rule (str-cat ?rule " (test (not (member$ ?o3 (send ?o1 get-" ?p ")))) => "))
	(bind ?rule (str-cat ?rule " (owl-insert-value ?o1 " ?p " ?o3))"))
	(assert (rule (name ?r-name)(type transitive)(code ?rule)))
)

;;; infer same objects for functional object properties
(defrule functional-object-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p) (type $?types &: (subsetp (create$ functional object) $?types)))
	=>
	(bind ?r-name (str-cat "$functional-object$-" ?p))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9001))"))
	(bind ?rule (str-cat ?rule "(object (is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?v1 $? ?v2 $?))"))
	(bind ?rule (str-cat ?rule "(object (name ?v1)(" ?*owl:sameAs* " $?sames &: (not (member$ ?v2 $?sames)))) =>"))
	(bind ?rule (str-cat ?rule " (owl-insert-value ?v1 " ?*owl:sameAs* " ?v2))"))
	(assert (rule (name ?r-name)(type functional)(code ?rule)))
)

;;; check consistency for functional datatype properties
(defrule functional-datatype-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p) (type $?types &: (subsetp (create$ functional datatype) $?types)))
	=>
	(bind ?r-name (str-cat "$functional-datatype$-" ?p))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "?obj <- (object (is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $?values))"))
	(bind ?rule (str-cat ?rule " (test (> (length$ $?values) 1 )) => "))
	(bind ?rule (str-cat ?rule " (debug warn \"The object \" (instance-name ?obj) \" has more than " one "\" values in \"" ?p "))"))
	(assert (rule (name ?r-name)(type functional)(code ?rule)))
)

;;; infer same objects
(defrule inverse-functional-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p)(type $? inversefunctional $?))
	=>
	(bind ?r-name (str-cat "$inversefunctional$-" ?p))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9001))"))
	(bind ?rule (str-cat ?rule "(object (name ?o1)(is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?v1 $?)"))
	(bind ?rule (str-cat ?rule "(" ?*owl:sameAs* " $?sames))"))
	(bind ?rule (str-cat ?rule "(object (name ?o2 &~ ?o1)(is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?v1 $?))"))
	(bind ?rule (str-cat ?rule " (test (not (member$ ?o2 $?sames))) => "))
	(bind ?rule (str-cat ?rule " (owl-insert-value ?o1 " ?*owl:sameAs* " ?o2))"))
	(assert (rule (name ?r-name)(type inversefunctional)(code ?rule)))
)

;;; infer owl:sameAs objects based on the owl:hasKey property
(defrule hasKey-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(CLASS (name ?c) (hasKey $?keys &: (> (length$ $?keys) 0)))
	=>
	(bind ?r-name (str-cat "$hasKey$-" ?c))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule " (object (name ?o1)(is-a " ?c ")"))
	(progn$ (?k $?keys)
		(bind ?rule (str-cat ?rule "(" ?k " $? ?key" ?k-index " $?)")))
	(bind ?rule (str-cat ?rule ")"))
	(bind ?rule (str-cat ?rule " (object (name ?o2&~?o1)(is-a " ?c ")"))
	(progn$ (?k $?keys)
		(bind ?rule (str-cat ?rule "(" ?k " $? ?key" ?k-index " $?)")))
	(bind ?rule (str-cat ?rule "(" ?*owl:sameAs* " $?sames &: (not (member$ ?o1 $?sames))))=>"))
	(bind ?rule (str-cat ?rule " (owl-insert-value ?o2 " ?*owl:sameAs* " ?o1))"))
	(assert (rule (name ?r-name)(type sameAs)(code ?rule)))
)

;;; handle the values of equivalent properties
(defrule equivalentProperty-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p)(equivalentProperty $? ?equiv $?))
	=>
	(bind ?r-name (str-cat "$equivalentProperty$-" ?p "-" ?equiv))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "?o <- (object (is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?v $?)"))
	(bind ?rule (str-cat ?rule "(" ?equiv " $?values))"))
	(bind ?rule (str-cat ?rule " (test (not (member$ ?v $?values))) => "))
	(bind ?rule (str-cat ?rule " (owl-insert-value ?o " ?equiv " ?v))"))
	(assert (rule (name ?r-name)(type equivalentProperty)(code ?rule)))
)

;;; handle the values of inverse properties
(defrule inverse-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p &: (is-object-property ?p))(inverse $? ?inverse $?))
	=>
	(bind ?r-name (str-cat "$inverse$-" ?p "-" ?inverse))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "(object (name ?o)(is-a " (nth$ 1 (get-property-domain ?p)) ")"))
	(bind ?rule (str-cat ?rule "(" ?p " $? ?v $?))"))
	(bind ?rule (str-cat ?rule "(object (name ?v)"))
	(bind ?rule (str-cat ?rule "(" ?inverse " $?values2))"))
	(bind ?rule (str-cat ?rule " (test (not (member$ ?o $?values2))) => "))
	(bind ?rule (str-cat ?rule " (owl-insert-value ?v " ?inverse " ?o))"))
	(assert (rule (name ?r-name)(type inverse)(code ?rule)))
)

;;;
(defrule differentFrom-static
	(declare (salience 9879))
	(goal (name delegators-defined))
	=>
	(bind ?r-name (str-cat "$differentFrom$"))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9001))"))
	(bind ?rule (str-cat ?rule "?o1 <- (object "))
	(bind ?rule (str-cat ?rule "(" ?*owl:sameAs* " $? ?v $?)"))
	(bind ?rule (str-cat ?rule "(" ?*owl:differentFrom* " $? ?v $?)) => "))
	(bind ?rule (str-cat ?rule " (debug error \"The object \" (instance-name ?o1) \" is both same and different from the object \" ?v ))"))
	(assert (rule (name ?r-name)(type differentFrom)(code ?rule)))
)

;;;
(defrule complement-restriction-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(CLASS	(name ?c)(complement $? ?complement &: (is-restriction-class ?complement) $?))
	=>
	(bind ?r-name (str-cat "$complement-res$-" ?c "-" ?complement))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "?o <- (object (is-a " ?c "))"))
	(bind ?rule (str-cat ?rule "(test (satisfy-restriction ?o " ?complement ")) =>"))
	(bind ?rule (str-cat ?rule " (debug error \"The object \" (instance-name ?o) \" belongs to complement classes.\"))"))
	(assert (rule (name ?r-name)(type complement)(code ?rule)))
)

;;; 
(defrule complement-named-dynamic
	(declare (salience 9879))
	(goal (name delegators-defined))
	(CLASS	(name ?c)(complement $? ?complement &: (not (is-restriction-class ?complement)) $?))
	=>
	(bind ?r-name (str-cat "$complement-class$-" ?c "-" ?complement))	
	(bind ?rule (str-cat " (defrule " ?r-name))
	(bind ?rule (str-cat ?rule " (declare (salience 9000))"))
	(bind ?rule (str-cat ?rule "(object (name ?o)(is-a " ?c "))"))
	(bind ?rule (str-cat ?rule "(object (name ?o)(is-a " ?complement ")) =>"))
	(bind ?rule (str-cat ?rule " (debug error \"The object \" ?o \" belongs to complement classes.\"))"))
	(assert (rule (name ?r-name)(type complement)(code ?rule)))
)

;;; Exchange the slot values between same objects
(defrule owl:sameAs-exchange-values
	(declare (salience 9879))
	(goal (name delegators-defined))
	=>
	(bind ?rule (str-cat " (defrule " "$sameAs-exchange-values"))
	(bind ?rule (str-cat ?rule " (declare (salience 9001))"))
	(bind ?rule (str-cat ?rule "(object (name ?obj1)(" ?*owl:sameAs* " $? ?obj2 $?))"))
	(bind ?rule (str-cat ?rule "(object (name ?obj2))"))
	(bind ?rule (str-cat ?rule "(test (not (is-shallow-copied ?obj1 ?obj2))) => "))
	(bind ?rule (str-cat ?rule " (exchange-values ?obj1 ?obj2))"))
	(assert (rule (name "$sameAs-exchange-values")(type sameAs-exchange)(code ?rule)))
)
