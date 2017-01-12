;;; ######################################################################################
;;; Rules for generating the objects. O-DEVICE uses two user-defined functions
;;; for creating objects and inserting slot values, namely owl-make-instance and
;;; owl-insert-values, respectively (see functions.clp for more details) 
;;; ###################################################################################### 
;;; create the object based on the rdf:type value
(defrule create-object
	(declare (salience 9900))
	(goal (name delegators-defined))
	?t <- (triple (subject ?o) (predicate ?term&:(eq ?term ?*rdf:type*))(object ?class))
	(CLASS(name ?class))
	=>
	(owl-make-instance ?o ?class)
	(retract ?t)	
)

;;; manage owl:oneOf
(defrule owl:oneOf
	(declare (salience 9900))
	(goal (name delegators-defined))
	?t <- (triple (subject ?c)(predicate ?term&:(eq ?term ?*owl:oneOf*))(object ?oneof))
	(CLASS (name ?c))
	=>
	(bind $?objects (collect-list-elements ?oneof))
	(progn$ (?obj $?objects)
		(owl-make-instance ?obj ?c))
	(retract ?t)
)

;;; owl:allDifferent construct
(defrule owl:allDifferent
	(declare (salience 9900))
	(goal (name delegators-defined))
	?t1 <- (triple 	(subject ?c)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
									(object ?term2&:(eq ?term2 ?*owl:AllDifferent*)))
	?t2 <- (triple 	(subject ?c)(predicate ?term3&:(eq ?term3 ?*owl:distinctMembers*))(object ?list))				
	=>
	(bind $?objects (collect-list-elements ?list))	
	(progn$ (?o1 $?objects)
		(progn$ (?o2 $?objects)
			(owl-insert-value ?o1 ?*owl:differentFrom* ?o2)))
	(retract ?t1)
	(retract ?t2)
)

;;; owl:hasValue construct
(defrule owl:hasValue-object
	(declare (salience 9899))
	(goal (name delegators-defined))
	(RESTRICTION (onProperty ?p)(restriction hasValue)(value ?o))
	(PROPERTY (name ?p&: (is-object-property ?p)))
	(test (not (instance-existp ?o)))
	=>
		(owl-make-instance ?o ?*owl:Thing*)
)

;;; insert values into the objects
(defrule insert-object-value
	(declare (salience 9899))
	(goal (name delegators-defined))
	?t <- (triple (subject ?o)(predicate ?p)(object ?v))
	(PROPERTY (name ?p))
	=>
	(owl-insert-value ?o ?p ?v) 
	(retract ?t)
)

