;;; ######################################################################################
;;; This is the main rule file of O-DEVICE. These rules are used in order to create
;;; the COOL model from the loaded ontologies. 
;;; ######################################################################################

;;; ######################################################################################
;;; Removes triples that are of no importance to O-DEVICE
;;; ######################################################################################
(defrule remove-owl:Thing-subclass
	(declare (salience 10000))
	?CL <- (CLASS (subclass $? ?term&:(eq ?term ?*owl:Thing*) $?))
	=>
	(modify ?CL (subclass (delete-member$ (fact-slot-value ?CL subclass) ?*owl:Thing*)))
)

(defrule remove-owl:Thing-domain
	(declare (salience 10000))
	?PR <- (PROPERTY (domain $? ?term&:(eq ?term ?*owl:Thing*) $?))
	=>
	(modify ?PR (domain (delete-member$ (fact-slot-value ?PR domain) ?*owl:Thing*)))
)

(defrule remove-owl:Thing-range
	(declare (salience 10000))
	?PR <- (PROPERTY (range $? ?term&:(eq ?term ?*owl:Thing*) $?))
	=>
	(modify ?PR	(range (delete-member$ (fact-slot-value ?PR range) ?*owl:Thing*)))
)

;;; ######################################################################################
;;; Generating the facts for the templates CLASS and RESTRICTION
;;; ######################################################################################
;;; for each class create the corresponding CLASS fact
(defrule owl:class
	(declare (salience 10000))
	?t <- (triple (subject ?c) (predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:Class*)))
	(not (CLASS (name ?c)))
	=>
	(assert (CLASS (name ?c)))
	(retract ?t)
)

;;; hold the asserted subclass relationships
(defrule owl:class-subclass
	(declare (salience 10000))
	?t <- (triple (subject ?c1) (predicate ?term&:(eq ?term ?*rdfs:subClassOf*))(object ?c2&~?c1))
	?CL <- (CLASS	(name ?c1) (subclass $?subclasses))
	=>
	(modify ?CL (subclass (create$ $?subclasses ?c2)))
	(retract ?t)
)

;;; intersection
(defrule owl:class-intersection
	(declare (salience 10000))
	?t <- (triple (subject ?c1) (predicate ?term&:(eq ?term ?*owl:intersectionOf*))(object ?i))
	?CL <- (CLASS (name ?c1) (intersection $?intersection))
	=>
	(bind $?classes (collect-list-elements ?i))
	(modify ?CL (intersection (create$ $?intersection $?classes)))
	(retract ?t)
)

;;; complement
(defrule owl:class-complement
	(declare (salience 10000))
	?t <- (triple (subject ?c1) (predicate ?term&:(eq ?term ?*owl:complementOf*))(object ?c2))
	?CL <- (CLASS (name ?c1)(complement $?complement))
	=>
	(modify ?CL (complement (create$ $?complement ?c2)))
	(retract ?t)
)

;;; class equivalence
(defrule owl:class-equivalent
	(declare (salience 10000))
	?t <- (triple (subject ?c1)(predicate ?term&:(eq ?term ?*owl:equivalentClass*))(object ?c2&~?c1))
	?CL <- (CLASS (name ?c1) (equivalent $?equivalent))
	=>
	(modify ?CL (equivalent (create$ $?equivalent ?c2)))
	(retract ?t)
)

;;; class disjointness
(defrule owl:class-disjoint
	(declare (salience 10000))
	?t <- (triple (subject ?c1)(predicate ?term&:(eq ?term ?*owl:disjointWith*))(object ?c2))
	?CL <- (CLASS (name ?c1)(disjoint $?disjoint))
	=>
	(modify ?CL (disjoint (create$ $?disjoint ?c2)))
	(retract ?t)
)

;;; union of classes
(defrule owl:class-union
	(declare (salience 10000))
	?t <- (triple (subject ?c1)(predicate ?term&:(eq ?term ?*owl:unionOf*))(object ?u))
	?CL <- (CLASS (name ?c1)(union $?union))
	=>
	(bind $?classes (collect-list-elements ?u))
	(modify ?CL	(union (create$ $?union $?classes)))
	(retract ?t)
)

;;; keys
(defrule owl:class-keys
	(declare (salience 10000))
	?t <- (triple (subject ?c1)(predicate ?term&:(eq ?term ?*owl:hasKey*))(object ?k))
	?CL <- (CLASS (name ?c1)(hasKey $?keys))
	=>
	(bind $?key-properties (collect-list-elements ?k))
	(modify ?CL	(hasKey (remove-duplicates$ (create$ $?keys $?key-properties))))
	(retract ?t)
)

;;; all disjoint classes
(defrule owl:AllDisjointClasses
	(declare (salience 10000))
	?t <- (triple (subject ?c1)(predicate ?term&:(eq ?term ?*rdf:type*))
					(object ?term2&:(eq ?term2 ?*owl:AllDisjointClasses*)))
	(not (ALL_DISJOINT_CLASSES (name ?c1)))
	=>
	(assert (ALL_DISJOINT_CLASSES (name ?c1)))
	(retract ?t)
)

;;; collect the members
(defrule owl:AllDisjointClasses-members
	(declare (salience 10000))
	?t <- (triple (subject ?s)(predicate ?term&:(eq ?term ?*owl:members*))(object ?list))
	?F <- (ALL_DISJOINT_CLASSES (name ?f)(members $?members))
	=>
	(bind $?values (collect-list-elements ?list))
	(modify ?F	(members $?values))
	(retract ?t)
)

;;; create a RESTRICTION fact for each restriction
(defrule owl:restriction
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:Restriction*)))
	(not (RESTRICTION (name ?r)))
	=>
	(assert (RESTRICTION (name ?r)))
	(retract ?t)
)

;;; store the property of the restriction
(defrule owl:restriction-onProperty
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*owl:onProperty*))(object ?p))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (onProperty ?p))
	(retract ?t)
)

;;; store the class of the restriction
(defrule owl:restriction-onClass
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*owl:onClass*))(object ?c))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (onClass ?c))
	(retract ?t)
)

;;; is a cardinality restriction
(defrule owl:restriction-cardinality
	(declare (salience 10000))
	?t <- (triple (subject ?r) (predicate ?term&:(eq ?term ?*owl:cardinality*))(object ?card))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL	(restriction cardinality)(value ?card))
	(retract ?t)
)

;;; is a min cardinality restriction
(defrule owl:restriction-minCardinality
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*owl:minCardinality*))(object ?card))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (restriction minCardinality)(value ?card))
	(retract ?t)
)

;;; is a max cardinality restriction
(defrule owl:restriction-maxCardinality
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*owl:maxCardinality*))(object ?card))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (restriction maxCardinality) (value ?card))
	(retract ?t)
)

;;; is a some values from restriction
(defrule owl:restriction-someValuesFrom
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*owl:someValuesFrom*))(object ?c))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL(restriction someValuesFrom)(value ?c))
	(retract ?t)
)

;;; is an all values from restriction
(defrule owl:restriction-allValuesFrom
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*owl:allValuesFrom*))(object ?c))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (restriction allValuesFrom)(value ?c))
	(retract ?t)
)

;;; is a has value restriction
(defrule owl:restriction-hasValue
	(declare (salience 10000))
	?t <- (triple (subject ?r) (predicate ?term&:(eq ?term ?*owl:hasValue*))(object ?obj))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (restriction hasValue)(value ?obj))
	(retract ?t)
)

;;; is a min qualified restriction
(defrule owl:restriction-minQualifiedCardinality
	(declare (salience 10000))
	?t <- (triple (subject ?r) (predicate ?term&:(eq ?term ?*owl:minQualifiedCardinality*))(object ?obj))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (restriction minQualifiedCardinality)(value ?obj))
	(retract ?t)
)

;;; is a max qualified restriction
(defrule owl:restriction-maxQualifiedCardinality
	(declare (salience 10000))
	?t <- (triple (subject ?r) (predicate ?term&:(eq ?term ?*owl:maxQualifiedCardinality*))(object ?obj))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (restriction maxQualifiedCardinality)(value ?obj))
	(retract ?t)
)

;;; is a qualified restriction
(defrule owl:restriction-qualifiedCardinality
	(declare (salience 10000))
	?t <- (triple (subject ?r) (predicate ?term&:(eq ?term ?*owl:qualifiedCardinality*))(object ?obj))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (restriction qualifiedCardinality)(value ?obj))
	(retract ?t)
)

;;; hold subclass restriction axioms
(defrule owl:restriction-subclass
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*rdfs:subClassOf*))(object ?c2&~?r))
	?CL <- (RESTRICTION (name ?r)(subclass $?subclasses))
	=>
	(modify ?CL (subclass (create$ $?subclasses ?c2))) 
	(retract ?t)
)

;;; restriction equivalent axioms
(defrule owl:restriction-equivalent
	(declare (salience 10000))
	?t <- (triple (subject ?r) (predicate ?term&:(eq ?term ?*owl:equivalentClass*))(object ?c2))
	?CL <- (RESTRICTION (name ?r)(equivalent $?equivalent))
	=>
	(modify ?CL (equivalent (create$ $?equivalent ?c2)))
	(retract ?t)
)

;;; hold the label of the class
(defrule label-class
	(declare (salience 10000))
	?t <- (triple (subject ?c) (predicate ?term&:(eq ?term ?*rdfs:label*)) (object ?label))
	?CL <- (CLASS (name ?c))
	=>
	(modify ?CL	(label ?label))
	(retract ?t)
)

;;; hold the label of the restriction
(defrule label-restriction
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*rdfs:label*))(object ?label))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (label ?label))
	(retract ?t)
)

;;; hold the comment of the class
(defrule comment-class
	(declare (salience 10000))
	?t <- (triple (subject ?c) (predicate ?term&:(eq ?term ?*rdfs:comment*))(object ?comment))
	?CL <- (CLASS (name ?c))
	=>
	(modify ?CL	(comment ?comment))
	(retract ?t)
)

;;; hold the comment for the restriction
(defrule comment-restriction
	(declare (salience 10000))
	?t <- (triple (subject ?r)(predicate ?term&:(eq ?term ?*rdfs:comment*))(object ?comment))
	?CL <- (RESTRICTION (name ?r))
	=>
	(modify ?CL (comment ?comment))
	(retract ?t)
)

;;; ######################################################################################
;;; Rules for implementing class equivalence symmetry and transitivity. Note that 
;;; the subclass transitivity is handled by the COOL model. Therefore we need only to
;;; define the direct superclasses of the classes
;;; ######################################################################################
(defrule equivalent-class-transitive
	(declare (salience 9990))
	?CL1 <- (CLASS (name ?c1)(equivalent $? ?equiv1 $?))
	?CL2 <- (CLASS(name ?equiv1&~?c1)(equivalent $? ?equiv2&~?c1 $?))
	(test (not (member$ ?equiv2 (fact-slot-value ?CL1 equivalent))))
	=>
	(modify ?CL1 (equivalent (remove-duplicates$ (create$ (fact-slot-value ?CL1 equivalent) ?equiv2)))) 
)

(defrule equivalent-class-symmetric
	(declare (salience 9990))
	?CL1 <- (CLASS (name ?c1)(equivalent $? ?equiv1 $?))
	?CL2 <- (CLASS(name ?equiv1 &~ ?c1)(equivalent $?equivalent))
	(test (not (member$ ?c1 $?equivalent)))
	=>
	(modify ?CL2 (equivalent (remove-duplicates$ (create$ $?equivalent ?c1))))
)

(defrule class-disjoint-symmetric
	(declare (salience 9990))
	?CL1 <- (CLASS (name ?c1)(disjoint $? ?c2 $?))
	?CL2 <- (CLASS (name ?c2&~?c1)(disjoint $?disjoint))
	(test (not (member$ ?c1 $?disjoint)))
	=>
	(modify ?CL2 (disjoint (remove-duplicates$ (create$ $?disjoint ?c1))))
)

(defrule class-disjoint-all-disjoint-classes
	(declare (salience 9990))
	(ALL_DISJOINT_CLASSES (members $? ?c1 $? ?c2 $?))
	?CL1 <- (CLASS (name ?c1)(disjoint $?disjoint))
	(test (not (member$ ?c2 $?disjoint)))
	=>
	(modify ?CL1 (disjoint (remove-duplicates$ (create$ $?disjoint ?c2))))
)


;;; ######################################################################################
;;; Define the delegator classes. Each set of equivalent classes has only a single
;;; delegator class. The delegator class is used in order to preserve the class
;;; equivalence semantics regarding the instances in the COOL model. The idea is to 
;;; define the delegator class to be the subclass of its equivalent classes and rules
;;; are generated dynamically that push all the objects of an equivalent class set to belong
;;; only to the delegator class. In that way, all the classes of an equivalent class set
;;; have the same instances.
;;; ######################################################################################

;;; if a class has no equivalent classes, then it is a delegator class
(defrule delegator-simple
	(declare (salience 9980))
	?CL <- (CLASS (name ?c) (delegator FALSE)(equivalent $?equivalent &: (= (length$ $?equivalent) 0)))
	=>
	(modify ?CL (delegator TRUE))
)

;;; make a named class of an equivalence class set to be the delegator (arbitrary)
(defrule delegator2
	(declare (salience 9979))
	?CL <- (CLASS (name ?c &: (is-named-class ?c)) (delegator FALSE)
								(equivalent $?equivalent &: (> (length$ $?equivalent) 0)))
	(not (CLASS (name ?c2 &: (member$ ?c2 $?equivalent)) (delegator TRUE)))
	=>
	(modify ?CL (delegator TRUE))
)

;;; if the there is not a named class to be the delegator, then choose 
;;; an anomymous class (not a restriction!)
(defrule delegator3
	(declare (salience 9978))
	?CL <- (CLASS (name ?c &: (is-anonymous-class ?c)) (delegator FALSE)
								(equivalent $?equivalent &: (> (length$ $?equivalent) 0)))
	(not (CLASS (name ?c2 &: (member$ ?c2 $?equivalent)) (delegator TRUE)))
	=>
	(modify ?CL	(delegator TRUE))
)

(defrule check-delegators
	(declare (salience 9977))
	?CL <- (CLASS (name ?c) (delegator FALSE)
								(equivalent $?equivalent &: (> (length$ $?equivalent) 0)))
	(not (CLASS	(name ?c2&~?c &: (member$ ?c2 $?equivalent)) (delegator TRUE)))
	=>
		(debug warn "[check-delegators] No delegator has been defined for class " ?c ".")
)

;;; ######################################################################################
;;; Changes the goal. 
;;; ######################################################################################
(defrule change-goal
	(declare (salience 9976))
	(not (goal (name delegators-defined)))
	=>
	(assert (goal (name delegators-defined)))
)

;;; ######################################################################################
;;; Manage the superclasses of the delegators. Each delegator should be subclass
;;; of its superclasses. Furthermore, the delegator class should have the superclasses,
;;; union and intersection classes of all of its equivalent classes. In other words,
;;; only delegator classes participate in subclass axioms
;;; ######################################################################################
;;; copy subclasses to the delegator class
(defrule change-delegator-superclasses
	(declare (salience 9970))
	(goal (name delegators-defined))
	?CL1 <- (CLASS (name ?c1) (delegator TRUE) (equivalent $? ?equiv $?) (subclass $?superclasses))
	?CL2 <- (CLASS	(name ?equiv &~ ?c1) (delegator FALSE)
									(subclass $? ?c3 &: (not (member$ (get-delegator ?c3) $?superclasses)) $?))
	=>
	(modify ?CL1 (subclass (create$ $?superclasses (get-delegator ?c3))))
	(modify ?CL2 (subclass (create$ (delete-member$ (fact-slot-value ?CL2 subclass) ?c3))))
)

;;; copy each intersection class to the delegator
(defrule change-delegator-intersection
	(declare (salience 9970))
	(goal (name delegators-defined))
	?CL1 <- (CLASS (name ?c1)(delegator TRUE)(equivalent $? ?c2 $?)(intersection $?intersection))
	?CL2 <- (CLASS 	(name ?c2)(delegator FALSE)
									(intersection $? ?c3 &: (not (member$ (get-delegator ?c3) $?intersection)) $?))
	=>
	(modify ?CL1 (intersection (create$ $?intersection (get-delegator ?c3))))
	(modify ?CL2 (intersection (create$ (delete-member$ (fact-slot-value ?CL2 intersection) ?c3))))
)

;;; copy each disjoint class to the delegator
(defrule change-delegator-disjoint
	(declare (salience 9970))
	(goal (name delegators-defined))
	?CL1 <- (CLASS (name ?c1)(equivalent $? ?c2 $?)(disjoint $?disjoint))
	?CL2 <- (CLASS (name ?c2)(disjoint $? ?c3 &: (not (member$ (get-delegator ?c3) $?disjoint)) $?))
	=>
	(modify ?CL1 (disjoint (create$ $?disjoint (get-delegator ?c3))))
)

;;; copy each union class to the delegator
(defrule change-delegator-union
	(declare (salience 9970))
	(goal (name delegators-defined))
	?CL1 <- (CLASS (name ?c1)(delegator TRUE)(equivalent $? ?c2 $?)(union $?union))
	?CL2 <- (CLASS (name ?c2)(union $? ?c3 &: (not (member$ (get-delegator ?c3) $?union)) $?))
	=>
	(modify ?CL1 (union (create$ $?union (get-delegator ?c3))))
	(modify ?CL2 (union (create$ (delete-member$ (fact-slot-value ?CL2 union) ?c3))))
)

;;; copy each key to the delegator
(defrule change-delegator-keys
	(declare (salience 9970))
	(goal (name delegators-defined))
	?CL1 <- (CLASS (name ?c1)(delegator TRUE)(equivalent $? ?c2 $?)(hasKey $?keys))
	?CL2 <- (CLASS (name ?c2)(hasKey $? ?p &: (not (member$ ?p $?keys)) $?))
	=>
	(modify ?CL1 (hasKey (create$ $?keys ?p)))
	(modify ?CL2 (hasKey (create$ (delete-member$ (fact-slot-value ?CL2 hasKey) ?p))))
)

;;; push non-delegator classes to belong to the subclass values of their delegator
(defrule make-delegators-subclasses-of-equivalents
	(declare (salience 9969))
	(goal (name delegators-defined))
	?CL1 <- (CLASS (name ?c1)(delegator TRUE)(subclass $?superclasses)(equivalent $? ?equivalent&~?c1 $?))
	(test (not (member$ ?equivalent $?superclasses)))
	=>
	(modify ?CL1 (subclass (create$ $?superclasses ?equivalent)))
)

;;; transform intersection into subclass axioms
(defrule intersection-to-subclass
	(declare (salience 9969))
	(goal (name delegators-defined))
	?CL <- (CLASS (name ?c)(intersection $? ?intersection $?)(subclass $?superclasses))
	(test (not (member$ (get-delegator ?intersection) $?superclasses)))
	=>
	(modify ?CL (subclass (remove-duplicates$ (create$ $?superclasses (get-delegator ?intersection)))))
)

;;;; transform union into subclass axioms
(defrule union-to-subclass
	(declare (salience 9969))
	(goal (name delegators-defined))
	?CL1 <- (CLASS (name ?c) (union $? ?union $?))
	?CL2 <- (CLASS 	(name ?un &: (eq ?un (get-delegator ?union)))
									(subclass $?superclasses &: (not (member$ ?c $?superclasses))))
	=>
	(modify ?CL2 (subclass (remove-duplicates$ (create$ $?superclasses ?c))))
)

;;; all the superclasses of a delegator should be delegators, apart from its equivalent classes
(defrule delegators-superclasses-should-be-delegators
	(declare (salience 9968))
	(goal (name delegators-defined))
	?CL1 <- (CLASS 	(name ?c1) (delegator TRUE) (subclass $? ?super &: (not (is-restriction-class ?super)) $?)
									(equivalent $?equivalents))
	(test (not (member$ ?super $?equivalents)))
	(test (not (is-delegator ?super)))
	(test (not (member$ (get-delegator ?super) (fact-slot-value ?CL1 subclass))))
	=>
	(modify ?CL1
		(subclass (create$ (replace-member$ (fact-slot-value ?CL1 subclass) (get-delegator ?super) ?super))))
)

;;; the union classes should be delegators
(defrule delegators-unions-should-be-delegators
	(declare (salience 9968))
	(goal (name delegators-defined))
	?CL1 <- (CLASS 	(name ?c1)(delegator TRUE)(union $? ?union &: (not (is-restriction-class ?union)) $?)
									(equivalent $?equivalents))
	(test (not (member$ ?union $?equivalents)))
	(test (not (is-delegator ?union)))
	(test (not (member$ (get-delegator ?union) (fact-slot-value ?CL1 union))))
	=>
	(modify ?CL1
		(union (create$ (replace-member$ (fact-slot-value ?CL1 union) (get-delegator ?union) ?union))))
)

;;; the intersection classes should be delegators
(defrule delegators-intersections-should-be-delegators
	(declare (salience 9968))
	(goal (name delegators-defined))
	?CL1 <- (CLASS 	(name ?c1)(delegator TRUE)(equivalent $?equivalents)
									(intersection $? ?intersection &: (not (is-restriction-class ?intersection)) $?))
	(test (not (member$ ?intersection $?equivalents)))
	(test (not (is-delegator ?intersection)))
	(test (not (member$ (get-delegator ?intersection) (fact-slot-value ?CL1 intersection))))
	=>
	(modify ?CL1
		(intersection (create$ 
			(replace-member$ (fact-slot-value ?CL1 intersection) (get-delegator ?intersection) ?intersection))))
)

;;; ######################################################################################
;;; # A rule for checking the consistency of the classes with respect to the
;;; disjoint class axioms
;;; ######################################################################################
(defrule check-disjoint-classes
	(declare (salience 9968))
	(goal (name delegators-defined))
	(CLASS (name ?c) (subclass $? ?sup $?) (disjoint $? ?sup $?))
	=>
	(debug error "Hierarchically related disjoint classes: " ?c ", " ?sup)
)

;;; check rule
(defrule check-non-delegator-subclass
	(declare (salience 9967))
	(goal (name delegators-defined))
	(CLASS (name ?c) (delegator FALSE) (subclass $? ?sup &: (neq ?sup ?*owl:Thing*) $?))	
	=>
	(debug error "[check-non-delegator-subclass] The non-delegator class " ?c " has superclasses!.")
)

;;; check rule
(defrule check-non-delegator-intersection
	(declare (salience 9967))
	(goal (name delegators-defined))
	(CLASS (name ?c) (delegator FALSE) (intersection $? ?inter $?))	
	=>
	(debug error "[check-non-delegator-subclass] The non-delegator class " ?c " has intersection!.")
)

;;; check rule
(defrule check-non-delegator-union
	(declare (salience 9967))
	(goal (name delegators-defined))
	(CLASS (name ?c) (delegator FALSE) (union $? ?union $?))	
	=>
	(debug error "[check-non-delegator-subclass] The non-delegator class " ?c " has union!.")
)

;;; ######################################################################################
;;; Rules for creating the facts of the PROPERTY template
;;; ######################################################################################
;data ranges
(defrule owl:DataRange
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?dr) (predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:DataRange*)))
	(not (DATARANGE (name ?dr)))
	=>
	(assert (DATARANGE (name ?dr)))
	(retract ?t)
)

;;; collect the values of data ranges
(defrule owl:DataRange-oneOf
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?dr)(predicate ?term&:(eq ?term ?*owl:oneOf*))(object ?list))
	?DR <- (DATARANGE (name ?dr)(oneOf $?ranges))
	=>
	(bind $?values (collect-list-elements ?list))
	(modify ?DR	(oneOf $?values))
	(retract ?t)
)

;;; object properties
(defrule owl:objectProperty1
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:ObjectProperty*)))
	(not (PROPERTY (name ?p)))
	=>
	(assert (PROPERTY (name ?p)(type object)))
	(retract ?t)
)

;;; datatype properties
(defrule owl:datatypeProperty1
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:DatatypeProperty*)))
	(not (PROPERTY (name ?p)))
	=>
	(assert (PROPERTY (name ?p)(type datatype)))
	(retract ?t)
)

;;; transitive properties
(defrule owl:TransitiveProperty1
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:TransitiveProperty*)))
	(not (PROPERTY (name ?p)))
	=>
	(assert (PROPERTY (name ?p)(type transitive)))
	(retract ?t)
)

;;; symmetric properties
(defrule owl:SymmetricProperty1
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:SymmetricProperty*)))
	(not (PROPERTY (name ?p)))
	=>
	(assert (PROPERTY (name ?p)(type symmetric)))
	(retract ?t)
)

;;; functional proeprties
(defrule owl:FunctionalProperty1
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:FunctionalProperty*)))
	(not (PROPERTY (name ?p)))
	=>
	(assert (PROPERTY (name ?p)(type functional)))
	(retract ?t)
)

;;; inverse functional properties
(defrule owl:InverseFunctionalProperty1
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:InverseFunctionalProperty*)))
	(not (PROPERTY (name ?p)))
	=>
	(assert (PROPERTY (name ?p)(type inversefunctional)))
	(retract ?t)
)

;;; auxiliary rule
(defrule owl:objectProperty2
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:ObjectProperty*)))
	?CL <- (PROPERTY (name ?p)(type $?type))
	=>
	(modify ?CL	(type (create$ $?type object)))
	(retract ?t)
)

;;; auxiliary rule
(defrule owl:datatypeProperty2
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
				(object ?term2&:(eq ?term2 ?*owl:DatatypeProperty*)))
	?CL <- (PROPERTY (name ?p)(type $?type))
	=>
	(modify ?CL (type (create$ $?type datatype)))
	(retract ?t)
)

;;; auxiliary rule
(defrule owl:propertyChainAxiom
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term&:(eq ?term ?*owl:propertyChainAxiom*))(object ?list))
	?CL <- (PROPERTY (name ?p)(propertyChain $?chain))
	=>
	;(bind $?values (collect-list-elements ?list))
	(modify ?CL	(propertyChain (create$ $?chain ?list)))
	(retract ?t)
)

;;; auxiliary rule
(defrule owl:TransitiveProperty2
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:TransitiveProperty*)))	
	?CL <- (PROPERTY (name ?p)(type $?type))
	=>
	(modify ?CL	(type (create$ $?type transitive)))
	(retract ?t)
)

;;; auxiliary rule
(defrule owl:SymmetricProperty2
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:SymmetricProperty*)))
	?CL <- (PROPERTY (name ?p)(type $?type))
	=>
	(modify ?CL (type (create$ $?type symmetric)))
	(retract ?t)
)

;;; auxiliary rule
(defrule owl:FunctionalProperty2
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:FunctionalProperty*)))
	?CL <- (PROPERTY (name ?p)(type $?type))
	=>
	(modify ?CL (type (create$ $?type functional)))
	(retract ?t)
)

;;; auxiliary rule
(defrule owl:InverseFunctionalProperty2
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term1&:(eq ?term1 ?*rdf:type*))
								(object ?term2&:(eq ?term2 ?*owl:InverseFunctionalProperty*)))
	?CL <- (PROPERTY (name ?p)(type $?type))
	=>
	(modify ?CL (type (create$ $?type inversefunctional)))
	(retract ?t)
)

;;; the domain of properties
(defrule rdfs:domain
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term&:(eq ?term ?*rdfs:domain*))(object ?d))
	?CL <- (PROPERTY (name ?p)(domain $?domain))
	=>
	(modify ?CL (domain (create$ $?domain (get-delegator ?d))))
	(retract ?t)
)

;;; the range of object properties
(defrule rdfs:range1
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term&:(eq ?term ?*rdfs:range*))(object ?r))
	?CL <- (PROPERTY (name ?p &: (is-object-property ?p))(range $?range))
	=>
	(modify ?CL (range (create$ $?range (get-delegator ?r))))
	(retract ?t)
)

;;; the range of datatype proeprties
(defrule rdfs:range2
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term&:(eq ?term ?*rdfs:range*))(object ?r))
	?CL <- (PROPERTY (name ?p)(type datatype)(range $?range))
	=>
	(modify ?CL (range (create$ $?range ?r)))
	(retract ?t)
)

;;; subproperties
(defrule rdfs:subPropertyOf
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p1)(predicate ?term&:(eq ?term ?*rdfs:subPropertyOf*))(object ?p2&~?p1))
	?CL <- (PROPERTY (name ?p1)(subproperty $?subproperty))
	=>
	(modify ?CL (subproperty (create$ $?subproperty ?p2)))
	(retract ?t)
)

;;; equivalent properties
(defrule owl:equivalentProperty
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p1)(predicate ?term&:(eq ?term ?*owl:equivalentProperty*))
								(object ?p2&~?p1))
	?CL <- (PROPERTY (name ?p1)(equivalentProperty $?equivalent))
	=>
	(modify ?CL	(equivalentProperty (create$ $?equivalent ?p2)))
	(retract ?t)
)

;;; inverse properties
(defrule owl:inverseOf
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p1)(predicate ?term&:(eq ?term ?*owl:inverseOf*))(object ?p2&~?p1))
	?CL <- (PROPERTY (name ?p1)(inverse $?inverse))
	=>
	(modify ?CL (inverse (create$ $?inverse ?p2)))
	(retract ?t)
)

;;; property labels
(defrule label-property
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term&:(eq ?term ?*rdfs:label*))(object ?label))
	?CL <- (PROPERTY (name ?p))
	=>
	(modify ?CL (label ?label))
	(retract ?t)
)

;;; property comments
(defrule comment-property
	(declare (salience 9960))
	(goal (name delegators-defined))
	?t <- (triple (subject ?p)(predicate ?term&:(eq ?term ?*rdfs:comment*))(object ?comment))
	?CL <- (PROPERTY (name ?p))
	=>
	(modify ?CL	(comment ?comment))
	(retract ?t)
)

;;; ######################################################################################
;;; Inference rules for properties
;;; ######################################################################################
(defrule equivalent-property-transitive
	(declare (salience 9950))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY (name ?p1) (equivalentProperty $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2&~?p1)(equivalentProperty $? ?p3&~?p1 $?))
	(test (not (member$ ?p3 (fact-slot-value ?PR1 equivalentProperty))))
	=>
	(modify ?PR1
		(equivalentProperty (delete-member$ 
			(remove-duplicates$ (create$ (fact-slot-value ?PR1 equivalentProperty) ?p3)) ?p1)))
)

(defrule equivalent-property-symmetric
	(declare (salience 9950))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(equivalentProperty $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2&~?p1)(equivalentProperty $?equivalent))
	(test (not (member$ ?p1 $?equivalent)))
	=>
	(modify ?PR2 (equivalentProperty (remove-duplicates$ (create$ $?equivalent ?p1))))
)

(defrule inverse-property-symmetric
	(declare (salience 9950))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(inverse $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2&~?p1)(inverse $?inverses))
	(test (not (member$ ?p1 $?inverses)))
	=>
	(modify ?PR2 (inverse (remove-duplicates$ (create$ $?inverses ?p1))))
)

(defrule subproperty-transitive
	(declare (salience 9950))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(subproperty $? ?subproperty1 $?))
	?PR2 <- (PROPERTY	(name ?subproperty1 &~ ?p1)(subproperty $? ?subproperty2 &~ ?p1 $?))
	(test (not (member$ ?subproperty2 (fact-slot-value ?PR1 subproperty))))
	=>
	(modify ?PR1 (subproperty (delete-member$ 
		(remove-duplicates$ (create$ (fact-slot-value ?PR1 subproperty) ?subproperty2)) ?p1)))
)

(defrule inverse-of-functional
	(declare (salience 9950))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(type $?types &: (member$ functional $?types))(inverse $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2 &~ ?p1)(type $?types &: (not (member$ inversefunctional $?types))))
	=>
	(modify ?PR2 (type (create$ $?types inversefunctional)))
)

(defrule inverse-of-inverse-functional
	(declare (salience 9950))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(type $?types &: (member$ inversefunctional $?types))(inverse $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2 &~ ?p1)(type $?types &: (not (member$ functional $?types))))
	=>
	(modify ?PR2 (type (create$ $?types functional)))
)

;;; ######################################################################################
;;; # Manage the domains / ranges of properties
;;; ######################################################################################
(defrule domain-equivalent-properties
	(declare (salience 9940))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(domain $?domains)(equivalentProperty $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2&~?p1)(domain $? ?d $?))
	(test (not (member$ ?d $?domains)))
	(test (has-no-subclass ?d $?domains))
	=>
	(modify ?PR1 (domain (create$ $?domains ?d)))
)

(defrule range-equivalent-object-properties
	(declare (salience 9940))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1 &: (is-object-property ?p1))(range $?ranges)(equivalentProperty $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2 &~ ?p1)(range $? ?r $?))
	(test (not (member$ ?r $?ranges)))
	(test (has-no-subclass ?r $?ranges))
	=>
	(modify ?PR1 (range (create$ $?ranges ?r)))
)

(defrule range-equivalent-datatype-properties
	(declare (salience 9940))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(type datatype)(range $?ranges)(equivalentProperty $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2 &~ ?p1)(range $? ?r $?))
	(test (not (member$ ?r $?ranges)))
	=>
	(modify ?PR1 (range (create$ $?ranges ?r)))
)

(defrule domain-subproperties
	(declare (salience 9940))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(domain $?domains)(subproperty $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2 &~ ?p1)(domain $? ?d $?))
	(test (not (member$ ?d $?domains)))
	(test (has-no-subclass ?d $?domains))
	=>
	(modify ?PR1 (domain (create$ $?domains ?d)))
)

(defrule range-object-subproperties
	(declare (salience 9940))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1 &: (is-object-property ?p1))(range $?ranges)(subproperty $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2 &~ ?p1)(range $? ?r $?))
	(test (not (member$ ?r $?ranges)))
	(test (has-no-subclass ?r $?ranges))
	=>
	(modify ?PR1 (range (create$ $?ranges ?r)))
)

(defrule range-datatype-subproperties
	(declare (salience 9940))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY (name ?p1)(type datatype)(range $?ranges)(subproperty $? ?p2 $?))
	?PR2 <- (PROPERTY (name ?p2 &~ ?p1)(range $? ?r $?))
	(test (not (member$ ?r $?ranges)))
	=>
	(modify ?PR1 (range (create$ $?ranges ?r)))
)

(defrule domain-inverse-properties
	(declare (salience 9939))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(range $?ranges)(inverse $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2 &~ ?p1)(domain $? ?d $?))
	(test (not (member$ ?d $?ranges)))
	(test (has-no-subclass ?d $?ranges))
	=>
	(modify ?PR1 (range (create$ $?ranges ?d)))
)

(defrule range-inverse-properties
	(declare (salience 9939))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p1)(domain $?domains)(inverse $? ?p2 $?))
	?PR2 <- (PROPERTY	(name ?p2 &~ ?p1)(range $? ?r $?))
	(test (not (member$ ?r $?domains)))
	(test (has-no-subclass ?r $?domains))
	=>
	(modify ?PR1 (domain (create$ $?domains ?r)))
)

(defrule domain-hasKey
	(declare (salience 9939))
	(goal (name delegators-defined))
	(CLASS (name ?c)(hasKey $? ?k $?))
	?P <- (PROPERTY (name ?k)(domain $?domains &: (not (member$ ?c $?domains))))
	=>
	(modify ?P (domain (create$ $?domains ?c)))
)

;the domain classes should be delegators
(defrule domain-substitute-non-delegators
	(declare (salience 9938))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p)(domain $? ?d $?))
	(test (not (is-delegator ?d)))
	=>
	(modify ?PR1 (domain (create$ (remove-duplicates$ 
		(replace-member$ (fact-slot-value ?PR1 domain) (get-delegator ?d) ?d)))))
)

;;; the range classes (object properties) should be delegators
(defrule range-substitute-non-delegators
	(declare (salience 9938))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p &: (is-object-property ?p))(range $? ?r $?))
	(test (not (is-delegator ?r)))
	=>
	(modify ?PR1 (range (create$ (remove-duplicates$ 
		(replace-member$ (fact-slot-value ?PR1 range) (get-delegator ?r) ?r)))))
)

;;; hold only the most specific classes in domains
(defrule remove-unnecessary-domains
	(declare (salience 9937))
	(goal (name delegators-defined))
	?PR <- (PROPERTY (name ?p)(domain $?domains &: (> (length$ $?domains) 1)))
	=>
	(bind $?classes (most-specific-classes-fact $?domains))
	(if (neq (length$ $?classes) (length$ $?domains)) then 
		(modify ?PR	(domain $?classes)))
)

;;; hold only the most specific class in ranges (dobject properties)
(defrule remove-unnecessary-ranges
	(declare (salience 9937))
	(goal (name delegators-defined))
	?PR <- (PROPERTY (name ?p &: (is-object-property ?p))(range $?ranges &: (> (length$ $?ranges) 1)))
	=>
	(bind $?classes (most-specific-classes-fact $?ranges))
	(if (neq (length$ $?classes) (length$ $?ranges)) then 
		(modify ?PR	(range $?classes)))
)

;;; ######################################################################################
;;; Manage multiple domains / ranges. The domain and range of each property should be 
;;; a single class. Therefore, if there is more than one class for a domain / range, a
;;; subclass is generated of all the domain / range classes.
;;; ######################################################################################
(defrule multiple-domain-property
	(declare (salience 9930))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p)(domain $?domains&:(> (length$ $?domains) 1)))
	=>
	(bind ?one-class (class-with-superclasses $?domains))
	(if (neq ?one-class FALSE) then
		(modify ?PR1 (domain ?one-class))
	else
		(bind ?new-class (sym-cat (create-unique-class-name)))
		(assert (CLASS (name ?new-class)(delegator TRUE)(subclass $?domains)))
		(modify ?PR1 (domain (create$ ?new-class))))
)

(defrule multiple-range-object-property
	(declare (salience 9930))
	(goal (name delegators-defined))
	?PR1 <- (PROPERTY	(name ?p &: (is-object-property ?p))(range $?ranges&:(> (length$ $?ranges) 1)))
	=>
	(bind ?one-class (class-with-superclasses $?ranges))
	(if (neq ?one-class FALSE) then
		(modify ?PR1 (range ?one-class))
	else
		(bind ?new-class (sym-cat (create-unique-class-name)))
		(assert (CLASS (name ?new-class)(delegator TRUE)(subclass $?ranges)))
		(modify ?PR1(range (create$ ?new-class))))
)

;;; checks if for some reason, there are still domains with more than one class 
(defrule multiple-domain-property-check
	(declare (salience 9929))
	(goal (name delegators-defined))
	(PROPERTY (name ?p) (domain $?domains&:(> (length$ $?domains) 1)))
	=>
	(debug warn "Property " ?p " has more than one domain.")
)

;;; checks if for some reason, there are still ranges with more than one class
(defrule multiple-range-property-check
	(declare (salience 9929))
	(goal (name delegators-defined))
	(PROPERTY (name ?p &: (is-object-property ?p))(range $?ranges&:(> (length$ $?ranges) 1)))
	=>
	(debug warn "Property " ?p " has more than one ranges.")
)

;;; ######################################################################################
;;; # Rules for inserting the properties into the CLASS fact of their domain classes
;;; ######################################################################################
(defrule insert-class-slots-in-owl:Thing
	(declare (salience 9920))
	(goal (name delegators-defined))
	(PROPERTY	(name ?p)(domain $?domains &: (eq (length$ $?domains) 0)))
	?THING <- (CLASS (name ?n &: (eq ?n ?*owl:Thing*))(slots $?slots &: (not (member$ ?p $?slots))))
	=>
	(modify ?THING (slots (create$ $?slots ?p)))
)

(defrule insert-class-slots
	(declare (salience 9920))
	(goal (name delegators-defined))
	(PROPERTY (name ?p) (domain $?domains&:(eq (length$ $?domains) 1)))
	?CL <- (CLASS (name ?c &: (member$ ?c $?domains))(slots $?slots &: (not (member$ ?p $?slots))))
	=>
	(modify ?CL (slots (create$ $?slots ?p)))
)

;;; the equivalent classes should have the same slots
(defrule make-equivalents-with-same-slots
	(declare (salience 9920))
	(goal (name delegators-defined))
	?CL1 <- (CLASS (name ?c1)(equivalent $? ?c2 $?)(slots $?slots))
	?CL2 <- (CLASS (name ?c2)(slots $? ?s &: (not (member$ ?s $?slots)) $?))
	=>
	(modify ?CL1 (slots (create$ $?slots ?s)))
)

;;; check if there are properties that have not been assigned to a class
(defrule check-unassigned-properties
	(declare (salience 9919))
	(goal (name delegators-defined))
	(PROPERTY (name ?p))
	(not (CLASS (slots $? ?p $?)))
	=>
	(debug warn "Property " ?p " has not been assigned to any class")
)
