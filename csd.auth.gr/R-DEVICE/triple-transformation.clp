(defrule create-instances-of-existing-classes
	(goal create-instances)
	?x <- (triple (subject ?res) (predicate rdf:type) (object ?class))
	(not (triple (subject ?res) (predicate rdf:type) (object ~?class)))
	(test (and 
		(class-existp ?class)
		(instance-existp (symbol-to-instance-name ?class))
		(not (resource-existp ?res))
	))
  =>
  	(verbose  "creating object: " ?res " of " ?class crlf)
  	(resource-make-instance ?class ?res (create$ ?class))
  	(triple-retract ?x)
)

(defrule changing-type-of-existing-instances
	(goal create-instances)
	?x <- (triple (subject ?res) (predicate rdf:type) (object ?class))
	;(not (triple (subject ?res) (predicate rdf:type) (object ~?class)))
	(test (and 
		(class-existp ?class)
		(instance-existp (symbol-to-instance-name ?class))
		(resource-existp ?res)
		(neq (resource-class ?res) ?class)
	))
  =>
  	(bind ?current-class (resource-class ?res))
  	(bind ?current-instance (resource-instance ?res))
  	(verbose  "Adding type " ?class " to object: " ?res " of type " ?current-class crlf)
	(bind ?new-class (exists-class-with-super-classes (create$ ?class ?current-class)))
	(if (eq ?new-class FALSE)
	   then
		(bind ?new-class (gensym*))
		(resource-make-instance rdfs:Class ?new-class (create$ ?new-class))
		(send (symbol-to-instance-name ?new-class) put-source system)
		(my-build (str-cat$ 
			"(" defclass ?new-class
				"(" is-a (create$ ?class ?current-class) ")"
				"(" multislot class-refs 
					"(" source composite ")"
					"(" default (unique-pairs (collect-defaults class-refs (create$ ?class ?current-class))) ")"
				")"
				"(" multislot aliases 
					"(" source composite ")"
					"(" default (unique-pairs (collect-defaults aliases (create$ ?class ?current-class))) ")"
				")"
			")"
		))
	)
	;(bind ?new-instance (gensym*))
  	(bind ?new-instance (make-instance of ?new-class))
	(shallow-copy ?current-instance ?new-instance)
	(send ?new-instance put-rdf:type (create$ ?class ?current-class))
	(duplicate-instance ?new-instance to ?current-instance)
	(send ?new-instance delete)
  	;(resource-make-instance ?class ?res (create$ ?class))
  	(triple-retract ?x)
)

(defrule create-instances-of-multiple-existing-classes
	(goal create-instances)
	?x <- (triple (subject ?res) (predicate rdf:type) (object ?class))
	(triple (subject ?res) (predicate rdf:type) (object ~?class))
	(test (and 
		(class-existp ?class)
		(instance-existp (symbol-to-instance-name ?class))
	))
 	(not (multi-type-object (name ?res)))
 =>
 	(assert (multi-type-object (name ?res) (classes ?class)))
  	(triple-retract ?x)
)

(defrule create-instances-of-multiple-classes-1
	(goal create-instances)
	?x <- (multi-type-object (name ?res) (classes $?classes))
	?y <- (triple (subject ?res) (predicate rdf:type) (object ?class&:(not (member$ ?class $?classes))))
	(test (and 
		(class-existp ?class)
		(instance-existp (symbol-to-instance-name ?class))
	))
  =>
	(modify ?x (classes (create$ ?class $?classes)))
  	(triple-retract ?y)
)

(defrule create-instances-of-multiple-classes-2
	(goal create-instances)
	?x <- (multi-type-object (name ?res) (classes $?classes))
	(not (triple (subject ?res) (predicate rdf:type)))
	(test (not (resource-existp ?res)))
  =>
 	(verbose  "creating object: " ?res " of multiple-classes: " $?classes crlf)
	(bind ?class-name (exists-class-with-super-classes $?classes))
	(if (eq ?class-name FALSE)
	   then
		(bind ?class-name (gensym*))
		(resource-make-instance rdfs:Class ?class-name (create$ ?class-name))
		(send (symbol-to-instance-name ?class-name) put-source system)
		(my-build (str-cat$ 
			"(" defclass ?class-name
				"(" is-a $?classes ")"
				"(" multislot class-refs 
					"(" source composite ")"
					"(" default (unique-pairs (collect-defaults class-refs $?classes)) ")"
				")"
				"(" multislot aliases 
					"(" source composite ")"
					"(" default (unique-pairs (collect-defaults aliases $?classes)) ")"
				")"
			")"
		))
	)         
	(resource-make-instance ?class-name ?res $?classes)
  	(retract ?x)
)



(defrule put-instance-slots-resources
	(goal put-slot-values)
	?x <- (triple (subject ?s) (predicate ?p) (object ?o))
	(test (resource-existp ?s))
	(test (slot-existp (resource-class ?s) ?p inherit))
	(test (member$ INSTANCE-NAME (slot-types (resource-class ?s) ?p)))
	(test (resource-existp ?o))
	(not (triple (subject ?o) (predicate rdf:type)))
  =>
	(if (compatible-types (resource-class ?o) (get-type-of (resource-class ?s) ?p))
	   then
	   	(if (not (member$ (resource-instance ?o) (funcall send (resource-instance ?s) (sym-cat get- ?p))))
	   	   then
  			(verbose  "object: " ?s " predicate: " ?p " value: " ?o crlf)
	   		(slot-insert$ (resource-instance ?s) ?p 1  (resource-instance ?o))
	   	)
  		(triple-retract ?x)
	   else
	      	; This should instead insert a triple that asserts a new type for the object!
	      	(verbose "Asserting new type " (get-type-of (resource-class ?s) ?p) " for object " ?o crlf)
		 (assert (triple (subject ?o) (predicate rdf:type) (object (get-type-of (resource-class ?s) ?p))))
		 (bind ?*triple_counter* (+ ?*triple_counter* 1))
	      	;(verbose  "Type conflict!" crlf)
	      	;(verbose  "Object: " ?o " is of type " (resource-class ?o) " while slot " ?p " of class " (resource-class ?s) " is of type " (get-type-of (resource-class ?s) ?p) crlf)
	   	;(assert (rejected-triple (subject ?s) (predicate ?p) (object ?o)))
	)
)

(defrule put-instance-slots-literals
	(goal put-slot-values)
	?x <- (triple (subject ?s) (predicate ?p) (object ?o))
	(test (resource-existp ?s))
	(test (slot-existp (resource-class ?s) ?p inherit))
	;(test (eq rdfs:Literal (nth$ 1 (send (symbol-to-instance-name ?p) get-rdfs:range))))
	(test (neq (create$ INSTANCE-NAME) (slot-types (resource-class ?s) ?p)))
	(test (not (resource-existp ?o)))
	(test (not (is-uri ?o)))
	(test (not (is-parsed-uri ?o)))
  =>
	(verbose  "object: " ?s " predicate: " ?p " value: " ?o crlf)
   	(slot-insert$ (resource-instance ?s) ?p 1 ?o)
  	(triple-retract ?x)
)


(defrule property-inheritance-domains
	(goal property-inheritance)
	?prop <- (object (is-a rdf:Property) (name ?property) (rdfs:subPropertyOf $? ?super-property $?) (rdfs:domain $?domains))
	(object (is-a rdf:Property) (name ?super-property) (rdfs:domain $? ?domain $?))
	(test (not (member$ ?domain $?domains)))
  =>
  	(verbose  "Inheriting domain " ?domain " to property " ?property " from super-property " ?super-property crlf)
  	(slot-insert$ ?prop rdfs:domain 1 ?domain)
)

(defrule property-inheritance-ranges
	(goal property-inheritance)
	?prop <- (object (is-a rdf:Property) (name ?property) (rdfs:subPropertyOf $? ?super-property $?) (rdfs:range $?ranges))
	(object (is-a rdf:Property) (name ?super-property) (rdfs:range $? ?range $?))
	(test (not (member$ ?range $?ranges)))
  =>
  	(verbose  "Inheriting range " ?range " to property " ?property " from super-property " ?super-property crlf)
  	(slot-insert$ ?prop rdfs:range 1 ?range)
)

(defrule property-with-multiple-domains
	(goal multiple-domains-ranges)
	?prop <- (object (is-a rdf:Property) (name ?property) (rdfs:domain $?domains&:(> (length$ $?domains) 1)))
	(test (= (length$ (exist-classes (instances-to-symbols $?domains))) 0)) ; All classes in range exist
	(test (> (length$ (most-specific-classes (instances-to-symbols $?domains))) 1))  
  =>
  	(bind $?classes (instances-to-symbols $?domains))
 	(verbose  "Property: " ?property " with multiple domains: " $?classes crlf)
	(bind ?class-name (exists-class-with-super-classes $?classes))
	(if (eq ?class-name FALSE)
	   then
		(bind ?class-name (gensym*))
		(resource-make-instance rdfs:Class ?class-name (create$ ?class-name))
		(send (symbol-to-instance-name ?class-name) put-source system)
		(my-build (str-cat$ 
			"(" defclass ?class-name
				"(" is-a (most-specific-classes $?classes) ")"
				"(" multislot class-refs 
					"(" source composite ")"
					"(" default (unique-pairs (collect-defaults class-refs $?classes)) ")"
				")"
				"(" multislot aliases 
					"(" source composite ")"
					"(" default (unique-pairs (collect-defaults aliases $?classes)) ")"
				")"
			")"
		))
	)
	(send ?prop put-rdfs:domain (symbol-to-instance-name ?class-name))
)

(defrule property-with-multiple-ranges
	(goal multiple-domains-ranges)
	?prop <- (object (is-a rdf:Property) (name ?property) (rdfs:range $?ranges&:(> (length$ $?ranges) 1)))
	(test (= (length$ (exist-classes (instances-to-symbols $?ranges))) 0)) ; All classes in range exist
	(test (> (length$ (most-specific-classes (instances-to-symbols $?ranges))) 1))
  =>
  	(bind $?classes (instances-to-symbols $?ranges))
 	(verbose  "Property: " ?property " with multiple ranges: " $?classes crlf)
	(bind ?class-name (exists-class-with-super-classes $?classes))
	(if (eq ?class-name FALSE)
	   then
		(bind ?class-name (gensym*))
		(resource-make-instance rdfs:Class ?class-name (create$ ?class-name))
		(send (symbol-to-instance-name ?class-name) put-source system)
		(my-build (str-cat$ 
			"(" defclass ?class-name
				"(" is-a (most-specific-classes $?classes) ")"
				"(" multislot class-refs 
					"(" source composite ")"
					"(" default (unique-pairs (collect-defaults class-refs $?classes)) ")"
				")"
				"(" multislot aliases 
					"(" source composite ")"
					"(" default (unique-pairs (collect-defaults aliases $?classes)) ")"
				")"
			")"
		))
	)
	(send ?prop put-rdfs:range (symbol-to-instance-name ?class-name))
)

(defrule create-non-existing-classes_create-candidate-class
	(goal create-new-classes)
  =>
  	(do-for-all-instances 
  		((?res rdfs:Class))
  		(and
  			(not (class-existp (instance-name-to-symbol ?res)))
  			;(not-rdf-resource (instance-name (nth$ 1 ?res:rdfs:isDefinedBy)))
  			(not (is-rdf-resource ?res))
  			(= (length$ (exist-classes (instances-to-symbols ?res:rdfs:subClassOf))) 0) ; All super-classes exist
  		)
		(assert 
			(candidate-class 
				(name (instance-name-to-symbol ?res)) 
				(isa-slot (instances-to-symbols ?res:rdfs:subClassOf))
				(class-refs-defaults (collect-defaults class-refs (instances-to-symbols ?res:rdfs:subClassOf)))
				(aliases-defaults (collect-defaults aliases (instances-to-symbols ?res:rdfs:subClassOf)))
			)
		)
	)
)


(defrule create-non-existing-classes_create-slots-type-Literal
	(goal create-new-classes)
	?x <- (candidate-class (name ?new-class) (isa-slot $?super-classes) (slot-definitions $?slot-defs) (aliases-defaults $?aliases))
	?y <- (object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?domains) (rdfs:range [rdfs:Literal]) (rdfs:subPropertyOf $?super-properties))
	;(test (verbose "new-class: " ?new-class crlf))
	;(test (verbose "new-slot: " ?new-slot crlf))
	;(test (verbose "domains: " $?domains crlf))
	;(test (= (length$ (exist-classes $?domains)) 0))
	(test (is-only-one-class (instances-to-symbols $?domains)))
	(test (eq ?new-class (get-only-one-class (instances-to-symbols $?domains))))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
 => 
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) "(" type LEXEME ")" ")" $?slot-defs)) 
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)
)

(defrule create-non-existing-classes_create-slots-type-Resource
	(goal create-new-classes)
	?x <- (candidate-class (name ?new-class) (isa-slot $?super-classes) (slot-definitions $?slot-defs) (class-refs-defaults $?class-refs) (aliases-defaults $?aliases))
	?y <- (object (is-a rdf:Property) (name ?new-slot) 
		(rdfs:domain $?domains) 
		;(rdfs:range ?type&:(instance-existp ?type)&:(neq ?type [rdfs:Literal])) 
		(rdfs:range $?ranges)
		(rdfs:subPropertyOf $?super-properties))
	;(test (= (length$ (exist-classes $?domains)) 0))
	;(test (verbose "domains exist!" crlf))
	(test (all-instance-existp $?domains))
	(test (is-only-one-class (instances-to-symbols $?domains)))
	(test (eq ?new-class (get-only-one-class (instances-to-symbols $?domains))))
	;(test (= (length$ (exist-classes (instances-to-symbols $?ranges))) 0))
	;(test (verbose "ranges exist!" crlf))
	(test (all-instance-existp $?ranges))
	(test (is-only-one-class (instances-to-symbols $?ranges)))
	(test (neq rdfs:Literal (get-only-one-class (instances-to-symbols $?ranges))))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
	;(test (verbose "new-class: " ?new-class crlf))
	;(test (verbose "new-slot: " ?new-slot crlf))
	;(test (verbose "domains: " $?domains crlf))
	;(test (verbose "ranges: " $?ranges crlf))
  =>
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) "(" type INSTANCE-NAME ")" ")" $?slot-defs))
		(class-refs-defaults (create$ (instance-name-to-symbol ?new-slot) (get-only-one-class (instances-to-symbols $?ranges)) $?class-refs))
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)
)

(defrule create-non-existing-classes_create-slots-type-no-range
	(goal create-new-classes)
	?x <- (candidate-class (name ?new-class) (isa-slot $?super-classes) (slot-definitions $?slot-defs) (aliases-defaults $?aliases))
	?y <- (object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?domains) (rdfs:range $?list&:(= (length$ $?list) 0)) (rdfs:subPropertyOf $?super-properties))
;	?y <- (triple (subject ?new-slot) (predicate rdfs:domain) (object ?new-class))
;	(not (triple (subject ?new-slot) (predicate rdfs:range)))
	;(test (verbose "new-class: " ?new-class crlf))
	;(test (verbose "new-slot: " ?new-slot crlf))
	;(test (verbose "domains: " $?domains crlf))
	;(test (= (length$ (exist-classes $?domains)) 0))
	(test (is-only-one-class (instances-to-symbols $?domains)))
	(test (eq ?new-class (get-only-one-class (instances-to-symbols $?domains))))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
  =>
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) ")" $?slot-defs))
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)
)


(defrule generate-non-existing-classes_create-create-final-class
	(goal generate-new-classes)
	?x <- (candidate-class 
			(name ?new-class) 
			(isa-slot $?super-classes) 
			(slot-definitions $?slot-defs) 
			(class-refs-defaults $?class-refs) 
			(aliases-defaults $?aliases))
	;(not (triple (predicate rdfs:domain) (object ?new-class)))
  =>
  	(verbose  "Creating class: " ?new-class crlf)
  	;(verbose "           Subclass of: " $?super-classes crlf)
	(my-build (str-cat$ 
		"(" defclass ?new-class
			"(" is-a 
				(if (> (length$ $?super-classes) 0)
				   then
				   	$?super-classes
				   else
				   	rdfs:Resource
				)
			")"
			$?slot-defs
			"(" multislot class-refs 
				"(" source composite ")"
				"(" default (unique-pairs $?class-refs) ")"
			")"
			"(" multislot aliases 
				"(" source composite ")"
				"(" default (unique-pairs $?aliases) ")"
			")"
		")"
	))
	;(ppdefclass ?new-class)
  	(retract ?x)
)

(defrule add-extra-superclass
	(goal put-new-properties)
	(object (is-a rdfs:Class) (name ?class) (rdfs:subClassOf $?superclasses))
	(not (class-to-undefine ?c&:(eq ?c (instance-name-to-symbol ?class))))
	(test (class-existp (instance-name-to-symbol ?class)))
;	(test (verbose "new superclasses: " (most-specific-classes (instances-to-symbols $?superclasses)) crlf))
;	(test (verbose "old superclasses: " (class-superclasses (instance-name-to-symbol ?class)) crlf))
	(test (> (length$ (difference$ (create$ (most-specific-classes (instances-to-symbols $?superclasses)) $$$ (class-superclasses (instance-name-to-symbol ?class))))) 0))
  =>
  	(verbose  "Backing up class: " (instance-name-to-symbol ?class) crlf)
	(backup-class (instance-name-to-symbol ?class))
)


(defrule put-new-properties-no-domain
	(goal put-new-properties)
	(object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?list&:(= (length$ $?list) 0)))
	(not (class-to-undefine rdfs:Resource))
	(test (class-existp rdfs:Resource))
	(test (not (slot-existp rdfs:Resource (instance-name-to-symbol ?new-slot) inherit)))
  =>
  	(verbose  "Backing up class: rdfs:Resource" crlf)
	(backup-class rdfs:Resource)
)

(defrule put-new-properties-with-one-domain
	(goal put-new-properties)
;	(object (is-a rdf:Property) (name ?new-slot) (rdfs:domain ?class))
	(object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?domains))
	(test (= (length$ (exist-classes $?domains)) 0))
	(test (is-only-one-class (instances-to-symbols $?domains)))
	(not (class-to-undefine ?c&:(eq ?c (get-only-one-class (instances-to-symbols $?domains)))))
	(test (class-existp (get-only-one-class (instances-to-symbols $?domains))))
	(test (not (slot-existp (get-only-one-class (instances-to-symbols $?domains)) (instance-name-to-symbol ?new-slot) inherit)))
  =>
  	(verbose  "Backing up class: " (get-only-one-class (instances-to-symbols $?domains)) crlf)
  	;(verbose  "    because of slot: " (instance-name-to-symbol ?new-slot) crlf)
	(backup-class (get-only-one-class (instances-to-symbols $?domains)))
)

(defrule insert-extra-superclass
	(goal put-new-properties)
	?x <- (redefined-class (name ?class) (isa-slot $?existing-superclasses))
	(object (is-a rdfs:Class) (name ?c&:(eq ?c (symbol-to-instance-name ?class))) (rdfs:subClassOf $?new-superclasses))
	(test (> (length$ (difference$ (create$ (most-specific-classes (instances-to-symbols $?new-superclasses)) $$$ (instances-to-symbols $?existing-superclasses)))) 0))
	;(test (verbose "new superclasses: " (most-specific-classes (instances-to-symbols $?new-superclasses)) crlf))
	;(test (verbose "old superclasses: " (instances-to-symbols $?existing-superclasses) crlf))
  =>
  	(bind $?superclasses (most-specific-classes (union$ (create$ $?new-superclasses $$$ $?existing-superclasses))))
  	(verbose  "New superclass(es): " (instances-to-symbols $?superclasses) " for class " ?class crlf)
	(modify ?x (isa-slot $?superclasses))
)


(defrule insert-new-property-no-domain-Literal
	(goal put-new-properties)
	?x <- (redefined-class (name rdfs:Resource) (slot-definitions $?slot-defs) (aliases-defaults $?aliases))
	?y <- (object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?list&:(= (length$ $?list) 0)) (rdfs:range [rdfs:Literal]) (rdfs:subPropertyOf $?super-properties))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
  =>
  	(verbose  "New property: " (instance-name-to-symbol ?new-slot) " for rdfs:Resource." crlf)
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) "(" type LEXEME ")" ")" $?slot-defs)) 
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)
)

(defrule insert-new-property-no-domain-Resource
	(goal put-new-properties)
	?x <- (redefined-class (name rdfs:Resource) (slot-definitions $?slot-defs) (class-refs-defaults $?class-refs) (aliases-defaults $?aliases))
	?y <- (object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?list&:(= (length$ $?list) 0)) (rdfs:range $?ranges) (rdfs:subPropertyOf $?super-properties))
	(test (= (length$ (exist-classes $?ranges)) 0))
	(test (is-only-one-class (instances-to-symbols $?ranges)))
	(test (neq rdfs:Literal (get-only-one-class (instances-to-symbols $?ranges))))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
  =>
  	(verbose  "New property: " (instance-name-to-symbol ?new-slot) " for rdfs:Resource." crlf)
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) "(" type INSTANCE-NAME ")" ")" $?slot-defs))
		(class-refs-defaults (create$ (instance-name-to-symbol ?new-slot) (get-only-one-class (instances-to-symbols $?ranges)) $?class-refs))
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)
)

(defrule insert-new-property-no-domain-no-range
	(goal put-new-properties)
	?x <- (redefined-class (name rdfs:Resource) (slot-definitions $?slot-defs) (aliases-defaults $?aliases))
	?y <- (object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?list1&:(= (length$ $?list1) 0)) (rdfs:range $?list2&:(= (length$ $?list2) 0)) (rdfs:subPropertyOf $?super-properties))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
  =>
  	(verbose  "New property: " (instance-name-to-symbol ?new-slot) " for rdfs:Resource." crlf)
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) ")" $?slot-defs))
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)
)

(defrule insert-new-property-one-domain-Literal
	(goal put-new-properties)
	?x <- (redefined-class (name ?class) (isa-slot $?super-classes) (slot-definitions $?slot-defs) (aliases-defaults $?aliases))
	(test (instance-existp (symbol-to-instance-name ?class))) ; this refers to "blank" multi-typing classes
	?y <- (object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?domains) (rdfs:range [rdfs:Literal]) (rdfs:subPropertyOf $?super-properties))
	(test (= (length$ (exist-classes $?domains)) 0))
	(test (is-only-one-class (instances-to-symbols $?domains)))
	(test (eq ?class (get-only-one-class (instances-to-symbols $?domains))))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
  =>
  	(verbose  "New property: " (instance-name-to-symbol ?new-slot) " for class " ?class crlf)
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) "(" type LEXEME ")" ")" $?slot-defs)) 
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)

)

(defrule insert-new-property-one-domain-Resource
	(goal put-new-properties)
	?x <- (redefined-class (name ?class) (isa-slot $?super-classes) (slot-definitions $?slot-defs) (class-refs-defaults $?class-refs) (aliases-defaults $?aliases))
	(test (instance-existp (symbol-to-instance-name ?class))) ; this refers to "blank" multi-typing classes
	?y <- (object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?domains) (rdfs:range $?ranges) (rdfs:subPropertyOf $?super-properties))
	(test (= (length$ (exist-classes $?domains)) 0))
	(test (is-only-one-class (instances-to-symbols $?domains)))
	(test (eq ?class (get-only-one-class (instances-to-symbols $?domains))))
	(test (= (length$ (exist-classes $?ranges)) 0))
	(test (is-only-one-class (instances-to-symbols $?ranges)))
	(test (neq rdfs:Literal (get-only-one-class (instances-to-symbols $?ranges))))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
  =>
	(verbose "new slot: " (instance-name-to-symbol ?new-slot) crlf)
	(verbose "existing slots: " $?slot-defs crlf)
  	(verbose  "New property: " (instance-name-to-symbol ?new-slot) " for class " ?class crlf)
  	(ppdefclass ?class)
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) "(" type INSTANCE-NAME ")" ")" $?slot-defs))
		(class-refs-defaults (create$ (instance-name-to-symbol ?new-slot) (get-only-one-class (instances-to-symbols $?ranges)) $?class-refs))
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)
)

(defrule insert-new-property-one-domain-no-range
	(goal put-new-properties)
	?x <- (redefined-class (name ?class) (isa-slot $?super-classes) (slot-definitions $?slot-defs) (aliases-defaults $?aliases))
	(test (instance-existp (symbol-to-instance-name ?class))) ; this refers to "blank" multi-typing classes
	?y <- (object (is-a rdf:Property) (name ?new-slot) (rdfs:domain $?domains) (rdfs:range $?list&:(= (length$ $?list) 0)) (rdfs:subPropertyOf $?super-properties))
	(test (= (length$ (exist-classes $?domains)) 0))
	(test (is-only-one-class (instances-to-symbols $?domains)))
	(test (eq ?class (get-only-one-class (instances-to-symbols $?domains))))
	(not (test (member$ (instance-name-to-symbol ?new-slot) $?slot-defs)))
  =>
  	(verbose  "New property: " (instance-name-to-symbol ?new-slot) " for class " ?class crlf)
	(modify ?x 
		(slot-definitions (create$ "(" multislot (instance-name-to-symbol ?new-slot) ")" $?slot-defs))
		(aliases-defaults (create$ (create-aliases (instance-name-to-symbol ?new-slot) (instances-to-symbols $?super-properties)) $?aliases))
	)
)


(defrule put-remaining-triples-container-membership-properties
	(declare (salience 100))
	(goal put-remaining-triples)
	(triple (predicate ?p))
	(test (not (resource-existp ?p)))
	(test (is-membership-property ?p))
	(not (triple (subject ?p) (predicate rdf:type) (object rdfs:ContainerMembershipProperty)))
  =>
  	;(verbose "Found new property: " ?p crlf)
  	(assert (triple (subject ?p) (predicate rdf:type) (object rdfs:ContainerMembershipProperty)))
  	(assert (triple (subject ?p) (predicate rdfs:subPropertyOf) (object rdfs:member)))
  	(bind ?*triple_counter* (+ ?*triple_counter* 2))
)

(defrule put-remaining-triples-properties
	(declare (salience 100))
	(goal put-remaining-triples)
	(triple (predicate ?p))
	(test (not (resource-existp ?p)))
	(test (not (is-membership-property ?p)))
	(not (triple (subject ?p) (predicate rdf:type) (object rdf:Property)))
	;(not (triple (subject ?p) (predicate rdf:type) (object ?PropClass&:(and (class-existp ?PropClass) (subclassp ?PropClass rdf:Property)))))
  =>
  	;(verbose "Found new property: " ?p crlf)
  	(assert (triple (subject ?p) (predicate rdf:type) (object rdf:Property)))
  	(bind ?*triple_counter* (+ ?*triple_counter* 1))
)

(defrule put-remaining-triples-subjects-with-domain
	(goal put-remaining-triples)
	(triple (subject ?s) (predicate ?p&~rdf:type))
	(not (triple (subject ?s) (predicate rdf:type)))
	(test (not (resource-existp ?s)))
	(object (is-a rdf:Property) (name ?n&:(eq (instance-name-to-symbol ?n) ?p)) (rdfs:domain $?domains))
	(not (triple (subject ?s) (predicate rdf:type)))
  =>
 	(bind ?end (length$ $?domains))
 	(loop-for-count (?n 1 ?end)
 	   do
 	   	(assert (triple (subject ?s) (predicate rdf:type) (object (instance-name-to-symbol (nth$ ?n $?domains)))))
 	)
  	(bind ?*triple_counter* (+ ?*triple_counter* ?end))
)

(defrule put-remaining-triples-subjects-no-domain
	(goal put-remaining-triples)
	(triple (subject ?s) (predicate ?p&~rdf:type))
	(not (triple (subject ?s) (predicate rdf:type)))
	(test (not (resource-existp ?s)))
	(object (is-a rdf:Property) (name ?n&:(eq (instance-name-to-symbol ?n) ?p)) (rdfs:domain $?d&:(= (length$ $?d) 0)))
  	(not (triple (subject ?s) (predicate rdf:type)))
  =>
  	(assert (triple (subject ?s) (predicate rdf:type) (object rdfs:Resource)))
  	(bind ?*triple_counter* (+ ?*triple_counter* 1))
)

(defrule put-remaining-triples-subjects-wrong-domain
	(goal put-remaining-triples)
	?x <- (triple (subject ?s) (predicate ?p) (object ?o))
	(test (resource-existp ?s))
	(test (not (slot-existp (resource-class ?s) ?p inherit)))
	(object (is-a rdf:Property) (name ?n&:(eq (instance-name-to-symbol ?n) ?p)) (rdfs:domain $?domains&:(> (length$ $?domains) 0)))
  	(not (triple (subject ?s) (predicate rdf:type)))
  =>
  	(if (> (length$ $?domains) 1)
  	   then
  		(bind ?type (exists-class-with-super-classes (instances-to-symbols $?domains)))
  	   else
  		(bind ?type (instance-name-to-symbol (nth$ 1 $?domains)))
  	)
   	(bind ?res-type (resource-class ?s))
	(if (not (compatible-types ?res-type ?type))
  	   then
  	   	; This should instead insert a triple that asserts a new type for the subject!
  	   	(verbose "Asserting new type " ?type " for object " ?s crlf)
  	   	 (assert (triple (subject ?s) (predicate rdf:type) (object ?type)))
  	   	 (bind ?*triple_counter* (+ ?*triple_counter* 1))
  	   	;(verbose  "Type conflict!" crlf)
  	   	;(verbose  "Subject: " ?s " is of type " ?res-type " while predicate " ?p " has domain " $?domains crlf)
  		;(assert (rejected-triple (subject ?s) (predicate ?p) (object ?o)))
   		;(triple-retract ?x)
 	)
)

(defrule put-remaining-triples-objects-with-range
	(goal put-remaining-triples)
	(triple (predicate ?p) (object ?o))
	(test (not (resource-existp ?o)))
	(object (is-a rdf:Property) (name ?n&:(eq (instance-name-to-symbol ?n) ?p)) (rdfs:range $?ranges&:(neq $?ranges (create$ [rdfs:Literal]))))
   	(not (triple (subject ?o) (predicate rdf:type)))
 =>
 	(bind ?end (length$ $?ranges))
 	(loop-for-count (?n 1 ?end)
 	   do
 	   	(assert (triple (subject ?o) (predicate rdf:type) (object (instance-name-to-symbol (nth$ ?n $?ranges)))))
 	)
  	(bind ?*triple_counter* (+ ?*triple_counter* ?end))
)

(defrule put-remaining-triples-objects-no-range
	(goal put-remaining-triples)
	(triple (predicate ?p) (object ?o))
	(test (not (resource-existp ?o)))
	(object (is-a rdf:Property) (name ?n&:(eq (instance-name-to-symbol ?n) ?p)) (rdfs:range $?r&:(= (length$ $?r) 0)))
	(test (is-uri ?o))	
	(not (triple (subject ?o) (predicate rdf:type)))
  =>
	(assert (triple (subject ?o) (predicate rdf:type) (object rdfs:Resource)))
	(bind ?*triple_counter* (+ ?*triple_counter* 1))
)

