(defglobal 
	?*undef_rules* = (create$)
	?*undef_functions* = (create$)
)

(deffunction backup-class-def (?class)
	(bind $?all-slots (delete-member$ (class-slots ?class) class-refs aliases))
	(bind $?slot-defs (create$))
	(while (> (length$ $?all-slots) 0)
	   do
	   	(bind $?slot-types (slot-types ?class (nth$ 1 $?all-slots)))
	   	(if (is-multislot ?class (nth$ 1 $?all-slots))
	   	   then
	   	   	(bind ?slot-field multislot)
	   	   else
	   	   	(bind ?slot-field slot)
	   	)
	   	(bind $?slot-defs (create$ $?slot-defs "(" ?slot-field (nth$ 1 $?all-slots) "(" type $?slot-types ")" ")"))
	   	(bind $?all-slots (rest$ $?all-slots))
	)
	(assert (redefined-class 
			(name ?class) 
			(isa-slot (class-superclasses ?class))
			(slot-definitions $?slot-defs)
			(class-refs-defaults (slot-default-value ?class class-refs))
			(aliases-defaults (slot-default-value ?class aliases))
	))
)

(deffunction backup-class-hierarchy (?class)
	(bind $?classes (create$ ?class (class-subclasses ?class inherit)))
	(bind ?end (length$ $?classes))
	(loop-for-count (?n 1 ?end)
	   do
	   	(backup-class-def (nth$ ?n $?classes))
	)
)

(deffunction backup-class (?class)
	(bind ?filename (str-cat "backup-class-" (str-replace ?class "-" ":") "-instances.txt"))
	(save-instances ?filename visible inherit ?class)
	(assert (backup-instances ?filename))
	;(do-for-all-instances ((?x ?class)) TRUE (send ?x delete))
	(backup-class-hierarchy ?class)
	(assert (class-to-undefine ?class))
	;(undefclass ?class)
)

(deffunction undefine-classes ()
	(bind $?facts (get-template-specific-facts class-to-undefine (get-fact-list)))
	(bind ?end (length$ $?facts))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind ?class (nth$ 1 (fact-slot-value (nth$ ?n $?facts) implied)))
	   	(if (class-existp ?class)
	   	   then
	   		(do-for-all-instances ((?x ?class)) TRUE (send ?x delete))
	   		(undefclass ?class)
	   	)
	   	(retract (nth$ ?n $?facts))
	)
)

;(deffunction undefine-functions ()
;	(bind ?end (length$ ?*undef_functions*))
;	(verbose "Undefining " ?end " functions" crlf)
;	(loop-for-count (?n 1 ?end)
;	   do
;	   	(verbose "Undefining function: " (nth$ ?n ?*undef_functions*) crlf)
;	   	(undeffunction (nth$ ?n ?*undef_functions*))
;	)
;	TRUE
;)

(deffunction undefine-functions ()
	(undeffunction load-rdf)
	(undeffunction load-namespaces)
	(undeffunction load-namespace)
	(undeffunction insert-triples)
	(undeffunction create-namespaces)
	(undeffunction scan_base)
	(undeffunction scan_namespaces)
	(undeffunction import-resource)
	(undeffunction create-aliases)
	(undeffunction find-all-super-properties)
	(undeffunction resource-make-instance)
)

(deffunction undefine-rules ()
	(bind ?end (length$ ?*undef_rules*))
	;(verbose "Undefining " ?end " rules" crlf)
	(loop-for-count (?n 1 ?end)
	   do
	   	;(verbose "Undefining rule: " (nth$ ?n ?*undef_rules*) crlf)
	   	(undefrule (nth$ ?n ?*undef_rules*))
	)
	TRUE
;	(undefrule create-instances-of-existing-classes)
;	(undefrule changing-type-of-existing-instances)
;	(undefrule create-instances-of-multiple-existing-classes)
;	(undefrule create-instances-of-multiple-classes-1)
;	(undefrule create-instances-of-multiple-classes-2)
;	(undefrule put-instance-slots-resources)
;	(undefrule put-instance-slots-literals)
;	(undefrule property-inheritance-domains)
;	(undefrule property-inheritance-ranges)
;	(undefrule property-with-multiple-domains)
;	(undefrule property-with-multiple-ranges)
;	(undefrule create-non-existing-classes_create-candidate-class)
;	(undefrule create-non-existing-classes_create-slots-type-Literal)
;	(undefrule create-non-existing-classes_create-slots-type-Resource)
;	(undefrule create-non-existing-classes_create-slots-type-no-range)
;	(undefrule generate-non-existing-classes_create-create-final-class)
;	(undefrule put-new-properties-no-domain)
;	(undefrule put-new-properties-with-one-domain)
;	(undefrule insert-new-property-no-domain-Literal)
;	(undefrule insert-new-property-no-domain-Resource)
;	(undefrule insert-new-property-no-domain-no-range)
;	(undefrule insert-new-property-one-domain-Literal)
;	(undefrule insert-new-property-one-domain-Resource)
;	(undefrule insert-new-property-one-domain-no-range)
;	(undefrule put-remaining-triples-container-membership-properties)
;	(undefrule put-remaining-triples-properties)
;	(undefrule put-remaining-triples-subjects-with-domain)
;	(undefrule put-remaining-triples-subjects-no-domain)
;	(undefrule put-remaining-triples-subjects-wrong-domain)
;	(undefrule put-remaining-triples-objects-with-range)
;	(undefrule put-remaining-triples-objects-no-range)
;	(undefrule add-extra-superclass)
;	(undefrule insert-extra-superclass)
)

(deffunction build-undefinitions ()
	(bind ?*undef_rules* (create$))
	(open (str-cat ?*R-DEVICE_PATH* "triple-transformation.clp") ttt "r")
	(bind ?line (readline ttt))
	(while (neq ?line EOF)
	   do
		;(verbose "line: " ?line crlf)
	   	(bind ?pos (str-index defrule ?line))
	   	(if (integerp ?pos)
	   	   then
	   	   	;(verbose "pos: " ?pos crlf)
	   	   	(bind ?line (sub-string (+ ?pos 8) (length ?line) ?line))
	   	   	;(verbose "new line: " ?line crlf)
	   	   	(bind ?pos (str-index " " ?line))
	   	   	;(verbose "new pos: " ?pos crlf)
	   	   	(if (integerp ?pos)
	   	   	   then
	   	   		(bind ?rule (sym-cat (sub-string 1 (- ?pos 1) ?line)))
	   	   	   else
	   	   	   	(bind ?rule (sym-cat ?line))
	   	   	)
	   	   	;(verbose "rule: " ?rule  crlf)
			(bind ?*undef_rules* (create$ ?*undef_rules* ?rule))
		)
		(bind ?line (readline ttt))
	)
	(close ttt)
)

(deffunction import ()
	(build-undefinitions)
   	(set-strategy mea)
 	;(bind ?*triple_counter* (length$ (get-template-specific-facts triple (get-fact-list))))
 	(while (> ?*triple_counter* 0)
 	   do
		(bind ?no-of-triples-before (+ ?*triple_counter* 1))
		(bind ?no-of-triples-after ?*triple_counter*)
		(while (> ?no-of-triples-before ?no-of-triples-after)
		   do
		   	(bind ?no-of-triples-before ?no-of-triples-after)
		   	(run-goal create-instances)
			(run-goal put-slot-values)
			(run-goal property-inheritance)
			(run-goal multiple-domains-ranges)
			(run-goal create-new-classes)
			(run-goal generate-new-classes)
			(bind ?no-of-triples-after ?*triple_counter*)
		)
		(run-goal put-new-properties)
 		(bind ?redef-classes (length$ (get-template-specific-facts redefined-class (get-fact-list))))
		(if (> ?redef-classes 0)
		   then
			;(verbose  "After 1st set of rules!" crlf)
			;(undefrule *)
			(undefine-rules)
			;(verbose  "Undefined rules!" crlf)
			(undefine-functions)
			;(verbose  "Undefined functions!" crlf)
			(if (member$ rdf_classes (get-definstances-list))
			   then
				(undefinstances rdf_classes)
			)
			;(verbose  "Undefined definstances!" crlf)
			(undefine-classes)
			;(verbose  "Undefined classes!" crlf)
			(load* (str-cat ?*R-DEVICE_PATH* "restore-classes.clp"))
			(run-goal restore-classes)
			;(verbose  "Before re-loading files" crlf)
			(load* (str-cat ?*R-DEVICE_PATH* "load-rdf.clp"))
			(load* (str-cat ?*R-DEVICE_PATH* "triple-transformation.clp"))
			;(verbose  "After re-loading files" crlf)
			(run-goal put-slot-values)
		)
		(run-goal put-remaining-triples)
	)
	TRUE
)

