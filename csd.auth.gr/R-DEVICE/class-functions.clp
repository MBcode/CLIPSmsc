
(deffunction is-multislot (?class ?slot)
	(eq (nth$ 1 (slot-facets ?class ?slot)) MLT)
)

(deffunction get-all-derived-classes ()
;	(bind $?classes (remove-duplicates$ (class-subclasses DERIVED-CLASS inherit)))
	(remove-duplicates$ (class-subclasses DERIVED-CLASS inherit))
;	(bind $?result (create$))
;	(while (> (length$ $?classes) 0)
;	   do
;		(bind $?result (create$ $?result (nth$ 1 $?classes)))
;	   	(bind $?classes (create$ (class-subclasses (nth$ 1 $?classes)) (rest$ $?classes)))
;	)
;	$?result
)

(deffunction is-subsumed (?class $?classes)
	(bind ?end (length$ $?classes))
	(loop-for-count (?n 1 ?end)
	   do
		(if (subclassp ?class (nth$ ?n $?classes))
		   then
		   	(return TRUE)
		)
	)
	(return FALSE)
)

(deffunction remove-subsumed-classes ($?classes)
	(bind $?result (create$))
	(bind ?end (length$ $?classes))
	(loop-for-count (?n 1 ?end)
	   do
	   	;(bind ?next-class (nth$ ?n $?classes))
	   	;(bind $?rest-classes (subseq$ $?classes (+ ?n 1) ?end))
	   	;(bind ?RESULT (is-subsumed ?next-class $?rest-classes))
	   	;(verbose "next-class: " ?next-class crlf)
	   	;(verbose "rest-classes: " $?rest-classes crlf)
	   	;(verbose "RESULT: " ?RESULT crlf)
	   	;(if (not ?RESULT)
	   	(if (and
	   		(not (is-subsumed (nth$ ?n $?classes) (subseq$ $?classes (+ ?n 1) ?end)))
	   		(not (is-subsumed (nth$ ?n $?classes) $?result)))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ ?n $?classes)))
	   	)
	)
	$?result
)

(deffunction no-of-derived-objects ()
	(bind $?classes (get-all-derived-classes))
	(bind ?result 0)
	(while (> (length$ $?classes) 0)
	   do
	   	(bind ?result (+ ?result (length$ (find-all-instances ((?x (nth$ 1 $?classes))) TRUE))))
	   	(bind $?classes (rest$ $?classes))
	)
	?result
)
	   
(deffunction exists-class (?class)
	(or
		(member$ ?class (get-deftemplate-list))
		(class-existp ?class)
	)
)

(deffunction exist-classes ($?classes)
	(bind $?result (create$))
	(bind ?end (length$ $?classes))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (not (exists-class (nth$ ?n $?classes)))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ ?n $?classes)))
	   	)
	)
	$?result
)

(deffunction get-template-specific-facts (?template $?facts)
	(bind $?result (create$))
	(bind ?end (length$ $?facts))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (eq (fact-relation (fact-index (nth$ ?n $?facts))) ?template)
	   	   then
	   	   	(bind $?result (create$ $?result (fact-index (nth$ ?n $?facts))))
	   	)
	)
	$?result
)

(deffunction get-slot-value-specific-facts (?slot ?value $?facts)
	(bind $?result (create$))
	(bind ?end (length$ $?facts))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (or 
	   		(eq (fact-slot-value (nth$ ?n $?facts) ?slot) ?value)
	   		(and 
	   			(eq (type (fact-slot-value (nth$ ?n $?facts) ?slot)) MULTIFIELD)
	   			(member$ ?value (fact-slot-value (nth$ ?n $?facts) ?slot))))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ ?n $?facts)))
	   	)
	)
	$?result
)


(deffunction get-specific-facts (?template ?slot ?value)
	;(bind $?all-facts (get-fact-list))
	;(bind $?temp-relative-facts (get-template-specific-facts ?template (get-fact-list)))
	(get-slot-value-specific-facts ?slot ?value (get-template-specific-facts ?template (get-fact-list)))
)


(deffunction check-non-existent-classes ($?cond-element)
)

(deffunction check-non-existent-classes-one ($?cond-element)
   	(if (or 
   		(eq (nth$ 2 $?cond-element) not)
   		(eq (nth$ 2 $?cond-element) or)
   		(eq (nth$ 2 $?cond-element) and))
   	   then
   	   	(check-non-existent-classes (subseq$ $?cond-element 3 (- (length$ $?cond-element) 1)))
   	   else
		(if (eq (nth$ 2 $?cond-element) <-)
		   then
			(if (eq (nth$ 4 $?cond-element) object)
		   	   then
		   		(bind ?class (nth$ (+ (member$ is-a $?cond-element) 1) $?cond-element))
		   	   else
		   	   	(bind ?class (nth$ 4 $?cond-element))
		   	)
		   else
			(if (eq (nth$ 2 $?cond-element) object)
		   	   then
		   		(bind ?class (nth$ (+ (member$ is-a $?cond-element) 1) $?cond-element))
		   	   else
		   	   	(bind ?class (nth$ 2 $?cond-element))
		   	)
		)
      		(if (not (exists-class ?class))
      	   	   then
      	   	   	?class
      	   	   else
      	   	   	(create$)
      	   	)
      	)
)


(deffunction check-non-existent-classes ($?cond-element)
	(bind $?result (create$))
	(while (> (length$ $?cond-element) 0)
	   do
   	   	(bind ?p2 (get-token $?cond-element))
		(bind $?result (create$ $?result (check-non-existent-classes-one (subseq$ $?cond-element 1 ?p2))))
		(bind $?cond-element (subseq$ $?cond-element (+ ?p2 1) (length$ $?cond-element)))
	)
	$?result
)

(deffunction get-type-of (?class ?slot)
	(bind $?class-refs (slot-default-value ?class class-refs))
	(bind ?pos (member$ ?slot $?class-refs))
	(if (numberp ?pos)
	   then
		(nth$ (+ ?pos 1) $?class-refs)
	   else
	   	FALSE
	)
)

(deffunction is_derived (?class)
	(> (length$ (get-specific-facts derived-class name ?class)) 0)
)

(deffunction is_namespace (?namespace)
	(> (length$ (get-specific-facts namespace name ?namespace)) 0)
)

(deffunction all-instance-existp ($?list)
	(bind ?end (length$ $?list))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (not (instance-existp (nth$ ?n $?list)))
	   	   then
	   	   	(return FALSE)
	   	)
	)
	TRUE
)

(deffunction user-slots (?class)
	(bind $?candidate-slots (delete-member$ (class-slots ?class inherit) counter derivators class-refs namespace source uri aliases))
	(if (slot-existp ?class aliases inherit)
	   then
	   	(bind $?aliased-slots (slot-default-value ?class aliases))
		(bind $?result (create$))
		(bind ?end (length$ $?candidate-slots))
		(loop-for-count (?n 1 ?end)
		   do
		   	(if (not (odd-member$ (nth$ ?n $?candidate-slots) $?aliased-slots))
		   	   then
		   	   	(bind $?result (create$ $?result (nth$ ?n $?candidate-slots)))
		   	)
		)
		(return $?result)
	   else
	   	(return $?candidate-slots)
	)
)

(deffunction aliased-slot (?class ?slot)
	(if (and (class-existp ?class) (slot-existp ?class aliases inherit))
	   then
	   	(bind $?aliased-slots (slot-default-value ?class aliases))
		(if (odd-member$ ?slot $?aliased-slots)
		   then
		   	(return TRUE)
		   else
		   	(return FALSE)
		)
	   else
	   	(return FALSE)
	)
)

(deffunction normal-slot (?class ?slot)
	(bind $?all-slots (delete-member$ (class-slots ?class inherit) counter derivators class-refs namespace source uri aliases))
	(if (not (member$ ?slot $?all-slots))
	   then
	   	(return FALSE)
	   else
		(if (slot-existp ?class aliases inherit)
		   then
		   	(bind $?aliased-slots (slot-default-value ?class aliases))
			(if (odd-member$ ?slot $?aliased-slots)
			   then
		   		(return FALSE)
			   else
		   		(return TRUE)
			)
		   else
	   		(return TRUE)
		)
	)
)

(deffunction aliases-of (?class ?alias)
	(bind $?aliased-slots (slot-default-value ?class aliases))
	(associate-pairs ?alias $?aliased-slots)
)

(deffunction needs-redefinition (?class $?slot-defs)
	(bind ?CHANGE FALSE)
	(bind $?result (create$))
	(bind $?copy-slot-defs $?slot-defs)
	(while (> (length$ $?copy-slot-defs) 0)
	   do
	   	(bind ?p2 (get-token $?copy-slot-defs))
	   	(bind $?first-slot-def (subseq$ $?copy-slot-defs 1 ?p2))
	   	(bind ?slot (nth$ 3 $?first-slot-def))
	   	(bind $?new-slot-types (subseq$ $?first-slot-def 6 (- (length$ $?first-slot-def) 2)))
	   	(bind $?old-slot-types (slot-types ?class ?slot))
		(if (or
			(and (eq $?new-slot-types (create$ "?VARIABLE")) (same-set$ (create$ $?old-slot-types $$$ FLOAT INTEGER SYMBOL STRING EXTERNAL-ADDRESS FACT-ADDRESS INSTANCE-ADDRESS INSTANCE-NAME)))
			(subsetp $?new-slot-types $?old-slot-types))
		   then
			(bind $?result (create$ $?result "(" (nth$ 2 $?first-slot-def) ?slot "(" type $?old-slot-types ")" ")"))
		   else
		   	(bind $?result (create$ $?result "(" (nth$ 2 $?first-slot-def) ?slot "(" type (remove-duplicates$ (create$ $?new-slot-types $?old-slot-types)) ")" ")"))
		   	(bind ?CHANGE TRUE)
		)
	   	(bind $?copy-slot-defs (subseq$ $?copy-slot-defs (+ ?p2 1) (length$ $?copy-slot-defs)))
	)
	(if (eq ?CHANGE FALSE)
	   then
		(return FALSE)
	   else
	   	(return $?result)
	)
)

(deffunction backup-rules ($?rules)
	(bind ?end (length$ $?rules))
	(bind ?backup-file "device-rules-backup.clp")
	;(assert (rule-backup-file ?backup-file))
	(my-dribble-on ?backup-file)
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind ?rule-fact (nth$ ?n $?rules))
	   	;(verbose "backup Rule fact: " crlf)
	   	;(facts ?rule-fact ?rule-fact)
	   	(bind ?rule (fact-slot-value ?rule-fact name))
	   	(bind ?del-rule (fact-slot-value ?rule-fact del-name))
	   	(if (neq ?rule nil)
	   	   then
	   		(ppdefrule ?rule)
	   		(ppdefrule ?del-rule)
	   	)
	)
	(my-dribble-off)
)

(deffunction undefine-rules-aux ($?rules)
	(bind ?end (length$ $?rules))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind ?rule-fact (nth$ ?n $?rules))
	   	;(verbose "undefine Rule fact: " crlf)
	   	;(facts ?rule-fact ?rule-fact)
	   	(bind ?rule (fact-slot-value ?rule-fact name))
	   	(bind ?del-rule (fact-slot-value ?rule-fact del-name))
	   	(if (neq ?rule nil)
	   	   then
	   		(undefrule ?rule)
	   		(undefrule ?del-rule)
	   	)
	)
)

(deffunction undefine-class-rules (?class)
	(bind $?rules (remove-duplicates$ (create$ 
		(get-specific-facts deductive-rule implies ?class)
		(get-specific-facts derived-attribute-rule implies ?class)
		(get-specific-facts aggregate-attribute-rule implies ?class)
		(get-specific-facts deductive-rule depends-on ?class)
		(get-specific-facts derived-attribute-rule depends-on ?class)
		(get-specific-facts aggregate-attribute-rule depends-on ?class)
	)))
	;(verbose "Rules: " $?rules crlf)
	(backup-rules $?rules)
	;(verbose "Rules have been backep up!" crlf)
	(undefine-rules-aux $?rules)
)

; The following "empty" declarations serve the purpose 
; of forward declaration for functions defined in RDF\import.clp
(deffunction backup-class (?class)
)

(deffunction undefine-classes ()
)

(deffunction collect-defaults (?slot $?super-classes)
	(bind $?result (create$))
	(bind ?end (length$ $?super-classes))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind $?result (create$ $?result (slot-default-value (nth$ ?n $?super-classes) ?slot)))
	)
	$?result
)


(deffunction restore-class (?class)
	(bind ?class-factid (nth$ 1 (get-specific-facts redefined-class name ?class)))
	(bind $?super-classes (fact-slot-value ?class-factid isa-slot))
	(bind $?slot-defs (fact-slot-value ?class-factid slot-definitions))
	(bind $?class-refs (fact-slot-value ?class-factid class-refs-defaults))
	(bind $?aliases (fact-slot-value ?class-factid aliases-defaults))
  	(verbose  "Restoring class: " ?class crlf)
	(my-build (str-cat$ 
		"(" defclass ?class
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
				"(" default (unique-pairs (create$ $?class-refs (collect-defaults class-refs $?super-classes))) ")"
			")"
			"(" multislot aliases 
				"(" source composite ")"
				"(" default (unique-pairs (create$ $?aliases (collect-defaults aliases $?super-classes))) ")"
			")"
		")"
	))
	(retract ?class-factid)
	(bind ?backup-instances-file-factid (nth$ 1 (get-template-specific-facts backup-instances (get-fact-list))))
	(bind ?instances-backup-file (nth$ 1 (fact-slot-value ?backup-instances-file-factid implied)))
  	(restore-instances ?instances-backup-file)
  	(retract ?backup-instances-file-factid)
  	(remove ?instances-backup-file)
)



(deffunction re-define-class (?class $?slot-defs)
	(backup-class ?class)   ; this asserts redefined-class fact
	;(verbose ?class " is backed up!" crlf)
	(bind ?factid (nth$ 1 (get-specific-facts redefined-class name ?class)))
	;(verbose "Before: " crlf)
	;(facts ?factid ?factid)
	(modify ?factid (slot-definitions $?slot-defs))
	;(bind ?factid (nth$ 1 (get-specific-facts redefined-class name ?class)))
	;(verbose "After: " crlf)
	;(facts ?factid ?factid)
	(undefine-class-rules ?class)
	(undefine-classes)  ; this needs class-to-undefine fact
	;(load* (str-cat ?*RDF_PATH* "restore-classes.clp"))
	;(verbose "before running restoring rules" crlf)
	;(watch facts)
	;(watch rules)
	;(watch activations)
	(restore-class ?class)
	;(unwatch all)
	;(verbose "after running restoring rules" crlf)
	;(bind ?backup-file-factid (nth$ 1 (get-template-specific-facts rule-backup-file (get-fact-list))))
	;(verbose "backup-file-factid: " ?backup-file-factid crlf)
	;(facts ?backup-file-factid ?backup-file-factid)
	;(bind ?rule-backup-file (str-cat (nth$ 1 (fact-slot-value ?backup-file-factid implied)) 1))
	;(bind ?rule-backup-file (nth$ 1 (fact-slot-value ?backup-file-factid implied)))
	;(retract ?backup-file-factid)
	;(load* ?rule-backup-file)
	(load* "device-rules-backup.clp")
	;(remove ?rule-backup-file)
	(remove "device-rules-backup.clp")
)
