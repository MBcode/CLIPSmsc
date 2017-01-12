; The functions below are new auxiliary functions

(deffunction set-verbose (?status)
	(bind ?*verbose_status* ?status)
)

(deffunction get-verbose ()
	?*verbose_status*
)

(deffunction verbose ($?list)
	(if (eq ?*verbose_status* on)
	   then
	   	(funcall printout t $?list)
	)
)

(deffunction pprint-list ($?list)
	(if (eq ?*verbose_status* on)
	   then
		(bind ?end (length$ $?list))
		(loop-for-count (?n 1 ?end)
			(printout t (nth$ ?n $?list) crlf)
		)
		TRUE
	)
)

(deffunction extract-filename (?filename)
	(bind ?pos (str-index "." ?filename))
	(if (neq ?pos FALSE)
	   then
	   	(return (sym-cat (sub-string 1 (- ?pos 1) ?filename)))
	   else
	   	(return (sym-cat ?filename))
	)
)

(deffunction is-uri (?string)
	;(verbose ?string crlf)
;	(if (and
;		(eq (sub-string 1 1 ?string) "<")
;		(eq (sub-string (length$ ?string) (length$ ?string) ?string) ">"))
	(if (and (lexemep ?string) (eq (sub-string 1 1 ?string) "<"))
	   then
	   	TRUE
	   else
	   	FALSE
	)
)

(deffunction is-parsed-uri (?string)
	(if (lexemep ?string)
	   then
		(bind ?pos (str-index ":" ?string))
		(and
			(symbolp ?string)
			(neq ?pos FALSE)
			(member$ (symbol-to-instance-name (sym-cat (sub-string 1 (- ?pos 1) ?string))) ?*resource_instances*)
			;(not (integerp (str-index ":" (sub-string (+ ?pos 1) (length ?string) ?string))))
		)
	   else
	   	FALSE
	)
)

(deffunction similar-uri (?uri1 ?uri2)
	(bind ?l1 (length$ ?uri1))
	(bind ?l2 (length$ ?uri2))
	(if (and 
		(= (- ?l1 ?l2) 1)
		(or 
			(eq (sub-string ?l1 ?l1 ?uri1) "#")
			(eq (sub-string ?l1 ?l1 ?uri1) "/")
		)
	    )
	   then
	   	(eq (sub-string 1 (- ?l1 1) ?uri1) ?uri2)
	   else
	   	(if (and 
			(= (- ?l2 ?l1) 1)
			(or 
				(eq (sub-string ?l2 ?l2 ?uri1) "#")
				(eq (sub-string ?l2 ?l2 ?uri1) "/")
			)
		    )
		   then
		   	(eq (sub-string 1 (- ?l2 1) ?uri2) ?uri1)
		   else
		   	FALSE
		)
	)
)

(deffunction is-prefix-of (?uri1 ?uri2)
	(bind ?pos (str-index ?uri1 ?uri2))
	(if (or
		(eq ?pos FALSE) 
		(<> ?pos 1)
	    )
	   then
	   	(return FALSE)
	   else
	   	(bind ?suffix (sub-string (+ 1 (length ?uri1)) (length ?uri2) ?uri2))
	   	(if (neq (str-index "/" ?suffix) FALSE)
	   	   then
	   	   	(return FALSE)
	   	   else
	   	   	(return TRUE)
	   	)
	)
)

(deffunction is-rdf-resource (?rid)
	(bind $?docs (send ?rid get-rdfs:isDefinedBy))
	(if (> (length$ $?docs) 0)
	   then
;	   	(if (or	(member$ (instance-address [rdf]) $?docs) (member$ (instance-address [rdfs]) $?docs))
	   	(if (or	(member$ [rdf] $?docs) (member$ [rdfs] $?docs))
	   	   then
	   	   	TRUE
	   	   else
	   	   	FALSE
		)
	   else
	   	FALSE
	)
)

(deffunction compatible-types (?type1 ?type2)
	(or
		(eq ?type2 FALSE)
		(eq ?type1 ?type2)
		(subclassp ?type1 ?type2)
	)
)

(deffunction subclass-of-one (?class $?super-classes)
	(if (member$ ?class $?super-classes)
	   then
	   	TRUE
	   else
	   	(bind ?end (length$ $?super-classes))
	   	(loop-for-count (?n 1 ?end)
	   	   do
	   	   	(if (subclassp ?class (nth$ ?n $?super-classes))
	   	   	   then
	   	   	   	(return TRUE)
	   	   	)
	   	)
	   	(return FALSE)
	)
)

(deffunction superclass-of-one (?class $?sub-classes)
	(if (member$ ?class $?sub-classes)
	   then
	   	TRUE
	   else
	   	(bind ?end (length$ $?sub-classes))
	   	(loop-for-count (?n 1 ?end)
	   	   do
	   	   	(if (superclassp ?class (nth$ ?n $?sub-classes))
	   	   	   then
	   	   	   	(return TRUE)
	   	   	)
	   	)
	   	(return FALSE)
	)
)

(deffunction most-specific-classes ($?classes)
	(bind $?result (create$))
	(while (> (length$ $?classes) 0)
	   do
	   	(if (not 
	   		(or 
	   			(superclass-of-one (nth$ 1 $?classes) (rest$ $?classes))
	   			(superclass-of-one (nth$ 1 $?classes) $?result)
	   		))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ 1 $?classes)))
	   	)
	   	(bind $?classes (rest$ $?classes))
	)
	$?result
)

(deffunction is-only-one-class ($?classes)
	(if (or
		(= (length$ $?classes) 1)
		(= (length$ (most-specific-classes $?classes)) 1))
	   then
	   	;(verbose "classes: " $?classes " are actually only one!" crlf)
	   	(return TRUE)
	   else
	   	;(verbose "classes: " $?classes " are many!" crlf)
	   	(return FALSE)
	)
)

(deffunction get-only-one-class ($?classes)
	(if (= (length$ $?classes) 1)
	   then
	   	;(verbose "classes: " $?classes " are actually only one: " (nth$ 1 $?classes) crlf)
	   	(return (nth$ 1 $?classes))
	   else
	   	;(verbose "classes: " $?classes " are actually only one: " (nth$ 1 (most-specific-classes $?classes)) crlf)
		(return (nth$ 1 (most-specific-classes $?classes)))
	)
)

;(deffunction slot-defined (?slot $?slot-defs)
;	(bind ?end (length$ $?slot-defs))
;	(loop-for-count (?n 1 ?end)
;	   do
;	   	(if (integerp (str-index ?slot (nth$ ?n $?slot-defs)))
;	   	(if (member$ ?slot (explode$ (nth$ ?n $?slot-defs)))
;	   	   then
;	   	   	(return TRUE)
;	   	)
;	)
;	(return FALSE)
;)

(deffunction slot-defined (?slot $?slot-defs)
	(bind ?pos (member$ ?slot $?slot-defs))
	(if (neq ?pos FALSE)
	   then
	   	TRUE
	   else
	   	FALSE
	)
)

(deffunction instances-to-symbols ($?instances)
	(bind $?result (create$))
	(bind ?end (length$ $?instances))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind $?result (create$ $?result (instance-name-to-symbol (nth$ ?n $?instances))))
	)
	$?result
)

(deffunction symbols-to-instances ($?symbols)
	(bind $?result (create$))
	(bind ?end (length$ $?symbols))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind $?result (create$ $?result (symbol-to-instance-name (nth$ ?n $?symbols))))
	)
	$?result
)


;(deffunction super-class-aliases ($?super-classes)
;	(bind $?result (create$))
;	(bind ?end (length$ $?super-classes))
;	(loop-for-count (?n 1 ?end)
;	   do
;	   	(bind $?result (create$ $?result (slot-default-value (nth$ ?n $?super-classes) aliases)))
;	)
;	$?result
;)



(deffunction exists-class-with-super-classes ($?classes)
	(bind ?end (length$ $?classes))
	(bind $?sub-classes (class-subclasses (nth$ 1 $?classes)))
	(loop-for-count (?n 2 ?end)
	   do
	   	(bind $?sub-classes (intersection$ (create$ (class-subclasses (nth$ ?n $?classes)) $$$ $?sub-classes)))
	)
	(bind ?end (length$ $?sub-classes))
	(if (= ?end 0)
	   then
	   	(return FALSE)
	   else
	   	(loop-for-count (?n 1 ?end)
	   	   do
	   	   	(if (same-set$ (create$ (class-superclasses  (nth$ ?n $?sub-classes)) $$$ $?classes))
	   	   	   then
	   	   	   	(return (nth$ ?n $?sub-classes))
	   	   	)
	   	)
	   	(return FALSE)
	)
)

(deffunction unique-instance-name (?namespace)
	;(verbose  "namespace: " ?namespace crlf)
	(if (instance-existp (symbol-to-instance-name ?namespace))
   	   then
	   	(bind ?n 1)
	   	(bind ?new-namespace (symbol-to-instance-name (sym-cat ?namespace ?n)))
	   	(while (instance-existp ?new-namespace)
	   	   do
	   	   	(bind ?n (+ ?n 1))
	   	   	(bind ?new-namespace (symbol-to-instance-name (sym-cat ?namespace ?n)))
	   	)
	   	(return ?new-namespace)
	   else
	   	(return (symbol-to-instance-name ?namespace))
	)
)



(deffunction resource-existp (?resource)
	(if (symbolp ?resource)
	   then
		(if (is-uri ?resource)
		   then
			(member$ ?resource ?*resource_instances*)
		   else
			(instance-existp (symbol-to-instance-name ?resource))
		)
	   else
	   	(return FALSE)
	)
)

(deffunction resource-instance (?resource)
	(if (is-uri ?resource)
	   then
	   	(nth$ (+ (member$ ?resource ?*resource_instances*) 1) ?*resource_instances*)
		   
	   else
		(symbol-to-instance-name ?resource)
	)
)

(deffunction resource-class (?resource)
	(class (resource-instance ?resource))
)

(deffunction triple-retract (?fid)
	(retract ?fid)
	(bind ?*triple_counter* (- ?*triple_counter* 1))
)

(deffunction is-membership-property (?prop)
	(if (symbolp ?prop)
	   then
	   	(bind ?pos (str-index ":" ?prop))
	   	(if (neq ?pos FALSE)
	   	   then
	   	   	(bind ?ns (sub-string 1 (- ?pos 1) ?prop))
	   	   	(bind ?label (sub-string (+ ?pos 1) (length ?prop) ?prop))
	   	   	(bind ?doc (nth$ 1 (send (symbol-to-instance-name (sym-cat ?ns)) get-rdfs:isDefinedBy)))
	   	   	(if	(and
	   	   			(eq ?doc [rdf])
	   	   			(eq (sub-string 1 1 ?label) "_")
	   	   			(numberp (string-to-field (sub-string 2 (length ?label) ?label)))
	   	   		)
	   	   	   then
	   	   	   	TRUE
	   	   	   else
	   	   	   	FALSE
	   	   	)
	   	   else
	   	   	FALSE
	   	)
	   else
	   	FALSE
	)
)

(deffunction get-namespace (?class)
	(bind ?pos (str-index ":" ?class))
	(if (integerp ?pos)
	   then
	   	(return (sym-cat (sub-string 1 (- ?pos 1) ?class)))
	   else
	   	(return FALSE)
	)
)

(deffunction get-label (?class)
	(bind ?pos (str-index ":" ?class))
	(if (integerp ?pos)
	   then
	   	(return (sym-cat (sub-string (+ ?pos 1) (length ?class) ?class)))
	   else
	   	(return FALSE)
	)
)

(deffunction get-ns-uri (?ns)
	(if (not (instancep ?ns))
	   then
	   	(bind ?ns (symbol-to-instance-name ?ns))
	)
	(bind ?uri (send ?ns get-uri))
	(if (eq ?uri "")
	   then
	   	(return (get-ns-uri (send ?ns get-rdfs:isDefinedBy)))
	   else
	   	(return ?uri)
	)
)

(deffunction is-system-class (?class)
	(if (instance-existp (symbol-to-instance-name ?class))
	   then
		(return (eq (send (symbol-to-instance-name ?class) source) system))
	   else
	   	(return FALSE)
	)
)

(deffunction file-exists (?file)
	(if (open ?file test "r")
	   then
	   	(close test)
	   	TRUE
	   else
	   	FALSE
	)
)

(deffunction file-empty (?file)
	(if (open ?file test "r")
	   then
	   	;(verbose "File: " ?file crlf)
	   	(bind ?first-line (readline test))
	   	;(verbose "First line: " ?first-line crlf)
	   	(close test)
	   	(if (eq ?first-line EOF)
	   	   then
	   	   	TRUE
	   	   else
	   	   	FALSE
	   	)
	   else
	   	FALSE
	)
)

(deffunction parse-ntriples (?address ?projectname)
	(bind ?filename (str-cat ?projectname ".rdf"))
	(if (not (file-exists ?filename))
	   then
		(verbose "Remote access: " ?address " for namespace: " ?projectname crlf)
		(bind ?command (str-cat "\"" ?*R-DEVICE_PATH* "arp.bat\" " ?address " " ?projectname))
		;(verbose "Command: " ?command crlf)
		(system ?command)
		(if (file-empty ?filename)
		   then
		   	(printout t "There is a problem with URL: " ?address crlf)
		   	(printout t "File: " ?filename " is empty!" crlf)
		   	(remove ?filename)
		   	(remove (str-cat ?projectname ".n3"))
		)
		;(bind ?*fetched_url* (create$ ?*fetched_url* ?address))
	)
)

(deffunction local-parse-ntriples (?projectname)
	(bind ?filename (str-cat ?projectname ".rdf"))
	(bind ?command (str-cat "\"" ?*R-DEVICE_PATH* "arp-only.bat\" " ?projectname))
	;(verbose "Command: " ?command crlf)
	(system ?command)
)


(deffunction shallow-copy (?old-instance ?new-instance)
	(bind ?old-class (class ?old-instance))
	(bind $?slots (delete-member$ (class-slots ?old-class inherit) class-refs aliases))
	(while (> (length$ $?slots) 0)
	   do
	   	(bind ?slot (nth$ 1 $?slots))
	   	(bind ?get-command (sym-cat get- ?slot))
	   	(bind ?put-command (sym-cat put- ?slot))
	   	(bind ?value (funcall send ?old-instance ?get-command))
	   	(funcall send ?new-instance ?put-command ?value)
	   	(bind $?slots (rest$ $?slots))
	)
	TRUE
)

(deffunction discover-disk-letter ()
	(system "cd > dir.txt")
	(open "dir.txt" file "r")
	(bind ?path (readline file))
	(bind ?disk-letter (sub-string 1 1 ?path))
	(close file)
	(remove "dir.txt")
	(return  ?disk-letter)
)
