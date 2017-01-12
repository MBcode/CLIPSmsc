

(deffunction export_rdf (?file $?initial-classes)
	(bind ?entities (str-cat
		  	"     <!ENTITY rdf \"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">%n"
		  	"     <!ENTITY rdfs \"http://www.w3.org/2000/01/rdf-schema#\">%n"
		  	"     <!ENTITY r_device \"http://startrek.csd.auth.gr/r-device/export/" ?file "#\"> %n"
	))
	(bind ?namespaces (str-cat
		"     xmlns:rdf='&rdf;'%n"
		"     xmlns:rdfs='&rdfs;'%n" 
		"     xmlns:r_device='&r_device;'%n"
	))
	(bind $?classes-schema $?initial-classes)
	(bind $?examined-classes-schema (create$))
	(bind $?classes-instances $?initial-classes)
	(bind ?class-text "")
	(bind $?properties-text (create$))
	(while (> (length$ $?classes-schema) 0)
	   do
	   	(bind ?class (nth$ 1 $?classes-schema))
	   	(bind $?examined-classes-schema (create$ $?examined-classes-schema ?class))
	   	(bind $?classes-schema (rest$ $?classes-schema))
		(if (is_derived ?class)
		   then
		   	(bind ?namespace-text "&r_device;")
		   	(bind ?namespace "r_device:")
			(bind ?class-text (str-cat ?class-text "<rdfs:Class rdf:about='" ?namespace-text ?class "'>%n"))
			(bind ?class-instance (symbol-to-instance-name ?class))
			(if (instance-existp ?class-instance)
			   then
				(bind ?meta-class (class ?class-instance))
				(bind $?subclasses (send ?class-instance get-rdfs:subClassOf))
				(bind $?class-properties (delete-member$ (user-slots ?meta-class) rdfs:subClassOf))
				(while (> (length$ $?class-properties) 0)
				   do
				   	(bind ?class-property (nth$ 1 $?class-properties))
				   	(bind $?values (funcall send ?class-instance (sym-cat get- ?class-property)))
				   	(while (> (length$ $?values) 0)
				   	   do
				   	   	(bind ?class-text (str-cat ?class-text 
				   	   		"     <" ?class-property " rdf:resource='" (nth$ 1 $?values) "'/>%n"))
				   	   	(bind $?values (rest$ $?values))
				   	)
				   	(bind $?class-properties (rest$ $?class-properties))
				)
			   else
				(bind $?subclasses (delete-member$ (class-subclasses ?class) RDF-CLASS DERIVED-CLASS TYPED-CLASS))
			)
			(while (> (length$ $?subclasses) 0)
			   do
			   	(bind ?class-text (str-cat ?class-text "     <rdfs:subClassOf rdf:resource='" (nth$ 1 $?subclasses) "'/>%n"))
			   	(bind $?subclasses (rest$ $?subclasses))
			)
			(bind ?class-text (str-cat ?class-text "</rdfs:Class>%n"))
			(bind $?properties (user-slots ?class))
			(bind ?end (length$ $?properties))
			(loop-for-count (?n 1 ?end)
			   do
			   	(bind ?property (nth$ ?n $?properties))
			   	(bind ?property-instance (symbol-to-instance-name ?property))
				(bind ?property-text (str-cat 
					"<rdf:Property rdf:about='" ?namespace-text ?property "'>%n" 
					"     <rdfs:domain rdf:resource='" ?namespace-text ?class "'/>%n" ))
				(if (instance-existp ?property-instance)
				   then
			   		(bind $?ranges (send ?property-instance get-rdfs:range))
			   		(bind $?superproperties (send ?property-instance get-rdfs:subPropertyOf))
			   	   else
					(bind $?types (slot-types ?class ?property))
					(if (eq $?types (create$ INSTANCE-NAME))
					   then
					   	(bind ?referenced-class (get-type-of ?class ?property))
					   	(if (is-system-class ?referenced-class)
					   	   then
					   	   	(bind $?ranges (class-superclasses ?referenced-class))
					   	   else
					   		(bind $?ranges (create$ ?referenced-class))
					   	)
					   	(bind ?r-end (length$ $?ranges))
					   	(loop-for-count (?r 1 ?r-end)
					   	   do
					   	   	(bind ?r-class (nth$ ?r $?ranges))
					   		(if (not (member$ ?r-class $?examined-classes-schema))
					 	  	   then
					   			;(bind $?classes-instances (create$ $?classes-instances ?referenced-class))
					   			(if (is_derived ?r-class)
					  	 		   then
					   				(bind $?classes-schema (create$ $?classes-schema ?r-class))
					   			   else
					   			   	(bind ?ns (get-namespace ?r-class))
					   			   	(if (not (str-index ?ns ?entities))
					   			   	   then
					   			   		(bind ?uri (get-ns-uri ?ns))
					   			  		(bind ?entities (str-cat ?entities
			  								"     <!ENTITY " ?ns " \"" ?uri "\">%n"))
										(bind ?namespaces (str-cat ?namespaces
											"     xmlns:" ?ns "='&" ?ns ";'%n"))
									)
								)
							)
						)
					   else
					   	(if (same-set$ (create$ $?types $$$ SYMBOL STRING))
					   	   then
					   		(bind $?ranges (create$ rdfs:Literal))
					   	   else
					   	   	(bind $?ranges (create$))
					   	)
					)
					(bind $?superproperties (create$))
				)
				(while (> (length$ $?ranges) 0)
				   do
				   	(bind ?property-text (str-cat ?property-text 
				   	        "     <rdfs:range  rdf:resource='" (nth$ 1 $?ranges) "'/>%n" ))
				   	(bind $?ranges (rest$ $?ranges))
				)
				(while (> (length$ $?superproperties) 0)
				   do
				   	(bind ?property-text (str-cat ?property-text 
				   	        "     <rdfs:subPropertyOf  rdf:resource='" (nth$ 1 $?superproperties) "'/>%n" ))
				   	(bind $?superproperties (rest$ $?superproperties))
				)
				(bind ?property-text (str-cat ?property-text "</rdf:Property>%n"))
				(bind $?properties-text (create$ $?properties-text ?property-text))
				;(bind $?properties (rest$ $?properties))
			)
		   else
			(bind ?ns (get-namespace ?class))
		   	(if (not (str-index ?ns ?entities))
		   	   then
		   		(bind ?uri (get-ns-uri ?ns))
		   		(bind ?entities (str-cat ?entities
		   			"     <!ENTITY " ?ns " \"" ?uri "\">%n"))
		   		(bind ?namespaces (str-cat ?namespaces
		   			"     xmlns:" ?ns "='&" ?ns ";'%n"))
			)
		)
	)
;	(open (str-cat "C:\\Program Files\\Apache\\r-device\\export\\" ?file) ttt "w")
	(open (str-cat ?file) ttt "w")
	(printout ttt
		"<?xml version='1.0' encoding=\"UTF-8\"?>" crlf
		"<!DOCTYPE rdf:RDF [" crlf)
	(format ttt ?entities)
	(printout ttt 
		"]>" crlf crlf)
	(printout ttt 
		"<rdf:RDF" crlf)
	(format ttt ?namespaces)
	(printout ttt 
		">" crlf crlf)
	(format ttt ?class-text)
	(printout ttt crlf)
	(while (> (length$ $?properties-text) 0)
	   do
	   	(format ttt (nth$ 1 $?properties-text))
		(printout ttt crlf)
	   	(bind $?properties-text (rest$ $?properties-text))
	)
	; Add actual result objects
	(bind $?printed-instances (create$))
	(bind $?not-printed-instances (create$))
	(while (> (length$ $?classes-instances) 0)
	  do
	  	(bind ?class (nth$ 1 $?classes-instances))
	  	(bind $?classes-instances (rest$ $?classes-instances))
	  	(if (is_derived ?class)
		   then
		   	(bind ?namespace "r_device:")
		   else
		   	(bind ?namespace "")
		)
		(bind ?inst-counter 1)
		(bind $?properties (user-slots ?class))
		(bind ?end (length$ $?properties))
		(do-for-all-instances ((?x ?class)) TRUE
			(bind $?printed-instances (create$ $?printed-instances ?x))
			(if (is_derived ?class)
			   then
			   	(bind ?uri (str-cat "http://startrek.csd.auth.gr/r-device/export/" ?file "#" ?class ?inst-counter))
				(bind ?inst-counter (+ ?inst-counter 1))
				(printout ttt "<" ?namespace ?class " rdf:about=\"" ?uri "\">" crlf)
			   else
			   	; The following is to cater for classes as instances as well
			   	; It must be completed in the future.
			   	;(if (class-existp (instance-name-to-symbol ?x))
			   	;   then
			   	;   	(if (eq ?x:source rdf)
			   	;   	   then
			   	;   		(bind ?ns (get-namespace (instance-name-to-symbol ?x)))
			   	;   		(bind ?label (get-label (instance-name-to-symbol ?x)))
				;   		(bind ?uri (str-cat (get-ns-uri ?ns) ?label))
				;   	   else
				;	)
				;   	   	
				;   else
				; )
				(bind ?uri ?x:uri)
			   	(printout ttt "<rdf:Description about=\"" ?uri "\">" crlf)
			)
			(loop-for-count (?n 1 ?end)
			   do
			   	(bind ?property (nth$ ?n $?properties))
			   	(if (is-multislot ?class ?property)
			   	   then
			   		(bind $?values ?x:?property)
				   	(while (> (length$ $?values) 0)
				   	   do
				   		(printout ttt "     <" ?namespace ?property ">")
				   		(bind ?val (nth$ 1 $?values))
				   		(if (instancep ?val)
				   		   then
				   		   	(bind ?value (send ?val get-uri))
				   		   	(if (not (member$ ?val $?printed-instances))
				   		   	   then
								(bind $?not-printed-instances (create$ $?not-printed-instances ?val))
							)
				   		   else
				   		   	(bind ?value ?val)
				   		)
				   		(printout ttt ?value)
				   		(printout ttt "</" ?namespace ?property ">" crlf)
				   		(bind $?values (rest$ $?values))
				   	)
				   else
				   	(bind ?val ?x:?property)
				   	(if (neq ?val nil)
				   	   then
				   	   	(printout ttt "     <" ?namespace ?property ">")
				   	   	(if (instancep ?val)
				   		   then
				   		   	(bind ?value (send ?val get-uri))
				   		   	(if (not (member$ ?val $?printed-instances))
				   		   	   then
								(bind $?not-printed-instances (create$ $?not-printed-instances ?val))
							)
				   		   else
				   		   	(bind ?value ?val)
				   		)
				   		(printout ttt ?value)
				   	   	(printout ttt "</" ?namespace ?property ">" crlf)
				   	)
				)
			)
			(if (is_derived ?class)
			   then
				(printout ttt "</" ?namespace ?class ">" crlf crlf)
			   else
				(printout ttt "</rdf:Description>" crlf crlf)
			)
		)
	)
	(bind ?inst-counter 1)
	(while (> (length$ $?not-printed-instances) 0)
	   do
	   	(bind ?x (nth$ 1 $?not-printed-instances))
	   	(bind $?not-printed-instances (rest$ $?not-printed-instances))
	   	(bind $?printed-instances (create$ $?printed-instances ?x))
	   	(bind ?class (class ?x))
		(bind $?properties (user-slots ?class))
		(bind ?end (length$ $?properties))
	  	(if (is_derived ?class)
		   then
		   	(bind ?namespace "r_device:")
		   	(bind ?uri (str-cat "http://startrek.csd.auth.gr/r-device/export/" ?file "#" ?class ?inst-counter))
			(bind ?inst-counter (+ ?inst-counter 1))
			(printout ttt "<" ?namespace ?class " rdf:about=\"" ?uri "\">" crlf)
		   else
		   	(bind ?namespace "")
		   	; The following must be completed in the future 
		   	; to cater for classes as instances as well
			(bind ?uri (send ?x get-uri))
		   	(printout ttt "<rdf:Description about=\"" ?uri "\">" crlf)
		)
		(loop-for-count (?n 1 ?end)
		   do
		   	(bind ?property (nth$ ?n $?properties))
		   	(if (is-multislot ?class ?property)
		   	   then
		   		(bind $?values (funcall send ?x (sym-cat get- ?property)))
			   	(while (> (length$ $?values) 0)
			   	   do
			   		(printout ttt "     <" ?namespace ?property ">")
			   		(bind ?val (nth$ 1 $?values))
			   		(if (instancep ?val)
			   		   then
			   		   	(bind ?value (send ?val get-uri))
			   		   	(if (not (member$ ?val $?printed-instances))
			   		   	   then
							(bind $?not-printed-instances (create$ $?not-printed-instances ?val))
						)
			   		   else
			   		   	(bind ?value ?val)
			   		)
			   		(printout ttt ?value)
			   		(printout ttt "</" ?namespace ?property ">" crlf)
			   		(bind $?values (rest$ $?values))
			   	)
			   else
			   	(bind ?val (funcall send ?x (sym-cat get- ?property)))
			   	(if (neq ?val nil)
			   	   then
			   	   	(printout ttt "     <" ?namespace ?property ">")
			   	   	(if (instancep ?val)
			   		   then
			   		   	(bind ?value (send ?val get-uri))
			   		   	(if (not (member$ ?val $?printed-instances))
			   		   	   then
							(bind $?not-printed-instances (create$ $?not-printed-instances ?val))
						)
			   		   else
			   		   	(bind ?value ?val)
			   		)
			   		(printout ttt ?value)
			   	   	(printout ttt "</" ?namespace ?property ">" crlf)
			   	)
			)
		)
		(if (is_derived ?class)
		   then
			(printout ttt "</" ?namespace ?class ">" crlf crlf)
		   else
			(printout ttt "</rdf:Description>" crlf crlf)
		)
	)
	(printout ttt "</rdf:RDF>" crlf)
	(close ttt)
	TRUE
)