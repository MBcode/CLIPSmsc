(deffunction resource-make-instance (?class ?resource $?types)
	;(verbose  "class: " ?class " resource: " ?resource crlf)
  	(if (is-uri ?resource)
  	   then
  	   	(bind ?inst (gensym*))
  		(make-instance ?inst of ?class
  			(rdf:type (symbols-to-instances $?types))
  			(uri (sub-string 2 (- (length$ ?resource) 1) ?resource))
  		)
  		(return ?inst)
  	   else
  		(make-instance ?resource of ?class
  			(rdf:type (symbols-to-instances $?types))
  		)
  		(return ?resource)
  	)
)

(deffunction find-all-super-properties (?property)
	(bind $?result (send (symbol-to-instance-name ?property) get-rdfs:subPropertyOf))
	(bind $?list $?result)
	(while (> (length$ $?list) 0)
	   do
;	   	(bind $?temp (send (symbol-to-instance-name (nth$ 1 $?list)) get-rdfs:subPropertyOf))
	   	(bind $?temp (send (nth$ 1 $?list) get-rdfs:subPropertyOf))
	   	(bind $?result (create$ $?result $?temp))
	   	(bind $?list (create$ $?temp (rest$ $?list)))
	)
	(instances-to-symbols $?result)
)

(deffunction create-aliases (?slot $?aliases)
	(bind $?result (create$))
	(bind ?end (length$ $?aliases))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind $?result 
	   		(create$ 
	   			$?result 
	   			(nth$ ?n $?aliases) ?slot 
	   			(create-aliases ?slot (find-all-super-properties (nth$ ?n $?aliases)))
	   		)
	   	)
	)
	$?result
)




(deffunction import-resource (?resource)
	(if (is-uri ?resource)
	   then
	   	(bind ?resource (sub-string 2 (- (length$ ?resource) 1) ?resource))
	   	(bind $?existing-resources (find-instance ((?rid rdfs:Resource)) (similar-uri ?rid:uri ?resource)))
	   	(if (= (length$ $?existing-resources) 0)
	   	   then
	   	   	;(verbose  "Resource: " ?resource crlf)
	   	   	;(verbose  "==========================" crlf)
	   	   	(do-for-instance
	   	   		((?rid rdfs:Resource)) 
	   	   		(and
	   	   			(eq ?rid (nth$ 1 ?rid:rdfs:isDefinedBy))
	   	   			(is-prefix-of ?rid:uri ?resource)
	   	   		)
	   	   		;(verbose  (instance-address ?rid) " " ?rid:rdfs:isDefinedBy " " ?rid:uri crlf)
	   	   		(bind ?uri-alias (nth$ 1 ?rid:rdfs:label))
	   	   		(if (eq (sub-string (+ (length ?rid:uri) 1) (+ (length ?rid:uri) 1) ?resource) "#")
	   	   		   then
	   	   			(bind ?label (sub-string (+ (length ?rid:uri) 2) (length ?resource) ?resource))
	   	   		   else
	   	   		   	(bind ?label (sub-string (+ (length$ ?rid:uri) 1) (length ?resource) ?resource))
	   	   		)
	   	   		;(verbose  "resource: " ?resource crlf)
	   	   		;(verbose  "uri: " ?rid:uri crlf)
	   	   		;(verbose  "label: " ?label crlf)
	   	   		;(verbose  ?uri-alias : ?label crlf)
	   	   		(if (eq ?label "")
	   	   		   then
	   	   		   	(return ?uri-alias )
	   	   		   else
	   	   			(return (sym-cat ?uri-alias : ?label ))
	   	   		)
	   	   	)
	   	   	;(verbose  ?uri-alias : ?label crlf)
			;(return (sym-cat ?uri-alias : ?label))
			;(return ?resource)
;			(bind ?namespace (gensym*))
;			(bind ?namespace-id (symbol-to-instance-name ?namespace))
;			(make-instance ?namespace-id of rdfs:Resource
;				(rdfs:isDefinedBy (instance-address ?namespace-id))
;				(rdf:type (instance-address [rdfs:Resource]))
;				(uri ?resource)
;				(rdfs:comment "Imported by X-DEVICE.")
;			   	(rdfs:label ?namespace)
;			)
			(return (sym-cat < ?resource >))
	   	   else
	   	   	;(return ?resource)
	   	   	(return (nth$ 1 (send (nth$ 1 $?existing-resources) get-rdfs:label)))
		)
	   else
	   	(return ?resource)
	)
)

(deffunction scan_namespaces (?line)
	(bind $?namespaces (create$))
   	(bind ?pos (str-index "xmlns:" ?line))
   	;(verbose  "xmlns-pos: " ?pos crlf)
   	(while (neq ?pos FALSE)
   	   do
   	   	(bind ?line (sub-string (+ ?pos 6) (length ?line) ?line))
   	   	(bind ?pos (str-index "=" ?line))
   	   	;(verbose  "=-pos: " ?pos crlf)
   	   	(bind ?namespace (sym-cat (sub-string 1 (- ?pos 1) ?line)))
   	   	(bind ?line (sub-string (+ ?pos 2) (length ?line) ?line))
   	   	(bind ?pos (str-index "\"" ?line))
   	   	;(verbose  "\"-pos" ?pos crlf)
   	   	(if (and (neq ?pos FALSE) (neq (sub-string (- ?pos 1) (- ?pos 1) ?line) ";"))
   	   	   then
   	   		(bind ?uri (sub-string 1 (- ?pos 1) ?line))
   	   	   else
   	   	   	;(verbose "Line: " ?line crlf)
   	   	   	(bind ?pos (str-index ";" ?line))
  	   	   	(bind ?entity (sym-cat (sub-string 2 (- ?pos 1) ?line)))
   	   	   	;(verbose "Entity: " ?entity crlf)
   	   	   	;(verbose "Entities: " ?*entities* crlf)
   	   	   	;(verbose "Member: " (member$ ?entity ?*entities*))
   	   	   	(bind ?uri (nth$ (+ (member$ ?entity ?*entities*) 1) ?*entities*))
   	   	   	;(verbose "URI: " ?uri " from entity: " ?entity crlf)
   	   	)
	   	(bind $?existing-resources (find-instance ((?rid rdfs:Resource)) (eq ?uri ?rid:uri)))
	   	(if (= (length$ $?existing-resources) 0)
		   then
		   	(if (instance-existp (symbol-to-instance-name ?namespace))
		   	   then
			   	(bind ?namespace-id (unique-instance-name ?namespace))
				(make-instance ?namespace-id of rdfs:Resource
					(rdfs:isDefinedBy ?namespace-id)
					(rdf:type [rdfs:Resource])
					(uri ?uri)
					(source system)
					;(rdfs:comment "Imported by X-DEVICE.")
				   	(rdfs:label ?namespace)
				)
			   else
				(make-instance (symbol-to-instance-name ?namespace) of rdfs:Resource
					(rdfs:isDefinedBy (symbol-to-instance-name ?namespace))
					(rdf:type [rdfs:Resource])
					(uri ?uri)
					(source system)
					;(rdfs:comment "Imported by X-DEVICE.")
				   	(rdfs:label ?namespace)
				)
			)
			(parse-ntriples ?uri ?namespace)
			(bind $?namespaces (create$ $?namespaces ?namespace))
		   else
		   	(if (not (instance-existp (symbol-to-instance-name ?namespace)))
		   	   then
		   	   	(make-instance (symbol-to-instance-name ?namespace) of rdfs:Resource
			   	   	(rdfs:isDefinedBy (nth$ 1 $?existing-resources))
					(source system)
			   	)
			)
		)
		(bind ?pos (str-index "xmlns:" ?line))
	)
	(return $?namespaces)
)

(deffunction scan_base (?namespace ?line)
   	(bind ?pos (str-index "xml:base" ?line))
   	;(verbose  "xmlns-pos: " ?pos crlf)
   	(if (neq ?pos FALSE)
   	   then
   	   	(bind ?line (sub-string (+ ?pos 10) (length ?line) ?line)) ; count equal sign and quotation mark
   	   	;(verbose  "=-pos: " ?pos crlf)
   	   	(bind ?pos (str-index "\"" ?line))
   	   	(if (neq ?pos FALSE)
   	   	   then
   	   		(bind ?uri (sub-string 1 (- ?pos 1) ?line))
   	   	   else
   	   	   	;(verbose "Line: " ?line crlf)
   	   	   	(bind ?pos (str-index ";" ?line))
   	   	   	(bind ?entity (sym-cat (sub-string 2 (- ?pos 1) ?line)))
   	   	   	;(verbose "Entity: " ?entity crlf)
   	   	   	;(verbose "Entities: " ?*entities* crlf)
   	   	   	;(verbose "Member: " (member$ ?entity ?*entities*))
   	   	   	(bind ?uri (nth$ (+ (member$ ?entity ?*entities*) 1) ?*entities*))
   	   	   	;(verbose "URI: " ?uri " from entity: " ?entity crlf)
   	   	)
   	   	;(verbose  "\"-pos" ?pos crlf)
	   	(bind $?existing-resources (find-instance ((?rid rdfs:Resource)) (eq ?uri ?rid:uri)))
	   	(if (= (length$ $?existing-resources) 0)
		   then
		   	(if (instance-existp (symbol-to-instance-name ?namespace))
		   	   then
			   	(bind ?namespace-id (unique-instance-name ?namespace))
				(make-instance ?namespace-id of rdfs:Resource
					(rdfs:isDefinedBy ?namespace-id)
					(rdf:type [rdfs:Resource])
					(uri ?uri)
					(source system)
					;(rdfs:comment "Imported by X-DEVICE.")
				   	(rdfs:label ?namespace)
				)
			   else
				(make-instance (symbol-to-instance-name ?namespace) of rdfs:Resource
					(rdfs:isDefinedBy (symbol-to-instance-name ?namespace))
					(rdf:type [rdfs:Resource])
					(uri ?uri)
					(source system)
					;(rdfs:comment "Imported by X-DEVICE.")
				   	(rdfs:label ?namespace)
				)
			)
			(parse-ntriples ?uri ?namespace)
			(return FALSE)
		   else
		   	(if (not (instance-existp (symbol-to-instance-name ?namespace)))
		   	   then
		   	   	(make-instance (symbol-to-instance-name ?namespace) of rdfs:Resource
			   	   	(rdfs:isDefinedBy (nth$ 1 $?existing-resources))
					(source system)
			   	)
			)
			(return TRUE)
		)
	   else
	   	(return nil)
	)
)

(deffunction scan_entities (?line)
	(bind $?tokens (explode$ ?line))
	(if (eq (nth$ 1 $?tokens) <!ENTITY)
	   then
	   	(bind ?entity (nth$ 2 $?tokens))
	   	(bind ?address (nth$ 3 $?tokens))
	   	;(verbose "Address before: " ?address crlf)
	   	(if (eq (sub-string 1 1 ?address) "'")
	   	   then
	   	   	(bind ?address (sub-string 2 (- (length ?address) 2) ?address))
	   	)
	   	;(verbose "Entity: " ?entity "   Address: " ?address crlf)
	   	(bind ?*entities* (create$ ?*entities* ?entity ?address))
	)
)

(deffunction create-namespaces (?projectname)
	(bind $?new-namespaces (create$))
	(bind ?base-namespace-exist FALSE)
	(bind ?rdf-filename (str-cat ?projectname ".rdf"))
	;(verbose  "RDF filename: " ?rdf-filename crlf)
	(if (open ?rdf-filename rdf "r")
	   then
		(bind ?line (readline rdf))
		(while (neq ?line EOF)
		   do
		   	;(verbose  "line: " ?line crlf)
		   	(scan_entities ?line)
			(bind $?new-namespaces (create$ $?new-namespaces (scan_namespaces ?line)))
			(bind ?flag (scan_base (sym-cat ?projectname) ?line))
			(if (neq ?flag nil)
			   then
				(bind ?base-namespace-exist ?flag)
			)
		   	(bind ?line (readline rdf))
		)
		(close rdf)
		;(verbose  "New namespaces: " $?new-namespaces crlf)
		(bind $?copy-new-namespaces $?new-namespaces)
		(while (> (length$ $?copy-new-namespaces) 0)
		   do
		   	(bind $?new-namespaces (create$ (create-namespaces (nth$ 1 $?copy-new-namespaces)) $?new-namespaces))
		   	(bind $?copy-new-namespaces (rest$ $?copy-new-namespaces))
		)
	)
	(if (not ?base-namespace-exist)
	   then
	   	(bind $?new-namespaces (create$ $?new-namespaces (sym-cat ?projectname)))
	)
	(return $?new-namespaces)
)


(deffunction insert-triples ($?triple-strings)
	(bind ?end (length$ $?triple-strings))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind $?triple (explode$ (nth$ ?n $?triple-strings)))
	   	(bind ?subject (nth$ 1 $?triple))
	   	(bind ?predicate (nth$ 2 $?triple))
	   	(bind $?remaining (subseq$ $?triple 3 (length$ $?triple)))
	   	(if (> (length$ $?remaining) 1)
	   	   then
	   		(bind ?object (implode$ $?remaining))
	   	   else
	   	   	(bind ?object (nth$ 1 $?remaining))
	   	)
	   	(if (assert (triple (subject ?subject) (predicate ?predicate) (object ?object)))
	   	   then
	   		(bind ?*triple_counter* (+ ?*triple_counter* 1))
	   	)
	)
	TRUE
)

(deffunction n3-open (?namespace)
	(bind ?n3-filename (str-cat ?namespace ".n3"))
	(bind ?nt-filename (str-cat ?namespace ".nt"))
	(if (or (open ?n3-filename n3 "r") (open ?nt-filename n3 "r"))
	   then
		(return TRUE)
	   else
	   	(bind ?rdf-filename (str-cat ?namespace ".rdf"))
	   	;(verbose  "RDF filename: " ?rdf-filename crlf)
	   	(if (open ?rdf-filename rdf "r")
	   	   then
	   	   	(close rdf)
	   	   	(local-parse-ntriples ?namespace)
	   	   	(open ?n3-filename n3 "r")
	   	   	(return TRUE)
	   	   else
	   	   	(return FALSE)
	   	)
	)
)
	   	

(deffunction load-namespace (?namespace)
	(verbose  ?namespace " ")
	(bind $?triple-strings (create$))
	;(verbose  "N3 filename: " ?n3-filename crlf)
	(if (n3-open ?namespace)
	   then
		(bind ?triple-string (readline n3))
		(while (neq ?triple-string EOF)
		   do
		   	(bind $?triple-strings (create$ $?triple-strings ?triple-string))
		   	(bind ?triple-string (readline n3))
		)
		(close n3)
		;(verbose  "Original triples:" crlf)
		;(verbose  "=================" crlf)
		;(pprint-list $?triple-strings)
		(bind $?new-triple-strings (create$))
		(while (> (length$ $?triple-strings) 0)
		   do
		   	(bind $?triple (explode$ (nth$ 1 $?triple-strings)))
		   	(if 	(and 
		   			(= (length$ $?triple) 4)
		   			(eq (nth$ 4 $?triple) .)
		   		)
		   	   then
		   		(bind ?subject (nth$ 1 $?triple))
		   		(bind ?predicate (nth$ 2 $?triple))
		   		(bind ?object (nth$ 3 $?triple))
		   		(bind ?subject (import-resource ?subject))
		   		(bind ?predicate (import-resource ?predicate))
		   		; The following is true only for my specific example, needs special treatment
		   		; in the future
		   		(if (eq ?object <online:>)
		   		    then
		   		    	(bind ?object (send (symbol-to-instance-name ?namespace) get-uri))
		   		    else
		   			(bind ?object (import-resource ?object))
		   		)
		   		(bind ?new-triple (str-cat$ (create$ ?subject ?predicate ?object)))
		   		(bind $?new-triple-strings (create$ $?new-triple-strings ?new-triple))
		   	)
		   	(bind $?triple-strings (rest$ $?triple-strings))
		)
		;(verbose  "Imported triples:" crlf)
		;(verbose  "=================" crlf)
		;(pprint-list $?new-triple-strings)
		(return $?new-triple-strings)
	   else
	   	(return (create$))
	)
)

(deffunction load-namespaces ($?namespaces)
	(verbose  "Loading namespaces: ")
	(bind $?triple-strings (create$))
	(bind ?end (length $?namespaces))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind $?triple-strings (create$ $?triple-strings (load-namespace (nth$ ?n $?namespaces))))
	)
	(verbose  crlf)
;	(bind $?triple-strings (sort string> (remove-duplicates$ $?triple-strings)))
	(bind $?triple-strings (remove-duplicates$ $?triple-strings))
	(verbose  "Triples imported" crlf "================" crlf)
	(pprint-list $?triple-strings)
	(insert-triples $?triple-strings)
)



(deffunction load-rdf (?projectname ?address)
	(if (neq ?address local)
	   then
	   	(parse-ntriples ?address ?projectname)
	)
	;(bind ?namespace (extract-filename ?filename))
	(bind $?new-namespaces (remove-duplicates$ (create-namespaces ?projectname)))
	;(verbose  "new-namespaces: " $?new-namespaces crlf)
	(load-namespaces $?new-namespaces)
)
