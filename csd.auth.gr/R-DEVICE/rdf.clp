(defglobal 
	;?*fetched_url* = (create$)
	?*entities* = (create$)
	?*resource_instances* = (create$)
	?*triple_counter* = 0
	?*verbose_status* = off
)

(deftemplate triple
	(slot subject (type LEXEME))
	(slot predicate (type LEXEME))
	(slot object (type LEXEME))
)

(deftemplate rejected-triple
	(slot subject (type LEXEME))
	(slot predicate (type LEXEME))
	(slot object (type LEXEME))
)

(deftemplate candidate-class
	(slot name (type SYMBOL))
	(multislot isa-slot (type SYMBOL))
	(multislot slot-definitions (type STRING))
	(multislot class-refs-defaults (type SYMBOL))
	(multislot aliases-defaults (type SYMBOL))
)

(deftemplate redefined-class
	(slot name (type SYMBOL))
	(multislot isa-slot (type SYMBOL))
	(multislot slot-definitions (type STRING))
	(multislot class-refs-defaults (type SYMBOL))
	(multislot aliases-defaults (type SYMBOL))
)

(deftemplate multi-type-object 
	(slot name (type SYMBOL)) 
	(multislot classes (type SYMBOL))
)

(defclass RDF-CLASS
	(is-a USER)
	(role concrete)
	(pattern-match reactive)
	(slot uri (type STRING))
	(slot source (type SYMBOL) (default rdf))
	(multislot class-refs 
		(type SYMBOL) 
		(storage shared)
		(access read-only)
	)
	(multislot aliases 
		(type SYMBOL) 
		(storage shared)
		(access read-only)
	)
)

(defmessage-handler RDF-CLASS put-uri after (?uri)
	(bind ?*resource_instances* (create$ (sym-cat < ?uri >) (instance-name ?self) ?*resource_instances*))
)

(defclass rdfs:Resource
	(is-a RDF-CLASS)
	(multislot rdfs:isDefinedBy (type INSTANCE-NAME))
	(multislot rdf:type (type INSTANCE-NAME))
	(multislot rdf:value)
	(multislot rdfs:comment (type LEXEME))
	(multislot rdfs:label (type LEXEME))
	(multislot rdfs:seeAlso (type INSTANCE-NAME))
	(multislot class-refs 
		(source composite) 
		(default (create$ 
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)

(defclass rdfs:Class
	(is-a rdfs:Resource)
	(multislot rdfs:subClassOf (type INSTANCE-NAME))
	(multislot class-refs 
		(source composite) 
		(default (create$ 
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
			rdfs:subClassOf rdfs:Class
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)

(defclass rdf:Property
	(is-a rdfs:Resource)
	(multislot rdfs:domain (type INSTANCE-NAME))
	(multislot rdfs:range (type INSTANCE-NAME))
	(multislot rdfs:subPropertyOf (type INSTANCE-NAME))
	(multislot class-refs 
		(source composite) 
		(default (create$ 
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
			rdfs:domain rdfs:Class
			rdfs:range rdfs:Class
			rdfs:subPropertyOf rdf:Property
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)

(defclass rdf:Statement
	(is-a rdfs:Resource)
	(multislot rdf:subject (type INSTANCE-NAME))
	(multislot rdf:predicate (type INSTANCE-NAME))
	(multislot rdf:object)
	(multislot class-refs 
		(source composite) 
		(default (create$ 
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
			rdf:subject rdfs:Resource
			rdf:predicate rdf:Property
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)

(defclass rdfs:Container
	(is-a rdfs:Resource)
	(multislot rdfs:member)
	(multislot class-refs 
		(source composite) 
		(default (create$
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)

(defclass rdf:Alt
	(is-a rdfs:Container)
	(multislot class-refs 
		(source composite) 
		(default (create$
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)

(defclass rdf:Bag
	(is-a rdfs:Container)
	(multislot class-refs 
		(source composite) 
		(default (create$
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)

(defclass rdf:Seq
	(is-a rdfs:Container)
	(multislot class-refs 
		(source composite) 
		(default (create$
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)

(defclass rdfs:ContainerMembershipProperty
	(is-a rdf:Property)
	(multislot class-refs 
		(source composite) 
		(default (create$ 
			rdfs:isDefinedBy rdfs:Resource
			rdf:type rdfs:Class
			rdfs:seeAlso rdfs:Resource
			rdfs:domain rdfs:Class
			rdfs:range rdfs:Class
			rdfs:subPropertyOf rdf:Property
		))
	)
	(multislot aliases 
		(source composite) 
		(default (create$ 
			rdfs:seeAlso rdfs:isDefinedBy
		))
	)
)


(definstances rdf_classes
	(rdf of rdfs:Resource 
		(rdfs:isDefinedBy [rdf])
		;Must be defined/inserted afterwards
		(rdf:type [rdfs:Resource])
		(uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
		(rdfs:comment "The core RDF syntax namespace")
		(rdfs:label rdf)
	)
	(rdfs-more of rdfs:Resource 
		(rdfs:isDefinedBy [rdfs-more])
		;Must be defined/inserted afterwards
		(rdf:type [rdfs:Resource])
		(uri "http://www.w3.org/2000/01/rdf-schema-more#")
		(rdfs:comment "This document provides additional statements about the RDF Schema namespace")
		(rdfs:label rdfs-more)
	)
	(rdfs of rdfs:Resource 
		(rdfs:isDefinedBy [rdfs])
		;Must be defined/inserted afterwards
		(rdf:type [rdfs:Resource])
		(uri "http://www.w3.org/2000/01/rdf-schema#")
		(rdfs:comment "The RDF Schema Vocabulary namespace")
		(rdfs:label rdfs)
		(rdfs:seeAlso [rdfs-more])
	)
	(rdfs:Class of rdfs:Class 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdfs:Class])
		;Must be defined/inserted afterwards
		(rdfs:subClassOf [rdfs:Resource])
		(rdfs:label Class)
		(rdfs:comment "The concept of Class")
	)
	(rdfs:Resource of rdfs:Class 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdfs:Class])
		(rdfs:label Resource)
		(rdfs:comment "The class resource, everything.")
	)
	(rdf:Property of rdfs:Class 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdfs:Class])
		(rdfs:subClassOf [rdfs:Resource])
		(rdfs:label Property)
		(rdfs:comment "The concept of a property.")
	)
	(rdfs:Literal of rdfs:Class 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdfs:Class])
		(rdfs:label Literal)
		(rdfs:comment "This represents the set of atomic values, eg. textual strings.")
	)
	(rdf:Statement of rdfs:Class 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdfs:Class])
		(rdfs:subClassOf [rdfs:Resource])
		(rdfs:label Statement)
		(rdfs:comment "The class of RDF statements.")
	)
	(rdfs:Container of rdfs:Class 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdfs:Class])
		(rdfs:subClassOf [rdfs:Resource])
		(rdfs:label Container)
		(rdfs:comment "This represents the set Containers.")
	)
	(rdf:Alt of rdfs:Class 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdfs:Class])
		(rdfs:subClassOf [rdfs:Container])
		(rdfs:label Alt)
		(rdfs:comment "A collection of alternatives.")
	)
	(rdf:Bag of rdfs:Class 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdfs:Class])
		(rdfs:subClassOf [rdfs:Container])
		(rdfs:label Bag)
		(rdfs:comment "An unordered collection.")
	)
	(rdf:Seq of rdfs:Class 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdfs:Class])
		(rdfs:subClassOf [rdfs:Container])
		(rdfs:label Seq)
		(rdfs:comment "An ordered collection.")
	)
	(rdfs:ContainerMembershipProperty of rdfs:Class 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdfs:Class])
		(rdfs:subClassOf [rdf:Property])
		(rdfs:label ContainerMembershipProperty)
		(rdfs:comment "The container membership properties, rdf:1, rdf:2, ..., all of which are sub-properties of 'member'.")
	)
	(rdf:object of rdf:Property 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdf:Statement])
		(rdfs:label object)
		(rdfs:comment "Identifies the object of a statement when representing the statement in reified form")
	)
	(rdf:predicate of rdf:Property 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdf:Statement])
		(rdfs:range [rdf:Property])
		(rdfs:label predicate)
		(rdfs:comment "Identifies the property used in a statement when representing the statement in reified form")
	)
	(rdf:subject of rdf:Property 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdf:Statement])
		(rdfs:range [rdfs:Resource])
		(rdfs:label subject)
		(rdfs:comment "Identifies the resource that a statement is describing when representing the statement in reified form")
	)
	(rdf:type of rdf:Property 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdfs:Resource])
		(rdfs:range [rdfs:Class])
		(rdfs:label type)
		(rdfs:comment "Identifies the Class of a resource")
	)
	(rdf:value of rdf:Property 
		(rdfs:isDefinedBy [rdf])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdfs:Resource])
		(rdfs:label value)
		(rdfs:comment "Identifies the principal value (usually a string) of a property when the property value is a structured resource")
	)
	(rdfs:comment of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdfs:Resource])
		(rdfs:range [rdfs:Literal])
		(rdfs:label comment)
		(rdfs:comment "Use this for descriptions")
	)
	(rdfs:domain of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdf:Property])
		(rdfs:range [rdfs:Class])
		(rdfs:label domain)
		(rdfs:comment "A domain class for a property type")
	)
	(rdfs:seeAlso of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdfs:Resource])
		(rdfs:range [rdfs:Resource])
		(rdfs:label seeAlso)
		(rdfs:comment "A resource that provides information about the subject resource")
	)
	(rdfs:isDefinedBy of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdfs:Resource])
		(rdfs:range [rdfs:Resource])
		(rdfs:subPropertyOf [rdfs:seeAlso])
		(rdfs:label isDefinedBy)
		(rdfs:comment "Indicates the namespace of a resource")
	)
	(rdfs:label of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdfs:Resource])
		(rdfs:range [rdfs:Literal])
		(rdfs:label label)
		(rdfs:comment "Provides a human-readable version of a resource name.")
	)
	(rdfs:member of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdfs:Container])
		(rdfs:label member)
		(rdfs:comment "Á member of a container")
	)
	(rdfs:range of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdf:Property])
		(rdfs:range [rdfs:Class])
		(rdfs:label range)
		(rdfs:comment "A range class for a property type")
	)
	(rdfs:subClassOf of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdfs:Class])
		(rdfs:range [rdfs:Class])
		(rdfs:label subClassOf)
		(rdfs:comment "Indicates membership of a class")
	)
	(rdfs:subPropertyOf of rdf:Property 
		(rdfs:isDefinedBy [rdfs])
		(rdf:type [rdf:Property])
		(rdfs:domain [rdf:Property])
		(rdfs:range [rdf:Property])
		(rdfs:label subPropertyOf)
		(rdfs:comment "Indicates specialization of properties")
	)

)

