
;;; ######################################################################################
;;; Deftemplate definition for storing and manipulating the order of the classes
;;; in the is-a constraint of defclass definitions. In that way, O-DEVICE prevents the
;;; throwing of errors relevant to class precedence lists
;;; ######################################################################################
(deftemplate strong-order
	(slot c1)
	(slot c2)
)

;;; ######################################################################################
;;; System template for regulating the execution of rules
;;; ######################################################################################
(deftemplate goal
	(slot name)
)

;;; ######################################################################################
;;; Template for holding the COOL code of classes
;;; ######################################################################################
(deftemplate DEFCLASS
	(slot code)
)

;;; ######################################################################################
;;; Template for holding the namespace-to-prefix mapping that Jena computes for the set
;;; of the loaded ontologies
;;; ######################################################################################
(deftemplate PrefixNsMap
	;e.g. rdfs, owl, ...
	(slot prefix)
	;e.g. xsd:, ...
	(slot namespace)
)

;;; ######################################################################################
;;; Deftemplate definition for storing the state of O-DEVICE. If a class in CLIPS
;;; is generated dynamically, then the objects of this class cannot be matched in the
;;; conditions of rules, since the class has been defined after the definition of the
;;; corresponding object pattern. Therefore, the rules should be reloaded in order 
;;; to incorporate further rule activations. This may cause an overhead to the 
;;; perfromance of O-DEVICE and it has to do with the semantics of OWL that require
;;; sometimes to generate dynamically classes, e.g. in the case of an object that 
;;; belongs simlultaneously to more than one class.
;;; ######################################################################################
(deftemplate UPDATE
	(slot refresh)
)

;;; ######################################################################################
;;; Deftemplate definition for storing the synamic rules that O-DEVICE generates.  
;;; ######################################################################################
(deftemplate rule
	;rule type
	(slot type) 	
	;rule name 
	(slot name) 	
	;rule definition
	(slot code)
)

;;; ######################################################################################
;;; Deftemplate definition for storing in the form of CLIPS facts the ontology triples 
;;; that the ARP Parser produces
;;; ######################################################################################
(deftemplate triple 
	;the subject of the triple
	(slot subject)
	;the predicate of the triples
	(slot predicate)
	;the object of the triple
	(slot object)
)

;;; ######################################################################################
;;; Deftemplate definition for collecting the information regarding the OWL classes.
;;; ######################################################################################
(deftemplate CLASS 
	;the name of the class
	(slot name) 
	;direct superclasses (rdfs:subClassOf)
	(multislot subclass) 
	;intesection classes (owl:intersectionOf)
	(multislot intersection)
	;equivalent classes (owl:equivalentClass)
	(multislot equivalent)
	;complement of classes (owl:complementOf)
	(multislot complement)
	;disjoint classes (owl:disjointWith)
	(multislot disjoint)
	;union classes (owl:unionOf)
	(multislot union)
	;owl:hasKey - only the named classes have keys
	(multislot hasKey)
	;the properties that have this class as a domain
	(multislot slots)
	;initially, all the classes are not delegators. The delegators are used
	;for the mapping of OWL class equivalence, since subclass
	;circles are forbitten in the oo model.
	(slot delegator (default FALSE))
	;label (rdfs:label)
	(slot label)
	;comment (rdfs:comment)
	(slot comment)
	;if the fact has been mapped on a COOL class
	(slot materialized (default FALSE))
)

;;; ######################################################################################
;;; Deftemplate definition for collecting the information regarding OWL restriction 
;;; classes. The restriction classes are not mapped on actual COOL classes, but they 
;;; are used in order to generate dynamically object classification rules.
;;; ######################################################################################
(deftemplate RESTRICTION 
	;the name
	(slot name)
	;the owl:onProperty value
	(slot onProperty)
	;owl:onClass
	(slot onClass)
	;owl:onDataRange
	(slot onDataRange)
	;the restriction type (owl:cardinality, owl:someValuesFrom, etc)
	(slot restriction)
	;the restriction value
	(slot value)
	;potential restriction superclass (currently O-DEVICE ignores
	;superclasses of restriction classes)
	(multislot subclass)
	;potential equivalent classes (currently O-DEVICE ignores 
	;equivalent classes of restriction classes)
	(multislot equivalent)
	;label
	(slot label)
	;comment
	(slot comment)
)

;;; ######################################################################################
;;; Deftemplate definition for collecting the information regarding OWL properties.
;;; ######################################################################################
(deftemplate PROPERTY
	;the property name
	(slot name)
	;the type of the property (object, datatype, transitive, etc)
	(multislot type)
	;the domain classes (rdfs:domain)
	(multislot domain)
	;the range classes (rdfs:range)
	(multislot range)
	;subproperties (rdfs:subPropertyOf)
	(multislot subproperty)
	;equivalent properties (owl:equivalentProperty)
	(multislot equivalentProperty)
	;inverse properties (owl:inverseOf)
	(multislot inverse)
	;property chains
	(multislot propertyChain)
	;label
	(slot label)
	;comment
	(slot comment)
)

;;; ######################################################################################
;;; Deftemplate definition for collecting the information regarding data ranges 
;;; (owl:DataRange).
;;; ######################################################################################
(deftemplate DATARANGE
	;the name 
	(slot name)
	;the values that the data range contains
	(multislot oneOf)
)

;;; ######################################################################################
;;; Deftemplate definition for collecting the information regarding disjoint classes
;;; (owl:AllDisjointClasses)
;;; ######################################################################################
(deftemplate ALL_DISJOINT_CLASSES
	;the name 
	(slot name)
	;the classes
	(multislot members)
)

