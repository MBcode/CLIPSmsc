(defglobal 
	?*verbose_status* = off
	?*untranslated_rules* = 0
)

(defclass TYPED-CLASS
	(is-a USER)
	(role concrete)
	(pattern-match reactive)
	(multislot class-refs 
		(type SYMBOL) 
		(storage shared)
		(access read-only)
	)
	(slot namespace
		(type SYMBOL)
		(storage shared)
		(access read-only)
	)
)

;(defclass DERIVED-CLASS
;	(is-a TYPED-CLASS)
;	(slot counter (type INTEGER) (default 1))
;	(multislot derivators (type STRING))
;	(multislot derivators (type INSTANCE-NAME))
;	(multislot derivators)
;)

(defclass DERIVED-CLASS
	(is-a TYPED-CLASS RDF-CLASS)
	(slot counter (type INTEGER) (default 1))
	(multislot derivators)
)

(deftemplate deductive-rule
	(slot name (type SYMBOL))
	(slot del-name (type SYMBOL))
	(slot deductive-rule (type STRING))
	(slot production-rule (type STRING))
	;(slot delete-production-rule (type STRING))
	(slot derived-class (type SYMBOL))
	(multislot depends-on (type SYMBOL))
	(slot implies (type SYMBOL))
)

(deftemplate derived-attribute-rule
	(slot name (type SYMBOL))
	(slot del-name (type SYMBOL))
	(slot derived-attribute-rule (type STRING))
	;(slot production-rule (type STRING))
	;(slot delete-production-rule (type STRING))
;	(slot derived-class (type SYMBOL))
	(multislot depends-on (type SYMBOL))
	(slot implies (type SYMBOL))
)

(deftemplate aggregate-attribute-rule
	(slot name (type SYMBOL))
	(slot del-name (type SYMBOL))
	(slot aggregate-attribute-rule (type STRING))
	;(slot production-rule (type STRING))
	;(slot delete-production-rule (type STRING))
;	(slot derived-class (type SYMBOL))
	(multislot depends-on (type SYMBOL))
	(slot implies (type SYMBOL))
)

(deftemplate derived-class
	(slot name (type SYMBOL))
	(slot stratum (type INTEGER) (default 1))
	(multislot deductive-rules (type SYMBOL))
)

(deftemplate namespace
	(slot name (type SYMBOL))
	(slot address (type STRING))
	(multislot classes (type SYMBOL))
)

(deftemplate pending-rule 
	(slot production-rule (type STRING)) 
	(slot delete-production-rule (type STRING)) 
	(multislot non-existent-classes (type SYMBOL))
)
