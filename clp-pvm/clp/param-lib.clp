;defn & msg-handlers for some of the PARAM class (has-a classes)	MTB
;-sometimes what was a glob-pram will be made of a few of what where loc-params
; should references to them be sent along, or by transfering the 'glob-param'
; does it calc it from the locals, if they have been updated
;=========================================================projection_PARAMeter
;defclass PARAM in param.clp
;=================================================================GRID
(defclass GRID
     (is-a ACCESSIBLE)
     (role concrete) 
     (pattern-match reactive)
 (slot units (type SYMBOL)		;actuall units (eg: ft,mi,m,km,deg) 
	(create-accessor read-write) (visibility public))	
 ;could take any 2 opposite corners, but this is easier for now
 (multislot corner-sw (type FLOAT)	;location of SW-lower corner
	(create-accessor read-write) (visibility public))	
 (multislot corner-ne (type FLOAT)	;location of NE-upper corner
	(create-accessor read-write) (visibility public))	
 (multislot delta (type FLOAT)	         ;length of delta-x-y-z segments
	(create-accessor read-write) (visibility public))	
 (multislot nseg (type INTEGER)	         ;# of segments (should=array's xyz)
	(create-accessor read-write) (visibility public))	
)
;deg would be in deg-min-sec, but can't do z this way
;will be able to have relation like subgrid-p & eq-sp-subgrid-p
;& fncs like grid-intersection & grid-union
;-----------------------------------------------------------------
;=================================================================UNITS
;SI base-units: meter, kilogram, second, ampere, Kelvin, mole, and candela
;               length, mass, time, current, temprature, mole, illum
;               l(m)    m(kg) t(s)  c(A)     t(K)        (M)   Cnd
;force=newton=kg m / s s
;--might not need an instance for this?  (more just standardization of names)
(defclass UNITS   	;name the instance w/ the basic-unit types (above order)
     (is-a ACCESSIBLE)
     (role concrete) 
     (pattern-match reactive)
 (multislot units (type SYMBOL)		;actuall units (eg: ft / sec sec) orStr?
	(create-accessor read-write) (visibility public))	
 (multislot units-type (type SYMBOL)	;type equiv (eg: length / time time)
	(create-accessor read-write) (visibility public))	
 (multislot units-si (type SYMBOL)	;SI equiv (eg: m / sec sec) [7 types]
	(create-accessor read-write) (visibility public))	
 (multislot syn (type SYMBOL)		;list of eqv unit defns (use member$)
	(create-accessor read-write) (visibility public)))
;have all numerator terms a / then all the denominator terms
;-----------------------------------------------------------------
;=================================================================DESCRIPT
;(defclass DESCRIPT   ;describe		maybe hold constraints -ref?
;    (is-a ACCESSIBLE)
;    (role concrete) 
;    (pattern-match reactive)
;(slot journal (type INSTANCE)	      ;list of proceedures applied to the param
;(create-accessor read-write) (visibility public))	
;(slot constr (type INSTANCE)		;list of constraint instances
;(create-accessor read-write) (visibility public))	
;;maybe put these in contraint objs:
;(multislot range 			;min & max of the values
;	(create-accessor read-write) (visibility public))	
;(slot default 				;default value for the array value(s)
;	(create-accessor read-write) (visibility public)))
;for units ft/(sec sec), ft/sec  sec, ft/sec/sec  or num= ft den= sec sec
;range/default values could be another param-inst
; which could mean use its range/default slots or the sep vals of the array
;could have get-actual-min get-actual-max get-mean get-median <-for arrays
;dumping the normed values or histogram of val bins to a fuz-fact ?
;would be nice to make arrays a base clips obj -or not
;------------------------------------------------------------------
;=================================================================CONSTR
(defclass CONSTR   ;constraints 
     (is-a SUBROUTINE)
     (role concrete) 
     (pattern-match reactive)
 )
;use the constraint obj that updates slots/params/etc
;make it general, maybe like a subroutine, have good backup fncs
;---------------------------------------------------------------------------
;---still want to have params which are composed of other params,so need map-fnc
;-------------------
;instead of mapping, just have full description which can be mapped between
; (multislot from-var (type SYMBOL) 	;variable(s) mapped from (usually 1)
;	(create-accessor read-write) (visibility public))	
; (slot to-var (type SYMBOL) 		;variable mapped to
;	(create-accessor read-write) (visibility public))	
;;;have to list the model separtely, if no proxy around
;;(multislot from-mod (type SYMBOL) 	;model(s) mapped from (almost always 1)
;	(create-accessor read-write) (visibility public))	
;;(slot to-mod (type SYMBOL) 		;model mapped to
;	(create-accessor read-write) (visibility public))	
; (slot map-fnc (type SYMBOL) 		;fnc to map between them
;	(create-accessor read-write) (visibility public))	) 
;-------------------
;Linda-like fncs/hndlers should be written around the param-
;------------------------------------------------------------------EOF
