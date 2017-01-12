;;; ######################################################################################
;;; # Rules for generating the COOL classes 
;;; ######################################################################################

;;; owl:Thing is the superclass of all the classes
(defrule realize-owl:Thing
	(declare (salience 9910))
	(goal (name delegators-defined))
	?T <- (CLASS (name ?term&:(eq ?term ?*owl:Thing*))(slots $?slots)(materialized FALSE))
	=>
	(define-class ?*owl:Thing* (create$ USER) $?slots)
	(modify ?T (materialized TRUE))
)

;;; create the class. Note that the restriction classes do not  
;;; participate physically in the OO model
(defrule realize-class
	(declare (salience 9909))
	(goal (name delegators-defined))
	?CL <- (CLASS (name ?c)(subclass $?superclasses)(slots $?slots)(materialized FALSE))
	(not (CLASS (name ?c2 & ~?c &:(member$ ?c2 $?superclasses))(materialized FALSE)))
	=>
	(define-class ?c $?superclasses $?slots)
	(modify ?CL	(materialized TRUE))
)
