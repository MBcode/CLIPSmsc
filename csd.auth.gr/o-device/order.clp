;;; ######################################################################################
;;; Manage class is-a ordering. In some cases, especially when there is a complex
;;; subclass hierarchy, an error may occur relevant to the class precedence list
;;; that CLIPS maintains. This set of rules holds the order of the classes in the
;;; is-a constraint and it makes potential reorderings to the initially defined 
;;; subclass values of the CLASS facts, in order to prevent such errors.
;;; ######################################################################################

;;; this rule checks for mutual subclass relationships between two classes
;;; and stops the execution
(defrule $detect-cyrcles
	(declare (salience 9916))
	(goal (name delegators-defined))
	(strong-order (c1 ?x)(c2 ?y))
	(strong-order (c1 ?y)(c2 ?x))
	=>
	(debug error "Subclass circle has been detected: " ?x ", " ?y)
)
;;; 
(defrule $strong-order "subclass order"
	(declare (salience 9915))
	(goal (name delegators-defined))
	(CLASS (name ?c)(subclass $? ?sup&~?c $?))
	(not (strong-order (c1 ?c)(c2 ?sup)))
	=>
	(assert (strong-order (c1 ?c)(c2 ?sup))))

;;; 
(defrule $strong-order-transitive "order transitivity"
	(declare (salience 9915))
	(goal (name delegators-defined))
	(strong-order (c1 ?x)(c2 ?y))
	(strong-order (c1 ?y)(c2 ?z&~?x))
	(not (strong-order (c1 ?x)(c2 ?z)))
	=>
	(assert (strong-order (c1 ?x)(c2 ?z))))

;;; make reorderings in order to prevent errors
(defrule $modify-classes
	(declare (salience 9915))
	(goal (name delegators-defined))
	(strong-order (c1 ?c)(c2 ?d))
	?class <- (CLASS (subclass $?H ?d $?M ?c $?T))
	=>
	(modify ?class (subclass (create$ $?H ?c ?d $?M $?T)))
)
