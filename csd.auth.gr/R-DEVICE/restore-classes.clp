(defrule restore-classes
	(goal restore-classes)
	?x <- (redefined-class (name ?class) (isa-slot $?super-classes) (slot-definitions $?slot-defs) (class-refs-defaults $?class-refs) (aliases-defaults $?aliases))
	(not (redefined-class (name ?super-class&:(member$ ?super-class $?super-classes))))
  =>
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
	(retract ?x)
)

  	
(defrule restore-instances
  	(goal restore-classes)
	(not (redefined-class))
  	?x <- (backup-instances ?filename)
  =>
  	(restore-instances ?filename)
  	(retract ?x)
  	(remove ?filename)
)

