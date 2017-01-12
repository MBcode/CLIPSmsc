;;; ######################################################################################
;;; Load the generated files of O-DEVICE and the necessary O-DEVICE source code.
;;; ######################################################################################
(set-dynamic-constraint-checking FALSE)
(object-pattern-match-delay 
	(load* (str-cat ?*src-folder* "global.clp"))
	(load* (str-cat ?*src-folder* "functions.clp"))
	(load* (str-cat ?*src-folder* "order.clp"))	
	(load* ?*class-file*)
	(load-facts ?*fact-file*)
	(load* ?*rule-file*)
	(restore-instances ?*object-file*))
(set-dynamic-constraint-checking TRUE)

(assert (goal (name delegators-defined)))

;use the 'build' function to define the rule in JAVA
(defrule $refresh$ 
  (declare (salience 8999))
  ?UP <- (UPDATE (refresh TRUE))
=>
  (debug info "updating...")
  (modify ?UP (refresh FALSE))
  (load* ?*rule-file*)
  (progn$ (?r ?*rule-files*)
  	(printout t test crlf)
  	(load* ?r))
  (run))

(run)
