(defrule translate-derived-attribute-rules
	(goal translate-derived-attribute-rules)
	?rule-idx <- (derivedattrule ?rule-string)
  =>
  	(bind $?classes (build-dependency-network ?rule-string))
  	;(verbose "classes: " $?classes crlf)
	(translate-derived-attribute-rule ?rule-string $?classes)
	(retract ?rule-idx)
	(bind ?*untranslated_rules* (- ?*untranslated_rules* 1))
)

(defrule translate-aggregate-attribute-rules
	(goal translate-aggregate-attribute-rules)
	?rule-idx <- (aggregateattrule ?rule-string)
  =>
  	(bind $?classes (build-dependency-network ?rule-string))
  	;(verbose "classes: " $?classes crlf)
	(translate-aggregate-attribute-rule ?rule-string $?classes)
	(retract ?rule-idx)
	(bind ?*untranslated_rules* (- ?*untranslated_rules* 1))
)

(defrule translate-2nd-order-rules
	(goal translate-2nd-order-rules)
	?rule-idx <- (2nd-order-rule ?rule-string $?results-2nd-order)
  =>
	(translate-2nd-order-rule ?rule-string $?results-2nd-order)
	(retract ?rule-idx)
	(bind ?*untranslated_rules* (- ?*untranslated_rules* 1))
)

(defrule pre-compile-deductive-rules
	(goal pre-compile-deductive-rules)
	?rule-idx <- (deductiverule ?rule-string)
  =>
	(pre-compile-deductive-rule ?rule-string)
	(retract ?rule-idx)
	(bind ?*untranslated_rules* (- ?*untranslated_rules* 1))
)

(defrule translate-deductive-rules
	(goal translate-deductive-rules)
	?rule-idx <- (deductive-rule (deductive-rule ?rule-string) (production-rule "") (depends-on $? ?class $?))
	(not (deductive-rule (production-rule "") (implies ?class)))
  =>
	(translate-deductive-rule ?rule-idx ?rule-string)
)

(defrule insert-pending-rules
	(goal insert-pending-rules)
	?rule-idx <- (pending-rule (production-rule ?pr) (delete-production-rule ?dpr) (non-existent-classes $?classes))
  =>
 	(insert-pending-rule ?pr ?dpr $?classes)
	(retract ?rule-idx)
)

(defrule calc-stratum-for-all
	(goal calc-stratum-for-all)
	?rule-idx <- (deductive-rule (production-rule ?rule-condition&~"") (derived-class ?class&~nil))
  =>
 	(calc-stratum-afterwards ?rule-condition ?class)
)

