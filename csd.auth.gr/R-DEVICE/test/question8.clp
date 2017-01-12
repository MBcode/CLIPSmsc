(deductiverule
	(dmoz:Topic (dc:title ?top) (dmoz:narrow $? ?n $?))
	?n <- (dmoz:Topic (dc:title ?t) (dmoz:link $? ?l $?))
	?l <- (dmoz:ExternalPage (dc:title ?lt) (dmoz:priority 1))
  =>
  	(result (top_title ?top) (title ?t) (link_title ?lt))
)

