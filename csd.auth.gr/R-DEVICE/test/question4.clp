(deductiverule
	(dmoz:Topic (dmoz:catid "4") (dc:title ?t) (dmoz:link $? ?l $?))
	?l <- (dmoz:ExternalPage (dc:title ?lt))
  =>
  	(result (title ?t) (link_title ?lt))
)

