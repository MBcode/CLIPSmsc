(deductiverule
	(dmoz:Topic (dmoz:catid ~"4") (dc:title ?t) (dmoz:link $? ?l $?))
	?l <- (dmoz:ExternalPage (dc:title ?lt) (dc:description ?d) (dmoz:priority 1))
  =>
  	(result (title ?t) (link_title ?lt) (link_desc ?d))
)

