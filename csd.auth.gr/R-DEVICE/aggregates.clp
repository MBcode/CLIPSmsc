(defclass aggregate-function
	(is-a USER)
	(role concrete)
	(pattern-match reactive)
	(slot class (type SYMBOL))
	(slot instance (type INSTANCE-NAME))
	(slot attribute (type SYMBOL))
	(multislot values)
	(multislot objects)
)

(defmessage-handler aggregate-function calc-result ($?result)
	$?result
)

(defclass sum
	(is-a aggregate-function)
)

(defmessage-handler sum calc-result ($?result)
	(sum$ $?result)
)

(defclass count
	(is-a aggregate-function)
)

(defmessage-handler count calc-result ($?result)
	(length$ $?result)
)

(defclass avg
	(is-a aggregate-function)
)

(defmessage-handler avg calc-result ($?result)
	(if (> (length$ $?result) 0)
	   then
		(/ (sum$ $?result) (length$ $?result))
	   else
	   	0
	)
)

(defclass max
	(is-a aggregate-function)
)

(defmessage-handler max calc-result ($?result)
	(bind ?class (send ?self get-class))
	(bind ?att (send ?self get-attribute))
	(bind $?types (slot-types ?class ?att))
	(if (or (member$ INTEGER $?types)
		(member$ FLOAT $?types))
	   then
	   	(max-int $?result)
	   else
	   	(max-string $?result)
	)
)

(defclass min
	(is-a aggregate-function)
)

(defmessage-handler min calc-result ($?result)
	(bind ?class (send ?self get-class))
	(bind ?att (send ?self get-attribute))
	(bind $?types (slot-types ?class ?att))
	(if (or (member$ INTEGER $?types)
		(member$ FLOAT $?types))
	   then
	   	(min-int $?result)
	   else
	   	(min-string $?result)
	)
)

(defclass list
	(is-a aggregate-function)
)

(defclass ord_list
	(is-a aggregate-function)
)

(defmessage-handler ord_list calc-result ($?list)
	(bind ?class (send ?self get-class))
	(bind ?att (send ?self get-attribute))
	(bind $?types (slot-types ?class ?att))
	(if (or (member$ INTEGER $?types)
		(member$ FLOAT $?types))
	   then
	   	(sort > $?list)
	   else
	   	(sort string> $?list)
	)
)

(defclass string
	(is-a aggregate-function)
)

(defmessage-handler string calc-result ($?list)
	(funcall str-cat $?list)
)

(defclass phrase
	(is-a aggregate-function)
)

(defmessage-handler phrase calc-result ($?list)
	(str-cat$ $?list)
)

(deffunction is-aggregate-function (?x)
	(and
		(class-existp ?x)
		(subclassp ?x aggregate-function)
	)
)
