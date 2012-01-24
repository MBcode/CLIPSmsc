;-------------------util fncs
(deffunction s-atoi (?str) 
  (if (or (null ?str) (eq ?str "")) then 0 else (atoi ?str)))
;(deffunction gn (?ins) (instance-name-to-symbol ?ins))
;(deffunction gn (?ins) (sub-string 11 55 (str-cat (sym-cat ?ins))))
(deffunction gn (?ins) ?ins)  ;just use instance-name
;=================================================================UPDATEABLE
;anything which is updated/ has a time-stamp /needs an explanation
(defclass UPDATEABLE
 (is-a INITIAL-OBJECT)
 (role concrete) 
 (pattern-match reactive)
					;set these in advance
 (slot expl (type STRING) 		;short description
	(create-accessor read-write) (visibility public))
 (slot time (type INTEGER) 		;time of last update
	(create-accessor read-write) (visibility public))
 ;get/put deamons will update, so can be used for 'freshness'/matching
 (slot get-time (type INTEGER) 		;time of last put bind
	(create-accessor read-write) (visibility public))
 (slot put-time (type INTEGER) 		;time of last get request
	(create-accessor read-write) (visibility public))
(slot fresh   (default FALSE)		;if the proj is newly filled          
	(create-accessor read-write))		
)
;-----------------------------------make-fresh
(deffunction make-fresh (?p)
  (send ?p put-fresh TRUE)
  (if (slot-existp (class ?p) params) then 
	(map1 make-fresh (send ?p get-params))))
  ;will be done during an unpack & by running appropriate subs ?

;-------------------------------------------updateable INIT after
(defmessage-handler UPDATEABLE init after ()
  (bind ?self:time (round (elapse-time))))
;-------------------
;=================================================================ACCESSIBLE
;-------------------
;used for any instance that will be transmitted between unix processes
(defclass ACCESSIBLE
 (is-a UPDATEABLE) ; (is-a INITIAL-OBJECT)
 (role concrete) 
 (pattern-match reactive)

 ;this will be even more of a numeric (rather than str) id, (no necc. msgtag)
 (slot msgtag (type INTEGER) 		;the flag used in the model (vid,fid)
	(create-accessor read-write) (visibility public))
					;set at runtime
 (slot in-task        		        ;task it is in   
	(create-accessor read-write) (visibility public))
 (slot in-tid (type INTEGER) 		;task-id it is in                     ??
	(create-accessor read-write) (visibility public))
 (slot count (type INTEGER) 		;number of this type of instance made
	(create-accessor read-write) (storage shared))
)
;-------------------------------------------accessible INIT after
(defmessage-handler ACCESSIBLE init after ()
  (bind ?self:put-time (round (elapse-time)))
  (if (instance-existp ?self:in-task) then 
    (printout t "[filling in-tid slot]")
    (bind ?self:in-tid (get-tid ?self:in-task))))
;--------------------------------------------------------GET-TAG(s)
(deffunction get-tag (?acc)      ;send in and accessible|| tag get out a tag
  (if (numberp ?acc) then ?acc else (send ?acc get-msgtag)))

(deffunction get-tags ($?accs)   (map1 get-tag ?accs))        ;outputs the tags

;--------------------------------------------------------
;keep simulated real time/ real clock time ratio   -to see how its doing
;--------------------------------------------------------EOF
