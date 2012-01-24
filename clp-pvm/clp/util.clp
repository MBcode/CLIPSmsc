;CLIPS version of ACA		c-util.fnc
;c-util.fnc	Basic utility fns that don't depend on the *.lib files
;			There are funcal/mapcar/etc  fncs on list etc
;-------------------------------------------------------------
;The major efficiency issue probably comes in rewriting some of the more
;often used util fncs, so there are not any interveneing lists.
;More safty for fncs w/ different #s of slots can also be done then.
;(as an example look at the util fncs rewritten as handlers in c-util2.fnc
; more of something like this would be good.)
;-------------------------------------------------------------

;--global variables----------------------------------------------------
;these are now in c-gloabl.bnd
;(defglobal ?*pol-data-counter* = 0)
;(defglobal ?*infringement-counter* = 0)
;(defglobal ?*case-study-counter* = 0)
;(defglobal ?*qualities-counter* = 0)
;(defglobal ?*copy-counter* = 0)
;(defglobal ?*control-counter* = 0)
;;(defglobal ?*gpq* = (create$))

(deffunction cons ($?args)  ?args)

;--MAKE-LAMBDA-----------------------------------------------------
(deffunction make-lambda (?lstr)
    (bind ?name (gensym))
    (build (str-cat "(deffunction " ?name " " ?lstr ")"))
    ?name)


;--TO-STR-----------------------------------------------------
(deffunction to-str ($?args) (implode$ ?args))
(deffunction    str ($?args) (implode$ ?args))
;CLIPS> (to-str hi there two)
;"hi there two"
;CLIPS> (str-cat hi there two)
;"hitheretwo"

;to-str could be used to create strings for msg-passing:
(deffunction paren (?s) (str-cat "(" ?s ")"))
(deffunction to-pstr ($?args) (paren (to-str ?args)))
(deffunction quotes   ($?args) (paren (to-str ?args)))
(deffunction quote    ($?args) (sym-cat "(" (implode$ ?args) ")"))
(deffunction quote-mf ($?args) (sym-cat "(create$ " (implode$ ?args) ")"))
(deffunction quote-list ($?args) (sym-cat "(create$ " (implode$ ?args) ")"))
(deffunction prn     ($?args) (paren (to-str ?args)))
;to-pstr is similar to quote, because it isn't evaluated (but is a str)
;so it will be evaluated when eval-ed

(deffunction print (?p) 
 (if (instance-existp ?p) then (send ?p print)))
;(if (message-handler-existp (class ?p) print) then (send ?p print)) -no

;--INCR--------------------------------------------------------
;this function takes a number and returns that number plus 1
(deffunction incrn (?v) (bind ?v (+ ?v 1)))

;this function takes a string w/ a variables name in it
;this variable is then incremented by one  (used for instance counters)
(deffunction incr (?var-str ?amt) 
	(eval (format nil "(bind \?%s (+ \?%s %d))" ?var-str ?var-str ?amt)))

;--NULL----------------------------------------------------
;just like null in LISP
(deffunction null (?a) (if (eq ?a nil) then TRUE else FALSE))

(deffunction null-lv (?a) 
  (if (or (and (multifieldp ?a) (eq (length ?a) 0)) (eq ?a nil)) then TRUE 
   else FALSE))

;--FUNCALL----------------------------------------------------
;similar to funcall in LISP, except non of the arguments can be lists
;(deffunction funcall (?fnc $?args)  
;	(eval (format nil "(%s %s)" ?fnc (implode$ ?args))))
;w/out the eval this has the same effect as quotes

;--FUNCALL-L----------------------------------------------------
;similar to funcall in LISP, except only the 1st argument can be list
(deffunction funcall-l (?fnc ?l $?args)  
 (eval (format nil "(%s (create$ %s) %s)" ?fnc (implode$ ?l) (implode$ ?args))))

;------------sym-to-ins
;a form of symbol-to-instance name that can take many args
(deffunction sym-to-ins ($?n)
 (symbol-to-instance-name  (funcall sym-cat ?n)))

(deffunction ins-existp ($?n) 
  (instance-existp (sym-to-ins ?n)))

;--LENGTH-LV----------------------------------------------------
(deffunction length-lv (?lv)
    (if (multifieldp ?lv) then (length ?lv) else 1))

;--NTH1-LV----------------------------------------------------
;returns the 1st elt. if multifield or value if a value
(deffunction nth1-lv (?lv)
	(if (multifieldp ?lv) then (nth$ 1 ?lv) else ?lv))

;--NTH-LV----------------------------------------------------
(deffunction nth-lv (?n ?lv)
	(if (multifieldp ?lv) then (nth$ ?n ?lv) else ?lv))

;--FIRST----------------------------------------------------
(deffunction first (?lv) (nth1-lv ?lv))
;	(if (multifieldp ?lv) then (first$ ?lv) else ?lv)
(deffunction second (?lv) (nth-lv 2 ?lv))
(deffunction third (?lv) (nth-lv 3 ?lv))

;--DFLT----------------------------------------------------
(deffunction dflt (?val ?dflt)
 (if (null ?val) then ?dflt else ?val))

;--NTH-DFLT----------------------------------------------------
(deffunction nth-dflt (?n ?lv ?dflt)
 (dflt (nth-lv ?n ?lv) ?dflt))

(deffunction first-dflt (?lv ?dflt) (dflt (first ?lv) ?dflt))
(deffunction second-dflt (?lv ?dflt) (dflt (nth-lv 2 ?lv) ?dflt))
(deffunction third-dflt (?lv ?dflt) (dflt (nth-lv 3 ?lv) ?dflt))

;(deffunction first-dflt (?lv ?dflt) (nth-dflt 1 ?lv ?dflt))
;(deffunction second-dflt (?lv ?dflt) (nth-dflt 2 ?lv ?dflt))
;(deffunction third-dflt (?lv ?dflt) (nth-dflt 3 ?lv ?dflt))

;--IN-RANGE----------------------------------------------------
;given value & range as  2values or as a m.f. return a val that is in the range
(deffunction in-range (?value $?range)
  (bind ?min (first-dflt ?range 0))
  (bind ?max (second-dflt ?range ?value))
  (if (> ?value ?max) then 
      (printout t "[warning " ?value " is > max of " ?max "]")
      (return ?max))
  (if (< ?value ?min) then
      (printout t "[warning " ?value " is < min of " ?min "]")
      (return ?min))
  ?value)
;if the in-rage call = the value then it was ok == in-range-p
;could have a version w/inst-sn & if not in range it would set it

;--SLOT-VALUE----------------------------------------------------
(deffunction slot-value (?ins ?sn)
  (if (slot-existp (class ?ins) ?sn inherit) then (send ?ins (sym-cat get- ?sn))
   else (printout t "[slot-value " ?ins " does not have a " ?sn " slot]"))) 

;--SLOT-PUT-VALUE---------------------------------------------
(deffunction slot-put-value (?ins ?sn ?val)
  (if (slot-existp (class ?ins) ?sn inherit) then
      (send ?ins (sym-cat put- ?sn) ?val)
   else (printout t "[slot-put-value " ?ins " does not have a " ?sn " slot]")))
;might need to check if write access & if val=mf then if slot-facet1=MLT

;--REPLACE----------------------------------------------------
;a version of replace$ that will append if out of range
(deffunction replace (?l ?a1 ?a2 ?l2)
  (bind ?ml (length$ ?l))
  (bind ?n1 (min ?a1 ?ml))
  (bind ?n2 (min ?a2 ?ml))
  (if (neq ?a1 ?n1) then (bind ?l2 (create$ (nth$ ?ml ?l) ?l2)))
  (replace$ ?l ?n1 ?n2 ?l2))

;--SLOT-REPLACE-----------------------------------------------
;(deffunction replace-mslot-value (?inst ?slot ?a1 ?a2 ?l2)
; (bind ?l (slot-value ?inst ?slot))
; (if (not (multifieldp ?l)) then (bind ?l (create$ ?l)))
; (slot-put-value ?inst ?slot (replace ?l ?a1 ?a2 ?l2)))

(deffunction slot-replace (?inst ?slot ?a1 ?a2 ?l2)
  (bind ?l  (slot-value ?inst ?slot))  ;list w/ values to replace (append)
  (bind ?ml (length$ ?l))              ;it's lenght
  (bind ?n1 (min ?a1 ?ml))             ;make sure that the replace bounds <= ml
  (bind ?n2 (min ?a2 ?ml))             ;if >then append
  ;if lower replace bnd > ml, then insert end of old list at begin of new
  ; so when it replaces the end of the old, that elt won't be lost
  ;(if (neq ?a1 ?n1) then (bind ?l2 (create$ (nth$ ?ml ?l) ?l2)))
  (if (neq ?a1 ?n1) then (bind ?l2 (create$ (nth$ ?ml ?l) ?l2)))
  (slot-replace$ ?inst ?slot ?n1 ?n2 ?l2))

;--APPEND----------------------------------------------------
(deffunction append (?l ?a)
  (insert$  ?l  (+ 1 (length$ ?l)) ?a))

;--SLOT-APPEND-----------------------------------------------
(deffunction slot-append (?inst ?slot ?a)
  (slot-insert$  ?inst ?slot  (+ 1 (length$ (slot-value ?inst ?slot))) ?a))

;--REST-LV----------------------------------------------------
;returns the rest of the elts. if multifield or value if a value
(deffunction rest-lv (?lv)
	(if (multifieldp ?lv) then (rest$ ?lv) else ?lv))

;==MAP===APPLY=========
;add $?args (for flexability)
;then map# would be themax # of lists & min # of total arguments
;--MAP1--APPLY1--------------------------------------------------

;takes a function and at least 1 argument, but not more than 1 list
;then acts like LISP's mapcar, returning a multifield of the results
(deffunction map1 (?f1 ?lv1 $?args)
(if (not (multifieldp ?lv1)) then 
    (printout t "[no mf]")
    (return (funcall ?f1 ?lv1 ?args)))
(if (eq (create$ ) ?lv1) then (create$ ) else
   (create$ (funcall ?f1 (nth1-lv ?lv1) ?args)
	    (map1 ?f1 (rest-lv ?lv1) ?args))))

;takes a function and at least 1 argument, but not more than 1 list
;then acts like LISP's apply, working only through side-effects
(deffunction apply1 (?f1 ?lv1 $?args)
(if (not (eq (create$ ) ?lv1)) then (funcall ?f1 (nth1-lv ?lv1) ?args)
				    (apply1 ?f1 (rest-lv ?lv1) ?args)))

(deffunction apply-1 (?f1 ?lv1 $?args)
 (loop-for-count (?i 1 (length$ ?lv1)) do (funcall ?f1 (nth-lv ?i ?lv1) ?args)))

;--MAP2--APPLY2--------------------------------------------------

;takes a function and at least 2 arguments, but not more than 2 lists
;then acts like LISP's mapcar, returning a multifield of the results
(deffunction map2 (?f2 ?lv1 ?lv2 $?args)
(if (and (not (multifieldp ?lv1)) (not (multifieldp ?lv2))) then 
    (printout t "[no mf]")
    (return (funcall ?f2 ?lv1 ?lv2 ?args)))
;(if (and (eq (length ?lv1) 1) (eq (length ?lv2) 1)) then (printout t "[no ln]")
;    (funcall ?f2 ?lv1 ?lv2 ?args))
(if (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2))
	then (create$ ) else
  (create$ (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) ?args)
           (map2 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) ?args))))

;takes a function and at least 2 arguments, but not more than 2 lists
;then acts like LISP's apply, working only through side-effects
(deffunction apply2 (?f2 ?lv1 ?lv2 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)))
 then (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) ?args)
      (apply2 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) ?args)))

(deffunction apply-2 (?f1 ?lv1 ?lv2 $?args)
 (bind ?l (max (length-lv ?lv1) (length-lv ?lv2)))
 (loop-for-count (?i 1 ?l) do 
   (funcall ?f1 (nth-lv ?i ?lv1) (nth-lv ?i ?lv2) ?args)))

;--MAP-LV---APLLY-LV-------------------------------------------------
;with the ?args  map1 could replace map-lv  <---*
;it is in hap.fnc & set.hnd right now, but can change

;the 2nd arg is treated as a value, even if it is a multifield
(deffunction map-lv (?f2 ?lv1 ?v2)
(if (eq (create$ ) ?lv1) then (create$ ) else
  (create$ (funcall ?f2 (nth1-lv ?lv1) ?v2)
           (map-lv ?f2 (rest-lv ?lv1) ?v2))))

(deffunction apply-lv (?f2 ?lv1 ?v2)
(if (eq (create$ ) ?lv1) then (funcall ?f2 (nth1-lv ?lv1) ?v2)
			      (map-lv ?f2 (rest-lv ?lv1) ?v2)))

;--MAP3--APPLY3--------------------------------------------------

;takes a function and at least 3 arguments, but not more than 3 lists
;then acts like LISP's mapcar, returning a multifield of the results
(deffunction map3 (?f2 ?lv1 ?lv2 ?lv3 $?args)
(if (and (not (multifieldp ?lv1)) (not (multifieldp ?lv2)) (not (multifieldp ?lv3))) then 
    (printout t "[no mf]")
    (return (funcall ?f2 ?lv1 ?lv2 ?lv3 ?args)))
(if (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)
	(eq (create$ ) ?lv3)) then (create$ ) else
  (create$ (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) ?args)
           (map3 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) ?args))))

;takes a function and at least 3 arguments, but not more than 3 lists
;then acts like LISP's apply, working only through side-effects
(deffunction apply3 (?f2 ?lv1 ?lv2 ?lv3 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2) (eq (create$ ) ?lv3))) then 
   (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) ?args)
   (apply3 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) ?args)))

(deffunction apply-3 (?f1 ?lv1 ?lv2 ?lv3 $?args)
 (bind ?l (max (length-lv ?lv1) (length-lv ?lv2) (length-lv ?lv3)))
 (loop-for-count (?i 1 ?l) do 
   (funcall ?f1 (nth-lv ?i ?lv1) (nth-lv ?i ?lv2) (nth-lv ?i ?lv3) ?args)))

;--MAP4--APPLY4--------------------------------------------------

;takes a function and at least 4 arguments, but not more than 4 lists
;then acts like LISP's mapcar, returning a multifield of the results
(deffunction map4 (?f2 ?lv1 ?lv2 ?lv3 ?lv4 $?args)
(if (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)
	(eq (create$ ) ?lv3) (eq (create$ ) ?lv4)) then (create$ ) else
 (create$ (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) (nth1-lv ?lv4) ?args)
          (map4 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) (rest-lv ?lv4) ?args))))

;takes a function and at least 4 arguments, but not more than 4 lists
;then acts like LISP's apply, working only through side-effects
(deffunction apply4 (?f2 ?lv1 ?lv2 ?lv3 ?lv4 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)
	(eq (create$ ) ?lv3) (eq (create$ ) ?lv4))) then 
 (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) (nth1-lv ?lv4) ?args)
 (apply4 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) (rest-lv ?lv4) ?args)))

(deffunction apply-4 (?f1 ?lv1 ?lv2 ?lv3 ?lv4 $?args)
 (bind ?l (max (length-lv ?lv1) (length-lv ?lv2) (length-lv ?lv3) (length-lv ?lv4)))
 (loop-for-count (?i 1 ?l) do 
 (funcall ?f1 (nth-lv ?i ?lv1) (nth-lv ?i ?lv2) (nth-lv ?i ?lv3) (nth-lv ?i ?lv4) ?args)))

;--MAP4--APPLY4--------------------------------------------------

;--MAP----------------------------------------------------
;(deffunction map (?fnc $?lvs)
; (if (eq (nth1-lv ?lvs) nil) then (create$ ) else
;          (create$ (funcall ?fnc (map1 nth1-lv ?lvs)) (map ?fnc (rest$ ?lvs)))))
;;(deffunction smap (?fnc $?lvs)
;;	(if (eq (map1 length$ ?lvs)) then (map ?fnc ?lvs) else FALSE))
;all the lists will get glommed together
;mabye by giving # of lists?

;--SMAP#----------------------------------------------------
;not much used,  only insures that the mapping is done only if same sized lists
;should have a warning msg if used

(deffunction smap2 (?f2 ?l1 ?l2)
    (if (eq (length$ ?l1) (length$ ?l2)) 
	then (map2 ?f2 ?l1 ?l2) else FALSE))

(deffunction smap3 (?f2 ?l1 ?l2 ?l3)
    (if (eq (length$ ?l1) (length$ ?l2) (length$ ?l3)) 
	then (map3 ?f2 ?l1 ?l2 ?l3) else FALSE))

(deffunction smap4 (?f2 ?l1 ?l2 ?l3 ?l4)
    (if (eq (length$ ?l1) (length$ ?l2) (length$ ?l3) (length$ ?l4)) 
	then (map4 ?f2 ?l1 ?l2 ?l3 ?l4) else FALSE))

;------------------------------------------------------
;don't think this is used,

(deffunction flush (?e1 ?l1)
    (if (eq (length$ ?l1) 1)
	then (if (eq ?e1 (nth$ 1 ?l1))  
		then (delete$ ?l1 1 1) else ?l1)
	else (if (member$ ?e1 ?l1)
		then (flush ?e1 (delete$ ?l1 (member$ ?e1 ?l1) 
					     (member$ ?e1 ?l1)))
		else ?l1)))

;could be rewritten
(deffunction inter-section (?l1 ?l2)
    (flush FALSE (smap2 same-element ?l1 ?l2)))

;use subsetp in related fnc
;------------------------------------------------------

;if zero predicate,  checks for 0 or 0.0  (which is not done automatically)***
(deffunction zerop (?n) (and (numberp ?n) (eq (float ?n) 0.0)))

;turns nulls into 0.0 otherwise leaves the argument alone
(deffunction nn (?v) (if (null ?v) then 0.0 else ?v))

(deffunction nnfloat (?n) (float (nn ?n)))
(deffunction nnint (?n) (integer (nn ?n)))

;turns non-numbers into 0.0 otherwise leaves the argument alone
(deffunction nnn (?v) (if (numberp ?v) then ?v else 0.0))

;turns non-numbers into 0.0 otherwise leaves the argument alone
;warns you if it had to do this
(deffunction nnnw (?v) (if (numberp ?v) then ?v else 
 (printout t "[WARNING] from nnnw, got " ?v crlf) 0.0))


;a mni & max fncs that uses nnn on its arguments
(deffunction nn-max (?a ?b) (if (> (nnn ?a) (nnn ?b)) then ?a else ?b))
(deffunction nn-min (?a ?b) (if (< (nnn ?a) (nnn ?b)) then ?a else ?b))

;a >= predicate
(deffunction biggerp (?a ?b) (>= (nn ?a) (nn ?b)))

;full is the same as not-null
(deffunction full (?x) (not (null ?x)))
(deffunction full-lv (?v) (not (null-lv ?v)))


;predicates to see if positive or negative
;not sure these should use nnn
(deffunction pos  (?x) (> (nnn ?x) 0.0))
(deffunction neg  (?x) (< (nnn ?x) 0.0))

;--SDIV----------------------------------------------------
;safe division takes a numerator & denominator & devides them
;it assumes numbers, and only checks that the divisor is <> zero
(deffunction sdiv (?n ?d) 
  (if (or (eq ?d 0.0) (eq ?d 0))
	 then (printout t "[warning] sdiv got " ?n " by0" crlf) (return 0.0)) 
  (/ ?n ?d))

;--SFUNCALL----------------------------------------------------

;a safe version of funcall for a fnc & 2 args (not really used)
(deffunction sfuncall (?fnc ?a1 ?a2)
   (if (null ?fnc) then (printout t "[Warning] nil FNC a1=" 
					?a1 " a2=" ?a2 crlf)
     else (if (null ?a1)
		   then (printout t "[Warning] nil ?a1 fnc=" 
					?fnc " a2=" ?a2 crlf)
	     else (if (null ?a2)
		   then (printout t "[Warning] nil ?a2 fnc=" 
					?fnc " a1=" ?a2 crlf)
		    else (funcall ?fnc ?a1 ?a2)))))

;--TRUNC----------------------------------------------------

;(deffunction trunc (?num ?pl)
;  (bind ?m (** 10.0 ?pl))
;  (- ?num (/ (- (* ?num ?m) (round (- (* ?num ?m) 0.5))) ?m)))

;trucate a ?number to ?pl places
;?pl can be negative
(deffunction trunc (?num ?pl)
  (bind ?m (** 10.0 ?pl))
  (/ (integer (round (* ?num ?m))) ?m))

;--check-fncs----------------------------------------------------
;only used in c-util, review this
;brought over from GW version,  I like having explicit checking
;in the functions that funcall (map/apply) are called with.

;applies the given function (fnc) if at least one is a numbers

(deffunction check2num (?fnc ?v ?v2) 
 (if (and (numberp ?v) (numberp ?v2)) then (funcall ?fnc ?v ?v2)
   else (if (and (numberp ?v) (null ?v2)) then 0.0
	  else (if (and (null ?v) (numberp ?v2)) then 0.0
	    	  else (if (and (null ?v) (null ?v2)) then nil
		         else ?v)))))


(deffunction checkf2n0 (?fnc ?v ?v2) 
 (if (and (numberp ?v) (numberp ?v2)) then (funcall ?fnc ?v ?v2)
   else (if (and (numberp ?v) (null ?v2)) then (funcall ?fnc ?v 0.0)
	  else (if (and (null ?v) (numberp ?v2)) then (funcall ?fnc 0.0 ?v2)
	    	  else (if (and (null ?v) (null ?v2)) then nil
		         else (sfuncall ?fnc ?v ?v2))))))

;---------------------------------------------------

;returns a list of values from 2 lists for which fnc was applied

(deffunction fnc-two-listsn0 (?fnc ?l1 ?l2)	
	(map2 ?fnc ?l1 ?l2))

;--LOCAL-SLOTNAMES----------------------------------------------

;given an instance, return the slotnames from that class only,no inhereted slots
(deffunction local-slotnames (?ins)  (class-slots (class ?ins)))

;--SLOTNAMES----------------------------------------------------

;given an instance, return the slotnames from that class only,w/ inhereted slots
(deffunction slotnames (?ins)  (class-slots (class ?ins) inherit))

;--SUM-SLOTS----------------------------------------------------

;given an instance and a list of slotnames in that instance
;sum up all of the given values
(deffunction sum-slots-sn (?ins ?snames)
  (if (not (instancep ?ins))  then
	(printout t "[Warning] bad sumslots ins=" ?ins " sn=" ?snames crlf)
	(return 0.0))
  (if (eq (nth$ 1 ?snames) nil) then (return 0.0)
    else  (+ (nnn (send ?ins (sym-cat get- (nth$ 1 ?snames))))
	 	       (sum-slots-sn ?ins (rest$ ?snames)))))

;given an instance sum up all of the values
(deffunction sum-slots (?ins)
	(sum-slots-sn ?ins (slotnames ?ins)))

;could get all the values and funcall + vals

;--SLOT-VALUE----------------------------------------------------

;given an instance and slotname, return the value
;it checks to make sure that the args are not nil
;it could check to make sure that the slot is there too
;(deffunction slot-value (?ins ?sn)
;   (if (null ?ins) 
;	then (printout t "[Warning] nil INST sn=" ?sn crlf)
;	else (if (null ?sn)
;		then (printout t "[Warning] nil sn int=" ?ins crlf)
;		else (send ?ins (sym-cat get- (str-cat ?sn))))))

;(deffunction slot-value (?ins ?sn)
; (if (slot-existp (class ?ins) ?sn inherit) then (send ?ins (sym-cat get- ?sn))
;   else (printout t "[slot-value " ?ins " does not have a " ?sn " slot]"))) 

;slot-value is pretty safe now, probably won't need sslot-value

;given an instance and slotname, return the value
;it checks to make sure that the args are not nil
;it also checks to make sure that the slot is there too
(deffunction sslot-value (?ins ?sn)
  (if (null ?ins) then 
	(printout t "[Warning] nil INST sn=" ?sn crlf) (return nil))
  (if (not (slot-existp (class ?ins) ?sn inherit)) then
	(printout t "[Warning] no slot named=" ?sn " for " ?ins crlf)
	(return nil))

  (if (null ?sn) then (printout t "[Warning] nil sn int=" ?ins crlf)
		 else (send ?ins (sym-cat get- (str-cat ?sn)))))

;--LOCAL-SLOTS-------------------------------------------------

;I think slotnames already does this (check)
(deffunction local-slots (?inst)
  (if (instancep (symbol-to-instance-name ?inst))
	then (map1 class-slots (class-superclasses (class ?inst)))
	else (create$ nil)))

;;;; then (frame-local-slots (instance-parent ?inst)) 

;;--SLOT-VALUES------------------------------------------------------------------
;returns list of slot values of an instance (?inst)

;given an instance, return a list of the slot values
;used in printing etc.
(deffunction slot-values (?inst)	
    (map2 sslot-value ?inst (slotnames ?inst)))

;more dangerous to use in utility fncs, because the slots from 2 instance
;might not line up.  Better not to have temp. lists anyway.

;;--SLOT-LOCAL-VALUES------------------------------------------------------------
;returns list of local [no inherited values]  slot values of an instance (inst)

;given an instance, return a list of the slot values 
;local to the instance's class
;used in printing etc.
(deffunction slot-local-values (?inst)	      
    (map2 sslot-value ?inst (local-slotnames ?inst)))

;---INSTANCE-ADD-VALUES----------------------------------------------------------

;adds values (values) into all the slots of the instance (inst)

;given an instance and a list of values, put the values in the slots
(deffunction instance-add-values (?inst ?values)		
    (smap2 bind (slotnames ?inst) ?values)
    ?inst)					; return the instance

;might be nice to have a version that takes the specific slotnames either
;in a seperate list, like w/  instance-add-sn-values (?inst ?values ?slotnames)
;or in the same list (eg. slotname value slotname value)
;--------------------------------------------------------------------

;add only local values (values) into all slots of the instance (?inst)

;a version of instance-add-values that works only on the uninherited slots
(deffunction instance-add-local-values (?inst ?values)	  
    (map3 rput ?values ?inst (local-slotnames ?inst))
    ?inst)					; return the instance

;--------------------------------------------------------------------
;having ?sn be 2 long, is unwise because ?self:slotname works,but is still legal

;given an instance and list of slotnames to track down, return the value
;it is a safe way to get a value
;?ins is the instance to get from
;?sn is the list of slot-names to track down
(deffunction rget (?ins $?sn)
   (if (not (instancep ?ins))  then 
     (printout t "[Warning] Bad rget ins=" ?ins " sn=" ?sn crlf) (return 0.0))
   (if (eq (length$ ?sn) 1) then			;return last value
	      (bind ?var (send ?ins (sym-cat get- (nth$ 1 ?sn))))
	      (if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
	 else	;do again
	      (bind ?var (send ?ins (sym-cat get- (nth$ 1 ?sn))))
	      (if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
  	      (rget ?var (rest$ ?sn))))

;a version more like remote-put
;like rget, but takes ?val and does a put-LAST-SLOT-NAME
;?val is the value to put, ?ins is the instance to put it in, and
;$?sn is the slot or list of slots to traverse   so it can be put in the last 1
(deffunction rput (?val ?ins $?sn)
   (if (not (instancep ?ins))  then 
      (printout t "[Warning] Bad rput val=" ?val " ins=" ?ins " sn=" ?sn crlf) 
	(return 0.0))
   (if (eq (length$ ?sn) 1) then  ;put value
	      (bind ?var (send ?ins (sym-cat put- (nth$ 1 ?sn)) ?val)) 
	      (if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
  	else ;do again
	      (bind ?var (send ?ins (sym-cat get- (nth$ 1 ?sn))))
	      (if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
  	      (rput ?val ?var (rest$ ?sn))))

;remote-get/put has extra safty features
;--------------------------------------------------------------------
;---NOTE will have to use slots-add-values when instances are not the same
;        so will need a parallel list of slotnames to got w/ the values to add
;add values into some slots (named in the list slotnames), into instance (inst)

(deffunction instance-add-sn-values (?inst ?values ?slotnames)		
    (smap2 bind ?slotnames ?values)
    ?inst)					; return the instance

;--DATA-FNC------------------------------------------------------------------

;returns a list of the result of a function on 2 instances' slot values
;only used for pollution-data instances at the moment  
;	[fnc on any 2 similar inst]

;returns a list of the result of a function on 2 instances' slot values
;can (like pd-fnc be used for any inst), careful if numeric fnc

;takes a functions name, and 2 instances on which the fnc will work
;it returns a list of the fnc results
;(will want to move away from intermediate lists, & assuming similar instances)
(deffunction data-fnc (?fnc ?pd1 ?pd2) 		
    (fnc-two-listsn0 ?fnc (slot-local-values ?pd1)  (slot-local-values ?pd2)))

;--------------------------------------------------------------------

;a version of data-fnc that doesn't use one of the 'check' fncs
;(the 'check' fncs might be phased out)
(deffunction data-fnc-nc (?fnc ?pd1 ?pd2) 		
    (map2 ?fnc (slot-local-values ?pd1)  (slot-local-values ?pd2)))

;--------------------------------------------------------------------
;replace w/ the assoc map fncs
;(I think it is better to just call the appropriate map fnc directly
;because it is not clear which ones will call slot-local-values
;so phase these out.)

(deffunction data-fnc-3 (?fnc ?pd1 ?pd2 ?x)
    (map3 ?fnc (slot-local-values ?pd1) (slot-local-values ?pd2) ?x))

(deffunction data-fnc-4 (?fnc ?pd1 ?pd2 ?x ?y)
    (map4 ?fnc (slot-local-values ?pd1) (slot-local-values ?pd2) ?x ?y))

;(deffunction data-fnc-4 (?fnc ?pd1 ?x ?y ?z) 		
;    (map4 ?fnc (slot-local-values ?pd1) ?x ?y ?z))

;--------------------------------------------------------------------

;note that both data-fnc and data-fnc-nc do the same thing!
;--NOTE  -the fnc-two-lists gets the slot local values, 
;	which might not be the same
;---a CHECK should be made to make sure the fnc is only done on similar slots
;could take the intersection of the slotname list, then get the lists from that
;-or have the fnc-2- get the slot-values itself
;--MIGHT have to put the values into yet another list-
;  might be a good reason to stay w/ an intersection list
;        & use it both when getting values & setting them
;-or calc the intersection once & send to both the fnc-2- & set-data

;--DATA-FNC1---------===================================================

;one instance version
(deffunction data-fnc1 (?fnc ?pd1) 	
    (map1 ?fnc (slot-local-values ?pd1)))

;returns a list of the result of a function on an instances' slot values 
;& a single value
;only used for pollution-data instances at the moment

;---------------------------------------=============================

;--SET-DATA-W-VALUES---------------------=============================

;adds the values in the list (?values) into the instance (pd1's) slots

(deffunction set-data-w-values (?pd1 ?values)
    (instance-add-local-values ?pd1 ?values))

;---------------------------------------=============================

;adds the values in the list (values) into the instance (pd1's) slots

;(deffunction gen-set-data-w-values (?pd1 ?pd2 ?values)
;    (instance-add-sn-values ?pd1
;		(intersection (local-slots ?pd1) (local-slots ?pd2))
;		?values))

;------------------------------------------------------


;------------------------------------------------------

;---NOTE will have to use slots-add-values when instances are not the same
;        this is from set-data-w-values,  will need a slotname list
;fills/replaces pd-inst1 with values from (?fnc ?pd-inst1 ?pd-inst2)

(deffunction set-data-from-fnc (?fnc ?pd-inst1 ?pd-inst2)
	(set-data-w-values ?pd-inst1 (data-fnc-nc ?fnc ?pd-inst1 ?pd-inst2)))

;------------------------------------------------------
;------------------------------------------------------

; Standard Temperature = 298.15 degrees K
; Standard Pressure = 101360.0 N/m^2
; Function to make flow rate the flow at standard conditions
;     Inputs: flowrate is actual flowrate
;             temp  is actual temperature (degrees K)
;             pres is actual pressure in N/m^2

(deffunction flowrate-at-STP (?flowrate ?temp ?pres)
        (/ (* ?pres ?flowrate 298.15) (* ?temp 101360.0)) )

;------------------------------------------------------

; Function to make flow rate the actual flow at given pressure and temperature
;     Inputs: flowrate is standard flowrate
;             temp  is actual temperature (degrees K)
;             pres is actual pressure in N/m^2

(deffunction flowrate-at-actual (?flowrate ?temp ?pres)
        (/ (* 101360.0 ?flowrate ?temp) (* 298.15 ?pres)) )

;------------------------------------------------------

; Using the MKS system:
;	TEMP is in degrees K, 
;	MW is unitless
;	P is in N/m2,  (standard presure, one atm is 101360. N/m2)
;	density is in kg/m3
;       ppm is unit-less

(deffunction density-to-ppm (?density ?MW ?TEMP ?P)
        (/ (* (nn ?density) 1000000. 8.314 1000. ?TEMP ) 
	   (* ?MW (if (null ?P) then 101360.0 else ?P))))

;------------------------------------------------------

(deffunction ppm-to-density (?ppm ?MW ?TEMP ?P)
        (/ (* ?ppm ?MW (if (null ?P) then 101360.0 else ?P)) 
	   (* 8.314 1000. ?TEMP 1000000. )))

;------------------------------------------------------
;------------------------------------------------------

;given a list of values, sum them up
(deffunction sumlist (?l)
     (if (eq ?l (create$ )) 			;(null ?l) 
       then 0.0 
       else (+ (nnn (nth$ 1 ?l)) (sumlist (rest$ ?l)))))

;------------------------------------------------------
;returns min number in a list
;can use (min 1 2 3)  if not alread in a list.
(deffunction min-list (?l)  (funcall min ?l))				
(deffunction max-list (?l)  (funcall max ?l))				

;------------------------------------------------------

;was used in gw version, 
;(deffunction run-cases ()
;  (load "c-disregard.rul")
;  (run)
;  (load "c-case.rul")
;  (load "c-hap.rul")
;  (run))

;--------------------------------------
;choose 1 of these

;takes a function, 2 input instances, and an output instance
(deffunction map2inst-f (?fnc ?in1 ?in2 ?out)
  (bind ?sn (slotnames ?out))
  (loop-for-count (?i 1 (length$ ?sn)) do
    (bind ?gs (sym-cat get- (nth$ ?i ?sn)))
    (send  (funcall ?fnc (send ?in1 ?gs) (send ?in2 ?gs))
	   (sym-cat put- (nth$ ?i ?sn)))))  
	   ;make a version that dumps to a list too

;--PRINT-INS--------------------------------------------------
;takes an instance, and send it the message to print itself
(deffunction print-ins (?ins)
  (if (instancep ?ins) then (send ?ins print)
		else (printout t "[Warning] print-ins got " ?ins crlf)))

;--PRINT-ALL-INS--------------------------------------------------
;takes an instance, and send it the message to print itself
(deffunction print-all-ins (?class)
  (map2 send (find-all-instances ((?t ?class)) TRUE ) print))

;--APPLY-R------------------------------------------------------------
;these are presently (unused) -
;it is a version of apply that takes its result as an arg to the recursive call
;then if ?fnc  wants to   use it  it can,  can be used for summing etc

;version that applys fnc to top of list and to the recursive product
;if there is a test-fnc it is incorperated into ?fnc
(deffunction apply-r (?fnc ?l $?args)
  (if (= (length$ ?l) 0) then (return (create$ )))
   then (funcall ?fnc  (first$ ?l)   (apply-r ?fnc (rest$ ?l) ?args)))

(deffunction apply2-r (?f2 ?lv1 ?lv2 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)))
 then (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) 
         (apply2-r ?f2 (rest-lv ?lv1) (rest-lv ?lv2) ?args) ?args)))

(deffunction apply3-r (?f2 ?lv1 ?lv2 ?lv3 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2) (eq (create$ ) ?lv3))) then 
   (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3)
        (apply3-r ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) ?args) ?args)))

(deffunction apply4-r (?f2 ?lv1 ?lv2 ?lv3 ?lv4 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)
	(eq (create$ ) ?lv3) (eq (create$ ) ?lv4))) then 
 (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) (nth1-lv ?lv4)
        (apply4-r ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) (rest-lv ?lv4) 
	?args) ?args)))


;--APPLY-IF------------------------------------------------------------
;version of apply that applies ?afnc to top of list and to the recursive product
;has a seprate test-function
;could use for collect-if or to sum up etc
(deffunction apply-if (?tfnc ?afnc ?l $?args)
  (if (= (length$ ?l) 0) then (return (create$ )))
  (if (funcall ?tfnc (nth$ 1 ?l) ?args) 
   then (funcall ?afnc  (first$ ?l)   (apply-if ?tfnc ?afnc (rest$ ?l) ?args))
   else 			      (apply-if ?tfnc ?afnc (rest$ ?l) ?args)))

;--COLLECT-IF------------------------------------------------------------
;like the LISP fnc
;takes a test-fnc a list & some optional args
;and creates a list of the values that pass the test
(deffunction collect-if (?tfnc ?l $?args)
  (apply-if ?tfnc create$ ?l ?args))

;(deffunction collect-if (?tfnc ?l $?args)
;  (if (= (length$ ?l) 0) then (return (create$ )))
;  (if (funcall ?tfnc (nth$ 1 ?l) ?args) 
;   then (create$  (first$ ?l)   (collect-if ?tfnc (rest$ ?l) ?args))
;   else 			(collect-if ?tfnc (rest$ ?l) ?args)))

;--COUNT-IF----------------------------------------------------
;takes a test-fnc and a list and counts the # of times the test-fnc passes
(deffunction count-if (?tfnc ?l)  (apply-if ?tfnc incrn ?l))

;counts the number of TRUEs in a list
(deffunction count-true (?l)
  (if (eq (length$ ?l) 0) then (return 0))
  (if (eq (nth$ 1 ?l) TRUE) then (+ (count-true (rest$ ?l)) 1)
		  	    else    (count-true (rest$ ?l))))

;--REMOVE-IF------------------------------------------------------------
;like collect-if but returns of list of everything that did not pass the ?tfnc
(deffunction remove-if (?tfnc ?l)
  (if (funcall ?tfnc (first$ ?l)) then 
   	                     (remove-if ?tfnc (rest$ ?l)))
   else	(create$ (first$ ?l) (remove-if ?tfnc (rest$ ?l))))
(deffunction collect_if (?tfnc ?l)
  (if (funcall ?tfnc (first$ ?l)) then 
	(create$ (first$ ?l) (collect_if ?tfnc (rest$ ?l)))
   else	                     (collect_if ?tfnc (rest$ ?l))))

(deffunction remove-if- (?tfnc ?l $?args)
  (if (funcall ?tfnc (first$ ?l) ?args) then 
	(create$ (first$ ?l) (remove-if- ?tfnc (rest$ ?l) ?args))
   else	                     (remove-if- ?tfnc (rest$ ?l) ?args)))
;  (loop-for-count (?i 1 (length$ ?l)) do
;    (if (funcall ?tfnc (nth$ ?i ?l) ?args) then (bind ?l (delete$ ?l ?i ?i))))
;    ?l  ;redo recursively


;--REMOVE-DUPLICATES---------------------------------------
;(write a) remove-duplicates
;compare first w/ all of rest, if any is eq, then remove it
;- for each one see if it is a member of the rest of the list, if it is remv
(deffunction remove-duplicates (?l)
 (if (null-lv ?l) then ?l else
  (if (member$ (first ?l) (rest$ ?l)) then  (remove-duplicates (rest$ ?l))
   else                (create$ (first$ ?l) (remove-duplicates (rest$ ?l))))))
;------------------------------------------------------

;SET fncs
;------------------------------------------------------
(deffunction subset-val (?v $?l) 
    (if (member$ ?v ?l) then ?v else nil))
;    (if (subsetp (create$ ?v) ?l) then ?v else nil)
;--UNION-----------------------------------------------
(deffunction union (?l1 ?l2)          (create$ ?l1 ?l2))
(deffunction union-nd (?l1 ?l2)    (remove-duplicates (create$ ?l1 ?l2)))
(deffunction union- (?l1 ?l2)          (create$ ?l1 ?l2)) ;for rul.clp -mb
;--INTERSECTION-----------------------------------------------
(deffunction intersection (?l1 ?l2)   
   (collect-if full (map1 subset-val ?l1 ?l2)))
;------------------------------------------------------

;--SET-DIFFERENCE---------------------------------------
;ret list of elts of l1 that do not appear in l2
;-like rm-dup but l is l1 & do memeber of l2
(deffunction set-difference (?l1 ?l2)  
 (if (null-lv ?l1) then ?l1 else
  (if    (member$ (first ?l1) ?l2) then    (set-difference (rest$ ?l1) ?l2)
   else  (create$ (first$ ?l1)             (set-difference (rest$ ?l1) ?l2)))))
;------------------------------------------------------


;--incr------------------------------------------------------------
(defmessage-handler OBJECT incr (?sn $?by) 
 (send ?self (sym-cat put- ?sn) 
	(+ (first-dflt ?by 1) (slot-value ?self ?sn))))

;	(+ (first-dflt ?by 1) (send ?self (sym-cat get- ?sn)))
;   (send ?self (sym-cat put- ?sn) (+ 1 (send ?self (sym-cat get- ?sn))))

;---could do apply's w/ a loop to avoid the recursive overhead
;apply-# (l-apply#)  uses the first lists(val)'s length  it should use:
;(max (map1 length$ (create$ ?lv1 ?lv2))) which won't work because of flat lists
; (max (map2 length$ ?lv1 ?lv2))
;CLIPS> (funcall (max (map2 length$ (create$ 1 2 3) (create$ 1 2 3 4 5))))
;[ARGACCES4] Function max expected at least 2 argument(s)
;need map2's mf to turn into 2 args, like: 
;CLIPS> (funcall max (create$ 1 2 3 4 5)) gives 5
;CLIPS> (funcall max (map2 length$ (create$ 1 2 3) (create$ 1 2 3 4 5)))
;CLIPS> (funcall max (map1 length$ (create$ 1 2 3) (create$ 1 2 3 4 5)))
;hell just (max (length$ ?lv1) (length$ ?lv2) (length$ ?lv3))
;(max (length-lv ?lv1) (length-lv ?lv2) (length-lv ?lv3))

;CLIPS util.fnc
;==========================================
;adding new code found on web, & then whatever

;;;;===============================================================
;;; counts the number of times that ?count_string  occurrs in ?master_string
;;;===============================================================
(deffunction str-count  ( ?count_string  ?master_string)
(bind ?len_count_string (str-length ?count_string))
(bind ?occurrences 0)
(while (str-index ?count_string ?master_string)
                 (bind ?occurrences (+ 1 ?occurrences ))
                 ;; current length of master
                 (bind ?len_master_string (str-length ?master_string))
                 (bind ?pos (str-index ?count_string ?master_string))
                 (bind ?front (sub-string 0 (- ?pos 1) ?master_string))
                 (bind ?back (sub-string (+ ?pos ?len_count_string)   
						?len_master_string  ?master_string))
                 (bind ?master_string (str-cat ?front ""  ?back)) ;; replace with empty string ""
                 )
;;(printout t "new string : "  ?master_string crlf)
(return  ?occurrences)) 


;;;;===============================================================
;;; replaces the first occurrence of sub_string 
;;; inside master_string with replace_string
;;;===============================================================
(deffunction str-replace  ( ?sub_string ?replace_string ?master_string)
(bind ?len_sub_string (str-length ?sub_string))
(bind ?len_master_string (str-length ?master_string))
(bind ?pos (str-index ?sub_string ?master_string))
(bind ?front (sub-string 0 (- ?pos 1) ?master_string))
(bind ?back (sub-string (+ ?pos ?len_sub_string)   ?len_master_string  ?master_string))
(bind ?master_string (str-cat ?front ?replace_string ?back))
(return ?master_string))


;;;;===============================================================
;;; replaces multiple occurrences of sub_string inside master_string with replace_string
;;;===============================================================
(deffunction multi-str-replace  ( ?sub_string ?replace_string ?master_string)
(bind ?len_sub_string (str-length ?sub_string))
(bind ?back ?master_string)
(bind ?new_string "")
(while (str-index ?sub_string ?back)
;;      (printout t "... replacing an occurrence.... " crlf)
;; current length of master
        (bind ?len_master_string (str-length ?back))
        (bind ?pos (str-index ?sub_string ?back))
        (bind ?front (sub-string 0 (- ?pos 1) ?back))
        (bind ?back (sub-string (+ ?pos ?len_sub_string) ?len_master_string  ?back))
        (bind ?new_string (str-cat ?new_string ?front ?replace_string))
        )
(bind ?new_string (str-cat ?new_string ?back))
;;              (printout t "------function multi-str-replace end -------" crlf)
(return ?new_string))
