;start of pvm clips  code, Mike B.  ;-needs:util.clp

;-------------------send/recv  functions
;----------------------------------------send-str
;general send a string to a task w/ tid (takes an int||task & string, w/opt int)
(deffunction send-str (?task ?str $?msgtag)
    (initsend 0)
    (if (and (integerp (bind ?tid (get-tid ?task))) (lexemep ?str)) then 
	(pkstr ?str)			  ;might use stringp
	(send_ ?tid (first-dflt ?msgtag 0))
    else (printout t "[bad send-str " ?task ", " ?str "]")))
;----------------------------------------send-str-to
;(deffunction send-str-to (?str ?task)
;    (initsend 1)
;    (if (and (integerp (bind ?tid (get-tid ?task))) (stringp ?str)) then 
;	(pkstr ?str)	(send_ ?tid 0)
;    else (printout t "[bad send-str-to " ?task ", " ?str "]")))

;----------------------------------------send_0
;(deffunction send_0 (?task)
;  (if (integerp (bind ?tid (get-tid ?task))) then (send_ ?tid 0)
;   else (printout t "[bad send_0 " ?tid "]")))
;task can be a task-inst a tid or a group-string,   msgtag will=0
(deffunction send_0 (?task)
  (if (integerp (bind ?tid (get-tid ?task))) then (send_ ?tid 0)
   else (if (stringp ?task) then (bcast ?task 0)
	 else (printout t "[bad send_0 " ?tid "]"))))

;----------------------------------------SEND-STR-TO
(deffunction send-str-to (?str $?tasks)
    (initsend 1)
    (if (stringp ?str) then (pkstr ?str) (map1 send_0 ?tasks)
    else (printout t "[bad send-str-to " ?tasks ", " ?str "]")))


;----------------------------------------send-str-to-deem
;(deffunction send-str-to-deem (?str)
;    (initsend 1) (pkstr ?str)	(bcast "deem" 0))
;----------------------------------------send-str-to-models
;(deffunction send-str-to-models (?str)
;    (initsend 1) (pkstr ?str)	(bcast "models" 0))

;---------------------------------------------------(u)pk strings by bytes
(deffunction pkstrb (?str)  
  (bind ?l (+ (str-length ?str) 1))
  (printout t "[pkstrb of len=" ?l "]")
  ;(free (pkbyte (deref b (imalloc ?l) ?str) ?l))
  (pkbyte (deref b (imalloc ?l) ?str) ?l))
;-------------------
;(deffunction upkstrb (?l)  (deref b (upkbyte (imalloc ?l) ?l)))
(deffunction upkstrb (?l)  
   (bind ?p (imalloc ?l))
   (printout t "[upkstrb of len=" ?l "into " ?p "]")
   (bind ?p2 (upkbyte ?p ?l))
   (printout t "final ptr=" ?p2)
   (deref b ?p2))
;-------------------
;----------------------------------------send-cl
;general send a string to a task w/ tid  (takes an int & string)
;pkbyte for sends to fortran, probably won't be used
(deffunction send-cl (?tid ?str ?len)
    (initsend 0)
    (pkbyte ?str ?len)
    (send_ ?tid 1))

(deffunction send-c (?tid ?str)
    (send-cl ?tid ?str (str-length ?str)))

;----------------------------------------TRECV_EVAL
;timed receive, which expects a string, and will evaluate it.
(deffunction trecv_eval ($?time)
    (bind ?t (first-dflt ?time 10))
    (if (<> (trecv -1 0 ?t) 0) then ;(eval (upkstr))
	(bind ?str (upkstr))
	(if (lexemep ?str) then (eval ?str)
	else (printout t "[bad trecv_eval:" ?str "]"))
    ))
;----------------------------------------recv-eval
;general receive any string and eval it (run this periodically)
(deffunction recv-eval ($?tid)
  (recv_ (first-dflt ?tid -1) 0)
  (eval (upkstr)))
;-------------------------------------------------EOF
