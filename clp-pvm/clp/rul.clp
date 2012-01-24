;-----------------------------------------------------new:
(defclass TID                                   ;task obj  id  ;mirror globals for now
  (is-a INITIAL-OBJECT)
  (role concrete) (pattern-match reactive)
  (slot tid  (type INTEGER) (create-accessor read-write))       ;task id ([inst] or int id)
  (slot pid  (type INTEGER) (create-accessor read-write))       ;parent task id ([inst] or int id)
  (slot start-time  (type FLOAT) (create-accessor read-write)) ;also was a global ;try diff type
  (slot recv-d-time  (type INTEGER) (create-accessor read-write)) ;also was a global ;does it change w/time?
  (slot elapse-time  (type FLOAT) (create-accessor read-write)) ;was a fact
  (slot model  (type INTEGER) (create-accessor read-write)) ;also was a global
  (multislot inst-tids  (create-accessor read-write)) ;also was a global
)
;------------------------------------------------RULES
;the first rule to run (goes only once/reset), 
;sets globals & some other stuff. 
(defrule startup-TIME
   (initial-fact)
 =>
    ;(add_nrcv_route)
   ;(assert (TIME (rt2)))
   (assert (TIME 0.0))
   (bind ?*my-tid* (mytid))
   (bind ?*parent-tid* (parent))
   (printout t " mytid= " ?*my-tid* crlf)
   (bind ?*start-time* (time))
;-new
  (make-instance mytid of TID ;new
   (start-time ?*start-time*)
   (tid ?*my-tid*)
   (pid ?*parent-tid*)
  )
  ;(send [mytid] put-start-time ?*start-time*)
  ;(send [mytid] put-tid ?*my-tid*)
  ;(send [mytid] put-pid ?*parent-tid*)
;
   ;(make-tasks)			;set up the TASK instances
   ;(bcast-str (tasks ?*my-tid*))       ;make sure others get this new 1
   (initsend)
   (agenda)
)

;the problem is after the 1st time test fails, it is never checked again
;until the fact chages,   (could try tick tock w/ nrecv_rout)

;updates the time, and does receives of command-strings
(defrule UPDATE-TIME
   (declare (salience -50))   ;could go up w/time
   ?t <- (TIME ?old-time)
;   (test (neq (rt2) ?old-time))
 =>
   (printout t "UT=" (rt2) " ")
   ;(if (not (nrecv_route)) then (system "sleep 1"))
   (trecv_eval ?*recv-d-time*)
  (send [mytid] put-recv-d-time ?*recv-d-time*)  ;new
  (send [mytid] put-elapse-time (elapse-time))  ;new
   (retract ?t)
   ;(assert (TIME (rt2)))
   ;(assert (TIME (- (time) ?*start-time*)))
   (assert (TIME (elapse-time)))
   (agenda)
)
;-------------------------------------------------context rules
;;;;;;--this is out of date, latest work is in the tmp rul files
;(deffunction find-pp (?ppname)
;   (find-instance (?pp PROVIDED-PARAM)
;      (eq ?pp:gname ?ppname)))
;fix for all.clp -mb  ;no class or gname elsewhere, glenda, howto-fix? ;also not called
(defclass PROVIDED-PARAM  ;add this, as this file was probably lost.
  (is-a PARAM) ;(is-a ACCESSIBLE)
  (role concrete) 
  (pattern-match reactive)
   (slot gname (create-accessor read-write))  ;maybe w/glenda? 
)        ;it is used in 'inputs' slot below, so there was even a produced|similar subclass?
(defclass PROCESS  ;add this, as this file was probably lost, which really sucks. -mb
  (is-a ACCESSIBLE)
  (role concrete) 
  (pattern-match reactive)
   (multislot inputs (create-accessor read-write))  ;
   (multislot outputs (create-accessor read-write))  ;
   (multislot comp-proc (create-accessor read-write))  ;
)
(deffunction find-pp (?ppname)
   (find-instance ((?pp PROVIDED-PARAM))
      (eq ?pp:gname ?ppname)))
(deffunction maprm (?l1 ?l2) (set-difference ?l1 ?l2)) ;just a guess right now-mb
;-------------------------------------------------FIND-PROC-PROVIDES
(defrule FIND-PROC-PROVIDES
   (declare (salience 5))   ;doing before make-proc-chunks could save time?
   ?p1 <- (object (is-a PROCESS)  (inputs ?in1)  ;mved a paren back up-mb
				 (outputs ?out1)
				 (comp-proc ?cp1))
 =>
   ;(map1 find-pp ?in1)  ;gives a list of params that are provided for the proc
   ;this process's params should then be marked as being available
   ; and can be taken out of the active input list
   ;-would be good to save the old list or mark as not matchable
   (send ?p1 put-inputs (maprm (map1 find-pp ?in1) ?in1))
)
;-------------------------------------------------MAKE-PROC-CHUNKS
;make a process out of 2 processes  (refire till no more chunking/its usable)
(defrule MAKE-PROC-CHUNKS
   ?p1 <- (object (is-a PROCESS) (inputs $?in1) 
				 (outputs $?out1)
				 (comp-proc $?cp1))
   ?p2 <- (object (is-a PROCESS) (inputs $?in2) 
				 (outputs $?out2)
				 (comp-proc $?cp2))
   (test (and (neq ?p1 ?p2)		;not combining the same process
	      (not (member$ ?p1 ?cp2))  ;process not alread a component
	      (not (member$ ?p2 ?cp1))  ;  of a (chunked) process
	      (null-lv (intersection ?cp1 ?cp2))))
 =>
   (bind ?int1to2 (intersection ?in1 ?out2))  ;calc any out to input matches
   (bind ?int2to1 (intersection ?in2 ?out1))
   ;if there are any make a chunked process
   (if (full-lv ?int1to2) then (make-instance 
		      (sym-cat (instance-name ?p1) -  (instance-name ?p2)) 
		       of PROCESS
		(inputs (union- ?in1 (set-difference ?in2 ?int1to2)))
		(outputs (union- ?out1 ?out2))
		(comp-proc (create$ ?p1 ?p2 ?cp1 ?cp2))))
   (if (full-lv ?int2to1) then (make-instance
		      (sym-cat (instance-name ?p2)  -  (instance-name ?p1))
		       of PROCESS
		(inputs (union- ?in2 (set-difference ?in1 ?int2to1)))
		(outputs (union- ?out2 ?out1))
		(comp-proc (create$ ?p2 ?p1 ?cp2 ?cp1))))
)
;inputs are all of the first ones and of of the 2nd except what the 1st provieds
;outputs are the combined outputs (even though used, still available-branch out)
;comprised proceedures are the 2 put together & all of there comp-proc s
;-------------------------------------------------
;(sym-cat (format nil "%s-%s" (instance-name ?p1) (instance-name ?p2)))
;-------------------------------------------------EOF

