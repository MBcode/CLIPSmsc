;start of pvm clips task code, Mike B.  ;-needs:util.clp
(defglobal ?*my-tid* = 0)
(defglobal ?*parent-tid* = 0)
(defglobal ?*model* = 0)  ;compiled w/ the model, or talking to it
(defglobal ?*inst-tids* = (create$ ))  ;tids of all the task instances
(defglobal ?*start-time* = 0)
(defglobal ?*recv-d-time* = 10)
;------------------------------------------------util
;(deffunction elapse-time () (- (time) ?*start-time*)) in misc-fnc.clp
(deffunction elapse-time () (- (time) ?*start-time*)) ;in misc-fnc.clp
(deffunction upk1int () (upkint))  

;holds the information on how to contact another unix process on the 
;virtual machine  (note: pvm's virtual machine can include many machines)
(defclass TASK
  (is-a INITIAL-OBJECT)
  (role concrete) (pattern-match reactive)
  (slot init-time (create-accessor read-write))
  (slot active (create-accessor read-write))
  (slot tid  (create-accessor read-write))
  (slot tpid (create-accessor read-write))
  (slot host (create-accessor read-write))
  (slot flag (create-accessor read-write))
  ;(multislot msgtags (create-accessor read-write)) ;tags of possible interest
 (slot global-name ;(type INSTANCE) 
	(create-accessor read-write) (visibility public)) 
  (slot Name (create-accessor read-write)))

;a type of task which will be a clips process which controls a model
(defclass CNTRL-TASK
  (is-a TASK)
  (role concrete) (pattern-match reactive)
  (slot init-time (create-accessor read-write)))

;a type of task which will be the actual FORTRAN/C(++) model
(defclass MODEL-TASK
  (is-a TASK)
  (role concrete) (pattern-match reactive)
  (slot init-time (create-accessor read-write)))

;send-str (implode$  (local-slotnames ?inst))
;send-str (implode$  (slot-local-values ?inst))
;can use to-str & to-pstr=quote now for any list of args

;-------------------------------------------------------send_to_tasks
;so can send whatever is packed up to many different tasks
;can use mcast too, or bcast & a group name
(deffunction send_to_tasks ($?tasks) 
   ;(map2 send_ (map1 get-tid ?tasks) 0)
   ;(map2 send_0 ?tasks) ;in my orig file
   (map2 send_0 ?tasks 0) ;a guess at a fix, mb
)

;-------------------------------------------------------GET-TID
(deffunction get-tid (?task)
  (if (numberp ?task) then ?task 
   else (if (instancep ?task) then (send ?task get-tid)
	 else (printout t "[get-tid:bad-arg " ?task "]"))))
;(if (numberp ?task) then ?task else (send ?task get-tid))
; else  if (stringp ?task) then return all the tids

;-------------------TASK msg handlers-----------------
;-------------------send/recv  handlers

;(deffunction send-str-to (?str $?tasks) 
;   (printout t "[send-str to defined below]"))
(deffunction send-str-to (?str $?tasks) 
   (printout t "[send-str to defined below]"))
;------------------------------------------------------task EVAL
;take the args make into a parened string, and send to task for evaluation
;(defmessage-handler TASK eval primary ($?args)
;  (send-str-to (quotes ?args) ?self))

;-------------------TASK init handler  -------------
;makes sure that a newly created task has many of its slots filled in.
(defmessage-handler TASK init after ()
  (send ?self put-init-time (elapse-time))
;if active slot isn't set, the task is waiting (by default)
;if spawned or gotton from tasks it should be set to active (if it is)
;flag has some of that status info
  (if (not (symbolp ?self:active)) then (send ?self put-active waiting))
;set host if not set
    (if (and (numberp ?self:tid) (not (numberp ?self:host)))  then
		    (send ?self put-host (tidtohost ?self:tid)))
  (insert$ ?*inst-tids* 1 ?self:tid)

  ;if there is a global-name for the task make that inst w/ the same tid
  (if (or (and (instancep ?self:global-name) (neq ?self:global-name [nil])) 
          (stringp ?self:global-name)) then
    (make-instance ?self:global-name of TASK (tid ?self:tid)))
)

(defmessage-handler TASK get-tid before ()
  (if (null ?self:tid) then (printout t "[" ?self " has no tid, so put-tid]")))


;--ADD-TASK    (takes 2 strings & and int right now)
;makes an instance of a task
(deffunction add-process (?name ?where ?tid)
  (make-instance (sym-cat task- ?name - ?tid) of TASK
    (tid ?tid) 
    (host ?where) 
    (Name ?name)))

;--MAKE-TASK    (takes 2 strings right now)
;makes an instance of a task
(deffunction make-process (?name ?where)
  (bind ?tid 
   (spawn ?name "(load pvm-agt)" 1 ?where (if (stringp ?where) then 1 else 0)) )
   (add-process ?name ?where ?tid))
;latter will just incr the #, and use the tid slot for sends

;might still want something like above, so when you have a task/spawn it
;that the rest of the (tasks tid) info can be parsed into the new instance

;------------------------------------------------------task EVAL
;take the args make into a parened string, and send to task for evaluation
(defmessage-handler TASK eval primary ($?args)
  (send-str-to (quotes ?args) ?self))
;------------------------------------------------------
;------------------------------------------------------OID
(defclass OID					;obj  id   (~= cORB-NAME)
  (is-a INITIAL-OBJECT)
  (role concrete) (pattern-match reactive)
  (slot tid  (create-accessor read-write))	;task id ([inst] or int id)
  (slot iid  (create-accessor read-write))	;inst id ([inst] or str id)
  (slot orb-name  (create-accessor read-write))) ;name given by naming service
;to have a globally seperate name, need 1 naming service
;either inst-name or orb-name slot should be unique
;-----------------------------------------------------new:
;(defclass TID					;task obj  id  ;mirror globals for now
;  (is-a INITIAL-OBJECT)
;  (role concrete) (pattern-match reactive)
;  (slot tid  (type INTEGER) (create-accessor read-write))	;task id ([inst] or int id)
;  (slot pid  (type INTEGER) (create-accessor read-write))	;parent task id ([inst] or int id)
;  (slot start-time  (type INTEGER) (create-accessor read-write)) ;also was a global
;  (slot recv-d-time  (type INTEGER) (create-accessor read-write)) ;also was a global
;  (slot model  (type INTEGER) (create-accessor read-write)) ;also was a global
;  (multislot inst-tids  (create-accessor read-write)) ;also was a global
;)
;-----------------------------------------------------EOF
