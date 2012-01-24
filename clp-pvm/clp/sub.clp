;=================================================================SUBROUTINE
;-------can be similar to Lambda Fncs (but no args as of yet)<-(objs for now)*
;used to hold the information on how to run a subroutine in a model
;can include the variables that need to be current to run, and the ones wich
;will be updated/or returned when the subroutine is finished
(defclass SUBROUTINE
 (is-a ACCESSIBLE)
 (role concrete) 
 (pattern-match reactive)
 (slot sub	 				;subroutine code to eval
	(create-accessor read-write))		

 (slot busy   (default FALSE)			;wether the subroutine is busy
	(create-accessor read-write))		

 (slot val_ptr (type INTEGER)			;LOC(sub-name) 
	(create-accessor read-write))		;to be used by DF2
 (multislot args (type INSTANCE)		;instances it will be called w/
	(create-accessor read-write))		; used to get arg typ/ptrs &#?

;might not use these-----------------get more data dict
 (multislot vars-needed (type INSTANCE)		;vars used /needed
	(create-accessor read-write))			;can check if updated
 (multislot proj-needed (type INSTANCE)		;vars used /needed
	(create-accessor read-write))			;can check if updated
 (multislot sub-needed (type INSTANCE)		;vars used /needed
	(create-accessor read-write))			;can check if updated
 (slot count (type INTEGER) 		;number of this type of instance made
	(create-accessor read-write) (storage shared))
) ;even id/fid/msgtag because nothing is returned
;-------------------------------------------subroutine INIT after
(defmessage-handler SUBROUTINE init after ()
; (if (and (stringp ?self:expl) (neq ?self:expl "")) then
;     (printout t "[ " ?self:expl " ]"))
  (printout t ?self ","))

;------------------------------------make-busy
(deffunction make-busy (?sub)
  (send ?sub put-busy TRUE))

;------------------------------------------------------CALL
(defmessage-handler SUBROUTINE call primary ()   
 (if (stringp ?self:sub) then  
  (if (and (stringp ?self:expl) (neq ?self:expl "")) then
      (printout t "[ " ?self:expl " ]"))
  (eval ?self:sub)
  else
     (printout t "[call->ptag " ?self:msgtag "]")
     (ptag ?self:msgtag)
 ))

(deffunction call-a-sub (?sub)
  (if (not (instance-existp ?sub)) then 
    (printout t "[WARNING: sub:" ?sub " not there]")
    (return nil) 
   else 
    (printout t "[sub:" ?sub "]")
    (send ?sub call)))

;takes a list of subs and send the call msg to them
(deffunction call ($?subs)
  (apply-1 call-a-sub ?subs))


;------------------------------------rcall
(deffunction rcall (?task $?subs)
  (send-str-to (quotes call ?subs) ?task)
  (map1 make-busy ?subs)) 

;=====================================================FUNCTION
;similar to a subroutine instance, but has a specific return value to look at
(defclass FUNCTION
 (is-a SUBROUTINE)
 (role concrete) 
 (pattern-match reactive)
 (multislot ret-value 		;a 'future'   to be filled latter
	(create-accessor read-write))	
) 
;-------------------a handler should construct the ret val send
;(quote send-str ?self:sub ?*my-tid*)
;will use: (send-back-to-param ?str ?task ?param)
;where the string gets eval-ed on the other side and
; the resulting value (not str) is put into the param's value slot

;------------------------------------------------------------------
;------------------------------------------------------------------EOF
