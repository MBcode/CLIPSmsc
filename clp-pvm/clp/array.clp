;class lib and msg handlers for arrays=(values of params)	M.Bobak,ANL
;--------------------------
;-needs: util.clp 
;--------------------------
;might have some array stuff accessible through PARAM handlers?
;lambda-fncs would still be nice (maybe tcl or scheme)-(has array,vect too)
;output to hdf format for viewing, trans this way?,can do quick mat.calcs

;==============================================================ARRAY
(defclass ARRAY
 (is-a ACCESSIBLE)
 (role concrete) 
 (pattern-match reactive)
 (slot count (type INTEGER) 		;number of this type of instance made
	(create-accessor read-write) (storage shared))
 (slot fresh   (default FALSE)		;if the array is newly filled
	(create-accessor read-write))		
;----------------------stuff for the array 0 to 3 dim
 (slot type (default f) 		;type of the array value (i/f/d/s)
	(create-accessor read-write) (visibility public))
; (multislot index (type INTEGER) (create-accessor read-write))	;max array index
  (slot lang (type SYMBOL) (create-accessor read-write))	;FORTRAN or C
 (slot x (type INTEGER) (default 1)	;1st dimension index
	(create-accessor read-write) (visibility public))
 (slot y (type INTEGER) (default 1)	;2nd dimension index
	(create-accessor read-write) (visibility public))
 (slot z (type INTEGER) (default 1)	;3rd dimension index
	(create-accessor read-write) (visibility public))
 (slot num (type INTEGER) (default 1)	;num of elts
	(create-accessor read-write) (visibility public))
 (slot size (type INTEGER) (default 1)	;num of elements * #bytes/element
	(create-accessor read-write) (visibility public)) ;can just calc
 (slot val_ptr (type INTEGER) 		;long_int to point to value
	(create-accessor read-write) (visibility public))
;----------------------if array a seperate class fill these
;for viewing & matching, which can be done with (param)arrays
;w/deamons can get and set val_ptr ed space, and update get/put-time
 (slot value  				;first value (usually only if 111)
	(create-accessor read-write) (visibility public))
 (multislot values  			;first values (usually only if n11)
	(create-accessor read-write) (visibility public))
)
;-----------------------------------------------------------GET-VALUE
(defmessage-handler ARRAY get-value after ()  ;for debugging
  (printout t "[" (instance-name ?self) " v=" ?self:value "]"))

(deffunction get-value (?p)      ;or (slot-value ?p value)
  (if (slot-existp (class ?p) value) then (send ?p get-value) 
   else (printout t "[WARNING:" ?p " does not have a value slot]")) )
(deffunction gv (?p)     (slot-value ?p value)) 
(deffunction pv (?p ?v)  (send ?p put-value ?v)) 
;if get rid of value slot have these fncs, then hndlrs too
;(deffunction get-value (?p)      (first (slot-value ?p values)))
;(deffunction put-value (?p ?val) (replace$ (slot-value ?p values) 1 1 ?val))

;-------------------------------------------array INIT after
(defmessage-handler ARRAY init after ()
  (printout t ?self ",")
  (send ?self incr count)
  (bind ?self:num (* ?self:x ?self:y ?self:z))
  (bind ?self:size (* ?self:num (typelen ?self:type)))
  (if (< ?self:val_ptr 999) then (bind ?self:val_ptr (imalloc ?self:size)))
; (if (or (and (instancep ?self:global-name) (neq ?self:global-name [nil])) 
;  (stringp ?self:global-name)) then
;   (make-instance ?self:global-name of ARRAY
;    (x ?self:x) (y ?self:y) (z ?self:z) 
;    (msgtag ?self:msgtag) (val_ptr ?self:val_ptr)))
)

;in the end it might not have the same val_ptr/msgtag-for printing

;-------------------------------------------(array)MPRINT
(defmessage-handler ARRAY mprint primary ()  ;for debugging
  (ptag (nnn ?self:msgtag)))

;-------------------------------------------(array)PUT-INDEX
(defmessage-handler ARRAY put-index ($?indx)  ;sets indecies
  (bind ?self:x (first-dflt ?indx 1))
  (bind ?self:y (second-dflt ?indx 1))
  (bind ?self:z (third-dflt ?indx 1)))

;=======================================================ARRAY STUFF
;'arrays' can be from 0 to 3 dimensions, (single= 1 1 1)
;-------------------------------------------------------Deref Handlers
(defmessage-handler ARRAY deref primary ($?nums)
 (if (<> (length$ ?nums) 0) then (funcall deref ?self:type ?self:val_ptr ?nums)
		            else         (deref ?self:type ?self:val_ptr)))
;-------------------
(defmessage-handler ARRAY deref-off primary (?offset $?nums)
  (if (> ?offset ?self:size) then 
    (printout t "WARNING:offset too large " ?offset crlf) (return nil))
  (printout t "[deref-off " ?offset " makes " ?self:val_ptr " into " (+ ?self:val_ptr (* ?offset 4)) "," ?nums "]" crlf)
  (if (<> (length$ ?nums) 0)  
     then (funcall deref ?self:type (+ ?self:val_ptr (* ?offset 4)) ?nums)
     else         (deref ?self:type (+ ?self:val_ptr (* ?offset 4)))))
;right now type-size is hard-coded to 4
;-------------------
(defmessage-handler ARRAY zero-to primary (?n)
  (loop-for-count (?i 0 ?self:num) do  (send ?self deref-off ?i ?n)))
;-------------------
(defmessage-handler ARRAY deref-off-n primary (?offset ?n)
  (bind ?top (+ ?offset ?n))
  (bind ?l (create$ ))
  (loop-for-count (?i 0 ?n) do  
    (printout t "[" (send ?self deref-off (- ?top ?i)) "]")
    (insert$ ?l 1 (send ?self deref-off (- ?top ?i))))
?l)
;-------------------
(deffunction add2 (?x ?y) (+ ?x ?y))
(deffunction sub2 (?x ?y) (- ?x ?y))
(deffunction div2 (?x ?y) (/ ?x ?y))
(deffunction mul2 (?x ?y) (* ?x ?y))

;maybe ?fnc ?outarray $?array where they could be nums or array
;so array becomes a new wilder m.f.
(defmessage-handler ARRAY deref-fnc2 primary (?fnc ?warray ?outarray $?off-n)
  (bind ?offset (first-dflt ?off-n 0))
  (bind ?n (second-dflt ?off-n ?self:num))
  (bind ?top (+ ?offset ?n))
  (loop-for-count (?i ?offset ?top) do  
    (send ?outarray deref-off ?i
      (funcall ?fnc (send ?self deref-off ?i) (send ?warray deref-off ?i)))))

;(get-nprcpk of SUBROUTINE 
; (sub "(send [rainc] deref-fnc2 add2 [rainnc] [nprcpk])"))
;then (call [get-nprcpk]) to calculate it  (do this in bats) rain(n)c state-vars
;-------------------
(defmessage-handler ARRAY check-ptr primary ()
  (if (< (nn ?self:val_ptr) 99) then 
    (printout t crlf "[WARNING val_ptr=" ?self:val_ptr "]") (return TRUE)
   else (return FALSE)))

;============================-----------------GET/PUT VALUE DEAMONS
;have a GET-value that does a get-value but gets it from the model 1st
;have a PUT-value that does a put-value then puts it into the model too
;--not needed in the same executable, as you are accessing the same space
;---------------------------------------------
;could just make value a multislot, or just have/use value, for now
;if just have values, can have get/put-value just access the 1st one <-*

;-------------------------PUT       after
(defmessage-handler ARRAY put-value after ($?val)
  (if (< (nn ?self:val_ptr) 99) then 
    (printout t crlf "[WARNING val_ptr=" ?self:val_ptr "]") (return nil))
  (if (> ?self:num 1) then 
    (printout t crlf "[WARNING you are overwriting the 1st array element"))
  (bind ?self:put-time (elapse-time))
  (printout t "[" (instance-name ?self) " put-v " (send ?self deref) "]")
  (send ?self deref ?val))  ;what put in value slot, goes in val_ptr space 

(defmessage-handler ARRAY put-values after ($?vals)
  (if (< (nn ?self:val_ptr) 99) then 
    (printout t crlf "[WARNING val_ptr=" ?self:val_ptr "]") (return nil))
  (bind ?self:put-time (elapse-time))
  (send ?self deref ?vals))  ;what put in values slot, goes in val_ptr space 

;-------------------------GET       before
(defmessage-handler ARRAY get-value before ()
  (if (< (nn ?self:val_ptr) 99) then 
    (printout t crlf "[WARNING val_ptr=" ?self:val_ptr "]") (return nil))
  (bind ?self:value (send ?self deref))  ;get value from val_ptr space, &cache 
  (printout t "[" (instance-name ?self) " get-v " ?self:value "]")
  (bind ?self:get-time (elapse-time)))

(defmessage-handler ARRAY get-values before ($?n)
  (if (< (nn ?self:val_ptr) 99) then 
    (printout t crlf "[WARNING val_ptr=" ?self:val_ptr "]") (return nil))
  (bind ?self:values (send ?self deref-n (first-dflt ?n 1)))  
  (bind ?self:get-time (elapse-time)))
  ;get values from val_ptr space, &cache 
;;;;-------------------------------------------------------------
;remeber the C deref fnc only takes a ptr & if it gets a number it sets it
;so to pick another array loc a handler has to recompute the ptr
;;;;-------------------------------------------------------------
;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\-array com code (might change)
;--------------------------
;-needs: util.clp & pvm.clp
;--------------------------
;=======================================array COMMUNICATION (pvm) packing
;can send stride to pk_tpn too
;--------------------------------------------(un)packing using the tpn C fnc
;write a tpn fnc that takes an offset---------actually just alter the old 1 
(defmessage-handler ARRAY pack-it primary ($?n-off)  ;then stride & binary-flag
  (bind ?n (first-dflt ?n-off ?self:num))
  (bind ?off (second-dflt ?n-off 0))
  (bind ?stride (third-dflt ?n-off 1))
  (pk_tpn ?self:type ?self:val_ptr ?n ?off ?stride)
  (send ?self get-value)) 

(defmessage-handler ARRAY upack-it primary ($?n-off)
  (bind ?n (first-dflt ?n-off ?self:num))
  (bind ?off (second-dflt ?n-off 0))
  (bind ?stride (third-dflt ?n-off 1))
  (pk_tpn (upcase ?self:type) ?self:val_ptr ?n ?off ?stride)
  ;(send ?self mprint)  ;to have the FORTRAN model print out the arrays
  (make-fresh ?self) 
  (send ?self get-value))
;--------------------------------------------(un)packing using the pvm_(u)pkbyte
(defmessage-handler ARRAY pack-byte primary ($?s)
  (bind ?s (first-dflt ?s ?self:size))
  (printout t "[pack-byte " ?self:val_ptr ", " ?s "]")
  (pkbyte ?self:val_ptr ?s)
  (send ?self get-value)) 

(defmessage-handler ARRAY upack-byte primary ($?s)
  (bind ?s (first-dflt ?s ?self:size))
  ;a version of unpkbyte that takes a ptr rather than returning 1
  (printout t "[upkbyte " ?self:val_ptr " " ?s " " ?self:size "]")
  (upkbyte ?self:val_ptr ?s)
  (make-fresh ?self) 
  (send ?self get-value)) 
;--------------------------------------------------------------------
;think about making array's xyz write-once  (unless want to realloc)
; but would be better to just make a new one and transfer the data
;;;;-------------------------------------------------------------EOF
