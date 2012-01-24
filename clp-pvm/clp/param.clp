;defn & msg-handlers for the PARAM class			MTB
;-sometimes what was a glob-pram will be made of a few of what where loc-params
; should references to them be sent along, or by transfering the 'glob-param'
; does it calc it from the locals, if they have been updated

;be able to mark if the array is in a model or malloced
;& if that array is in fortran or C format

;=========================================================projection_PARAMeter
(defclass PARAM
 (is-a ACCESSIBLE)
 (role concrete) 
 (pattern-match reactive)
 (slot count (type INTEGER) 		;number of this type of instance made
	(create-accessor read-write) (storage shared))
;---------------------------------------------------------------has-a instances
;---------------------description of gridding of data
 (slot grid (type INSTANCE) 		;inst w/gridding info
	(create-accessor read-write) (visibility public))
;---------------------description of gridding of data
 (slot units (type INSTANCE) 		;inst w/units info
	(create-accessor read-write) (visibility public))
;---------------------holds the array (is in array.clp) 
 (slot array (type INSTANCE) 		;inst w/memory &assoc descript
	(create-accessor read-write) (visibility public))
;---------------------holds the constraint instances
 (multislot cnstrs (type INSTANCE) 
	(create-accessor read-write) (visibility public))
;---------------------holds the process/sub instances which act of the inst
;=have the lists only be for the current & last simulation timesteps 
;(finest grain or diferrent in each model- except for reasoning)
;-can use something like journal to show the goal state params
; or state at the begin/end of any process  (as the annotation)
;This annotation will have to use the abstract process name  (eg. [srfx])
 (multislot journal (type INSTANCE)   ;would be nice to also add the time
	(create-accessor read-write) (visibility public))
 (multislot journal-time (type INTEGER)   ;time of the journal entry
	(create-accessor read-write) (visibility public))
 (multislot journal-use (type INTEGER)   ;used as in out in-out
	(create-accessor read-write) (visibility public)) ;assume only 'out'?
;-journal might get really long quickly with looping
; easier to keep a journal of calls, & then reconstruct the params-touched ?
;;---------------------description of type of data (meaning??)
; (slot descript (type INSTANCE) 	;might hold constraints
;	(create-accessor read-write) (visibility public))
;----------------------------------------------------------------extra val rep??
;for viewing & matching, which can be done with (param)arrays
;w/deamons can get and set val_ptr ed space, and update get/put-time
 (slot value  				;first value (usually only if xyz=111)??
	(create-accessor read-write) (visibility public))
 (multislot values  			;first values(usually only if xyz=n11)??
	(create-accessor read-write) (visibility public)))
;if copy over all the slots, then the refered to instances latter, they can
; be chekced with a sim-time stamp,  and the value(s) slot too
;-----------------------------------------------------------------
;constraints checked when the value is updated  (maybe for get/put  seperately)
; might have w/>1 param  so put in each to be 2way
;-----------------------------------------------------------------
;use descriptive/(standard) names (so could even do defaults from the name)
;defclass GRID   in param-lib.clp
;defclass UNITS  in param-lib.clp
;defclass CONSTR in param-lib.clp
;defclass ARRAY  in array.clp
;if copy param to another task,refer to has-a as needed,use in-task slot to find
;------------------------------------------------------------------
(defmessage-handler PARAM  pack-it primary ($?n-off)
	(send ?self:array  pack-it ?n-off))
(defmessage-handler PARAM upack-it primary ($?n-off)
	(send ?self:array upack-it ?n-off))
;-------------------
;Linda-like fncs/hndlers should be written around the param-
;------------------------------------------------------------------EOF
