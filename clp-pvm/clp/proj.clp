;defn & msg-handlers for the PROJ class			MTB
;=================================================================PROJection
(defclass PROJ
 (is-a ACCESSIBLE)
 (role concrete) 
 (pattern-match reactive)
 (slot from   (type INSTANCE)		;where is comes from                  ??
	(create-accessor read-write))		
 (slot to   (type INSTANCE)		;where is goes to                     ??
	(create-accessor read-write))		
 (slot for   (type INSTANCE)		;what subroutine gets called after    ?? 
	(create-accessor read-write))	;it gets this data (redo so data-driven)	
 (multislot params ;(default (create$)) ;param instances which hold values
    (create-accessor read-write) (visibility public))	
)
;-----------------------------------------------------

;-----------------------------------------------------proj SEND-TO
;pack the upk cmd in a string then pack all the params
;(map1 pack-byte ?self:params ?tid) ;then one send

;-----------------------------------------------------(U)PK-(G)-PARAM
(deffunction  pk-param (?param) (send (send ?param get-array) pack-byte))
(deffunction upk-param (?param) (send (send ?param get-array) upack-byte))

;----------------------------------------------------send-to
;(defmessage-handler PROJ send-to primary (?task)
; (if (< (length ?self:params) 1) then 
;	(printout t "[WARNING: PROJ send-to has no params " ?self:params "]"))
; (initsend 1)
;;need to have params stay a mf, but can't (quote (quote)) w/out messed up ""
; (pkstr (quotes map1 upk-param (quote create$ ?self:params))) 
; (map1 pk-param ?self:params)
; (send_0 ?task))
;
;(defmessage-handler PROJ send_to_n primary (?task)
; (initsend 1)
; (pkstr (quotes apply-2 send (quote create$ ?self:params) upack-n)) 
; (apply-2 send ?self:params pack-n)
;;this is more like mark's proj-param-array send
;;(pkstr (quotes apply-2 send (quote create$ ?self:params) upack-byte)) 
;;(apply-2 send ?self:params pack-byte)
; (send_0 ?task))
;then the trecv-eval loop on the other side will get the string & upk the params
;assumes the glob params are set up the same on the other side
;the string that is sent along, runs upk-param which can updates/touchs the inst
;this is more efficient than the presend deem++send, so it should be reworked

;----------------------------------------------------proj SEND_TO
(defmessage-handler PROJ send_to primary (?task $?opt)
 (initsend 1)
 (pkstr (quotes apply-2 send (quote create$ ?self:params) upack-it ?opt)) 
 (apply-2 send ?self:params pack-it ?opt)
 (send_0 ?task))

;then the trecv-eval loop on the other side will get the string & upk the params
;----------------------
;----------------------------------------------------GET_FROM
;(defmessage-handler PROJ get_from primary (?task $?to-opt)
; (bind ?to-task (first-dflt ?to-opt (mytid))) (initsend 1)
; (pkstr (quotes send  ?self  send_to  ?to-task ?to-opt)) 
; (send_0 ?task)) ;this only works if that proj is on the other side
;could do (send [clim-to-bats-init-proj] get_from [clim] [bats])
;if could assume the proper proj was there (could copy it)

;do by using a send_to for PARAM
(defmessage-handler PROJ get_from primary (?task $?opt)
 (initsend 1)
 (pkstr (quotes apply-2 send (quote create$ ?self:params) send_to (mytid) ?opt))
 (send_0 ?task)) ;this only works if params are on the other side 

 ;(pkstr (quotes apply-2 send (quote create$ ?self:params) pack-it ?opt)) 
 ;(apply-2 send ?self:params pack-it ?opt)

;param version of eval-send-to & send-back-to (in eval.clp)
;----------------------------------------------------
;probably have to reconfigure to synch w/ st
;----------------------------------------------------EOF
