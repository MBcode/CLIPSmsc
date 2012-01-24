;-------fnc/hndlers to eval stuff on the other side  MTB
;will need util.clp & pvm.clp (a send-str-to fnc)
;=====================
;----------------------------------------EVAL-SEND-TO
(deffunction eval-send-to (?str ?task)
 (send-str-to (str-cat (eval ?str)) ?task))

;will evaluate the string and turn the result into a strin & send it to ?task
;often called remotely to get a result back from an eval
;-more in eval.clp -all use send-str-to

;----------------------------------------SEND-BACK-TO
(deffunction send-back-to (?str ?task $?to-opt)
 (bind ?to-task (first-dflt ?to-opt (mytid)))
 (send-str-to (quotes eval-send-to ?str ?to-task) ?task))

;send a str ready for evaluation to task, it is eval-ed and the result is
; sent back in string form to your task (or optionally to another task).
;--make a send-back-to-param & eval-send-to-param (which sticks it in the value)

;=====================
;----------------------------------------------------EVAL-SEND-TO-ARRAY
(deffunction eval-send-to-array (?str ?task ?array)
 (send-str-to (quotes send ?array put-value (eval ?str)) ?task))

;(send-str-to (quote send ?array put-value (str-cat (eval ?str))) ?task)
;don't want string, but the real value now, make sure it's the right type

;will evaluate the string and turn the result into a strin & send it to ?task
; (& this version puts it in the value slot of the given array)
;often called remotely to get a result back from an eval

;----------------------------------------------------SEND-BACK-TO-ARRAY
(deffunction send-back-to-array (?str ?task ?array)
 (send-str-to (quotes eval-send-to-array ?str (mytid) ?array) ?task))

;?task could default to (mytid) so would always get sent back
;or the other side could do a bufinfo to see what the source is

;send a str ready for evaluation to task, it is eval-ed and the result is
; sent back in string form to your task.
; (& this version puts it in the value slot of the given array)
;---want to make sure that it puts in the correct type

;-might have a version that can send a mf back to the values slot
;-might have a version that lets you pick the slot to put it into -better

;=====================
;a version that 
;lets you return the ?str eval-ed at ?task and put it in the  ?slot of your ?ins

;----------------------------------------------------EVAL-SEND-TO-INS
(deffunction eval-send-to-ins (?str ?task ?ins ?slot)
 (send-str-to (quotes send ?ins (sym-cat put- ?slot) (eval ?str)) ?task))

;----------------------------------------------------SEND-BACK-TO-INS
(deffunction send-back-to-ins (?str ?task ?ins ?slot)
 (send-str-to (quotes eval-send-to-ins ?str (mytid) ?ins ?slot) ?task))

;=================================================================COPY routines=
;----------------------turn slot & value into a parened symbol
(deffunction sv-sym (?ins ?sn)  (quote ?sn (slot-value ?ins ?sn)))
;----------------------------------------------------COPY-NEW-INS-TO
(deffunction copy-new-ins-to (?task ?ins $?sn-s)
 (if (not (instance-existp ?ins)) then 
   (printout t "[WARNING: No " ?ins " in copy-ins-to]") (return nil))
 (bind ?sns (if (eq (length ?sn-s) 0) then (slotnames ?ins) else ?sn-s))
 (send-str-to 
	(quotes make-instance ?ins of (class ?ins) (map2 sv-sym ?ins ?sns))
	?task))
;----------------------------------------------------COPY-OLD-INS-TO
(deffunction copy-old-ins-to (?task ?ins $?sn-s)
 (if (not (instance-existp ?ins)) then 
   (printout t "[WARNING: No " ?ins " in copy-ins-to]") (return nil))
 (bind ?sns (if (eq (length ?sn-s) 0) then (slotnames ?ins) else ?sn-s))
 (send-str-to 
	(quotes modify-instance ?ins of (class ?ins) (map2 sv-sym ?ins ?sns))
	?task))
;later give another name to copy it too
;;;;-------------------------------------------------------------
;;;;-------------------------------------------------------------EOF

