;misc-fnc.clp  has various misc functions     MTB
;----------------------------------------time etc
(deffunction elapse-time () (- (time) ?*start-time*)) 

(deffunction rt () (round (time)))
(deffunction rt1 () (round (/ (time) 10)))
(deffunction rt2 () (round (/ (time) 100)))

(deffunction debug (?level) (setopt 2 ?level))		;sets it up for debugs
(deffunction rr ()  (reset) (run 1) (agenda) (debug 1)) ;to start it up
(deffunction e ()  (agenda) (exit_pvm) (exit))		;exit in a clean way

(deffunction ri (?file)  (load-instances ?file))
(deffunction sleep (?t) (system (format nil "sleep %d" ?t)))

(deffunction is () (initsend 1))  ;1=no encodeing,0=xdr (avoid 2 for strs)
(deffunction bi () (bufinfo))
(deffunction rbi () (progn (recv_ -1) (bufinfo)))
(deffunction lrbi (?i) (loop-for-count ?i (printout t (rbi) crlf))) 

;----------------------------------------------------------------DEBUG FNCS
(deffunction wa () (watch all))
(deffunction wmsg () (watch messages))
(deffunction whnd () (watch message-handlers))
(deffunction uwa () (unwatch all))
(deffunction wdf ($?fncs) (funcall watch deffunctions ?fncs))
(deffunction uwdf ($?fncs) (funcall unwatch deffunctions ?fncs))
(deffunction wmh ($?fncs) (funcall watch message-handlers ?fncs))
(deffunction uwmh ($?fncs) (funcall unwatch message-handlers ?fncs))
(deffunction insm (?class) (instances MAIN ?class))
(deffunction list-insts (?class) (instances MAIN ?class))
(deffunction list-insts-from (?class) (instances MAIN ?class))
;might make a (wa) that takes extra args that would be fncs to (uwdf)
;----------------------------------------------------------------
(deffunction list ($?stuff) (create$ ?stuff))
;(deffunction let* ($?l2)    (map-skip 2 bind ?l2))
;--------------------------------------------------------EOF
