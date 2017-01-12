(defglobal 
	?*current-dribble* = (create$)
)

(deffunction set-verbose (?status)
	(bind ?*verbose_status* ?status)
)

(deffunction get-verbose ()
	?*verbose_status*
)

(deffunction verbose ($?list)
	(if (eq ?*verbose_status* on)
	   then
	   	(funcall printout t $?list)
	)
)

(deffunction my-dribble-on (?file)
	(if (> (length$ ?*current-dribble*) 0)
	   then
		(dribble-off)
		(bind ?pfile (nth$ 1 ?*current-dribble*))
		(bind ?pnumber (nth$ 2 ?*current-dribble*))
		(bind ?pfilename (str-cat ?pfile ?pnumber))
		(bind ?command (str-cat "type " ?pfilename " >> " ?pfile))
		(system ?command)
		(remove ?pfilename)
	)
	(bind ?*current-dribble* (create$ ?file 1 ?*current-dribble*))
	(if (open ?file ttt "r")
	   then
	   	(close ttt)
	   	(remove ?file)
	)
	(bind ?n-file (str-cat ?file 1))
	(dribble-on ?n-file)
)

(deffunction my-dribble-off ()
	(dribble-off)
	(bind ?file (nth$ 1 ?*current-dribble*))
	(bind ?number (nth$ 2 ?*current-dribble*))
	(bind ?command (str-cat "type " ?file ?number " >> " ?file))
	(system ?command)
	(remove (str-cat ?file ?number))
	(bind ?*current-dribble* (rest$ (rest$ ?*current-dribble*)))
	(if (> (length$ ?*current-dribble*) 0)
	   then
		(bind ?file (nth$ 1 ?*current-dribble*))
		(bind ?number (nth$ 2 ?*current-dribble*))
		(bind ?new-file (str-cat ?file (+ ?number 1)))
		(bind ?*current-dribble* (create$ ?file (+ ?number 1) (rest$ (rest$ ?*current-dribble*))))
		(dribble-on ?new-file)
	   else
	   	TRUE
	)
)

(deffunction str-replace (?original-string ?replace-string ?search-string)
	(bind ?search-length (length$ ?search-string))
	(bind ?pos (str-index ?search-string ?original-string))
	(while (neq ?pos FALSE)
	   do
	   	(bind ?original-string 
	   		(str-cat 
	   			(sub-string 1 (- ?pos 1) ?original-string)
	   			?replace-string
	   			(sub-string (+ ?pos ?search-length) (length$ ?original-string) ?original-string)
	   		)
	   	)
	   	(bind ?pos (str-index ?search-string ?original-string))
	)
	?original-string
)

(deffunction str-cat$ ($?list)
	(bind ?end (length$ $?list)) 
	(if (> ?end 0)
	   then
		(bind ?string-result (nth$ 1 $?list))
		(loop-for-count (?n 2 ?end)
		   do
			(bind ?string-result (str-cat ?string-result " " (nth$ ?n $?list)))
		)
		(str-replace ?string-result "\"" "#$%")
	   else
	   	""
	)
)


(deffunction my-explode$ (?string)
	(explode$ (str-replace ?string "#$%" "\""))
)

(deffunction double-member$ (?x $?list)
	(bind ?pos (member$ ?x $?list))
	(if (neq ?pos FALSE)
	   then
	   	(member$ ?x (subseq$ $?list (+ ?pos 1) (length$ $?list)))
	   else
	   	FALSE
	)
)

(deffunction odd-member$ (?x $?list)
	(bind ?pos (member$ ?x $?list))
	(if (integerp ?pos)
	   then
		(oddp ?pos)
	)
)

(deffunction associate-pairs (?x $?list)
	(bind $?result (create$))
	(while (> (length$ $?list) 0)
	   do
		(if (eq (nth$ 1 $?list) ?x)
		   then
		   	(bind $?result (create$ $?result (nth$ 2 $?list)))
		)
		(bind $?list (rest$ (rest$ $?list)))
	)
	$?result
)

(deffunction subseq-pos ($?double-list)	
	(bind $?small-list (subseq$ $?double-list 1 (- (member$ $$$ $?double-list) 1)))
	(bind $?big-list (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list)))
	(bind ?big-index 1)
	(bind ?big-length (length$ $?big-list))
	(bind ?small-length (length$ $?small-list))
	(while (<= ?big-index ?big-length)
	  do
	  	(bind ?small-index 1)
	  	(while	(and
	  			(<= ?small-index ?small-length)
	  			(eq (nth$ (- (+ ?big-index ?small-index) 1) $?big-list) (nth$ ?small-index $?small-list))
	  		)
	  	   do
	  	   	(bind ?small-index (+ ?small-index 1))
	  	)
	  	(if (> ?small-index ?small-length)
	  	   then
	  		(return ?big-index)
	  	   else
	  	   	(bind ?big-index (+ ?big-index 1))
	  	)
	)
	FALSE
)

(deffunction is-singlevar (?x)
	(if (and (stringp ?x) (eq (sub-string 1 1 ?x) "?"))
	   then
	   	TRUE
	   else
	   	FALSE
	)
)

(deffunction is-multivar (?x)
	(if (and (stringp ?x) (eq (sub-string 1 2 ?x) "$?"))
	   then
	   	TRUE
	   else
	   	FALSE
	)
)

(deffunction is-var (?x)
	(or (is-singlevar ?x) (is-multivar ?x))
)

(deffunction string> (?a ?b)
	(> (str-compare ?a ?b) 0)
)


(deffunction max-string ($?list)
	(if (= (length$ $?list) 0)
	   then
	   	nil
	   else
	   	(bind ?max-result (nth$ 1 $?list))
	   	(bind $?list (rest$ $?list))
	   	(while (> (length$ $?list) 0)
	   	   do
	   	   	(if (string> (nth$ 1 $?list) ?max-result)
	   	   	   then
	   	   	   	(bind ?max-result (nth$ 1 $?list))
	   	   	)
	   	   	(bind $?list (rest$ $?list))
	   	)
	   	?max-result
	)
)

(deffunction min-string ($?list)
	(if (= (length$ $?list) 0)
	   then
	   	nil
	   else
	   	(bind ?min-result (nth$ 1 $?list))
	   	(bind $?list (rest$ $?list))
	   	(while (> (length$ $?list) 0)
	   	   do
	   	   	(if (string> ?min-result (nth$ 1 $?list))
	   	   	   then
	   	   	   	(bind ?min-result (nth$ 1 $?list))
	   	   	)
	   	   	(bind $?list (rest$ $?list))
	   	)
	   	?min-result
	)
)

(deffunction min-int ($?list)
	(if (= (length$ $?list) 0)
	   then
	   	nil
	   else
	   	(if (= (length$ $?list) 1)
	   	   then
	   	   	(nth$ 1 $?list)
	   	   else
	   	   	(funcall min $?list)
	   	)
	)
)

(deffunction max-int ($?list)
	(if (= (length$ $?list) 0)
	   then
	   	nil
	   else
	   	(if (= (length$ (rest$ $?list)) 0)
	   	   then
	   	   	(nth$ 1 $?list)
	   	   else
	   	   	(funcall max $?list)
	   	)
	)
)

(deffunction sum$ ($?x)
	(if (= (length$ $?x) 0)
	   then
	   	0
	   else
	   	(if (= (length$ (rest$ $?x)) 0)
	   	   then
	   	   	(nth$ 1 $?x)
	   	   else
			(funcall + $?x)
		)
	)
)
	   	   
(deffunction reverse$ ($?list)
	(bind $?result (create$))
	(bind ?end (length$ $?list))
	(loop-for-count (?n 1 ?end)
	   do
	   	(bind $?result (create$ (nth$ ?n $?list) $?result))
	)
	$?result
)

(deffunction remove-duplicates$ ($?list)
	(bind $?result (create$))
	(bind ?end (length$ $?list))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (not (member$ (nth$ ?n $?list) (subseq$ $?list (+ ?n 1) ?end)))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ ?n $?list)))
	   	)
	)
	$?result
)

(deffunction collect-singletons$ ($?list)
	(bind $?result (create$))
	(while (> (length$ $?list) 0)
	   do
	   	(if (not (member$ (nth$ 1 $?list) (rest$ $?list)))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ 1 $?list)))
	   	   	(bind $?list (rest$ $?list))
	   	   else
	   	   	(bind $?list (delete-member$ (rest$ $?list) (nth$ 1 $?list)))
	   	)
	)
	$?result
)

(deffunction difference$ ($?double-list)
	(bind $?first-list (subseq$ $?double-list 1 (- (member$ $$$ $?double-list) 1)))
	(bind $?second-list (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list)))
	(bind $?result (create$))
	(bind ?end (length$ $?first-list))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (not (member$ (nth$ ?n $?first-list) $?second-list))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ ?n $?first-list)))
	   	)
	)
	$?result
)

(deffunction intersection$ ($?double-list)
	(bind $?list1 (subseq$ $?double-list 1 (- (member$ $$$ $?double-list) 1)))
	(bind $?list2 (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list)))
	(bind $?intersection (create$))
	(bind ?end (length$ $?list1))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (member$ (nth$ ?n $?list1) $?list2)
	   	   then
	   	   	(bind $?intersection (create$ $?intersection (nth$ ?n $?list1)))
	   	)
	)
	(return $?intersection)
)

(deffunction union$ ($?double-list)
	(bind $?list1 (subseq$ $?double-list 1 (- (member$ $$$ $?double-list) 1)))
	(bind $?list2 (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list)))
	(bind $?union (create$))
	(bind ?end (length$ $?list1))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (not (member$ (nth$ ?n $?list1) $?list2))
	   	   then
	   	   	(bind $?union (create$ $?union (nth$ ?n $?list1)))
	   	)
	)
	(return $?union)
)
	

;(deffunction same-set$ ($?double-list)
;	(bind $?list1 (subseq$ $?double-list 1 (- (member$ $$$ $?double-list) 1)))
;	(bind $?list2 (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list)))
;	(bind ?end (length$ $?list1))
;	(if (= ?end (length$ $?list2))
;	   then
;	   	(loop-for-count (?n 1 ?end)
;	   	   do
;	   	   	(if (not (member$ (nth$ ?n $?list1) $?list2))
;	   	   	   then
;	   	   	   	(return FALSE)
;	   	   	)
;	   	)
;	   	(return TRUE)
;	   else
;	   	FALSE
;	)
;)

(deffunction same-set$ ($?double-list)
	(bind $?first-list (subseq$ $?double-list 1 (- (member$ $$$ $?double-list) 1)))
	(bind $?second-list (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list)))
	(and (subsetp $?first-list $?second-list) (subsetp $?second-list $?first-list))
)

;(deffunction supersetp ($?double-list)
;	(bind $?first-list (subseq$ $?double-list 1 (- (member$ $$$ $?double-list) 1)))
;	(bind $?second-list (subseq$ $?double-list (+ (member$ $$$ $?double-list) 1) (length$ $?double-list)))
;	(and (subsetp $?second-list $?first-list) (not (subsetp $?first-list $?second-list)))
;)

(deffunction get-token ($?condition)
	(bind ?count 0)
	(bind ?end (length$ $?condition))
	(loop-for-count (?pos 1 ?end)
	   do
	   	(if (eq (nth$ ?pos $?condition) "(")
	   	   then
	   	   	(bind ?count (+ ?count 1))
	   	   else
	   	   	(if (eq (nth$ ?pos $?condition) ")")
	   	   	   then
	   	   	   	(if (= ?count 1)
	   	   	   	   then
	   	   	   	   	(return ?pos)
	   	   	   	   else
	   	   	   	   	(bind ?count (- ?count 1))
	   	   	   	)
	   	   	)
	   	)
	)
	(return 0)
)


(deffunction token-length ($?code)
	(bind ?result 0)
	(while (> (length$ $?code) 0)
	   do
		(bind ?p2 (get-token $?code))
		(if (> ?p2 0)
		   then
			(bind $?code (subseq$ $?code (+ ?p2 1) (length$ $?code)))
			(bind ?result (+ ?result 1))
		   else
		   	(return 0)
		)
	)
	?result
)
	
(deffunction get-nth-token (?n $?token-list)
	(while (and (> (length$ $?token-list) 0) (>= ?n 1))
	   do
		(bind ?p2 (get-token $?token-list))
	   	(bind $?first-token (subseq$ $?token-list 1 ?p2))
		(bind $?token-list (subseq$ $?token-list (+ ?p2 1) (length$ $?token-list)))
		(bind ?n (- ?n 1))
	)
	(if (= ?n 0)
	   then
	   	$?first-token
	   else
	   	nil
	)
)

(deffunction make-symbol (?x)
	(if (instance-addressp ?x)
	   then
	   	(instance-name ?x)
	   else
	   	?x
	)
)

(deffunction inverse-brackets ($?code)
	(bind $?result (create$))
	(bind ?end (length$ $?code))
	(loop-for-count (?n 1 ?end)
	   do
	   	(if (eq (nth$ ?n $?code) ")")
	   	   then
	   	   	(bind $?result (create$ $?result "("))
	   	   else
	   	   	(if (eq (nth$ ?n $?code) "(")
	   	   	   then
	   	   	   	(bind $?result (create$ $?result ")" ))
	   	   	   else
	   	   	   	(bind $?result (create$ $?result (nth$ ?n $?code)))
	   	   	)
	   	)
	)
	$?result
)

(deffunction load-file (?file)
	(bind ?pos (str-index "." ?file))
	(if (neq ?pos FALSE)
	   then
	   	(bind ?suffix (sub-string (+ ?pos 1) (length ?file) ?file))
	   	(if (eq ?suffix "clp")
	   	   then
	   	   	(load* ?file)
	   	   else
	   	   	(if (eq ?suffix "bat")
	   	   	   then
	   	   	   	(batch* ?file)
	   	   	   else
	   	   	   	(printout t "Cannot handle file: " ?file crlf)
	   	   	   	(printout t "Can only handle .clp or .bat files!" crlf)
	   	   	)
	   	)
	   else
   	   	(printout t "Cannot handle file: " ?file crlf)
   	   	(printout t "Can only handle .clp or .bat files!" crlf)
	)
)

(deffunction load-files ($?file-list)
	(bind ?end (length$ $?file-list))
	(loop-for-count (?n 1 ?end)
	   do
	   	(load-file (nth$ ?n $?file-list))
	)
)


(deffunction replace-anonymous-variables (?rule-string-before)
	(bind $?rule (my-explode$ ?rule-string-before))
	(bind $?result (create$))
	(while (> (length$ $?rule) 0)
	   do
	   	(if (eq (nth$ 1 $?rule) "?")
	   	   then
	   	   	(bind $?result (create$ $?result (sym-cat "?" (gensym))))
	   	   else
	   	   	(bind $?result (create$ $?result (nth$ 1 $?rule)))
	   	)
	   	(bind $?rule (rest$ $?rule))
	)
	(str-cat$ $?result)
)

(deffunction run-goal (?goal)
   	(bind ?goal-id (assert (goal ?goal)))
   	(run)
   	(retract ?goal-id)
)

(deffunction remove-vars ($?list)
	(bind $?result (create$))
	(while (> (length$ $?list) 0)
	   do
	   	(if (not (is-var (nth$ 1 $?list)))
	   	   then
	   	   	(bind $?result (create$ $?result (nth$ 1 $?list)))
	   	)
	   	(bind $?list (rest$ $?list))
	)
	$?result
)

(deffunction pair-member (?x1 ?x2 $?pair-list)
	(bind ?pos (member$ ?x1 $?pair-list))
	(if (neq ?pos FALSE)
	   then
	   	(if (eq (nth$ (+ ?pos 1) $?pair-list) ?x2)
	   	   then
	   	   	(return TRUE)
	   	   else
	   	   	(pair-member ?x1 ?x2 (subseq$ $?pair-list (+ ?pos 2) (length$ $?pair-list)))
	   	)
	   else
	   	FALSE
	)
)

(deffunction unique-pairs ($?pairs-list)
	(bind $?result (create$))
	(while (> (length$ $?pairs-list) 0)
	   do
;	   	(if (eq FALSE (subseq-pos (create$ (subseq$ $?pairs-list 1 2) $$$ (subseq$ $?pairs-list 3 (length$ $?pairs-list)))))
	   	(if (not (pair-member (nth$ 1 $?pairs-list) (nth$ 2 $?pairs-list) (subseq$ $?pairs-list 3 (length$ $?pairs-list))))
	   	   then
	   	   	(bind $?result (create$ $?result (subseq$ $?pairs-list 1 2)))
	   	)
	   	(bind $?pairs-list (subseq$ $?pairs-list 3 (length$ $?pairs-list)))
	)
	$?result
)

(deffunction my-build (?construct)
	(open "r-device-auxiliary-file.clp" fout "w")
	(printout fout ?construct crlf)
	(close fout)
	(load* "r-device-auxiliary-file.clp")
	(remove "r-device-auxiliary-file.clp")
)
