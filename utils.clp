;CLIPS util funcitons, copyright of Michael Bobak@computer.org
;to be incl in any work I do, but as per contract not owned by anyone in particular
;that is not to say that non-utility code is not fully owned by the people who pay me.
;-bits of this are collected code from other, but much is my own; I wouldn't mind getting
; others to use it; I lean copyleft as I've benefitted, but could go bsd/etc.
;-consider using parts of auxiliary-functions.clp/ut-h &others, incl new clp ont files.
;=I hope to keep the RDF2clipsObject code w/in this file soon(as soon as I get rid of the C needed)
;= also event-rules for timer events/etc
(deffunction remove-duplicates (?l) ?l)
(deffunction bag-get (?a ?b) ?a)
(deffunction bag-set (?a ?b ?c) ?a)
(deffunction bag-create (?a) ?a)
(deffunction bag-delete (?a) ?a)
(deffunction tlst-get (?n ?b) ?n)
(deffunction tlst-set (?a ?b) ?a)
(deffunction tlst-create (?a) ?a)
(deffunction tlst-delete (?a) ?a)
(defglobal ?*tlst* = FALSE)
(defglobal ?*jess* = FALSE)
;[Only collected are really the LIST methods and a sort fnc.]
;(deffunction e () (exit)) ;change for jess
(deffunction ex () (exit))
(deffunction lo () (exit))
;(deffunction lo () (date) (ex))
(deffunction lt () (load t.clp))
(deffunction lt2 () (load t2.clp))
(deffunction lt3 () (load t3.clp))
;(deffunction lt2 () (load fxm2.clp)) ;changes
;(deffunction lf () (load fixmsg.clp))
(deffunction map2 (?f ?a ?b $?args)  (printout t crlf "redef"));redef below
(deffunction lf () (load fxm2.clp))
(deffunction lfc () (load fxc.clp))
(deffunction lb () (batch b.bat))
(deffunction lb2 () (batch b2.bat))
(deffunction lb0 () (batch b0.bat))
(deffunction lp () (load poc.clp))
(deffunction lbp () (lp) (lb))
(deffunction decrn (?n) (if (numberp ?n) then (- ?n 1) else 0))
;temp put this here:
(defglobal ?*cme-clp-files* = (create$ ;strings.clp io.clp 
   message-functions.clp
   FIX-Recev-Common.clp FIX-Recev-EOY.clp FIX-Send-Common.clp FIX-Send-EOY.clp
   cyclops_prep.clp ;inbound-cyclops2.clp interpreter_control.clp
   market_data_templates.clp mdp_outbound_interpreter.clp
   nsc_inbound_interpreter.clp nsc_outbound_interpreter.clp
   outbound_cyclops2.clp red_modules.clp task.clp 
))
(deffunction fixint (?valstr)
 ;(if (eq (eval ?valstr) 2147483647) then (eval (str-cat ?valstr ".0")))
  (if (> (eval ?valstr) 2147483646) then (eval (str-cat ?valstr ".0")) else (eval ?valstr))
)  ;a scm/lsp is really needed, too bad it can't be worked into this; I still like parts of xlips too
(deffunction numpre-p (?valstr)
 (str-index (sub-string 1 1 ?valstr) "1234567890")
)
(deffunction numstr-p (?valstr)
  (loop-for-count (?cnt 1 (str-length ?valstr)) do
       (if (not (integerp (string-to-field (sub-string ?cnt ?cnt ?valstr)))) 
	then (return FALSE))
) TRUE)
(deffunction fix-int (?valstr)
 (if (numberp ?valstr) then ?valstr 
  else
   (if (numstr-p ?valstr) then (fixint ?valstr) else ?valstr)
 )
)
;(deffunction cmeclp2L ()  (map1 file2lists  ?*cme-clp-files*))
;trading.clp noman_interface_templates.clp

;CLIPS> (type [Rolls-Royce])
;CAR
(deffunction class-of (?inst)
  (if (instancep ?inst) then (type ?inst)
   else if (class-existp ?inst) then ?inst)
)
;-both clips&jess have and and or that only return TRUE/FALSE not 1 of the values
(deffunction ptf (?v) (if ?v then T else F))
;-also warning that and of nil is still TRUE
(deffunction last$ (?l) (nth$ (length$ ?l) ?l))
(deffunction first (?l) (nth 1 ?l))  ;being redefined
(deffunction and_ ($?args)
 (if (not (first $?args)) then (return FALSE) else (and_ (rest$ $?args)))
)
(deffunction and- ($?args)
  (if (not ;(or  ;?
	(member$ FALSE ?args)  
	   ;(member$ nil ?args)) ;?
      )
  ;if (funcall and $?args) 
	then (last$ ?args))
)
(deffunction andl ($?args)
  (if (not (or  ;?
	(member$ FALSE ?args)  
	   (member$ nil ?args)) ;?
      )
	then (last$ ?args))
)
(deffunction first-full ($?args)
  (if (first ?args) then (first ?args) else (first-full (rest$ ?args)))
)
(deffunction or- ($?args)
  (if (funcall or $?args) then (first-full ?args))
) ;lisp like or orl is below
;CLIPS> (or- nil a nil b nil c nil) nil
;CLIPS> (orl nil a nil b nil c nil) a

(deffunction nop (?a) ?a)
(deffunction funcall$ (?fnc $?args)   ;makes up for the new funcall not being so great
	(eval (format nil "(%s %s)" ?fnc (implode$ ?args))))

;while (loop-for-count <range-spec> [do] <action>*)
;lookat:
;TYPE: Returns a symbol which is the name of the type (or class) of its of argument.
;(type <expression>)
;-could combine w/switch to make a typecase sort of situation

;APROPOS: Displays all symbols currently defined in CLIPS which contain a specified substring
;(apropos <lexeme>)

;-new ;this should take a class too
(deffunction slot-names (?inst)
  (if (instancep ?inst) then (class-slots (class ?inst) inherit)
   else 
    (if (class-existp ?inst) then (class-slots ?inst inherit)
     else (printout t crlf "[not instance or class]"))
  )
)
;-LIST  (consider having a next method that remember an elt iterated on)?
(defgeneric position)  ;in lisp there is search  also like find here.
(defgeneric write-to-string)  ;from list
(defgeneric listp)
(defgeneric make-list) 

(defmethod listp ((?lst MULTIFIELD))  ;LIST
  TRUE)

(defmethod listp ((?lst SYMBOL (or (eq ?lst nil)
                                   (eq ?lst NIL))))
  TRUE)

(defmethod listp (?other-type)
  FALSE)

(defgeneric value)  ;more below
(defmethod value (?a) 
  ?a)

 
;;; implementations 
;lookat:
;
;SYSTEM: Appends its arguments together to form a command which is
;        then sent to the operating system.
;(system <lexeme-expression>*)

(deffunction sleep (?t) (system (format nil "sleep %d" ?t)))
(deffunction s1 () (sleep 1))
;-
;CLIPS version of ACA		c-util.fnc
;c-util.fnc	Basic utility fns that don't depend on the *.lib files
;			There are funcal/mapcar/etc  fncs on list etc
;-------------------------------------------------------------
;The major efficiency issue probably comes in rewriting some of the more
;often used util fncs, so there are not any interveneing lists.
;More safty for fncs w/ different #s of slots can also be done then.
;(as an example look at the util fncs rewritten as handlers in c-util2.fnc
; more of something like this would be good.)
;-------------------------------------------------------------

;--global variables----------------------------------------------------
;these are now in c-gloabl.bnd
;(defglobal ?*pol-data-counter* = 0)
;(defglobal ?*infringement-counter* = 0)
;(defglobal ?*case-study-counter* = 0)
;(defglobal ?*qualities-counter* = 0)
;(defglobal ?*copy-counter* = 0)
;(defglobal ?*control-counter* = 0)
;;(defglobal ?*gpq* = (create$))
(defglobal ?*lp* = "(")
(defglobal ?*rp* = ")")
(defglobal ?*lp2* = "((")
(defglobal ?*rp2* = "))")

;(deffunction cons ($?args)  ?args)  ;generic below

;--MAKE-LAMBDA-----------------------------------------------------
(deffunction make-lambda (?lstr)
    (bind ?name (gensym))
    (build (str-cat "(deffunction " ?name " " ?lstr ")"))
    ?name)
;EVAL: Evaluates a string as though it were entered at the command prompt.
;      Only allows functions to be evaluated.
;(eval <lexeme-expression>)
(deffunction eval- (?s)
 (if (lexemep ?s) then (eval ?s) else ?s)
)
(deffunction eval-str (?valstr)
  (eval- (fix-int ?valstr))
)
;
;BUILD: Evaluates a string as though it were entered at the command prompt.
;       Only allows constructs to be evaluated.
;(build <lexeme-expression>)

;CHECK-SYNTAX: Allows the text representation of a construct or function
;              call to be checked for syntax and semantic errors.
;(check-syntax <construct-or-function-string>)

;lookat:
;STRING-TO-FIELD: Parses a string and converts its contents to a primitive data type.
;(string-to-field  <string-or-symbol-expression>)
;-looks like evaluate
; also upcase & lowcase

(deffunction to-lst ($?args) ?args)
;--TO-STR-----------------------------------------------------
(deffunction to-str ($?args) (implode$ ?args))
;(deffunction to-str ($?args) 
;  (if (stringp $?args) then (return $?args) ;fix
;   else (implode$ ?args)))
;redef to-str below 
;(deffunction to-str ($?args)  ;below
;  (str-strip-quote (implode$ ?args)))
(deffunction    str ($?args) (implode$ ?args))
;CLIPS> (to-str hi there two)
;"hi there two"
;CLIPS> (str-cat hi there two)
;"hitheretwo"


(deffunction to-sym ($?s) 
(funcall$ sym-cat $?s)
;(sym-cat ?s)
) ;new/fix
(deffunction multi-str-replace (?a ?b ?str) (system (str-cat "tr " ?a " " ?b " " ?str))) ;system$ but redef below
(deffunction pstr (?s)
  (if (stringp ?s) then (write-to-string ?s) else ?s))
(deffunction pstr- ($?args)
 (to-sym (multi-str-replace "\\" "" (multi-str-replace "\"" "" (str-cat (implode$ ?args)))))
)
(deffunction symbol ($?args)
;(to-sym (multi-str-replace "\"" "" (to-str ?args)))
;(to-sym (multi-str-replace "\\\"" "" (str-cat (implode$ ?args))))
; (to-sym (multi-str-replace "\\" "" (multi-str-replace "\"" "" (str-cat (implode$ ?args)))))
 (to-sym (pstr- ?args))
)

;deffunction pad (?n ?p)
(deffunction zpad (?n ?p)
  "pad, probably in format"
 (bind ?s (str ?n))
 (bind ?sl (length ?s))
 (if (> ?p ?sl) then 
    (bind ?d (- ?p ?sl))
    (loop-for-count (?i 1 ?d) do (bind ?s (str-cat "0" ?s))))
?s) ;CLIPS> (zpad 1 4) "0001"

;to-str could be used to create strings for msg-passing:
(deffunction paren (?s) (str-cat "(" ?s ")"))
(deffunction to-pstr ($?args) (paren (to-str ?args)))
(deffunction quotes   ($?args) (paren (to-str ?args)))
(deffunction quote    ($?args) (sym-cat "(" (implode$ ?args) ")"))
(deffunction quote-mf ($?args) (sym-cat "(create$ " (implode$ ?args) ")"))
(deffunction quote-list ($?args) (sym-cat "(create$ " (implode$ ?args) ")"))
(deffunction prn     ($?args) (paren (to-str ?args)))
;to-pstr is similar to quote, because it isn't evaluated (but is a str)
;so it will be evaluated when eval-ed

(deffunction qins (?ps)
 "clos style ins name"
 (if (instancep ?ps) then (str-cat "'" (instance-name ?ps)) else ?ps)
)
(deffunction qis (?ps)
 "quote if a string"
 (if (stringp ?ps) then (str-cat "\"" ?ps "\"") else ?ps)
)

;STR-CAT: Concatenates its arguments to form a single string.
;(str-cat <expression>*)
;
;SYM-CAT: Concatenates its arguments to form a single symbol.
;(sym-cat <expression>*)
;
;SUB-STRING: Retrieves a subportion from a string.
;(sub-string <integer-expression> <integer-expression> <string-expression>)
;
;STR-INDEX: Returns the position of the first argument within the second argument.
;(str-index <lexeme-expression> <lexeme-expression>)

;(deffunction suffixp (?post ?str) ;new ;use for file-types
;  "see if this is the end of the str"
;  (bind ?l (length ?post))
;  (eq (str-index ?post ?str) ?l)) ;wrong ones below better

(deffunction prefixp (?pre ?str)
  "see if this is the start of the str"
  (eq (str-index ?pre ?str) 1))

(deffunction prefix (?pre ?str)
 "if not a prefix already, then add it"
 (if (prefixp ?pre ?str) then ?str else (str-cat ?pre ?str))
)

(deffunction postfixp (?post ?str)
  "see if this is the start(end) of the str"
  (eq (str-index ?post ?str) (- (length ?str) (length ?post))))
;might be wrong, so try
;mvded blwo w/suffix-p
;(postfix-p "abc" "a.abc")
;TRUE

(deffunction postfix (?post ?str)
 "if not a postfix already, then add it"
 (if (postfixp ?post ?str) then ?str else (str-cat ?str ?post))
)
(deffunction end$ (?l ?n)
   (subseq$ ?l ?n (length$ ?l)))

(deffunction ending (?post ?str)
 "make sure the file ends w/[.]post"
 (postfix (prefix . ?post) ?str))

(deffunction ending- (?post ?str)
 "make sure the file ends w/post"
 (postfix  ?post ?str))

(deffunction path-cat (?p1 ?p2)
  "a str-cat for2path parts"
  (str-cat (ending- "/" ?p1) ?p2)
)

;suffix stuff was already just below here, but not set more mapping
;(deffunction ending-p (?str ?post)
;  (if (full$ ?post) then (bind ?post (first ?post)))
;  (ending ?post ?str)) ;need a -p not a str


;lookat:
;DESCRIBE-CLASS: Provides a verbose gescription of a class.
;(describe-class <class-name>)
;
(deffunction print_class (?c)  (describe-class ?c)) ;specialize below
;BROWSE-CLASSES: Provides a rudimentary display of the inheritance
;                relationships between a class and all its subclasses.
;(browse-classes [<class-name>]) ;taxonomy

;-should make this a method that takes a stream-obj, that could incl an external-pipe,
; look into routers/streams, and see if you could open a stream to a pipe-file anyway
;deffunction print (?p)  ;mved below
;(if (instance-existp ?p) then (send ?p print))
;;(if (message-handler-existp (class ?p) print) then (send ?p print)) -no

(deffunction princ (?a)  
    (printout t " " ?a))

;--INCR--------------------------------------------------------
;this function takes a number and returns that number plus 1
(deffunction incrn (?v) (bind ?v (+ ?v 1)))

;this function takes a string w/ a variables name in it
;this variable is then incremented by one  (used for instance counters)
(deffunction incr (?var-str ?amt) 
	(eval (format nil "(bind \?%s (+ \?%s %d))" ?var-str ?var-str ?amt)))

;---
(deffunction null$ (?lv)
  (or (eq ?lv nil) (and (multifieldp ?lv) (= (length ?lv) 0))))
(deffunction full$ (?lv)
  (if (multifieldp ?lv) then (> (length ?lv) 0) else (neq ?lv nil)))
;---
(deffunction fulll$ (?lv)
  (and (multifieldp ?lv) (> (length ?lv) 0))
)

(deffunction first-full$ ($?args)
  (if (full$ (first ?args)) then (first ?args) else (first-full (rest$ ?args)))
)
(deffunction orl ($?args)
  (if (funcall or $?args) then (first-full$ ?args))
)

(deffunction member- (?a ?l) (if (full$ ?l) then (member$ ?a ?l))) ;new
(deffunction member-i (?i ?l) (member- (instance-name ?i) ?l)) ;Very Useful for rule tests
;--NULL----------------------------------------------------
;just like null in LISP
;(deffunction null (?a) (if (eq ?a nil) then TRUE else FALSE))
(defmethod null (?v)
  (if (eq ?v nil) then TRUE else FALSE))
(defmethod null ((?l MULTIFIELD))
  (if (eq (length ?l) 0) then TRUE else FALSE))

(deffunction null-lv (?a) 
  (if (or (and (multifieldp ?a) (eq (length ?a) 0)) (eq ?a nil)) then TRUE 
   else FALSE))

;deffunction nnull ($?any)
(deffunction nnull (?any)
  "~like my full"
 (not (null ?any)))

(deffunction full-eq (?a ?b)
  "both are not null and still eq"
  (and (full$ ?a) (full$ ?b) (eq ?a ?b))
)

;--FUNCALL----------------------------------------------------
;similar to funcall in LISP, except non of the arguments can be lists
;-i don't think the c-version is the same as mine below, so i'm adding the $
;(deffunction funcall$ (?fnc $?args)  ;mved up
;	(eval (format nil "(%s %s)" ?fnc (implode$ ?args))))
;w/out the eval this has the same effect as quotes

;--FUNCALL-L----------------------------------------------------
;similar to funcall in LISP, except only the 1st argument can be list
(deffunction funcall-l (?fnc ?l $?args)  
 (eval (format nil "(%s (create$ %s) %s)" ?fnc (implode$ ?l) (implode$ ?args))))
;use (expand$ )
;EXPAND$: When used inside of a function call, expands its arguments
;         as separate arguements to the function. The $ operator is 
;         merely a shorthand notation for the expand$ function call.
;(expand$; <multifield-expression>)

;lookat:
;INSTANCE-NAME: Returns a symbol which is the name of its instance argument.
;(instance-name <instance-expression>)
;SYMBOL-TO-INSTANCE-NAME: Converts a symbol to an instance name.
;(symbol-to-instance-name <symbol-expression>)
; is a sort of ins-to-sym
;INSTANCE-NAME-TO-SYMBOL: Converts an instance name to a symbol.
;(instance-name-to-symbol <instance-name-expression>)
;
;INSTANCE-NAMEP: Returns TRUE if its argument is an instance name, FALSE otherwise.
;(instance-namep <expression>)
;------------sym-to-ins
;a form of symbol-to-instance name that can take many args
(deffunction sym-to-ins ($?n)  ;s2i
 "make an inst name"
 (bind ?n  (funcall$ sym-cat ?n)) ;another funcall change
 (if (nnull ?n) then (symbol-to-instance-name  ?n))
)
;-this &/or s2i below should check for nil
;check these out
(deffunction genins ()
  (sym-to-ins (gensym))
)

(deffunction ins-existp ($?n) 
  (instance-existp (sym-to-ins ?n)))

;--LENGTH-LV----------------------------------------------------
;(deffunction length-lv (?lv)
;    (if (multifieldp ?lv) then (length ?lv) else 1))
;new  ;but not tripping on nil
(deffunction length-lv (?lv)
 (if (and ?lv (neq ?lv nil)) then
    (if (multifieldp ?lv) then (length ?lv) else 1)
  else 0)
)

;--NTH1-LV----------------------------------------------------
;returns the 1st elt. if multifield or value if a value
(deffunction nth1-lv (?lv)
	(if (multifieldp ?lv) then (nth$ 1 ?lv) else ?lv))

;--NTH-LV----------------------------------------------------
(deffunction nth-lv (?n ?lv)
	(if (multifieldp ?lv) then (nth$ ?n ?lv) else ?lv))

;--FIRST----------------------------------------------------
(deffunction first (?lv) (nth1-lv ?lv))
;	(if (multifieldp ?lv) then (first$ ?lv) else ?lv)
(deffunction second (?lv) (nth-lv 2 ?lv))
(deffunction third (?lv) (nth-lv 3 ?lv))
(deffunction fourth (?lv) (nth-lv 4 ?lv))
(deffunction fifth (?lv) (nth-lv 5 ?lv))
(deffunction sixth (?lv) (nth-lv 6 ?lv))

;(deffunction last (?lv) (if (multifiedp ?lv) (nth$ (length ?lv) ?lv) else ?lv))
(deffunction last (?lv) (nth-lv (length-lv ?lv) ?lv))

;---
(deffunction default_args (?d $?args)
  (if (null$ ?args) then ?d else (first ?args))
)
;--DFLT----------------------------------------------------
(deffunction dflt (?val ?dflt)
 (if (null ?val) then ?dflt else ?val))

;--NTH-DFLT----------------------------------------------------
(deffunction nth-dflt (?n ?lv ?dflt)
 (dflt (nth-lv ?n ?lv) ?dflt))

(deffunction first-dflt (?lv ?dflt) (dflt (first ?lv) ?dflt))
(deffunction second-dflt (?lv ?dflt) (dflt (nth-lv 2 ?lv) ?dflt))
(deffunction third-dflt (?lv ?dflt) (dflt (nth-lv 3 ?lv) ?dflt))
(deffunction fourth-dflt (?lv ?dflt) (dflt (nth-lv 4 ?lv) ?dflt))
(deffunction fifth-dflt (?lv ?dflt) (dflt (nth-lv 5 ?lv) ?dflt))
(deffunction sixth-dflt (?lv ?dflt) (dflt (nth-lv 6 ?lv) ?dflt))
(deffunction last-dflt (?lv ?dflt) (dflt (last ?lv) ?dflt))

;CLIPS> (last (create$ a b c))   -> c
;CLIPS> (last a)   -> a
;CLIPS> (last (create$))   -> nil
;CLIPS> (last-dflt (create$) b)   -> b


;(deffunction first-dflt (?lv ?dflt) (nth-dflt 1 ?lv ?dflt))
;(deffunction second-dflt (?lv ?dflt) (nth-dflt 2 ?lv ?dflt))
;(deffunction third-dflt (?lv ?dflt) (nth-dflt 3 ?lv ?dflt))

(deffunction reduce-vl (?binary-fnc ?val ?list)
  (if (null ?list) then (return ?val))
  (reduce-vl ?binary-fnc (funcall ?binary-fnc ?val (first ?list)) (rest$ ?list)) 
)
(deffunction reduce$ (?binary-fnc ?list)
   "from python"
  (if (or (null ?list) (< (length$ ?list) 2)) then (return ?list))
  (bind ?val (funcall ?binary-fnc (first ?list) (second ?list))) 
  (reduce-vl ?binary-fnc ?val (rest$ (rest$ ?list)))
)
;CLIPS> (reduce$ max (create$ 1 4 2 5 3)) 5
;CLIPS> (reduce$ * (create$ 1 4 2 5 3)) 120
;CLIPS> (reduce$ - (create$ 1 4 2 5 3)) -13

(deffunction ls ($?filt) ;add a path
  "system ls"
  (bind ?f (first-dflt ?filt ""))
  (system (str-cat "ls " ?f))
);use ls$ to get the vals
(deffunction pwd () ;can use for CurrentWorkingDir
  "print workind directory"
  (system "pwd")
)
;(deffunction txt-files () (filter$ (ls$) ".ics")) ;rdef below filter
;-=new ;another version down in array code
(deffunction explode-str$ (?str)
  "take each char of a str to a list, opposite fnc is sym-cat"
 (if (listp ?str) then (bind ?str (first ?str)))
 (bind ?str (to-str ?str))
 (bind ?l (create$))
 (loop-for-count (?i 1 (length ?str)) do
  (bind ?l (create$ ?l  (sub-string ?i ?i ?str)))
) ?l)
  
(deffunction rev$ (?lst)
 (bind ?len (length$ ?lst))
 (bind ?l (create$))
 (loop-for-count (?i 1 ?len) do
  (bind ?l (create$  (subseq$ ?lst ?i ?i) ?l))
) ?l)

;lookat: mv-subseq
;subsetp subset -same

(deffunction reverse$ (?l)
  (if (> (length$ ?l) 1) then (create$ (reverse$ (rest$ ?l)) (first$ ?l))
   else ?l))
   
(deffunction rev-str (?str)
 (bind ?len (length ?str))
 (bind ?rs "")
 (loop-for-count (?i 1 ?len) do
   (bind ?ie (- ?len ?i))
;  (printout t crlf "ie:" ?ie " rs:" ?rs " ss:" (sub-string ?ie ?ie ?str))
   (bind ?rs (str-cat ?rs (sub-string ?ie ?ie ?str)))
 )
?rs)
;-=
(deffunction sp ($?n)
 "n spaces"
  (bind ?ct (first-dflt ?n 1))
  (bind ?sp "")
  (switch ?ct
                (case 1 then " ")
                (case 2 then "  ")
                (case 3 then "   ")
                (case 4 then "    ")
                (case 5 then "     ")
                (case 6 then "      ")
	(default (loop-for-count (?i 1 ?ct) do (bind ?sp (str-cat ?sp " "))) ?sp)
  )
)
(deffunction cln-sp ($?n)
  (str-cat ":" (sp ?n))
)
(deffunction str-cat-wsp ($?args)
 "so you don't have to send spaces in"
 (implode$ ?args)
) ;CLIPS> (str-cat-wsp a b c) "a b c"
;(deffunction str-cat-sort ($?args)
; (implode$ 
;    ;(sort > ?args)
;    (smembers$  ?args)))
;CLIPS> (str-cat-sort (create$ "c" "d" "a" "b")) ""a" "b" "c" "d""


;--IN-RANGE----------------------------------------------------
;given value & range as  2values or as a m.f. return a val that is in the range
(deffunction in-range (?value $?range)  ;bound by range
  (bind ?min (first-dflt ?range 0))
  (bind ?max (second-dflt ?range ?value))
  (if (> ?value ?max) then 
      (printout t "[warning " ?value " is > max of " ?max "]")
      (return ?max))
  (if (< ?value ?min) then
      (printout t "[warning " ?value " is < min of " ?min "]")
      (return ?min))
  ?value)
;if the in-rage call = the value then it was ok == in-range-p
;could have a version w/inst-sn & if not in range it would set it

;lookat:
;SLOT-FACETS: Returns the facet values for the specified slot of a class in a multifield value.
;(slot-facets <class-name> <slot-name>)
;
;SLOT-SOURCES: Returns the names of the classes which provide facets for a 
;              slot of a class in a multifield variable.
;(slot-sources <class-name> <slot-name>)
;
;SLOT-TYPES: Returns the names of the primitive types allowed for a slot
;            in a multifield variable.
;(slot-types <class-name> <slot-name>)
;
;SLOT-CARDINALITY: Returns the minimum and maximum number of fields allowed 
;                  for a multislot in a multifield variable.
;(slot-cardinality <class-name> <slot-name>)
;
;SLOT-ALLOWED-VALUES: Returns the allowed values for a slot in a  
;                     multifield value.
;(slot-allowed-values <class-name> <slot-name>)
;
;SLOT-RANGE: Returns the minimum and maximum numeric values allowed
;            for a slot.
;(slot-range <class-name> <slot-name>)
;
;SLOT-DEFAULT-VALUE: Returns the default value associated with a slot.
;(slot-default-value <class-name> <slot-name>)
;
;--SLOT-VALUE-types----------------------------------------------
;use (slot-types ?class ?slotname) right next to slot-value
(deffunction slot-value-types (?ins ?sn)
  "gives the  slot-types, for that ins, use near slot-value"
  (slot-types (class ?ins) ?sn)
)
(deffunction slot-value-type (?ins ?sn)
  "give the 1st type returned by: slot-types"
  (first (slot-value-types ?ins ?sn))
)
;(deffunction slot-value-wstr (?ins ?sn)
; "get it ready to print"
; (bind ?sv (slot-value ?ins ?sn))
; (if (member STRING (slot-value-types ?ins ?sn)) then
;   (bind ?sv (quote ?sf)))
;?sf)
;--SLOT-VALUE----------------------------------------------------
(deffunction slot-get (?ins ?sn)
 "jess compat"
  (send ?ins (sym-cat get- ?sn))
)
(deffunction slot-value (?ins ?sn)
  (if (slot-existp (class ?ins) ?sn inherit) then  (slot-get ?ins ?sn)
   else (printout t "[slot-value " ?ins " does not have a " ?sn " slot]")
        (return FALSE)) ;new
) 

;-
(deffunction slot-value-wstr (?ins ?sn)
 "use LIST method to dispatch on type, for a writable str"
 (write-to-string (slot-value ?ins ?sn))
)
;
;(deffuntion ins-wstrs (?ins)
;  (bind ?cls (class-of ?ins))
;  (map1 slot-value-wstr ..)
;)

(deffunction try ($?args) 
  "jess compat" ;no macros so doesn't work
 (printout t "clips can not TRY:" ?args)
)
;--SLOT-PUT-VALUE---------------------------------------------
(deffunction slot-set (?ins ?sn ?val)  ;the setter for slot-value
  "jess compat"
  (send ?ins (sym-cat put- ?sn) ?val)
)
(deffunction slot-put-value (?ins ?sn ?val)  ;the setter for slot-value
  (if (slot-existp (class ?ins) ?sn inherit) then
      (slot-set ?ins ?sn ?val)
   else (printout t "[slot-put-value " ?ins " does not have a " ?sn " slot]")))
;might need to check if write access & if val=mf then if slot-facet1=MLT

(deffunction slot-allowed-value-p (?class ?sn ?val)  ;the checker for slot-put-value- or sav
   (if (not (class-existp ?class)) then (printout t crlf "no class:" ?class crlf) (return nil))
   (bind ?a (slot-allowed-values ?class ?sn))
   (and (fulll$ ?a) (member$ ?val ?a))
) ;but it is a class of values, so a specific val vs a class won't work, fix
;-;can use this new one to incr/decr, ins-name to ins-address etc
(deffunction slot-update-fnc (?ins ?sn ?fnc)  
  "reset the value w/the present val as input to fnc"
  (slot-put-value ?ins ?sn (funcall ?fnc (slot-value ?ins ?sn))))
;-
(defmessage-handler OBJECT slot-ref2addr (?sn)
; (bind ?self:?sn (instance-address ?self ?sn))
  (slot-put-value ?self ?sn  (instance-address ?self ?sn))
)
;-
;(deffunction slot-allowed-classes (?c ?sn)
;  "for jess compat" ;this won't work well; I need a better(dbg)fix
; (printout t "[WARN:clpOnlyHasAllowedValuesNow:" ?c "," ?sn)
; (slot-allowed-values ?c ?sn) ;make sure it's a multifield
;) 6.2.4 has this now

;checking allowed-value before a put, then setting an error slot/facet if it is off&needs recording
 ;-this could kick off other (generic)function-ality

(deffunction sav- (?ins ?sn ?val)  ;the setter for slot-value
  (if (instance-existp ?ins) then (bind ?class (class-of ?ins)) else (return nil))
  (if (slot-existp ?class ?sn inherit) then
    (if (slot-allowed-value-p ?class ?sn ?val) then (send ?ins (sym-cat put- ?sn) ?val)
     else (printout t "[slot-put-value " ?ins "," ?sn " slot, unallowed-val:" ?val 
		":" (slot-allowed-values ?ins ?sn) "]"))
   else (printout t "[slot-put-value " ?ins " does not have a " ?sn " slot]"))
)
(deffunction sav (?ins ?sn ?val)  ;the setter for slot-value
  (if (instance-existp ?ins) then (slot-put-value ?ins ?sn ?val))
)

;lookat:
;DELETE-MEMBER$: Deletes specific values contained within a multifield 
;                value and returns the modified multifield value.
;(delete-member$ <multifield-expression> <expression>+)
;
;REPLACE-MEMBER$: Replaces specific values contained within a multifield 
;                 value and returns the modified multifield value.
;(replace-member$ <multifield-expression> <substitute-expression> <search-expression>+)

;-also my subset-val looks like member


;--REPLACE----------------------------------------------------
;a version of replace$ that will append if out of range
(deffunction replace (?l ?a1 ?a2 ?l2)
  (bind ?ml (length$ ?l))
  (bind ?n1 (min ?a1 ?ml))
  (bind ?n2 (min ?a2 ?ml))
  (if (neq ?a1 ?n1) then (bind ?l2 (create$ (nth$ ?ml ?l) ?l2)))
  (replace$ ?l ?n1 ?n2 ?l2))

;--SLOT-REPLACE-----------------------------------------------
;(deffunction replace-mslot-value (?inst ?slot ?a1 ?a2 ?l2)
; (bind ?l (slot-value ?inst ?slot))
; (if (not (multifieldp ?l)) then (bind ?l (create$ ?l)))
; (slot-put-value ?inst ?slot (replace ?l ?a1 ?a2 ?l2)))

(deffunction slot-replace (?inst ?slot ?a1 ?a2 ?l2)
  (bind ?l  (slot-value ?inst ?slot))  ;list w/ values to replace (append)
  (bind ?ml (length$ ?l))              ;it's lenght
  (bind ?n1 (min ?a1 ?ml))             ;make sure that the replace bounds <= ml
  (bind ?n2 (min ?a2 ?ml))             ;if >then append
  ;if lower replace bnd > ml, then insert end of old list at begin of new
  ; so when it replaces the end of the old, that elt won't be lost
  ;(if (neq ?a1 ?n1) then (bind ?l2 (create$ (nth$ ?ml ?l) ?l2)))
  (if (neq ?a1 ?n1) then (bind ?l2 (create$ (nth$ ?ml ?l) ?l2)))
  (slot-replace$ ?inst ?slot ?n1 ?n2 ?l2))

;--APPEND----------------------------------------------------
;see mv-append and mv-delete also
(defgeneric append) 
(defmethod append (?l ?a)  ;generic below
  (insert$  ?l  (+ 1 (length$ ?l)) ?a))  ;could use create$ too
(defgeneric nconc) 
(defmethod nconc (?l ?a) (append ?l ?a)) ;w/o ins, it won't know the diff?

;--SLOT-APPEND-----------------------------------------------
(deffunction slot-append (?inst ?slot ?a)
  (slot-insert$  ?inst ?slot  (+ 1 (length$ (slot-value ?inst ?slot))) ?a))
;I need a version that doesn't add if it is the same thing, also from hw2
;(deffunction slot-append-new (?ins ?slot ?a)
;  (if (not (member$ ?a (slot-value ?ins ?slot))) then
;    (slot-append ?ins ?slot ?a)))
(deffunction slot-append-new (?ins ?slot ?a)
  (if (member$ ?a (bind ?l (slot-value ?ins ?slot))) then ?l  ;so always get ?l back
    else (slot-append ?ins ?slot ?a)))

(deffunction slot-put-values (?inst ?slot $?args)
  (slot-append ?inst ?slot ?args)
)
;new
(deffunction remove$ (?a ?l)
  (if (bind ?e-index (member$ ?a ?l)) then
      (bind ?l (delete$ ?l ?e-index ?e-index))
      (bind ?l (remove$ ?a ?l)) ;once more to rm extras
  ) 
?l)
(deffunction slot-remove$ (?ins ?sn ?a)
  (bind ?l (slot-value ?ins ?sn))
  (if (bind ?e-index (member$ ?a ?l)) then
      (bind ?l (slot-delete$ ?ins ?sn ?e-index ?e-index))
      (bind ?l (slot-remove$ ?ins ?sn ?a)) ;once more to rm extras
  ) 
?l) ;here the main (side) effect is in the ins
(deffunction remove1 (?a ?l)
  (progn$ (?e ?l)
    (if (eq ?a ?e) then (bind ?l (delete$ ?l ?e-index ?e-index)))
  )
?l)
(deffunction remove2$ (?a ?l)
 "remove all of the a's from the list" ;doesn't work w/>1 ?a
 (bind ?rl (create$))
 (progn$ (?e ?l)
   (if (neq ?a ?e) then (bind ?rl (create$ ?rl ?e)) )
 )
?rl)


;new
(deffunction slot-swap (?i1 ?i2 ?sn)
  "swap values"
  (bind ?v1 (slot-value ?i1 ?sn))
  (bind ?v2 (slot-value ?i2 ?sn))
  (slot-put-value ?i1 ?sn ?v2)
  (slot-put-value ?i2 ?sn ?v1)
)

;--REST-LV----------------------------------------------------
;returns the rest of the elts. if multifield or value if a value
(deffunction rest-lv (?lv)
	(if (multifieldp ?lv) then (rest$ ?lv) else ?lv))
;new version below to handle LIST too

(deffunction range- (?start ?end)
 "a version of range callable by it, that does reverse ranges";so can just call range to get it
 (if (not (and (numberp ?start) (numberp ?end) (< ?end ?start)))  ;could use in-range
   then (printout t "bad range" ?start ":" ?end))
  (bind ?max (+ ?start ?end))
 (bind ?r (create$))
 (loop-for-count (?i ?end ?start) do (bind ?r (append ?r (- ?max ?i))))
?r)
(deffunction range (?start ?end)
  "from python"
 ;if (not (and (numberp ?start) (numberp ?end) (< ?start ?end)))  ;could use in-range
 (if (not (and (numberp ?start) (numberp ?end)))  ;could use in-range
   then (printout t "bad range" ?start ":" ?end))
 (if (> ?start ?end) then (return (range- ?start ?end)))
 ;if (= ?start ?end) then
 (bind ?r (create$))
 (loop-for-count (?i ?start ?end) do (bind ?r (append ?r ?i)))
?r)
(deffunction range1 (?end) (range 1 ?end))
;I coud call range then map over it, or:
;deffunction range-f (?start ?end ?fnc)

;-
;deffunction map-class (?class ?f1 ?lv1 $?args)
(deffunction map-class (?f1 ?class $?args)
 "map over all instances of the class"
  (do-for-all-instances ((?i ?class))  
       TRUE
     ;(funcall ?f1 ?i ?lv1 ?args)
     (funcall ?f1 ?i ?args)
  )
) 
(deffunction map_class (?f1 ?class $?args)
  (if (full$ ?args) then (map-class ?f1 ?class ?args)
	else (do-for-all-instances ((?i ?class))  TRUE
		     (funcall ?f1 ?i))))
;==MAP===APPLY=========
;add $?args (for flexability)
;then map# would be themax # of lists & min # of total arguments
;use (progn$ )
;--MAP1--APPLY1--------------------------------------------------

;takes a function and at least 1 argument, but not more than 1 list
;then acts like LISP's mapcar, returning a multifield of the results
(deffunction map1 (?f1 ?lv1 $?args)
(if (not (multifieldp ?lv1)) then 
    (printout t "[no mf]")
    (return (funcall ?f1 ?lv1 ?args)))
(if (eq (create$ ) ?lv1) then (create$ ) else  ;clean this
   (create$ (funcall$ ?f1 (nth1-lv ?lv1) ?args)
	    (map1 ?f1 (rest-lv ?lv1) ?args)))) ;should i pass as $?args 

(deffunction map1send (?f1 ?lv1 $?args)
 "should make a lambda if i could"
(if (not (multifieldp ?lv1)) then 
    (printout t "[no mf]")
    (return (send ?lv1 ?f1 ?args)))
(if (eq (create$ ) ?lv1) then (create$ ) else  ;clean this
   (create$ (send (nth1-lv ?lv1) ?f1 ?args)
	    (map1 ?f1 (rest-lv ?lv1) ?args)))) ;should i pass as $?args 


;deffunction foreach (?tv ?lv1 ?f1 $?args) ;also in newest clips so had2fix
(deffunction for-each (    ?lv1 ?f1 $?args)
 "for jess compatability"
 (map1 ?f1 ?lv1 ?args)
)
;(deffunction agendas () (foreach ?m (get-focus-stack) (agenda ?m))) ;not completely compatable
(deffunction agendas () (for-each (get-focus-stack) agenda))
;try june-04
(deffunction map-1 (?f1 ?lv1 $?args)
 "map that works w/o a list, as a funcall alone"
  (if (not (multifieldp ?lv1)) then  (funcall ?f1 ?lv1 $?args)
   else (map1 ?f1 ?lv1 $?args))) 

(deffunction rmap1 (?fnc ?l $?args) ;might want to call rmap-1
 "recursive map-1"
 (if (null ?l) then (return))
 (create$  ;as it isn't just apply
  (bind ?v  (map-1 ?fnc ?l $?args)) ;make a fnc w/side effect that does the same thing
  (rmap1 ?fnc ?v $?args)
 )
)
(deffunction map1i (?f1 ?lv1 $?args)
 "progn$ that has -index as 2nd arg"
 (if (not (multifieldp ?lv1)) then 
    (printout t "[no mf]")
    (return (funcall ?f1 ?lv1 ?args)))
 (progn$ (?v ?lv1) (funcall ?f1 ?v ?v-index ?args))
)
(deffunction map1$ (?f1 ?lv1 $?args)
 "maybe more efficient than map1, by using progn$"
 (if (not (multifieldp ?lv1)) then 
    (printout t "[no mf]")
    (return (funcall ?f1 ?lv1 ?args)))
 (progn$ (?v ?lv1) (funcall$ ?f1 ?v ?args)) ;not a real map, as doesn't ret a list
)
(deffunction pris (?any ?i ?s) ;do I have a map that will send the -index down too?
   (printout t ?s ?i ": " ?any crlf)
);use
;CLIPS> (map1i pris (create$ a b) num-lv)
;num-lv1: a
;num-lv2: b

;lookat:
;CLASS-EXISTP: Returns TRUE if the specified class is defined, FALSE otherwise.
;(class-existp <class-name>)
;
;SUPERCLASSP: Returns TRUE if the 1st class is a superclass of the second class, FALSE otherwise.
;(superclassp <class1-name> <class2-name>)
;
;SUBCLASSP: Returns TRUE if the first class is a subclass of the second class, FALSE otherwise.
;(subclassp <class1-name> <class2-name>)

;CLASS-SUPERCLASSES: Returns the names of the direct superclasses of a class in
;                    a multifield variable. If the optional "inherit" argument 
;                    is given, indirect superclasses are also included.
;(class-superclasses <class-name> [inherit])
;
;CLASS-SUBCLASSES: Returns the names of the direct subclasses of a class in a
;                  multifield variable. If the optional "inherit" argument is
;                  given, indirect subclasses are also included.

;-a use of it w/o side effect
(deffunction class-supers (?class)
  "collect all supers into a flat list"
 (rmap1 class-superclasses ?class)
)
(deffunction class-subs (?class)
  "collect all subs into a flat list, like w/inherit"
 (rmap1 class-subclasses ?class)
)
;now something that will have the side effect of printing out sub -> super at each step
;class-super-fnc class-sub-fnc  ;same return so can recurse the same as above
(deffunction class-super-fnc (?class ?fnc)
 (bind ?r (remove-duplicates (class-superclasses ?class)))
 (map-1 ?fnc ?r ?class)
?r)
(deffunction class-sub-fnc (?class ?fnc)
 (bind ?r (remove-duplicates (class-subclasses ?class)))
 (map-1 ?fnc ?r ?class)
?r
)
;p-sup-sub p-sub-sup
(deffunction p-sup-sub (?sup ?sub)
  (printout t crlf ?sup " -> " ?sub)
)
(deffunction p-sub-sup (?sub ?sup)
  (printout t crlf ?sup " -> " ?sub)
)
;
(deffunction class-sup-dot (?class)
   (bind ?class (to-lst ?class))
   (rmap1 class-super-fnc ?class p-sub-sup)
)
(deffunction class-sub-dot (?class)
   (bind ?class (to-lst ?class))
   (rmap1 class-sub-fnc ?class p-sup-sub)
)
(deffunction local-slotnames (?bad) );redef below
(deffunction p-clos-slt (?sn)
  ;(printout t crlf "  (" ?sn " :initarg :" ?sn " :initform nil)") ;instead of :accessor
  (printout t crlf "  (" ?sn " :initarg :" ?sn " :reader get-" ?sn " :writer put-" ?sn " :initform nil)")
)
(deffunction p-clos-cls (?sub ?sup)
 ;(printout t crlf "(Define-Class " ?sub " has (" ?sup ")" (local-slotnames ?sub) ")")
 (if  (member$ ?sub ?*tlst*) 
      ;(tlst-get clos-tax ?sub) ;(member ?sub [clos-tax]) ;(bag-get ?sub clos-tax)
       then (return))  ;for now 
  (printout t crlf "(def-named-kb-class " ?sub " (" ?sup ") (")
  ;(bind ?ls (local-slotnames ?sub)) 
  ; (if (full$ ?ls) then (map1 p-clos-slt ?ls) 
  ;  else (printout t crlf "()"))
  (map1 p-clos-slt (local-slotnames ?sub)) 
  (printout t "))")
  (bind ?*tlst* (append ?*tlst* ?sub)) 
  ;(tlst-set clost-tax ?sub) ;(nconc [clos-tax] ?sub) ;(bag-set ?sub clos-tax)
)  ;should be out of ()s, go go deeper in tree  ;try getting slots in km 1st
(deffunction p-km-slt (?slt)  
  (printout t crlf "(" ?slt ")")  ;put more here
)
(deffunction p-km-slt2 (?slt ?class)  
  (printout t crlf " (" ?slt " ")  ;put more here
  (printout t crlf "  ;" (slot-facets ?class ?slt))
  (if (neq ?class (first (slot-sources ?class ?slt))) then
   (printout t crlf "  ;sources=" (slot-sources ?class ?slt)))
  (if (< (length$ (slot-types ?class ?slt)) 8) then
   (printout t crlf "  ;types=" (slot-types ?class ?slt)))
  (if (full$ (slot-cardinality ?class ?slt)) then
   (printout t crlf "  ;cardinality=" (slot-cardinality ?class ?slt)))
  (if (slot-allowed-values ?class ?slt) then
   (printout t crlf "  ;allowed=" (slot-allowed-values ?class ?slt)))
  (if (slot-range ?class ?slt) then
   (printout t crlf "  ;range=" (slot-range ?class ?slt)))
  (bind ?dflt  (slot-default-value ?class ?slt)) 
  (if  ?dflt then
   (printout t crlf "  (" (pstr ?dflt) ")"))
  (printout t crlf " )")
)
(deffunction p-km-cls (?sub ?sup)  
  (printout t crlf "(" ?sub " has (superclasses (" ?sup ")))")
  ;could also put out some slot info now too
  (bind ?ls (local-slotnames ?sub))
  (if (full$ ?ls) then
    (printout t crlf "(every " ?sub " has ")
    ;(map1 p-km-slt (local-slotnames ?sub)) 
    (map2 p-km-slt2 ?ls ?sub)
    (printout t " )"))
)
(deffunction class-sub- (?class ?fnc)
   (bind ?class (to-lst ?class))
   (rmap1 class-sub-fnc ?class ?fnc)
)
(deffunction class-sub-clos (?class)
  (class-sub- ?class p-clos-cls))
(deffunction class-sub-km (?class)
  (class-sub- ?class p-km-cls)) 
;(deffunction class-sub-km (?class)
;   (bind ?class (to-lst ?class))
;   (rmap1 class-sub-fnc ?class p-km-cls))

(deffunction taxonomy () (class-sub-dot USER)) ;not quite right
(deffunction km-tax (?cls) 
  (class-sub-km ?cls))
(deffunction km-taxonomy () 
  "well class is-a defns"
  (km-tax USER)) ;not quite right
(deffunction clos-tax (?cls) 
  "well class is-a defns"
 (bind ?*tlst* (create$ ?cls)) 
 (printout t crlf "tlst-" ?*tlst*)
 ;(tlst-create clost-tax) ;(make-instance clos-tax of LIST)  ;(bag-create clos-tax)
  (class-sub-clos ?cls)
 (bind ?*tlst* FALSE) 
 ;(tlst-delete clos-tax) ;(send [clos-tax] delete) ;(bag-delete clos-tax)
) ;not quite right
(deffunction clos-taxonomy ()
    (clos-tax USER))
    ;then get the local slots specified
(deffunction class-path-dot (?class)
 (printout t crlf "digraph " ?class " {")
 (bind ?l (create$
  (class-sup-dot ?class)
  (class-sub-dot ?class)))
 (printout t crlf "}" crlf)
?l)
;-have to decide which one calls rmap
;(deffunction class-dot-supers (?class)
; (rmap1 class-sup-dot ?class))
;(deffunction class-dot-subs (?class)
; (rmap1 class-sub-dot ?class))

(deffunction print+semi (?s) (printout t crlf ?s ";"))
(deffunction printsubgraph (?cls) 
    (printout t crlf "subgraph " ?cls " {label=\"" ?cls "\";" crlf)
    (map1 print+semi (slot-names ?cls)) 
    (printout t crlf "}")) 
(deffunction subclassgraphs (?cls)
    (map1 printsubgraph (class-subclasses ?cls)))

;lookat:
;TIME: Returns a float representing the elapsed seconds since the system reference time.
;(time)
;TIMER: Returns the number of seconds elapsed evaluating a series of expressions.
; (timer <expression>*)

(defglobal ?*start-time* = 0.0);try this now
(deffunction elapse-time () (- (time) ?*start-time*))

;takes a function and at least 1 argument, but not more than 1 list
;then acts like LISP's apply, working only through side-effects
(deffunction apply1 (?f1 ?lv1 $?args)
(if (not (eq (create$ ) ?lv1)) then (funcall$ ?f1 (nth1-lv ?lv1) ?args)
				    (apply1 ?f1 (rest-lv ?lv1) ?args)))

(deffunction apply-1 (?f1 ?lv1 $?args)
 (loop-for-count (?i 1 (length$ ?lv1)) do (funcall$ ?f1 (nth-lv ?i ?lv1) ?args)))

(deffunction apply (?f1 ?lv1 $?args)
 "for jess compat"
 (apply1 ?f1 ?lv1 $?args)
)
;--MAP2--APPLY2--------------------------------------------------

;takes a function and at least 2 arguments, but not more than 2 lists
;then acts like LISP's mapcar, returning a multifield of the results
(deffunction map2 (?f2 ?lv1 ?lv2 $?args)
(if (and (not (multifieldp ?lv1)) (not (multifieldp ?lv2))) then 
    (printout t "[no mf]")
    (return (funcall$ ?f2 ?lv1 ?lv2 ?args)))
;(if (and (eq (length ?lv1) 1) (eq (length ?lv2) 1)) then (printout t "[no ln]")
;    (funcall ?f2 ?lv1 ?lv2 ?args))
(if (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2))
	then (create$ ) else
  (create$ (funcall$ ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) ?args)
           (map2 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) ?args))))

;takes a function and at least 2 arguments, but not more than 2 lists
;then acts like LISP's apply, working only through side-effects
(deffunction apply2 (?f2 ?lv1 ?lv2 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)))
 then (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) ?args)
      (apply2 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) ?args)))

(deffunction apply-2 (?f1 ?lv1 ?lv2 $?args)
 (bind ?l (max (length-lv ?lv1) (length-lv ?lv2)))
 (loop-for-count (?i 1 ?l) do 
   (funcall ?f1 (nth-lv ?i ?lv1) (nth-lv ?i ?lv2) ?args)))

;--MAP-LV---APLLY-LV-------------------------------------------------
;with the ?args  map1 could replace map-lv  <---*
;it is in hap.fnc & set.hnd right now, but can change

;the 2nd arg is treated as a value, even if it is a multifield
(deffunction map-lv (?f2 ?lv1 ?v2)
(if (eq (create$ ) ?lv1) then (create$ ) else
  (create$ (funcall ?f2 (nth1-lv ?lv1) ?v2)
           (map-lv ?f2 (rest-lv ?lv1) ?v2))))

(deffunction apply-lv (?f2 ?lv1 ?v2)
(if (eq (create$ ) ?lv1) then (funcall ?f2 (nth1-lv ?lv1) ?v2)
			      (map-lv ?f2 (rest-lv ?lv1) ?v2)))

;--MAP3--APPLY3--------------------------------------------------

;takes a function and at least 3 arguments, but not more than 3 lists
;then acts like LISP's mapcar, returning a multifield of the results
(deffunction map3 (?f2 ?lv1 ?lv2 ?lv3 $?args)
(if (and (not (multifieldp ?lv1)) (not (multifieldp ?lv2)) (not (multifieldp ?lv3))) then 
    (printout t "[no mf]")
    (return (funcall$ ?f2 ?lv1 ?lv2 ?lv3 ?args)))
(if (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)
	(eq (create$ ) ?lv3)) then (create$ ) else
  (create$ (funcall$ ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) ?args)
           (map3 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) ?args))))

;takes a function and at least 3 arguments, but not more than 3 lists
;then acts like LISP's apply, working only through side-effects
(deffunction apply3 (?f2 ?lv1 ?lv2 ?lv3 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2) (eq (create$ ) ?lv3))) then 
   (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) ?args)
   (apply3 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) ?args)))

(deffunction apply-3 (?f1 ?lv1 ?lv2 ?lv3 $?args)
 (bind ?l (max (length-lv ?lv1) (length-lv ?lv2) (length-lv ?lv3)))
 (loop-for-count (?i 1 ?l) do 
   (funcall ?f1 (nth-lv ?i ?lv1) (nth-lv ?i ?lv2) (nth-lv ?i ?lv3) ?args)))

;--MAP4--APPLY4--------------------------------------------------

;takes a function and at least 4 arguments, but not more than 4 lists
;then acts like LISP's mapcar, returning a multifield of the results
(deffunction map4 (?f2 ?lv1 ?lv2 ?lv3 ?lv4 $?args)
(if (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)
	(eq (create$ ) ?lv3) (eq (create$ ) ?lv4)) then (create$ ) else
 (create$ (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) (nth1-lv ?lv4) ?args)
          (map4 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) (rest-lv ?lv4) ?args))))

;takes a function and at least 4 arguments, but not more than 4 lists
;then acts like LISP's apply, working only through side-effects
(deffunction apply4 (?f2 ?lv1 ?lv2 ?lv3 ?lv4 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)
	(eq (create$ ) ?lv3) (eq (create$ ) ?lv4))) then 
 (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) (nth1-lv ?lv4) ?args)
 (apply4 ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) (rest-lv ?lv4) ?args)))

(deffunction apply-4 (?f1 ?lv1 ?lv2 ?lv3 ?lv4 $?args)
 (bind ?l (max (length-lv ?lv1) (length-lv ?lv2) (length-lv ?lv3) (length-lv ?lv4)))
 (loop-for-count (?i 1 ?l) do 
 (funcall ?f1 (nth-lv ?i ?lv1) (nth-lv ?i ?lv2) (nth-lv ?i ?lv3) (nth-lv ?i ?lv4) ?args)))

;--MAP4--APPLY4--------------------------------------------------

;--MAP----------------------------------------------------
;(deffunction map (?fnc $?lvs)
; (if (eq (nth1-lv ?lvs) nil) then (create$ ) else
;          (create$ (funcall ?fnc (map1 nth1-lv ?lvs)) (map ?fnc (rest$ ?lvs)))))
;;(deffunction smap (?fnc $?lvs)
;;	(if (eq (map1 length$ ?lvs)) then (map ?fnc ?lvs) else FALSE))
;all the lists will get glommed together
;mabye by giving # of lists?

;lookat
;PROGN$: Performs a set of actions for each field of a multifield value.
;(progn$ <list-spec> <expression>*)
;<list-spec> ::= <multifield-expression> |
;                (<list-variable> <multifield-expression>)
;
;SWITCH: Allows a particular group of actions to be performed based
;        on a specified value.
;(switch <test-expression>
;   <case-statement>*
;   [<default-statement>])
;<case-statement> ::= (case <comparison-expression> then <action>*)
;<default-statement> ::= (default <action>*)

;--SMAP#----------------------------------------------------
;not much used,  only insures that the mapping is done only if same sized lists
;should have a warning msg if used

(deffunction smap2 (?f2 ?l1 ?l2)
    (if (eq (length$ ?l1) (length$ ?l2)) 
	then (map2 ?f2 ?l1 ?l2) else FALSE))

(deffunction smap3 (?f2 ?l1 ?l2 ?l3)
    (if (eq (length$ ?l1) (length$ ?l2) (length$ ?l3)) 
	then (map3 ?f2 ?l1 ?l2 ?l3) else FALSE))

(deffunction smap4 (?f2 ?l1 ?l2 ?l3 ?l4)
    (if (eq (length$ ?l1) (length$ ?l2) (length$ ?l3) (length$ ?l4)) 
	then (map4 ?f2 ?l1 ?l2 ?l3 ?l4) else FALSE))

;------------------------------------------------------
;don't think this is used,

(deffunction flush (?e1 ?l1)
    (if (eq (length$ ?l1) 1)
	then (if (eq ?e1 (nth$ 1 ?l1))  
		then (delete$ ?l1 1 1) else ?l1)
	else (if (member$ ?e1 ?l1)
		then (flush ?e1 (delete$ ?l1 (member$ ?e1 ?l1) 
					     (member$ ?e1 ?l1)))
		else ?l1)))

(deffunction same-element (?e1 ?e2) (eq ?e1 ?e2)) ;?
;could be rewritten
(deffunction inter-section (?l1 ?l2)
    (flush FALSE (smap2 same-element ?l1 ?l2)))

;use subsetp in related fnc
;------------------------------------------------------

;if zero predicate,  checks for 0 or 0.0  (which is not done automatically)***
(deffunction zerop (?n) (and (numberp ?n) (eq (float ?n) 0.0)))

;turns nulls into 0.0 otherwise leaves the argument alone
(deffunction nn (?v) (if (null ?v) then 0.0 else ?v))

(deffunction nnfloat (?n) (float (nn ?n)))
(deffunction nnint (?n) (integer (nn ?n)))

;turns non-numbers into 0.0 otherwise leaves the argument alone
(deffunction nnn (?v) (if (numberp ?v) then ?v else 0.0))

;turns non-numbers into 0.0 otherwise leaves the argument alone
;warns you if it had to do this
(deffunction nnnw (?v) (if (numberp ?v) then ?v else 
 (printout t "[WARNING] from nnnw, got " ?v crlf) 0.0))


;a mni & max fncs that uses nnn on its arguments
(deffunction nn-max (?a ?b) (if (> (nnn ?a) (nnn ?b)) then ?a else ?b))
(deffunction nn-min (?a ?b) (if (< (nnn ?a) (nnn ?b)) then ?a else ?b))

;a >= predicate
(deffunction biggerp (?a ?b) (>= (nn ?a) (nn ?b)))

;full is the same as not-null
(deffunction full (?x) (not (null ?x)))
(deffunction full-lv (?v) (not (null-lv ?v)))


;predicates to see if positive or negative
;not sure these should use nnn
(deffunction pos  (?x) (> (nnn ?x) 0.0))
(deffunction neg  (?x) (< (nnn ?x) 0.0))

;--SDIV----------------------------------------------------
;safe division takes a numerator & denominator & devides them
;it assumes numbers, and only checks that the divisor is <> zero
(deffunction sdiv (?n ?d) 
  (if (or (eq ?d 0.0) (eq ?d 0))
	 then (printout t "[warning] sdiv got " ?n " by0" crlf) (return 0.0)) 
  (/ ?n ?d))

;--SFUNCALL----------------------------------------------------

;a safe version of funcall for a fnc & 2 args (not really used)
(deffunction sfuncall (?fnc ?a1 ?a2)
   (if (null ?fnc) then (printout t "[Warning] nil FNC a1=" 
					?a1 " a2=" ?a2 crlf)
     else (if (null ?a1)
		   then (printout t "[Warning] nil ?a1 fnc=" 
					?fnc " a2=" ?a2 crlf)
	     else (if (null ?a2)
		   then (printout t "[Warning] nil ?a2 fnc=" 
					?fnc " a1=" ?a2 crlf)
		    else (funcall ?fnc ?a1 ?a2)))))

;--TRUNC----------------------------------------------------

;(deffunction trunc (?num ?pl)
;  (bind ?m (** 10.0 ?pl))
;  (- ?num (/ (- (* ?num ?m) (round (- (* ?num ?m) 0.5))) ?m)))

;trucate a ?number to ?pl places
;?pl can be negative
(deffunction trunc (?num ?pl)
  (bind ?m (** 10.0 ?pl))
  (/ (integer (round (* ?num ?m))) ?m))

;--check-fncs----------------------------------------------------
;only used in c-util, review this
;brought over from GW version,  I like having explicit checking
;in the functions that funcall (map/apply) are called with.

;applies the given function (fnc) if at least one is a numbers

(deffunction check2num (?fnc ?v ?v2) 
 (if (and (numberp ?v) (numberp ?v2)) then (funcall ?fnc ?v ?v2)
   else (if (and (numberp ?v) (null ?v2)) then 0.0
	  else (if (and (null ?v) (numberp ?v2)) then 0.0
	    	  else (if (and (null ?v) (null ?v2)) then nil
		         else ?v)))))


(deffunction checkf2n0 (?fnc ?v ?v2) 
 (if (and (numberp ?v) (numberp ?v2)) then (funcall ?fnc ?v ?v2)
   else (if (and (numberp ?v) (null ?v2)) then (funcall ?fnc ?v 0.0)
	  else (if (and (null ?v) (numberp ?v2)) then (funcall ?fnc 0.0 ?v2)
	    	  else (if (and (null ?v) (null ?v2)) then nil
		         else (sfuncall ?fnc ?v ?v2))))))

;---------------------------------------------------

;returns a list of values from 2 lists for which fnc was applied

(deffunction fnc-two-listsn0 (?fnc ?l1 ?l2)	
	(map2 ?fnc ?l1 ?l2))

;CLASS-SLOTS: Returns the names of the explicitly defined slots of a class in
;             a multifield variable. If the optional inherit keyword is
;             given, inherited slots are also included.
;(class-slots <class-name> [inherit])
;--LOCAL-SLOTNAMES----------------------------------------------

;given an instance, return the slotnames from that class only,no inhereted slots
(deffunction local-slotnames (?ins)  
  (if (instancep ?ins) then (class-slots (class ?ins))
   else (class-slots ?ins))
)

;--SLOTNAMES----------------------------------------------------

;given an instance, return the slotnames from that class only,w/ inhereted slots
(deffunction slotnames (?ins)  (class-slots (class ?ins) inherit))

;--SUM-SLOTS----------------------------------------------------

;given an instance and a list of slotnames in that instance
;sum up all of the given values
(deffunction sum-slots-sn (?ins ?snames)
  (if (not (instancep ?ins))  then
	(printout t "[Warning] bad sumslots ins=" ?ins " sn=" ?snames crlf)
	(return 0.0))
  (if (eq (nth$ 1 ?snames) nil) then (return 0.0)
    else  (+ (nnn (send ?ins (sym-cat get- (nth$ 1 ?snames))))
	 	       (sum-slots-sn ?ins (rest$ ?snames)))))

;given an instance sum up all of the values
(deffunction sum-slots (?ins)
	(sum-slots-sn ?ins (slotnames ?ins)))

;could get all the values and funcall + vals

;--SLOT-VALUE----------------------------------------------------

;given an instance and slotname, return the value
;it checks to make sure that the args are not nil
;it could check to make sure that the slot is there too
;(deffunction slot-value (?ins ?sn)
;   (if (null ?ins) 
;	then (printout t "[Warning] nil INST sn=" ?sn crlf)
;	else (if (null ?sn)
;		then (printout t "[Warning] nil sn int=" ?ins crlf)
;		else (send ?ins (sym-cat get- (str-cat ?sn))))))

;(deffunction slot-value (?ins ?sn)
; (if (slot-existp (class ?ins) ?sn inherit) then (send ?ins (sym-cat get- ?sn))
;   else (printout t "[slot-value " ?ins " does not have a " ?sn " slot]"))) 

;slot-value is pretty safe now, probably won't need sslot-value

;given an instance and slotname, return the value
;it checks to make sure that the args are not nil
;it also checks to make sure that the slot is there too
(deffunction sslot-value (?ins ?sn)
  (if (null ?ins) then 
	(printout t "[Warning] nil INST sn=" ?sn crlf) (return nil))
  (if (not (slot-existp (class ?ins) ?sn inherit)) then
	(printout t "[Warning] no slot named=" ?sn " for " ?ins crlf)
	(return nil))

  (if (null ?sn) then (printout t "[Warning] nil sn int=" ?ins crlf)
		 else (send ?ins (sym-cat get- (str-cat ?sn)))))

(deffunction sslot-value2 (?ins ?sn)
  (bind ?r (slot-value ?ins ?sn))
  (if (fulll$ ?r) then (make-list ?r) else ?r)
)

;--LOCAL-SLOTS-------------------------------------------------

;I think slotnames already does this (check)
(deffunction local-slots (?inst)
  (if (instancep (symbol-to-instance-name ?inst))
	then (map1 class-slots (class-superclasses (class ?inst)))
	else (create$ nil)))
;i would of though class-slots w/o inherit would be enough; like w/local-slotnames
;deffunction inherited-slots (?inst) ;new end04 below

;;;; then (frame-local-slots (instance-parent ?inst)) 

;;--SLOT-VALUES------------------------------------------------------------------
;returns list of slot values of an instance (?inst)

;given an instance, return a list of the slot values
;used in printing etc.
(deffunction slot-values (?inst)	;if a MF make a LIST?
   ;(map2 sslot-value ?inst (slotnames ?inst))
    (map2 sslot-value2 ?inst (slotnames ?inst))
)

;more dangerous to use in utility fncs, because the slots from 2 instance
;might not line up.  Better not to have temp. lists anyway.

;;--SLOT-LOCAL-VALUES------------------------------------------------------------
;returns list of local [no inherited values]  slot values of an instance (inst)

;given an instance, return a list of the slot values 
;local to the instance's class
;used in printing etc.
(deffunction slot-local-values (?inst)	      
    (map2 sslot-value ?inst (local-slotnames ?inst)))

;---INSTANCE-ADD-VALUES----------------------------------------------------------

;adds values (values) into all the slots of the instance (inst)

;given an instance and a list of values, put the values in the slots
(deffunction instance-add-values (?inst ?values)		
    (smap2 bind (slotnames ?inst) ?values)
    ?inst)					; return the instance

;might be nice to have a version that takes the specific slotnames either
;in a seperate list, like w/  instance-add-sn-values (?inst ?values ?slotnames)
;or in the same list (eg. slotname value slotname value)
;--------------------------------------------------------------------

;add only local values (values) into all slots of the instance (?inst)

;a version of instance-add-values that works only on the uninherited slots
(deffunction instance-add-local-values (?inst ?values)	  
    (map3 rput ?values ?inst (local-slotnames ?inst))
    ?inst)					; return the instance

;--------------------------------------------------------------------
;having ?sn be 2 long, is unwise because ?self:slotname works,but is still legal

;given an instance and list of slotnames to track down, return the value
;it is a safe way to get a value
;?ins is the instance to get from
;?sn is the list of slot-names to track down
(deffunction rget (?ins $?sn)
   (if (not (instancep ?ins))  then 
        (printout t "[Warning] Bad rget ins=" ?ins " sn=" ?sn crlf) ;(return 0.0)
	(return nil)
   )
   (bind ?var (slot-value ?ins (nth$ 1 ?sn))) ;new
   (if (eq (length$ ?sn) 1) then			;return last value
	      ;(bind ?var (send ?ins (sym-cat get- (nth$ 1 ?sn))))
	      (if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
	 else	;do again
	      ;(bind ?var (send ?ins (sym-cat get- (nth$ 1 ?sn))))
	      ;(if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
  	      (rget ?var (rest$ ?sn))))
(deffunction rget$ (?ins $?sn)
 "ret full list this time" ;should make to handle list of ins now too, map1 over it
   (if (multifieldp ?ins) then (return (map1 rget$ ?ins $?sn))) ;this might work 
   (if (not (instancep ?ins))  then 
        (printout t "[Warning] Bad rget ins=" ?ins " sn=" ?sn crlf) ;(return 0.0)
	(return nil)
   )
   (bind ?var (slot-value ?ins (nth$ 1 ?sn))) ;new
   (if (eq (length$ ?sn) 1) then			;return last value
	      ;(bind ?var (send ?ins (sym-cat get- (nth$ 1 ?sn))))
	      ?var ;(if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
	 else	;do again
	      ;(bind ?var (send ?ins (sym-cat get- (nth$ 1 ?sn))))
	      ;(if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
  	      (rget ?var (rest$ ?sn))))

;a version more like remote-put
;like rget, but takes ?val and does a put-LAST-SLOT-NAME
;?val is the value to put, ?ins is the instance to put it in, and
;$?sn is the slot or list of slots to traverse   so it can be put in the last 1
(deffunction rput (?val ?ins $?sn)
   (if (not (instancep ?ins))  then 
      (printout t "[Warning] Bad rput val=" ?val " ins=" ?ins " sn=" ?sn crlf) 
	(return nil)  ;(return 0.0)
   )
   (if (eq (length$ ?sn) 1) then  ;put value
	      ;(bind ?var (send ?ins (sym-cat put- (nth$ 1 ?sn)) ?val)) 
	       (bind ?var (slot-put-value ?ins (nth$ 1 ?sn) ?val)) ;new
	      (if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
  	else ;do again
	      ;(bind ?var (send ?ins (sym-cat get- (nth$ 1 ?sn))))
	       (bind ?var (slot-value ?ins (nth$ 1 ?sn))) ;new
	      ;(if (multifieldp ?var) then (nth$ 1 ?var) else ?var)
  	      (rput ?val ?var (rest$ ?sn))))

;remote-get/put has extra safty features
;--------------------------------------------------------------------
;---NOTE will have to use slots-add-values when instances are not the same
;        so will need a parallel list of slotnames to got w/ the values to add
;add values into some slots (named in the list slotnames), into instance (inst)

(deffunction instance-add-sn-values (?inst ?values ?slotnames)		
    (smap2 bind ?slotnames ?values)
    ?inst)					; return the instance

;--DATA-FNC------------------------------------------------------------------

;returns a list of the result of a function on 2 instances' slot values
;only used for pollution-data instances at the moment  
;	[fnc on any 2 similar inst]

;returns a list of the result of a function on 2 instances' slot values
;can (like pd-fnc be used for any inst), careful if numeric fnc

;takes a functions name, and 2 instances on which the fnc will work
;it returns a list of the fnc results
;(will want to move away from intermediate lists, & assuming similar instances)
(deffunction data-fnc (?fnc ?pd1 ?pd2) 		
    (fnc-two-listsn0 ?fnc (slot-local-values ?pd1)  (slot-local-values ?pd2)))

;--------------------------------------------------------------------

;a version of data-fnc that doesn't use one of the 'check' fncs
;(the 'check' fncs might be phased out)
(deffunction data-fnc-nc (?fnc ?pd1 ?pd2) 		
    (map2 ?fnc (slot-local-values ?pd1)  (slot-local-values ?pd2)))

;--------------------------------------------------------------------
;replace w/ the assoc map fncs
;(I think it is better to just call the appropriate map fnc directly
;because it is not clear which ones will call slot-local-values
;so phase these out.)

(deffunction data-fnc-3 (?fnc ?pd1 ?pd2 ?x)
    (map3 ?fnc (slot-local-values ?pd1) (slot-local-values ?pd2) ?x))

(deffunction data-fnc-4 (?fnc ?pd1 ?pd2 ?x ?y)
    (map4 ?fnc (slot-local-values ?pd1) (slot-local-values ?pd2) ?x ?y))

;(deffunction data-fnc-4 (?fnc ?pd1 ?x ?y ?z) 		
;    (map4 ?fnc (slot-local-values ?pd1) ?x ?y ?z))

;--------------------------------------------------------------------

;note that both data-fnc and data-fnc-nc do the same thing!
;--NOTE  -the fnc-two-lists gets the slot local values, 
;	which might not be the same
;---a CHECK should be made to make sure the fnc is only done on similar slots
;could take the intersection of the slotname list, then get the lists from that
;-or have the fnc-2- get the slot-values itself
;--MIGHT have to put the values into yet another list-
;  might be a good reason to stay w/ an intersection list
;        & use it both when getting values & setting them
;-or calc the intersection once & send to both the fnc-2- & set-data

;--DATA-FNC1---------===================================================

;one instance version
(deffunction data-fnc1 (?fnc ?pd1) 	
    (map1 ?fnc (slot-local-values ?pd1)))

;returns a list of the result of a function on an instances' slot values 
;& a single value
;only used for pollution-data instances at the moment

;---------------------------------------=============================

;--SET-DATA-W-VALUES---------------------=============================

;adds the values in the list (?values) into the instance (pd1's) slots

(deffunction set-data-w-values (?pd1 ?values)
    (instance-add-local-values ?pd1 ?values))

;---------------------------------------=============================

;adds the values in the list (values) into the instance (pd1's) slots

;(deffunction gen-set-data-w-values (?pd1 ?pd2 ?values)
;    (instance-add-sn-values ?pd1
;		(intersection (local-slots ?pd1) (local-slots ?pd2))
;		?values))

;------------------------------------------------------


;------------------------------------------------------

;---NOTE will have to use slots-add-values when instances are not the same
;        this is from set-data-w-values,  will need a slotname list
;fills/replaces pd-inst1 with values from (?fnc ?pd-inst1 ?pd-inst2)

(deffunction set-data-from-fnc (?fnc ?pd-inst1 ?pd-inst2)
	(set-data-w-values ?pd-inst1 (data-fnc-nc ?fnc ?pd-inst1 ?pd-inst2)))

;------------------------------------------------------
;------------------------------------------------------

; Standard Temperature = 298.15 degrees K
; Standard Pressure = 101360.0 N/m^2
; Function to make flow rate the flow at standard conditions
;     Inputs: flowrate is actual flowrate
;             temp  is actual temperature (degrees K)
;             pres is actual pressure in N/m^2

(deffunction flowrate-at-STP (?flowrate ?temp ?pres)
        (/ (* ?pres ?flowrate 298.15) (* ?temp 101360.0)) )

;------------------------------------------------------

; Function to make flow rate the actual flow at given pressure and temperature
;     Inputs: flowrate is standard flowrate
;             temp  is actual temperature (degrees K)
;             pres is actual pressure in N/m^2

(deffunction flowrate-at-actual (?flowrate ?temp ?pres)
        (/ (* 101360.0 ?flowrate ?temp) (* 298.15 ?pres)) )

;------------------------------------------------------

; Using the MKS system:
;	TEMP is in degrees K, 
;	MW is unitless
;	P is in N/m2,  (standard presure, one atm is 101360. N/m2)
;	density is in kg/m3
;       ppm is unit-less

(deffunction density-to-ppm (?density ?MW ?TEMP ?P)
        (/ (* (nn ?density) 1000000. 8.314 1000. ?TEMP ) 
	   (* ?MW (if (null ?P) then 101360.0 else ?P))))

;------------------------------------------------------

(deffunction ppm-to-density (?ppm ?MW ?TEMP ?P)
        (/ (* ?ppm ?MW (if (null ?P) then 101360.0 else ?P)) 
	   (* 8.314 1000. ?TEMP 1000000. )))

;------------------------------------------------------
;deffunction new (?class $?vals)  ;this is replaced in the very newsets CLIPS, so FIX
(deffunction new- (?class $?vals)  ;this is replaced in the very newsets CLIPS, so FIX
 (bind ?s (slot-names ?class))
 ;make an alist of slots and values, as part of the make-instance
 (if (fulll$ ?vals) then
   (bind ?svs (to-str (map2 print-slot-val ?s ?vals)))
   (eval (str-cat "(make-instance " (genins) " of " ?class " " ?svs ")")) ;finish
  else
   (eval (str-cat "(make-instance " (genins) " of " ?class ")")) 
))
;I might change genins to take $?class prefix and change the gen of that symbol
; someday I'd like the count to be kept by every class, so you could skip gensym
;Going w/gensym now though will give you a timeline/ordering of ins creation (by#)
; I just need a replace-prefix &-suffix while at it; I have something that adds if not there but..
;------------------------------------------------------
(deffunction accumulate (?combiner ?nullvalue ?l) 
     (if (null ?l) then ?nullvalue 
      else  (funcall ?combiner (first ?l)
                   (accumulate ?combiner ?nullvalue (rest$ ?l)))))
;CLIPS> (accumulate + 0 (create$ 1 2 3)) 6
;------------------------------------------------------

;given a list of values, sum them up
(deffunction sumlist (?l)
     (if (eq ?l (create$ )) 			;(null ?l) 
       then 0.0 
       else (+ (nnn (nth$ 1 ?l)) (sumlist (rest$ ?l)))))

;------------------------------------------------------
;returns min number in a list
;can use (min 1 2 3)  if not alread in a list.
(deffunction min-list (?l)  (funcall$ min ?l))				
(deffunction max-list (?l)  (funcall$ max ?l))				
(deffunction min$ (?l)  (funcall$ min ?l))				
(deffunction max$ (?l)  (funcall$ max ?l))				

;------------------------------------------------------

;was used in gw version, 
;(deffunction run-cases ()
;  (load "c-disregard.rul")
;  (run)
;  (load "c-case.rul")
;  (load "c-hap.rul")
;  (run))

;--------------------------------------
;choose 1 of these

;takes a function, 2 input instances, and an output instance
(deffunction map2inst-f (?fnc ?in1 ?in2 ?out)
  (bind ?sn (slotnames ?out))
  (loop-for-count (?i 1 (length$ ?sn)) do
    (bind ?gs (sym-cat get- (nth$ ?i ?sn)))
    (send  (funcall ?fnc (send ?in1 ?gs) (send ?in2 ?gs))
	   (sym-cat put- (nth$ ?i ?sn)))))  
	   ;make a version that dumps to a list too

;--PRINT-INS--------------------------------------------------
;takes an instance, and send it the message to print itself
(deffunction print-ins (?ins)
  (if (multifieldp ?ins) then (progn$ (?i ?ins) (print-ins ?i)) ;new
   else
   (if (instancep ?ins) then (send ?ins print)
		else (printout t "[Warning] print-ins got " ?ins crlf))))

;deffunction save-ins2 (?ins ?fn) ;moved down

;defmessage-handler USER pins ()
(defmessage-handler OBJECT pins ()
 ;(ppinstance)
 (bind ?sn (slot-names ?self))
 (bind ?sv (slot-values ?self)) 
 (printout t crlf "(" (instance-name ?self) " of " (class ?self) " ") ;write-to-string
 ;discard nil's now, but could discard anything that is a default value at some point
 (progn$ (?v ?sv) (if (full ?v) then (printout t crlf "(" (nth$ ?v-index ?sn) " " (qis ?v) ")")))
 (printout t crlf ")" crlf)
)
(defmessage-handler OBJECT pclins () ;new
  "clos make-instance for the ins"
 (bind ?sn (slot-names ?self))
 (bind ?sv (slot-values ?self)) 
 (printout t crlf "(make-instance '" (class ?self) " :name " (qins ?self) ) 
 (progn$ (?v ?sv) (if (full ?v) then (printout t crlf ":" (nth$ ?v-index ?sn) " " (qins (qis ?v)) " ")))
 (printout t crlf ")" crlf)
)
;pins2 works well w/qis&typing info  ;I'd like a print-diff w/2vals/line if they differ
 ;really should make generic, and have soemthing special for lists, so it won't fail
(deffunction printins (?ins) ;used in pins and ppi 
 "skips slot_value if val it is nil"  ;could check again default(type)
 (if (instancep ?ins) then (send ?ins pins) ;new
  else ?ins))
(defmessage-handler OBJECT pins2 () ;could also give recurse depth4print&tab in
 ;(ppinstance)
 (bind ?sn (slot-names ?self))
 (bind ?sv (slot-values ?self)) 
 (printout t crlf "(" (instance-name ?self) " of " (class ?self) " ") ;write-to-string
 ;discard nil's now, but could discard anything that is a default value at some point
 (progn$ (?v ?sv) (if (full ?v) then (printout t crlf "(" (nth$ ?v-index ?sn) " " (printins (qis ?v)) ")")))
 (printout t crlf ")" crlf)
)

(deffunction posativep (?num)
  (and (numberp ?num) (> ?num 0)))

(deffunction rpins (?ins ?depth) 
 (bind ?sn (slot-names ?ins))
 (bind ?sv (slot-values ?ins)) 
 (printout t crlf "(" (instance-name ?ins) " of " (class ?ins) " ") ;write-to-string
 ;discard nil's now, but could discard anything that is a default value at some point
 (if (posativep ?depth) then
   (progn$ (?v ?sv) 
	(if (full ?v) then 
		(printout t crlf "(" (nth$ ?v-index ?sn) " " (rpins (qis ?v) (decrn ?depth)) ")")))
   else (printout t crlf "(" ?ins ")"))
 (printout t crlf ")" crlf)
)
(deffunction rpins4 (?ins) 
  (rpins ?ins 4))

(deffunction pclins (?ins) 
 (send ?ins pclins))

(deffunction find (?class ?slot ?value)
 "find all of that class w/that slot having that value"
 (find-all-instances ((?t ?class)) (eq ?t:slot ?value))
)
(deffunction find-ins (?class ?slot ?value)
  (find-all-instances ((?t ?class)) (eq (slot-value ?t ?slot) ?value))
)
;(deffunction find-ins-str (?class ?slot ?value) ;below defn of findall
;  (find-all-instances ((?t ?class)) (findall ?value (slot-value ?t ?slot) ))) 
(deffunction find1 (?class ?slot ?value)
  "just find the 1st, not a mf"
  (first (find ?class ?slot ?value))
)
;(deffunction map-class (?fnc ?class) (do-for-all-instances ((?i ?class))  (funcall ?fnc ?i)))
;(deffunction instances$ (?class)
; (find-all-instances ((?t ?class)) TRUE ))
(deffunction instances$ ($?dclass)
 (bind ?class (first-dflt ?dclass USER))
 (find-all-instances ((?t ?class)) TRUE ))
;--PRINT-ALL-INS--------------------------------------------------
;takes an instance, and send it the message to print itself
(deffunction print-all-ins ($?class)
  (bind ?class (first-dflt ?class USER))
  (map2 send (instances$ ?class) print))

(deffunction save-all-mkins ($?class)
  (bind ?class (first-dflt ?class USER))
  (map2 send (instances$ ?class) pclins))

(deffunction ldc () (list-defclasses))   ;get-defclass-list
(deffunction ldi () (list-definstances)) ;get-definstances-list

(deffunction dir () ;later take module
;make it so it is diff for diff args
	  "like the python dir"
	(list-defmodules)
	(list-defclasses)
	(list-deffunctions)
	(list-defgenerics)
	(list-defmethods)
	(list-defmessage-handlers) ;USER inherit
	(list-definstances)
	(list-deffacts)
	(list-defglobals)
	(list-defrules)
	(list-focus-stack)
        (print-all-ins USER)
	(browse-classes)
)
(deffunction classes ($?of)
  (bind ?c (first-dflt ?of USER))
  (class-subclasses ?c)
)
;(deffunction ppdefrulem (?module)
; "ppdefrule for all w/in a module"
;  (map1 ppdefrule (list-defrules ?module)))
(deffunction ppdefrules ()
 "ppdefrule for all modules"
 ;(map1 ppdefrulem (get-defmodule-list))
  (map1 ppdefrule (get-defrule-list))
)
;get-auto-float-dividend.............................239
;get-class-defaults-mode....................221, 285
;get-current-module...................................230
;get-defclass-list.........................................212
;get-deffacts-list.........................................204
;get-deffunction-list...................................207
;get-defgeneric-list.....................................208
;get-defglobal-list.......................................206
;GetDefglobalValue..................................284
;get-definstances-list..................................223
;get-defmessage-handler-list......................216
;get-defmethod-list.....................................208
;get-defmodule-list.....................................229
;get-defrule-list..........................................204
;get-deftemplate-list...................................188
;get-dynamic-constraint-checking..............240
;get-fact-duplication...................................247
;get-fact-list.......................................194, 291
;get-focus...................................................205
;get-focus-stack..........................................205
;get-function-restrictions......................73, 187
;get-incremental-reset................................252
;get-method-restrictions.......................73, 212
;--APPLY-R------------------------------------------------------------
;these are presently (unused) -
;it is a version of apply that takes its result as an arg to the recursive call
;then if ?fnc  wants to   use it  it can,  can be used for summing etc

;version that applys fnc to top of list and to the recursive product
;if there is a test-fnc it is incorperated into ?fnc
(deffunction apply-r (?fnc ?l $?args)
  (if (= (length$ ?l) 0) then (return (create$ )))
   then (funcall ?fnc  (first$ ?l)   (apply-r ?fnc (rest$ ?l) ?args)))

(deffunction apply2-r (?f2 ?lv1 ?lv2 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)))
 then (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) 
         (apply2-r ?f2 (rest-lv ?lv1) (rest-lv ?lv2) ?args) ?args)))

(deffunction apply3-r (?f2 ?lv1 ?lv2 ?lv3 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2) (eq (create$ ) ?lv3))) then 
   (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3)
        (apply3-r ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) ?args) ?args)))

(deffunction apply4-r (?f2 ?lv1 ?lv2 ?lv3 ?lv4 $?args)
(if (not (or (eq (create$ ) ?lv1) (eq (create$ ) ?lv2)
	(eq (create$ ) ?lv3) (eq (create$ ) ?lv4))) then 
 (funcall ?f2 (nth1-lv ?lv1) (nth1-lv ?lv2) (nth1-lv ?lv3) (nth1-lv ?lv4)
        (apply4-r ?f2 (rest-lv ?lv1) (rest-lv ?lv2) (rest-lv ?lv3) (rest-lv ?lv4) 
	?args) ?args)))


;--APPLY-IF------------------------------------------------------------
;version of apply that applies ?afnc to top of list and to the recursive product
;has a seprate test-function
;could use for collect-if or to sum up etc
;first$->first
(deffunction apply-if (?tfnc ?afnc ?l $?args)
  (if (= (length$ ?l) 0) then (return (create$ )))
  (if (= (length-lv ?l) 1) then (return ?l)) ;was in hw1
  (if (funcall ?tfnc (nth$ 1 ?l) ?args) 
   then (funcall ?afnc  (first ?l)   (apply-if ?tfnc ?afnc (rest$ ?l) ?args))
   else 			      (apply-if ?tfnc ?afnc (rest$ ?l) ?args)))

;--COLLECT-IF------------------------------------------------------------
;like the LISP fnc
;takes a test-fnc a list & some optional args
;and creates a list of the values that pass the test
(deffunction collect-if (?tfnc ?l $?args)
  (apply-if ?tfnc create$ ?l ?args))

;(deffunction collect-member (?m ?s-lst)
; (bind ?ret (create$))
; (progn$ (?s ?s-lst) (if (member$ ?m ?s) then (append ?ret ?m)))
; ?ret)
;use str-index instead of member, or just use filter$  strs str

;(deffunction collect-if (?tfnc ?l $?args)
;  (if (= (length$ ?l) 0) then (return (create$ )))
;  (if (funcall ?tfnc (nth$ 1 ?l) ?args) 
;   then (create$  (first$ ?l)   (collect-if ?tfnc (rest$ ?l) ?args))
;   else 			(collect-if ?tfnc (rest$ ?l) ?args)))

;--COUNT-IF----------------------------------------------------
;takes a test-fnc and a list and counts the # of times the test-fnc passes
;(deffunction count-if (?tfnc ?l)  (apply-if ?tfnc incrn ?l))
(deffunction count-if (?fnc ?l)
  ;if (listp ?l) then 
  (if (multifieldp ?l) then 
    (bind ?n 0)
    (progn$ (?lv ?l) (if (funcall ?fnc ?lv) then (bind ?n (+ ?n 1))))
  else (bind ?n nil))
?n)
(deffunction count_if (?fnc ?l $?args)
  (if (multifieldp ?l) then 
    (bind ?n 0)
    (progn$ (?lv ?l) (if (funcall ?fnc ?lv $?args) then (bind ?n (+ ?n 1))))
  else (bind ?n nil))
?n)

(deffunction and_all (?fnc ?l $?args)
 "ret TRUE only if true for all"
  (bind ?n 0)
  (if (multifieldp ?l) then
    (progn$ (?lv ?l) (if (funcall ?fnc ?lv $?args) then (bind ?n (+ ?n 1))
                      else (break)))
  )
  (eq ?n (length$ ?l))
)

;counts the number of TRUEs in a list
(deffunction count-true (?l)
  (if (eq (length$ ?l) 0) then (return 0))
  (if (eq (nth$ 1 ?l) TRUE) then (+ (count-true (rest$ ?l)) 1)
		  	    else    (count-true (rest$ ?l))))

;--REMOVE-IF------------------------------------------------------------
;like collect-if but returns of list of everything that did not pass the ?tfnc
;first$->first
;-check these:fix?
(deffunction remove-if (?tfnc ?l)
  (if (null$ ?l) then (return ?l))
  (if (funcall ?tfnc (first ?l)) then 
   	                     (remove-if ?tfnc (rest$ ?l)))
   else	(create$ (first ?l) (remove-if ?tfnc (rest$ ?l)))
)
(deffunction collect_if (?tfnc ?l) ;only1working as it had test up front
  (if (null$ ?l) then (return ?l))
  (if (funcall ?tfnc (first ?l)) then 
	(create$ (first ?l) (collect_if ?tfnc (rest$ ?l)))
   else	                     (collect_if ?tfnc (rest$ ?l)))
)
;;these seem broken till it got test  /no still needs fixing
(deffunction remove-if- (?tfnc ?l $?args)
  (if (null$ ?l) then (return ?l))
  (if (funcall ?tfnc (first ?l) ?args) then 
	(create$ (first ?l) (remove-if- ?tfnc (rest$ ?l) ?args))
   else	                     (remove-if- ?tfnc (rest$ ?l) ?args))
)
;  (loop-for-count (?i 1 (length$ ?l)) do
;    (if (funcall ?tfnc (nth$ ?i ?l) ?args) then (bind ?l (delete$ ?l ?i ?i))))
;    ?l  ;redo recursively


;--REMOVE-DUPLICATES---------------------------------------
;(write a) remove-duplicates
;compare first w/ all of rest, if any is eq, then remove it
;- for each one see if it is a member of the rest of the list, if it is remv
(deffunction remove-duplicates (?l)
 (if (null-lv ?l) then ?l else
  (if (member$ (first ?l) (rest$ ?l)) then  (remove-duplicates (rest$ ?l))
   else                (create$ (first$ ?l) (remove-duplicates (rest$ ?l))))))
;deffunction remove-duplicates (?v) ;CLIPS Support <gdronline2008@swbell.net>:
(deffunction rm-duplicates (?v)
  (bind ?rv (create$))
  (progn$ (?s ?v)
     (if (not (member$ ?s ?rv))
        then
        (bind ?rv (create$ ?rv ?s))))
  ?rv)
;------------------------------------------------------
;new from hw2
(deffunction duplicates (?l)
 (if (null-lv ?l) then ?l else
  (if (not (member$ (first ?l) (rest$ ?l))) then  (duplicates (rest$ ?l))
   else                (create$ (first$ ?l) (duplicates (rest$ ?l))))))

(deffunction sort$ (?f ?vals)   
  (funcall sort ?f ?vals))

(deffunction s> (?a ?b) ;like auxilary-fnc string>
  (> (str-compare (to-str ?a) (to-str ?b)) 0))

(deffunction sort-strings (?sl)
  (sort str-compare ?sl))
(deffunction sort-as-strings (?sl)
  (sort s> ?sl))

;auxilary-fn has a max-string, but can just sort w/above and take the first
; &min-string where you'd just take the last
; unless it's a long list, then use that fnc
  
(deffunction smembers$ (?vals)
  "sorted short list of members in a list"
  (remove-duplicates (sort$ s> ?vals)))

(deffunction smember$ (?v1 ?v2)
  (if (full ?v2) then (member$ ?v1 ?v2)
   else (printout t "[WARN:member]" ?v1 ?v2) nil)
)

(deffunction str-cat-sort ($?args)
 (implode$ (smembers$  ?args)))

;SET fncs
;------------------------------------------------------
(deffunction subset-val (?v $?l) 
    (if (member$ ?v ?l) then ?v else nil))
;    (if (subsetp (create$ ?v) ?l) then ?v else nil)
;--UNION-----------------------------------------------
(deffunction union (?l1 ?l2)          (create$ ?l1 ?l2))
(deffunction union-nd (?l1 ?l2)    (remove-duplicates (create$ ?l1 ?l2)))
(deffunction unique (?l1 ?l2)    (remove-duplicates (create$ ?l1 ?l2))) ;added
;--INTERSECTION-----------------------------------------------
;(deffunction intersection (?l1 ?l2)   
;   (collect-if full (map1 subset-val ?l1 ?l2)))
;new from hw2
(deffunction intersection (?l1 ?l2)
   (if (or (null$ ?l1) (null$ ?l2)) then nil ;new
    else
    ;(collect-if full (map1 subset-val ?l1 ?l2))
    ;(collect-if nnull (map1 subset-val ?l1 ?l2))
    (flush nil (map1 subset-val ?l1 ?l2))
   )
)
;look@ subsetp
;------------------------------------------------------

;--SET-DIFFERENCE---------------------------------------
;ret list of elts of l1 that do not appear in l2
;-like rm-dup but l is l1 & do memeber of l2
(deffunction set-difference (?l1 ?l2)  
 "ret list of elts of l1 that do not appear in l2"
 (if (null-lv ?l1) then ?l1 else
  (if    (member$ (first ?l1) ?l2) then    (set-difference (rest$ ?l1) ?l2)
   else  (create$ (first$ ?l1)             (set-difference (rest$ ?l1) ?l2)))))
;(deffunction difference$ (?l1 ?l2) 
; (set-difference ?l1 ?l2))  
;------------------------------------------------------
(deffunction superclasses (?ci)
   (if (instance-existp ?ci) then (class-superclasses (class-of ?ci) inherit)
    else (if (class-existp ?ci) then (class-superclasses ?ci inherit)
    else nil))
)
(deffunction superclasses+self (?ci)
   (if (instance-existp ?ci) then (bind ?ci (class-of ?ci)))
   (if (class-existp ?ci) then 
    (create$ ?ci (class-superclasses ?ci inherit))
    else nil)
)
(deffunction lcs- (?a ?b) ;I think this was wrong
  "least common superclass" ;should probably incl the calling class
  (last$ (intersection (superclasses+self ?a) (superclasses+self ?b) )))
(deffunction lcs (?a ?b)
  "least common superclass" ;should probably incl the calling class
  (first (intersection (superclasses+self ?a) (superclasses+self ?b) )))
;------------------------------------------------------
(deffunction inherited-slots (?inst) ;new end04
  (set-difference (local-slotnames ?inst) (slot-names ?inst))
)
(deffunction  print-class- (?c) ;new end04
  "print name and (types of) slots"
  (bind ?is (inherited-slots ?c)) 
  (printout t crlf ?c ":")
  (if (full$ ?is) then 
   (printout t "inherited:" ?is)
   (printout t ",local:" (local-slots ?c))
  else
   (printout t ":" (local-slots ?c))
))

;deffunction print (?p) 
(deffunction print (?p $?args)  ;args unused right now
 (if (instance-existp ?p) then (send (instance-name ?p) print)
  else ;(print-class ?p) ;not just a print
   (if (class-existp ?p) then (ppdefclass ?p)
    else (printout t crlf ?p))
  ))

(deffunction print1 (?p $?args) (print ?p))

;--incr------------------------------------------------------------
(defmessage-handler OBJECT incr (?sn $?by) 
 (send ?self (sym-cat put- ?sn) 
	(+ (first-dflt ?by 1) (slot-value ?self ?sn))))

;	(+ (first-dflt ?by 1) (send ?self (sym-cat get- ?sn)))
;   (send ?self (sym-cat put- ?sn) (+ 1 (send ?self (sym-cat get- ?sn))))

(defmessage-handler USER decr+ (?sn $?by)
 (send ?self (sym-cat put- ?sn)
    (max 0 (+ (first-dflt ?by 1) (slot-value ?self ?sn))))
)
;--

;---could do apply's w/ a loop to avoid the recursive overhead
;apply-# (l-apply#)  uses the first lists(val)'s length  it should use:
;(max (map1 length$ (create$ ?lv1 ?lv2))) which won't work because of flat lists
; (max (map2 length$ ?lv1 ?lv2))
;CLIPS> (funcall (max (map2 length$ (create$ 1 2 3) (create$ 1 2 3 4 5))))
;[ARGACCES4] Function max expected at least 2 argument(s)
;need map2's mf to turn into 2 args, like: 
;CLIPS> (funcall max (create$ 1 2 3 4 5)) gives 5
;CLIPS> (funcall max (map2 length$ (create$ 1 2 3) (create$ 1 2 3 4 5)))
;CLIPS> (funcall max (map1 length$ (create$ 1 2 3) (create$ 1 2 3 4 5)))
;hell just (max (length$ ?lv1) (length$ ?lv2) (length$ ?lv3))
;(max (length-lv ?lv1) (length-lv ?lv2) (length-lv ?lv3))

;CLIPS util.fnc
;-from scs-utils
;lookat:
;DRIBBLE-ON: Sends trace information to the specified file.
;(dribble-on <file-name>)
;
;DRIBBLE-OFF: Closes the trace file.
;(dribble-off)
;
;WATCH: Enables trace information for the specified item.
;(watch <watch-item>)
;<watch-item> ::= all |
;                 compilations | 
;                 statistics |
;                 focus |
;                 messages |
;                 deffunctions <deffunction-name>* |
;                 globals <global-name>* |
;                 rules <rule-name>* |
;                 activations <rule-name>* |
;                 facts <deftemplate-name>* |
;                 instances <class-name>* |
;                 slots <class-name>* |
;                 message-handlers <handler-spec-1>* [<handler-spec-2>] |
;                 generic-functions <generic-name>* |
;                 methods <method-spec-1>* [<method-spec-2>]
;
;<handler-spec-1> ::= <class-name> <handler-name> <handler-type>
;<handler-spec-2> ::= <class-name> [<handler-name> [<handler-type>]]
;
;<method-spec-1> ::= <generic-name> <method-index>
;<method-spec-2> ::= <generic-name> [<method-index>]
;
;UNWATCH: Disables trace information for the specified item.
;(unwatch <watch-item>)
;
;LIST-WATCH-ITEMS: Displays the current state of watch items.
;(list-watch-items [<watch-item>])

;MATCHES: Displays the facts which match the patterns of a rule.
;(matches <rule-name>)
(deffunction pmatches (?rule)
  (printout t crlf ?rule " ")
  (matches ?rule))
(deffunction ppmatches (?rule)
  (ppdefrule ?rule)
  (matches ?rule))
;SET-BREAK: Sets a breakpoint on a rule.
;(set-break <rule-name>)
;
;REMOVE-BREAK: Removes a breakpoint on a rule.
;(remove-break [<rule-name>])
;
;SHOW-BREAKS: Displays all rules having breakpoints.
;(show-breaks [<module-name>])
;
;REFRESH: Places all current activations of a rule on the agenda.
;(refresh <rule-name>)

;AGENDA: Displays all activations on the agenda of the specified module.
;(agenda [<module-name>])
;
;RUN: Starts execution of rules.  Rules fire until agenda is empty or
;     the number of rule firings limit specified by the first argument 
;     is reached (infinity if unspecified).
;(run [<integer-expression>])
;
;FOCUS: Pushes one or more modules onto the focus stack.
;(focus <module-name>+)
;
;HALT: Stops rule execution.
;(halt)

;PREVIEW-GENERIC: Lists all applicable methods for a particular generic
;                 function call in order of decreasing precedence.
;(preview-generic <generic-function-name> <expression>*)

;PREVIEW-SEND: Displays a list of all the applicable message-handlers for
;              a message sent to an instance of a particular class.
;(preview-send <class-name> <message-name>)

;SET-PROFILE-PERCENT-THRESHOLD: Sets the minimum percentage of time that 
;                               must be spent executing a construct or user 
;                               function for it to be displayed by the 
;                               profile-info command.
;(set-profile-percent-threshold <number in the range 0 to 100>)
;
;GET-PROFILE-PERCENT-THRESHOLD: Returns the current value of the
;                               profile percent threshold.
;(get-profile-percent-threshold)
;
;PROFILE-RESET: Resets all profiling information currently collected 
;               for constructs and user functions.
;(profile-reset)
;
;PROFILE-INFO: Displays profiling information currently collected for 
;              constructs or user functions.
;(profile-info)
;
;PROFILE: Enables/disables profiling of constructs and user functions.
;(profile constructs | user-functions | off)
(deffunction prun ($?n)
 (profile constructs) ;or user-functions 
 (if (full$ ?n) then (run ?n) else  (run))
 (profile off)
 (profile-info)
 (profile-reset)
)

;-------------------debug fncs:
(deffunction wa () (watch all))
(deffunction uwa () (unwatch all))
;If you could test for deffunction/generic-function/mh, I'd do it all in one
; although message-handlers probably have a sperate namespace; so just fncs
(deffunction wdf ($?fncs) (funcall$ watch deffunctions ?fncs))
(deffunction uwdf ($?fncs) (funcall$ unwatch deffunctions ?fncs))
(deffunction wmh ($?fncs) (funcall$ watch message-handlers ?fncs))
(deffunction uwmh ($?fncs) (funcall$ unwatch message-handlers ?fncs))
(deffunction wgf ($?fncs) (funcall$ watch generic-functions ?fncs))
(deffunction uwgf ($?fncs) (funcall$ unwatch generic-functions ?fncs))
(deffunction list-insts-from ($?class) 
  (bind ?class (first-dflt ?class USER))
 ;(instances MAIN ?class)
  (instances$ ?class)
)
(deffunction insm ($?class) (list-insts-from ?class))
(deffunction lic ($?class) (length (list-insts-from ?class)))
(deffunction list-insts ($?class) (list-insts-from ?class))
;(deffunction pins (?i) (print-ins ?i)) ;had a typo
(deffunction pai (?c) (print-all-ins ?c))
(deffunction sb (?rule) (set-break ?rule))
(deffunction m (?rule) (matches ?rule))   ;can refresh it too
(deffunction a () (agenda))   ;new


(deffunction bc ($?c) 
  (if (full$ ?c) then (browse-classes (first ?c))
   else (browse-classes))
)   ;new
(deffunction bc-subs (?c)
  (bc (class-subclasses ?c)))

;(deffunction s2i ($?s) (symbol-to-instance-name (to-sym ?s)))
(deffunction s2i ($?s) 
  (bind ?s (to-sym ?s))
  (if (nnull ?s) then
   (symbol-to-instance-name (to-sym ?s))) ;already rebound
)
;(deffunction pins (?i) (print (s2i ?i))) 
(deffunction pins- (?i) (print-ins (s2i ?i))) 
(deffunction pins (?i) (printins (s2i ?i))) 
(deffunction pins_ (?i) (send (s2i ?i) pins2)) 
(deffunction pclins (?i) (send  (s2i ?i) pclins)) 
(deffunction pclosins (?c) (map1  pclins (insm ?c))) 
(deffunction pclos-ins (?c) (map1  pclins (instances$ ?c))) 
(deffunction pin (?n) (pins (str-cat "gen" ?n))) 
(deffunction n2i (?n) (s2i (str-cat "gen" ?n)))
;(deffunction ppi (?n) (printins (s2i (str-cat "gen" ?n)))) 
(deffunction ppi (?n) (printins (n2i  ?n))) 
(deffunction pins$ (?il) (map1 pins ?il)) 
(deffunction pins2 (?from ?to) (map1 ppi (range ?from ?to))) 
(deffunction pin2 (?to $?from) 
  (bind ?from- (first-dflt ?from 1))
  (map1 pin (range ?from- ?to)))
;send in something other than "gen" in case they are named
(deffunction pinstances$ (?cls) ;also see pai
  (pins$ (instances$ ?cls)))
;----------more math:?:
(deffunction sample-range (?xmin ?dx ?xmax)
 "eg ;xr = xmin:0.1:xmax;           % sample range to plot"
 ;(printout t crlf "min=" ?xmin ",dx=" ?dx ",max=" ?xmax crlf)
 (bind ?val ?xmin)
 (bind ?vals (create$ ?xmin))
 (while (< ?val ?xmax) do
   (bind ?val (+ ?val ?dx))
   (bind ?vals (append ?vals ?val))
 )
?vals)
;eg(sample-range 1 1 10)
;------------------------------------------from new-list.clp
;-start by getting a list of strings; one per line in the file.
(deffunction file2str-mf (?file $?skip)
  "read file lines into MF of strings"
  (bind ?s (first-dflt ?skip ""))
  (if (not (open ?file tmp-2mf "r")) then 
     (printout t "[bad:" ?file "]")
     (return))
 (bind ?lines (create$))
 (while (not (eq (bind ?line (readline tmp-2mf)) EOF)) do
    ;(bind ?lines (insert$  ?lines  (+ 1 (length$ ?lines)) ?line))
    (if (neq ?line ?s) then
	    (bind ?lines (create$ ?lines ?line)) ;not good w/huge files -fix
    )                   ;was append
 )
 (close tmp-2mf)
?lines)

;deffunction file2str_mf (?file $?skip)
(deffunction file2str_mf (?file ?skip ?lf) ;eg lowcase
  "read file lines into MF of strings" ;no dup lines next2each other/expand on this though/&generalize
  (bind ?s (first-dflt ?skip ""))
  (bind ?ll "")
  (if (not (open ?file tmp-2mf "r")) then 
     (printout t "[bad:" ?file "]")
     (return))
 (bind ?lines (create$))
 (while (not (eq (bind ?line (readline tmp-2mf)) EOF)) do
    (if ?lf then (bind ?line (funcall ?lf ?line)))
    ;(bind ?lines (insert$  ?lines  (+ 1 (length$ ?lines)) ?line))
    (if (and (neq ?line ?s) (neq ?line ?ll)) then
	    (bind ?lines (create$ ?lines ?line)) ;not good w/huge files -fix
    )                   ;was append
    (bind ?ll ?line)
 )
 (close tmp-2mf)
?lines)
(deffunction file2str=mf (?file)
   (file2str_mf ?file "" lowcase));eg lowcase
(deffunction print-line (?l)
  (printout t crlf ?l))
(deffunction print-lines (?ls)
  (map1 print-line ?ls))

(deffunction file2mf (?file)
  "put all space separted segments into one long MF"
  (if (null (bind ?strings (file2str-mf ?file))) then
    (printout t crlf "[no strings generated]") (return))
  (bind ?line (create$))
  (progn$ (?l ?strings)
    (bind ?line (create$ ?line (explode$ ?l)))))

;write a file2list &use the list class, so can have one per line
 ;-if reading (rdb)info for ins, if could change ins class, could keep orig info in mf 2lk4changes
;already had it, but for one list so adding file2lists

(deffunction filter$ (?strings ?str)
  "return a MF of strings that incl str" ;should take any fnc
  (bind ?lines (create$))
  (progn$ (?s ?strings)
    (if (str-index ?str ?s) then
        (bind ?lines (append ?lines ?s))))
?lines)
(deffunction filter-not$ (?strings ?str)
  "return a MF of strings that incl str" ;should take any fnc
  (bind ?lines (create$))
  (progn$ (?s ?strings)
    (if (not (str-index ?str ?s)) then
        (bind ?lines (append ?lines ?s))))
?lines)
;-----------------------------------------from scs-util.clp
;CLIPS util.fnc
;==========================================
;adding new code found on web, & then whatever
;;;;===============================================================
;;; counts the number of times that ?count_string  occurrs in ?master_string
;;;===============================================================
(deffunction str-count  ( ?count_string  ?master_string)
  (bind ?len_count_string (str-length ?count_string))
  (bind ?occurrences 0)
  (while (str-index ?count_string ?master_string)                 
    (bind ?occurrences (+ 1 ?occurrences ))                 ;; current length of master                 
    (bind ?len_master_string (str-length ?master_string))                 
    (bind ?pos (str-index ?count_string ?master_string))                 
    (bind ?front (sub-string 0 (- ?pos 1) ?master_string))                 
    (bind ?back (sub-string (+ ?pos ?len_count_string)   ?len_master_string  ?master_string))
    (bind ?master_string (str-cat ?front ""  ?back)) ;; replace with empty string ""
  )
;;(printout t "new string : "  ?master_string crlf)
(return  ?occurrences))

;;;;===============================================================
;;; replaces the first occurrence of sub_string 
;;; inside master_string with replace_string
;;;===============================================================
(deffunction str-replace  ( ?sub_string ?replace_string ?master_string)
        (bind ?len_sub_string (str-length ?sub_string))
        (bind ?len_master_string (str-length ?master_string))
        (bind ?pos (str-index ?sub_string ?master_string))
        (bind ?front (sub-string 0 (- ?pos 1) ?master_string))
        (bind ?back (sub-string (+ ?pos ?len_sub_string)   ?len_master_string
                                                           ?master_string))
        (bind ?master_string (str-cat ?front ?replace_string ?back))
(return ?master_string))

(deffunction str-complement (?a ?b) 
  (str-replace ?a "" ?b) ;CLIPS> (str-complement "who." "who.there") "there"
)
(deffunction str-cat$ ($?list) ;from auxilary-functions
        (bind ?end (length$ $?list))
        (if (> ?end 0)
           then
                (bind ?string-result (nth$ 1 $?list))
                (loop-for-count (?n 2 ?end)
                   do
                        (bind ?string-result (str-cat ?string-result " " (nth$ ?n $?list)))
                )
                (str-replace ?string-result "\"" "#$%")
           else ""
        )
)

(deffunction no-prefix (?pre ?str)
 "if not a prefix already, then add it"
 (if (prefixp ?pre ?str) then (str-replace  ?pre "" ?str)
	else ?str )
)

;;;;===============================================================
;;; replaces multiple occurrences of sub_string inside master_string 
;;;  with replace_string
;;;===============================================================
(deffunction multi-str-replace  ( ?sub_string ?replace_string ?master_string)
 (bind ?len_sub_string (str-length ?sub_string))
 (bind ?back ?master_string)
 (bind ?new_string "")
 (while (str-index ?sub_string ?back)
;;      (printout t "... replacing an occurrence.... " crlf)
;; current length of master
   (bind ?len_master_string (str-length ?back))
   (bind ?pos (str-index ?sub_string ?back))
   (bind ?front (sub-string 0 (- ?pos 1) ?back))
   (bind ?back (sub-string (+ ?pos ?len_sub_string) ?len_master_string  ?back))
   (bind ?new_string (str-cat ?new_string ?front ?replace_string))
 )
 (bind ?new_string (str-cat ?new_string ?back))
;;              (printout t "------function multi-str-replace end -------" crlf)
 (return ?new_string))

(deffunction str-sed (?a ?b ?c)
  (multi-str-replace ?a ?b ?c))
(deffunction str-rm (?a  ?c)
  (multi-str-replace ?a "" ?c))
(deffunction str_rm (?c  ?a)
  (multi-str-replace ?a "" ?c))
(deffunction str-rms (?c ?al)
  (reduce-vl str_rm ?c ?al)
)
;-
(deffunction mstr-rep-wnil (?s ?d)
  "eg:if ,, then ,nil, before an explode"
  (bind ?s (multi-str-replace (str-cat ?d ?d) (str-cat ?d "nil" ?d) ?s))
  (bind ?s (multi-str-replace ?d " " ?s))
);fix:
;mstr-rep-wnil ED:1 (",,,," ",") " nil  nil "

(deffunction dash (?str) 
  "makes symbols more readable"
  (multi-str-replace " " "-" ?str)
) ;CLIPS> (dash "a long string") "a-long-string"
(deffunction underscore (?str) 
  "makes symbols more readable"
  (multi-str-replace " " "_" ?str)
)
;make a impode-w to output to csv at some point
(deffunction pair-w (?v ?d)
  (create$ ?v ?d)
)
(deffunction pair-w$ (?l ?d)
 (map1 pair-w ?l ?d)
);CLIPS> (pair-w$ (create$ a b c) ,) (a , b , c ,)
(deffunction implode-w$ (?l ?d)
  (implode$ (pair-w$ ?l ?d))
) ;CLIPS> (implode-w$ (create$ a b c) ,) "a , b , c ,"
;already have csv-implode and related that already remove-last
;-----------------------------------------EXPLODE-BY
(deffunction explode-by (?s ?d)
  "eg. turn,a,comma,str into a list of those elts"
  (bind ?s (to-str ?s))   ;fix
  ;as an option, so you don't loose empty places
 ;(bind ?s (multi-str-replace (str-cat ?d ?d) (str-cat ?d "nil" ?d) ?s))
 ;(bind ?s (multi-str-replace ?d " " ?s))
  (explode$ 
    (mstr-rep-wnil ?s ?d)
))

(deffunction explode-by$ (?s ?dl)
 "explode by a list of strings"
 (bind ?s (reduce-vl mstr-rep-wnil ?s ?dl))
 (explode$ ?s)
)
;CLIPS> (explode-by$ "ml110804dc1.0005.01_3cell193" (create$ "." "_"))
;"ml110804dc1 0005 01 3cell193"
;CLIPS> (explode-by$ "ml110804dc1.0005.01_3cell193" (create$ "." "_" "cell"))
;"ml110804dc1 0005 01 3 193"
(deffunction explode-cellfile (?fi)
 "file/ins name w/cell in it, broken into a list of info"
 (explode-by$ ?fi (create$ "." "_" "cell"))
)
;also: echo ml110804dc1.0005.01_3cell193 |tr "._" " "
;ml110804dc1 0005 01 3cell193
;-
(deffunction explode-by-positions$ (?s ?dl)
 "mf: 0 dl-n's len"
 (bind ?dle (append ?dl (length ?s)))
 (bind ?start 0)
 (bind ?ssl (create$))
 (progn$ (?n ?dle) do 
   (bind ?ss ;string-to-field would make it more like explode, but could loose parts of strings
      (sub-string ?start ?n ?s))
   (bind ?start (+ ?n 1))
   (bind ?ssl (append ?ssl ?ss)))
?ssl)
;CLIPS> (explode-by-positions$ "abcdefg" (create$ 2 4 5))
;("ab" "cd" "e" "fg")

(deffunction mk-ins-if (?ins ?class)
  "if ins not there, make it w/o slots"
  (bind ?inst (s2i ?ins))
  (if (instance-existp  ?inst) then ?inst
   else (make-instance (to-sym ?ins) of ?class)
  )
)

(deffunction csv-line2mf (?s)
  (explode-by ?s ","))

(deffunction tab2com (?file)
  "can see a file, w/tabs turned to ,'s, use tab2com$ to get info though"
  (system (str-cat "cat " ?file "|~/bin/tab2cs ")))
(deffunction txt2csv (?file)
  "like tab2com, but *.txt goes to *.csv"
  (system (str-cat "cat " ?file ".txt |~/bin/tab2cs|more> " ?file ".csv")))

;new ;the above per slot might be safer, on a csv2mf 
(deffunction caret2ins (?str ?class)
  "gensym up an ins fro ^ delim str"
  (bind ?st 
    (str-cat "(make-instance of " ?class  ;(s2i (gensym)) 
      " " 
     (postfix "))"
      (str-replace "(A \"\")" ")" ;for the end
       (no-prefix ")" 
         (multi-str-replace "=" " "  
           (multi-str-replace "" ")(A" ?str))) 
     ))))
  (printout t crlf ?st)
  (eval ?st) 
)
;mv below
;(deffunction tab2com$ (?file)
;  (sys2l (str-cat "tab2com " ?file)))

;lookat:
;LOAD*: Loads constructs from a file without displaying informational messages.
;(load* <file-name>)
;
;SAVE: Saves constructs to a file.
;(save <file-name>)
;
;BLOAD: Loads a binary image from a file.
;(bload <file-name>)
;
;BSAVE: Saves a binary image to a file.
;(bsave <file-name>)

;BATCH: Executes commands from a file.
;(batch <file-name>)

;BATCH*: Executes commands from a file. Unlike the batch command,
;        evaluates all of the commands in the specified file before 
;        returning rather than replacing standard input.
;(batch* <file-name>)

;INSTANCES: Displays a list of instances.
;(instances [<module-name> [<class-name> [inherit]]])
;
;PPINSTANCE: Prints the slots of the active instance when called from 
;            within the body of a message-handler.
;(ppinstance)
;
;SAVE-INSTANCES: Saves all instances to the specified file.
;(save-instances <file-name>)
;[local | visible [[inherit] <class>+]
;
;LOAD-INSTANCES: Loads instances from the specified file.
;(load-instances <file-name>)
;
;RESTORE-INSTANCES: Loads instances from the specified file.
;(restore-instances <file-name>)

;PPDEFINSTANCES: Displays the text of a given definstances.
;(ppdefinstances <definstances-name>)
;
;LIST-DEFINSTANCES: Displays the list of all definstances in the specified
;                   module (or the current module if none specified).
;(list-definstances [<module-name>])

(deffunction function-existp (?fnc)
  (member$ ?fnc (get-deffunction-list))
) ;CLIPS> (member$ pai (get-deffunction-list)) 241



;------------------for protege:
(defclass %3ATHING (is-a USER) (role abstract) (pattern-match non-reactive))
(defclass %3ASYSTEM-CLASS (is-a USER) (role abstract) (pattern-match non-reactive))
(defclass %3ACLIPS-DEFINITION (is-a USER) (role abstract) (pattern-match non-reactive))
;(defclass %3ACLIPS_TOP_LEVEL_SLOT_CLASS (is-a USER) (role abstract) (pattern-match non-reactive)) 

(deffunction loadp (?file $?rest)
 "load protege project, dflt .pont .pins"
 (bind ?pont (prefix "." (first-dflt ?rest ".pont")))
 (bind ?pins (prefix "." (second-dflt ?rest ".pins")))
 (bind ?pof (str-cat ?file ?pont))
 (bind ?pif (str-cat ?file ?pins))
 (printout t crlf "pont:" ?pof)
 (load ?pof)
 (printout t crlf "pins:" ?pif)
 (load-instances ?pif)
)
(deffunction loadp- (?file)
  (loadp ?file)
  (load (str-cat ?file ".clp"))) ;also get realted code
(deffunction loadpc (?file)
 "loadp w/.clp .ins"
  (loadp ?file ".clp" ".ins"))
(deffunction loadp2 (?file)
 "loadp w/.clp .ins"
  (loadp ?file ".clp" ".ins"))
;---
(deffunction eclp () (loadp "eclp"))
(deffunction eclp2 () (loadp "eclp2"))
;---
(deffunction load-p (?cn)
  "load cls+ins of classname"
  ;if multifiledp  map1 on self
  (loadp (str-cat "p/" ?cn)))  ;will mk fnc to save-class+instances a cls-lst to p/
(deffunction load-insp (?cn)
  "just get the cls instances"
  (load-instances (str-cat "p/" ?cn ".pins")))
;---
;lookat:
;DEPENDENCIES: Lists the partial matches from which a fact or 
;              instance receives logical support.
;(dependencies <fact-or-instance-specifier>)
;
;DEPENDENTS: Lists all facts or instances which receive logical support 
;           from a fact or instance.
;(dependents <fact-or-instance-specifier>)

(deffunction str2file (?str ?file $?p)
  (bind ?permission (first-dflt ?p "w"))
  (if (not (open ?file tmp-s2f ?permission)) then 
     (printout t "[bad:" ?file "]")
     (return))
  (if (and (multifieldp ?str) (full$ ?str)) then (progn$ (?s ?str) (printout tmp-s2f t ?s)) ;in case a mf passed in
   else
  (printout tmp-s2f t ?str) ;need the newline
  )
  (close tmp-s2f)
)

;-tmp
(deffunction file-existp (?f) FALSE) ;will redefine below 
(deffunction tfile ()
 (bind ?tfile (prefix . (gensym)))
 ;consider reusing the file if it isn't removed, but it is
;;(if (file-existp ?tfile) then (tfile) else ?tfile)
;;for now avoid inf pair spiral out w/just 1more try;safe enough but might always do/so fix
;;still calls in pairs, so let the recursive call here -soon
;(if (file-existp ?tfile) then (prefix . (gensym)) else ?tfile)
 ;-hard to avoid w/need to call w/same ext file, so blow off for now
?tfile)
(deffunction file2str (?file)
 (str-cat (expand$ (file2str-mf ?file))) 
)
(deffunction sys2l (?cmd-str)
 "system w/what is returned read into a multifield"
 (bind ?tfile (tfile))
 (system (str-cat ?cmd-str "|cat>" ?tfile))  
 (bind ?l (file2str-mf ?tfile))
 (remove ?tfile)
 ?l)
(deffunction sys2mf (?cmd-str)
 "system w/what is returned read into a multifield"
 (bind ?tfile (tfile))
 (system (str-cat ?cmd-str "|cat>" ?tfile))  
 (bind ?l (file2mf ?tfile))
 (remove ?tfile)
 ?l)
(deffunction sysf2l (?cmd-str-fnc ?str) ;new ;fnc takes the tmp file as an arg
 "system to list, but tmp file also becomes an arg this time"
 (bind ?tfile (tfile))
 (system (str-cat (funcall ?cmd-str-fnc ?str  ?tfile)))  
 (bind ?l (file2str-mf ?tfile))
 (remove ?tfile)
 ?l)
;example fnc   (database on PC)
(deffunction logcmnd-str2file (?str ?file)
  (str-cat ?str " /LOG " ?file))
(deffunction sqlcmd-dbcmnd (?db ?cmd)
  (sysf2l logcmnd-str2file (str-cat "sqlcmd /DB " ?db " /COMMAND \"" ?cmd "\"")) ;finish/fix
)
(deffunction len1p (?l)
 (= (length ?l) 1))
(deffunction pairwise (?l ?fnc)
  (if (or (null ?l) (len1p ?l)) then ?l 
   else (create$ (funcall ?fnc (first ?l) (second ?l)) (pairwise (rest$ ?l) ?fnc))))
;CLIPS> (pairwise (create$ a b c d) str-cat-wsp) ("a b" "b c" "c d" d)
(deffunction pairwise- (?l)
   "put pairs into one str"
   ;(pairwise ?l str-cat-wsp)
   (pairwise ?l str-cat-sort)
)
(deffunction pairwise-expl (?sl)
  "break apart str&make pairs of each neighbor along way"
  (pairwise- (explode$ ?sl)))

(deffunction pairwise-mapstr (?f ?l)
  "map a fnc over each neighbor/pair in the list"
  (map1 ?f (pairwise-expl ?l)))

(deffunction pairwise-common (?str)
  "run comm-on on each pair of strings near each other"
  (pairwise-mapstr comm-on ?str))

(deffunction grep (?str ?f)
 (sys2l (str-cat "grep " (write-to-string ?str) ?f)))
;(deffunction grepf (?in ?f)
; (system (str-cat "grep " (write-to-string ?in) ?f ">" ?f)))
(deffunction grep1f (?in ?gf)
 (bind ?ss (str-cat "grep " ?in " " ?gf ">" ?in))
 (printout t crlf ?ss)
 (system ?ss))
(deffunction grep1fw (?fw)
  (grep1f ?fw "gin"))
(deffunction grep1fl (?l)
  (map1 grep1fw ?l))

;(deffunction comm-on (?s2)
;  (system (str-cat "comm -1 -2 " ?s2 ">" (str-replace " " "+" ?s2))))
(deffunction comm-on (?s2)
  ;(bind ?sf (sort-strings (str-replace " " "+" ?s2))) would have2expl, so sort above
  (bind ?sf (str-replace " " "+" ?s2))
  (if (not (file-existp ?sf)) then 
	  (system (str-cat "comm -1 -2 " ?s2 ">" ?sf))))

;maybe having a wrapper around this and the sqlite stuff would let me test on both machines
;-I have sqlite below, which I'd rather use on the mac right now.
(deffunction system$ ($?args)
 "cat system args together and return as a list"
 (sys2l (to-str $?args))
)
(deffunction sys$ ($?args) ;want one w/o stripping quotes
 "cat system args together and return as a list"
 (sys2l (implode$ $?args))  
     ;still needs str protection though-but then extra quote just get right1st time
)
(deffunction sys1$ (?arg) ;want one w/o stripping quotes
 "cat system args together and return as a list"
 (sys2l $?arg)
)  
;==
(deffunction fbc$ (?op $?args)
 (bind ?str (str-cat "echo \"" (first ?args)))
 (bind ?nl (length ?args))
 ;progn$ (?n ?l) do ;if (> ?n-index 1) then (bind ?str (str-cat ?str " " ?n)) 
 (loop-for-count (?n 2 ?nl) do
  (bind ?nv (nth$ ?n ?args))
  (bind ?str (str-cat ?str " " ?op " " ?nv)))
 (system (str-cat ?str  "\"|cat>.tbc"))
 (system "echo halt |cat>>.tbc")
 (system$ "bc -q .tbc")
)
;==
(defgeneric +)
(defgeneric -)
(defgeneric *)
(defgeneric /)
(defgeneric mod)
(defgeneric <)
(defgeneric >)
;=make a comparable class (mixin?) to get-value then call the specific cmp-op
; ~more for if have to do a get-value; might be nice2have hier of ops2?if possible?
;if it is a str+num, maybe something diff?
(defmethod + ((?s STRING) (?n NUMBER) $?args) (+ ?s (to-str ?n) $?args))
(defmethod - ((?s STRING) (?n NUMBER) $?args) (- ?s (to-str ?n) $?args))
(defmethod / ((?s STRING) (?n NUMBER) $?args) (/ ?s (to-str ?n) $?args))
(defmethod * ((?s STRING) (?n NUMBER) $?args) (* ?s (to-str ?n) $?args))
(defmethod mod ((?s STRING) (?n NUMBER)) (mod ?s (to-str ?n)))
(defmethod < ((?s STRING) (?n NUMBER) $?args) (< ?s (to-str ?n) $?args))
(defmethod > ((?s STRING) (?n NUMBER) $?args) (> ?s (to-str ?n) $?args))
(defmethod + ((?n NUMBER) (?s STRING) $?args) (+ (to-str ?n) ?s $?args))
(defmethod - ((?n NUMBER) (?s STRING) $?args) (- (to-str ?n) ?s $?args))
(defmethod / ((?n NUMBER) (?s STRING) $?args) (/ (to-str ?n) ?s $?args))
(defmethod * ((?n NUMBER) (?s STRING) $?args) (* (to-str ?n) ?s $?args))
(defmethod mod ((?n NUMBER) (?s STRING)) (mod (to-str ?n) ?s))
(defmethod < ((?n NUMBER) (?s STRING) $?args) (< (to-str ?n) ?s $?args))
(defmethod > ((?n NUMBER) (?s STRING) $?args) (> (to-str ?n) ?s $?args))
 ;why w/strs? and want to avoid reusing a file-stream or using file sys for this
  ;if it is needed &I have sockets anyway, I might embed bc or similar (maybe a scheme:)
   ;if this is needed I could do a nop, or try to do the op w/o a system call -finish/fix
;-dangerous to redef, changed version above to eql-*
;(defmethod + ((?a STRING) (?b STRING) $?args) (fbc$ + (create$ ?a ?b ?args)))
;(defmethod - ((?a STRING) (?b STRING) $?args) (fbc$ - (create$ ?a ?b ?args)))
;(defmethod * ((?a STRING) (?b STRING) $?args) (fbc$ * (create$ ?a ?b ?args)))
;(defmethod / ((?a STRING) (?b STRING) $?args) (fbc$ / (create$ ?a ?b ?args)))
;;(defmethod mod ((?a STRING) (?b STRING) $?args) (fbc$ % (create$ ?a ?b ?args)))
;(defmethod mod ((?a STRING) (?b STRING)) (fbc$ % (create$ ?a ?b)))
;(defmethod eq ((?a STRING) (?b STRING) $?args) (fbc$ == (create$ ?a ?b ?args)))
;(defmethod < ((?a STRING) (?b STRING) $?args) (eq (first (fbc$ < (create$ ?a ?b ?args))) "1"))
;(defmethod > ((?a STRING) (?b STRING) $?args) (eq (first (fbc$ > (create$ ?a ?b ?args))) "1"))
(deffunction len1plus (?l)
  ;and (full$ ?l)  len>1
  (not (or (null ?l) (len1p ?l))))
(defmethod > ((?a STRING) (?b STRING) $?args) (and (s> ?a ?b) 
          (if (len1plus ?args) then (funcall >  (rest$ (rest$ ?args)))
           else TRUE)))
(defmethod < ((?a STRING) (?b STRING) $?args) (and (not (s> ?a ?b))
          (if (len1plus ?args) then (funcall >  (rest$ (rest$ ?args)))
           else TRUE)))
;;==
(defmethod + ((?a SYMBOL) (?b SYMBOL) $?args) (funcall and  ?a ?b ?args))
(defmethod - ((?a SYMBOL) (?b SYMBOL) $?args) (funcall or ?a ?b ?args))
(defmethod + (?a ?b $?args) 
  ;(funcall +  ?a ?b ?args) ;is circular now
  (printout t crlf "no+:" ?a ?b ?args)
)
;==
(deffunction sqlDSN (?db ?cmd)
  (printout t crlf "Opening DSN:" ?db " to run query:" ?cmd crlf)
  (sys1$ (str-cat "java sqlDSN " ?db " \"" ?cmd "\"")) ;finish/fix
)
;=I want something for the java version that does a load-instance (&maybe keeps the file around)?
;== ;setting env just started giving a segfault after seemingly unrelated changes, so mv2bottom
;(defglobal ?*env* = (system$ env))  ;might want to updae this, so write a fnc (fix/finish)
;(deffunction echo$ (?var)
; "given env var name, get a value string"
; (filter$ ?*env* (str-cat (upcase ?var) =)))
(deffunction vi (?file)
  (system (str-cat "vi " (to-str ?file)))
)
(deffunction sys2l-with (?cmd-str ?with-str)
  (filter$ (sys2l ?cmd-str) ?with-str)
)
(deffunction sys2s (?cmd-str)
;(bind ?tfile (prefix . (gensym)))
;(system (str-cat ?cmd-str "|cat>" ?tfile))  
;(bind ?str (file2str ?tfile))
;(remove ?tfile)
;?str
 (str-cat (expand$ (sys2l ?cmd-str)))
)

(deffunction ls$ ($?path) ;add a path
  "system ls$"
  (bind ?pth (first-dflt ?path ""))
  (sys2l (to-str "ls " ?pth))
  ;(sys2l "ls")
)

(deffunction path-cat1 (?p2 ?p1) 
  "for use w/map1"
   (path-cat ?p1 ?p2)
)
(deffunction tilde-filep (?f) ?f) ;redefine below
(deffunction bat-p (?f) ?f) ;redefine below
(deffunction not-tilde-filep (?f) (not (tilde-filep ?f))) 
;also want2get rid of .gz, easier to just make sure a .bat
(deffunction ls-bat ($?p) (filter$ (ls$ ?p) ".bat")) 
(deffunction ls_bat ($?p) ;this isn't working 
   (bind ?allbat (filter$ (ls$ ?p) ".bat")) 
   (if (full ?allbat) then (collect_if bat-p ?allbat))) ;was not-tilde-filep
(deffunction nth-bat (?n $?p) (batch (nth$ ?n (filter$ (ls$ ?p) ".bat")))) 
(deffunction all-bat ($?p) 
  (bind ?l (ls_bat ?p)) ;was ls-bat
  ;(loop-for-count (?i 1 (length$ ?l)) do  (batch (nth$ ?i ?l)) (reset))
  (progn$ (?n ?l) do  (batch ?n) (reset))
)
(deffunction ab () (all-bat)) ;if only 1bat file, it is a nice start-ing point 
;careful gets .bat~ too ;till I started usig ls_bat
;-4instances
(deffunction ls-ins (?p) (filter$ (ls$ ?p) ".ins")) 
(deffunction ls-ins-base (?p) 
  "just the base part of the ins filenames, classname by convention"
  (map1 to-sym (map1 get-prefix (ls-ins ?p))))
;(deffunction li-all (?p) (map1 load-instances (ls-ins ?p)))
(deffunction li-all (?p) 
 "li all"
  (bind ?files (ls-ins ?p))
  (if (> (str-length ?p) 1) then (bind ?files (map1 path-cat1 ?files ?p)))
  (map1 load-instances ?files)
)
;-
(deffunction pwd$ () ;can use for CurrentWorkingDir
  "get workind directory"
  (sys2l "pwd")
)
(deffunction file-existp (?file)  ;does it work if a path is given?
 ;(numberp
   (member$ ?file (ls$))
 ;)
) 
;(deffunction logcmnd-str2file (?str ?file) ;do something like this again
;  (str-cat ?str " /LOG " ?file))
(deffunction wget$ (?url)  ;finish
  (sysf2l  str-cat (to-str "wget " ?url " --output-document="))
);eg: (wget$ "http://michael-bobaks-computer.local/~bobak/") 
;consider something for octave/etc to get a matrix in
(deffunction wget (?url)  ;finish, for a put, when don't anything back
  (system  (str-cat  "wget " ?url ))
)
(deffunction wget+ (?url)  ;finish
 (bind ?wg "/usr/local/bin/wget ")
 (system (str-cat ?wg ?url))
)
(deffunction wget+pd (?url ?pd)  ;finish
  (wget+ (str-cat ?url " --post-data=" ?pd))
)
(deffunction plusify (?txt)
 (multi-str-replace " " "+" ?txt)
)
(deffunction wget+ncbo (?txt)  ;finish
 (bind ?url "http://ncbolabs-dev2.stanford.edu:8080/OBS_v1/oba/")
 (bind ?pd2 "&longestOnly=true&wholeWordOnly=true\"")
 (wget+pd ?url (str-cat "\"text=" (plusify ?txt) ?pd2))
)
(deffunction cd$ ($?path) ;add a path
  "system cd$"
  (bind ?pth (first-dflt ?path ""))
  (sys2l (to-str "cd " ?pth))
)

(deffunction tab2com$ (?file)
  "can get a file, w/tabs turned to ,'s"
 (sys2l (str-cat "cat " ?file "|~/bin/tab2cs "))
 ;(system$ (str-cat "cat " ?file "|~/bin/tab2c "))
 ;(sys2s (str-cat "cat " ?file "|~/bin/tab2c "))
 ;(sys2mf (str-cat "cat " ?file "|~/bin/tab2c "))
)
(deffunction tab2com$jc (?file)
  "can get a file, w/tabs turned to ,'s"
 (sys2l (str-cat "cat " ?file "|~/bin/tab2cs|~/bin/jc "))
) ;sys2L version down below

(deffunction x-files (?x $?filt)
  (bind ?fb (first-dflt ?filt ""))
  ;(bind ?r (filter$ (ls$) ?x))
  (bind ?r (collect-if suffix-p (ls$) ?x))
  (if (eq ?fb "") then ?r else 
    ;(filter-not$ (filter$ ?r ?fb) "~") ;or filter by file-type (so match str-end)
    (filter$ ?r ?fb)
    ;(collect-if suffix-p ?r ?fb)
  )
)
(deffunction txt-files ($?filt)
  (x-files ".txt" ?filt)
)
(deffunction ins-files ($?filt)
  (x-files ".ins" ?filt)
)
;shoud filter out *.gz & *~
;-too specific ehre
(deffunction ics-files ()
  (filter$ (ls$) ".ics"))
(deffunction load-ics-ins ()
  (if (not (class-existp icsHeader)) then (load ics.clp))
  (map1 load-instances (ins-files ics))
)
(deffunction load-rois-ins ()
  (if (not (class-existp rois)) then (load rois.clp))
 ;(map1 load-instances (ins-files rois))
  (bind ?nums (map1 load-instances (ins-files rois.ins)))
  (printout t crlf "(rois-init-after)")
  ;(rois-init-after)
?nums)
(deffunction ria () );redef in rois.clp
(deffunction lri () (load-rois-ins) (ria))
;-=new ;put in s.clp
;(deffunction get-rois-en (?e ?n)
; "get rois w/?exp-name and img-step-num ?n"
;  (bind ?l (create$))
;  (do-for-all-instances ((?r rois))  
;    (and (eq ?r:experiment+name ?e)  (eq ?r:image+set+number ?n))
;     (bind ?l (append ?l ?r))
;  )
;?l)

;truckworld used mknod pipes too.
;--from IDE (pwd$) -> ("/") ;could key off of this
(defglobal ?*ins-dir* = "/Users/bobak/Documents/downloads/sci/bio/BioSig/mdb/calbay/")

;-short:
(deffunction li (?file)
  "load-instances will append .ins if not there"
 (load-instances (ending ins ?file)))

(deffunction lid (?file $?dir) ;could have li do this
  (bind ?d (first-dflt ?dir ?*ins-dir*))
  ;could (bind ?d (ending "/" ?d))
  (li (str-cat ?d ?file))  
)

(deffunction date ()
  (system "date"))

(deffunction etime ()
  (* 5.0 (time)))
(deffunction et ()
  (* (time) 5.0))

(deffunction q ()
  (printout t crlf "exit-ing@:" (date) " " (etime))
  (exit))

(defglobal ?*months-days* =       (create$ 31 28 31 30  31  30  31  31  30  31  30 31))
(defglobal ?*months-days-sofar* = (create$  0 31 59 90 120 151 181 212 243 273 304 334 365))
(deffunction months-days (?month) (nth$ ?month ?*months-days*))
(deffunction julian (?month ?days) (+ ?days (nth$ ?month ?*months-days-sofar*)))
(defclass DATE 
  (is-a USER) 
  (role concrete) 
  (pattern-match reactive) 
  (slot year (create-accessor read-write))
  (slot month (create-accessor read-write))
  (slot day (create-accessor read-write))
  (slot hour (create-accessor read-write))
  (slot minute (create-accessor read-write))
  (slot second (create-accessor read-write))
  (slot msec (create-accessor read-write))
  (slot valid (create-accessor read-write))
)
(defmessage-handler DATE validate ()
 "check that the values are ok"
 (bind ?self:valid  
   (and (<= 1991 ?self:year 2021) (<= 1 ?self:month 12) (<= 0 ?self:day (months-days ?self:month))
				      (<= 0 ?self:hour 23) (<= 0 ?self:minute 59) (<= 0 ?self:second 50))
  )
)
(defmessage-handler DATE init after ()
  (send ?self validate)
  (send ?self print-str)
)
(defmessage-handler DATE print-str ()
 "make into a str, then print for now/finish"
 ;printout t crlf 
 (bind ?str
	 (str-cat  
		"year=" ?self:year ",month=" ?self:month ",day=" ?self:day 
		",hour=" ?self:hour ",min=" ?self:minute ",sec=" ?self:second ":" 
		(if ?self:valid then "[ok]" else "[bad]"))
 )
 (printout t crlf  ?str)
?str)
(defmessage-handler DATE julian ()  ;getting julian day would help here
 (julian ?self:month ?self:day)
)
(defmessage-handler DATE as-seconds ()  ;getting julian day would help here
 (+
  (* 60 (- ?self:year 1970)) ;I think this is the standard 
  (* 24 3600  (send ?self julian)) 
  (* 3600 ?self:hour) 
  (* 60 ?self:minute) 
  ?self:second
 )
);could do math by usign seconds too
(defmessage-handler DATE gt (?dt2)
 (and ?self:valid (send ?dt2 get-valid)
  (> ?self:year) (send ?dt2 get-year)
  (> ?self:month) (send ?dt2 get-month)
  (> ?self:day) (send ?dt2 get-day)
  (> ?self:hour) (send ?dt2 get-hour)
  (> ?self:minute) (send ?dt2 get-minute)
  (> ?self:second) (send ?dt2 get-second)
  (> ?self:msec) (send ?dt2 get-msec))
)
(defmessage-handler DATE eql (?dt2)
 (eq ?self ?dt2))
(defmethod = ((?s DATE) (?n DATE) $?args) (send ?s eql ?n $?args)
)
(defmessage-handler DATE lt (?dt2)
 "put in = &change this/finish"
 (and (not (= ?self ?dt2)) (not (send ?self gt ?dt2))) ;&ne-where it would be the same ins now.
)
;make generics for these  ;make to work w/multi-args finish w/a reduce?of sorts?
(defmethod > ((?d1 DATE) (?d2 DATE) $?args) (send ?d1 gt ?d2 $?args))
(defmethod < ((?d1 DATE) (?d2 DATE) $?args) (send ?d1 lt ?d2 $?args))
(defmethod + ((?d1 DATE) (?d2 DATE) $?args) (data-fnc + ?d1 ?d2)) ; $?args
(defmethod - ((?d1 DATE) (?d2 DATE) $?args) (data-fnc - ?d1 ?d2)) ; $?args

;deffunction h ()
;(help "/Users/bobak/bin"))
;-----make-pipe
;could have a set env-var hold pipe-names you expect, if you want; and use echo$
(defclass PIPE 
  (is-a USER) 
  (role concrete) 
  (pattern-match reactive) 
 ;(multislot sys (create-accessor read-write))
  (slot sys (create-accessor read-write))
  (slot pp (create-accessor read-write))
)
(defmessage-handler PIPE print ()
  (printout t crlf ?self ",init:" ?self:sys)
)
(defmessage-handler PIPE init after ()
;(bind ?self:sys (system$ mknod (instance-name ?self) p))
  ;-also4file
 (bind ?s-name (instance-name-to-symbol (instance-name ?self)))
 (close ?s-name) (remove ?s-name)  ;dangerous
 (bind ?fo (open ?s-name "rw"))
  ;-jsut4pipe
 (bind ?self:sys (str-cat (first (system$ mkfifo ?s-name)) ?fo))
 (bind ?self:pp ?s-name)
)
(defmessage-handler PIPE destroy before ()
;(bind ?s-name (instance-name-to-symbol (instance-name ?self)))
 (bind ?s-name ?self:pp)
 (close ?s-name)
 (remove ?s-name)
)
(defgeneric pass)
(defmethod pass ((?p PIPE) ?msg)
  "like print, but to a pipe" ;getting symbol for stream is tricky in clips
;(printout (send ?p get-pp) ?msg)
 (system (to-str echo ?msg > (send ?p get-pp)))
) ;looks like the pipe blocks while waiting for a read, not-good, want asynchrony
;Now try using these instances in place of a stream?, well for methods that take a stream
; (would be cool if streams where in the obj heirarchy), can do it, sort of -in more oop utils

;-other than env-var sharing names of places to share, that can give names of pipes that could
; then give the name of a tmp file after you join..well no, but it is a way to wait till some1joins.
;_can use tee, look for small java app, like cmdtool -I "cat >~/command" &   or "cat ~/sensor" &
;--from UIC AI2 work:
(deffunction get-opt (?ok-ops ?dflt-choice)  
 (printout t crlf ?ok-ops "[" ?dflt-choice "]:")
;(if ?*auto* then (bind ?op ?dflt-choice) 
; else
     (while 
           (not (or (member$ (bind ?op (read)) ?ok-ops) 
                    (and (not ?op)  ;return means take dflt-choice
                         (member$ ?dflt-choice ?ok-ops)
                    )
                )
         ) do 
         (printout t "[" ?op "]is bad")
         (printout t crlf ?ok-ops ":")
     ) 
; ) 
?op) 
;i'll make a version to pass in a predicate, to make it more useful
(deffunction read-pd (?pred ?dflt-choice)  
 "(read) and make sure it passes the pred, or else it goes to a default"
 (printout t crlf ?pred "[" ?dflt-choice "]:")
 (while 
    (not (or (funcall ?pred (bind ?op (read))) 
	    (and (not ?op)  ;return means take dflt-choice
		 (funcall ?pred ?dflt-choice)
	    )
	)
    ) do 
    (printout t "[" ?op "]is bad")
    (printout t crlf ?pred ":")
 ) 
?op) 
(deffunction read-p2d (?pred ?dflt-choice)  
 "like read-pd but pred uses default as 2nd arg"
 (printout t crlf ?pred "[" ?dflt-choice "]:")
 (while 
    (not (or (funcall ?pred (bind ?op (read)) ?dflt-choice) 
	    (and (not ?op)  ;return means take dflt-choice
		 (funcall ?pred ?dflt-choice)
	    )
	)
    ) do 
    (printout t "[" ?op "]is bad")
    (printout t crlf ?pred ":")
 ) 
?op) 
;-art like:
(deffunction position$ (?s ?l)
  "ret num where s is in l"
  ;(if (stringp ?l) then (return (str-index ?s ?l)))
  (if (lexemep ?l) then (return (str-index ?s ?l)))
  (if (not (multifieldp ?l)) then (bind ?l (value ?l)))
  (progn$ (?n ?l) do (if (eq ?n ?s) then (return ?n-index)))
)
(defmethod position (?s (?l MULTIFIELD))
  (position$ ?s ?l)
)

(defgeneric cut-in2) ;at position or search, cut into 2mf 
(defmethod cut-in2 ((?bp NUMBER) (?str STRING) $?bpl)
 (bind ?bpl (first-dflt ?bpl 1))
 (bind ?sl  (str-length ?str))
 (create$ 
  (sub-string 1 (in-range (- ?bp 1) 1 ?sl) ?str)
  (sub-string (in-range (+ ?bp ?bpl)  1 ?sl) ?sl ?str)
  ;(sub-string 1 (min ?bp ?sl) ?str)
  ;(sub-string (min (+ ?bp 1)  ?sl) ?sl ?str)
))
(defmethod cut-in2 ((?bp NUMBER) (?s SYMBOL) $?bpl)
  (cut-in2 ?bp (to-str ?s) ?bpl)
)
(defmethod cut-in2 ((?match LEXEME) ?s)
  (if (numberp (bind ?bp (position$ ?match ?s))) then 
	(cut-in2 ?bp ?s (str-length (to-str ?match)))
  else (create$ ?s nil))
) ;CLIPS> (cut-in2 a abc) ("a" "bc")
;CLIPS> (cut-in2 " " "0005 then other") ("0005 " "then other")
;-
(deffunction first-position$ (?s ?l)
  (sub-string 1 (- (position$ ?s ?l) 1) ?l)
);CLIPS> (first-position$ . abc.def) "abc"
(deffunction rest-position$ (?s ?l)
  (bind ?st (position$ ?s ?l)) 
  ;(printout t crlf "st:" ?st)
  (if (or (not ?st) (eq ?st 1)) then (return ?l))
  ;(bind ?en (length (to-str ?l)))
  (bind ?en (length ?l))
  (if (and (numberp ?st)
	   (< 0 ?st ?en)
      ) then  ;subseq$
	(if (multifieldp ?l) then (end$ ?l ?st) else
	  (if (stringp ?l) then
	    (sub-string (- (position$ ?s ?l) 1) (length (to-str ?l)) ?l)))
    else (printout t crlf "[bad position:" ?st ",:" ?en "]")
  )
) ;CLIPS>  (rest-position$ . abc.def) "def"
(deffunction get-prefix (?fn)
  "not really prefix, everything till file-type 'suffix'"
  (first-position$ . ?fn)
)
;could explode-by and really take the first?
;(deffunction rest-position$ (?s ?l)
;"from s on in l" ;assume s len 1 right now
; (sub-string  (+ (position$ ?s ?l) 1) (str-length ?l) ?l))
(deffunction get-suffix (?fn) ;assuming only one . in filename
  "not really suffix, everything after file-type 'suffix'"
  (rest-position$ . ?fn)
)
;-

(deffunction positions$ (?s ?l $?nums)
  (if (numberp (bind ?n (position$ ?s ?l))) then 
	(positions$ ?s (sub-string (+ ?n 1) (length ?l) ?l)  ?nums (+ ?n (last-dflt ?nums 0)))
   else ?nums)             ;4strings only right now
)
;CLIPS> (positions$ "." "ab.c.de.f")
;(3 5 8) 

(deffunction postfix-p (?post ?str)
  "see if this is the start of the str"
  (eq (position$ ?post ?str) (+ (- (length ?str) (length ?post)) 1)))
(deffunction suffix-p (?str ?post) ;new ;use for file-types 
     ;want to use w/collect-if, but doesn't take an extra arg yet ?
  "see if this is the end of the str"
  ;(if (multifieldp ?post) then (bind ?post (first ?post)))
  (if (full$ ?post) then (bind ?post (first ?post)))
  (postfix-p ?post ?str))

(deffunction tilde-filep (?f) (postfix-p "~" ?f)) 
(deffunction bat-p (?f) (postfix-p ".bat" ?f)) 

;key-value, similar to getf
(deffunction key-value (?key $?args)
  "send in end args, w/key info &get assoc value"
  (bind ?n (position$ ?key ?args))
  (if (integerp ?n) then (nth-lv (+ ?n 1) ?args)
   else FALSE)
)
(deffunction getf (?key $?args)
 "like the lsp fnc"
 (key-value ?key ?args)
)
;new
(deffunction str2 (?n ?l)
  (if (numberp ?n) then (sub-string 1 (- ?n 1) ?l) else ?l)
); CLIPS> (str2 3 "abcdef") -> "ab"
(deffunction str2position (?s ?l)
  "string/list up till search char ?s"
  (bind ?n (position$ ?s ?l))
  ;(if (numberp ?n) then (sub-string 1 (- ?n 1) ?l) else ?l)
  (str2 ?n ?l)
)
(deffunction file-base (?file)
  "str up to 1st ." ;should be to the last--
  (str2position "." ?file)
);CLIPS> (file-base "abc.de.fgh") -> "abc"
(deffunction file_base (?file)
 (str2 (last (positions$ "." ?file)) ?file)
) ;CLIPS> (file_base "abc.de.fgh") -> "abc.de"
(deffunction replace-ext (?file ?new-ext)
  "replace file extension, eg. .txt"
  (str-cat (file_base ?file) (prefix "." ?new-ext))
) ;CLIPS> (replace-ext "abc.def.ghi.txt" "ins") ->"abc.def.ghi.ins"

;----numeric
(deffunction expt10 (?num)
  (** 10 ?num))

(deffunction rand (?num)
  "rand between 1 & num"
  (+ (mod (random) ?num) 1))

(deffunction rand1 ()
  (/ (rand 100) 100))
;---
(defmessage-handler USER incr (?sn $?by) 
 (send ?self (sym-cat put- ?sn) 
        (+ (first-dflt ?by 1) (slot-value ?self ?sn)))
)
(defmessage-handler USER decr+ (?sn $?by)
 (send ?self (sym-cat put- ?sn)
    (max 0 (+ (first-dflt ?by 1) (slot-value ?self ?sn))))
)
;deffunction posativep (?num)

(deffunction sign (?num)
  (if (> ?num 0.0) then 1.0 else -1.0))

(deffunction precision (?float)
  (if (not (floatp ?float)) then (return 0))
  (bind ?s (to-str ?float))
  (- (length$ ?s) (position$ "." ?s)))

(deffunction  actually  (?float  ?min-p)   ;(?min-p :optional 5)
   "recover from a storage problem then puts a number just a little off"  ;can use some other name
  (bind ?p (precision ?float))
  (bind ?p (max ?p ?min-p))  ;don't get rid of real values
  (bind ?n  (+  (/ (sign ?float) (expt10 ?p)) ?float)) ;was expt
  ; (printout t crlf "n=" ?n crlf) 
  ;dbg  ;;If precision changes by alot=1/2 w/addition of smallest delta, fix to actual value
  (if (>= (length$ (to-str ?float)) (+ (length$ (to-str ?n)) (/ ?p 2))) then ?n else ?float))

(deffunction  with-precision  (?float ?p)   
  (trunc ?float ?p))
(deffunction nearly (?a ?b ?p)
  "see if the same at the set precsion"
  (eq (with-precision ?a ?p) (with-precision ?b ?p)))

;-was EOF-------------------------------------------------------------now: class-list-2.clp
;;; -*- mode: lisp; mode: outline-minor; mode: auto-fill; -*- 
;;; $Id: class-list.clp,v 1.22 1998/03/20 11:17:05 d791013 Exp $ 
 
;; Note that the comments below is a little bit out of date and may be 
;; different from the implementation.  
 
;;There are several ways to make a LIST instance or modify its content: 
;;\footnote{CLIPS provides three handlers which deal with slot creatation or 
;;  modification, the |put-| handler, |direct-modify| handler and 
;;  |direct-duplicate| handler. All responds with slot-overrides. The init 
;;  message handler set the default value for slots not designated in 
;;  slot-overrides. For multislot, there are six extra functions available, 
;;  they are |slot-direct-insert$|, |slot-direct-replace$|, 
;;  |slot-direct-delete$|, and |slot-insert$|, |slot-replace$|, 
;;  |slot-delete$|. Note that the last three functions will call the put- 
;;  message handler to modify slot value. Of course, one can define their own 
;;  handlers to bind slot values.} 
;; 
;;\begin{itemize} 
;;\item Using \func{make-list} to create a LIST instance.  For example, 
;;  |(make-list "(+ 1 2)")| will return an instance with three atoms.  Or say 
;;  |(make-list 1 (make-list c d))| will return the same result as |(make-list 
;;  "(1 (c d))")|.  The first and last parentheses can be omitted in a string 
;;  input. 
;;   
;;  Input to the \func{make-list} function can be a list of elements of type 
;;  NUMBER, LEXEME, or LIST.  If a string value is expected as one of a list 
;;  elements, the string escape character, i.e. backslach |\|, should be used, 
;;  e.g. |(make-list "\"a string\")"|. Alternatively use the \func{list}  ;)
;;  function, e.g. |(list "a string")|. 
;; 
;;\item A MULTIFIELD input in the slot-override. Each field in the multifield 
;;  value must be complied to the above syntax, i.e. it must be a STRING, 
;;  SYMBOL, NUMBER, or a LIST, as well as the two extra string |"("| and 
;;  |")"|. This type of format is a little bit complicated because several 
;;  types of object would be appeared in a multifield list. E.g. a multifield 
;;  like this |(create$ + "(* 5 3)" 2 [gen7] "1 a")| is a valid LIST input, 
;;  i.e. a making action shown as below: 
;;  \begin{verbatim} 
;;  (make-instance of LIST (elements + "(* 5 3)" 2 [gen7] "1 a")) 
;;  \end{verbatim} 
;;  Another example likes |(create$ "(" + 5 "(" * 2 a ")" ")")|, which also 
;;  may be created by explode$ to the string |"(+ 5 (* 2 a))"|. 
;; 
;;\item A LEXEME or NUMBER input in the slot-override.  E.g. 
;;  \begin{verbatim} 
;;    (make-instance of LIST (elements 1.2)) 
;;    (make-instance of LIST (elements "a b")) 
;;    (make-instance of LIST (elements "(a b)")) 
;;    (modify-instance [a] (elements "(+ (* 5 3) 2)")) 
;;  \end{verbatim} 
;;  and so on. This method is somewhat confusing and is not recommended 
;;  because the given value is not directly written to the |elements| slot.  It 
;;  is the same as the \func{make-list} function except \func{make-list} can 
;;  be assigned whether to create a LIST instance or just to return a 
;;  multifield value. 
;;   
;;\item Use the \func{list} function to create a LIST instance. This command 
;;  will make LIST instance only opon object of LIST, LEXEME, or NUMBER. 
;;  Namely, the function will not do any further processing to its arguments 
;;  but just enclose them to form a list. 
;;   
;;\item Using slot modification functions, e.g. \func{slot-direct-replace}, to 
;;  modify an existing LIST instance. 
;; 
;;\item Duplicate a list by 
;;  \begin{verbatim} 
;;  (duplicate-instance [b] to [a]) 
;;  \end{verbatim} 
;;   
;;\item Create user-defined handlers. User is responsible for 
;;  setting a correct input. 
;;\end{itemize} 
;;;; 
;; We haven't implemented the dotted list, e.g. append a list to an atom, 
;; (append '(a) 'b) ==> (a . b). Or (cons 'a 'b) ==> (a . b) 
;; 
;; We also couldn't implement macros, e.g. push, pop, etc, because  
;; the eval function can't handle local variables.  
;; 
 
;;; The List class 
 
(defclass LIST 
  (is-a USER) 
  (role concrete) 
  (pattern-match non-reactive) 
  (multislot elements 
             (create-accessor read) 
             (override-message make-list) 
	     (visibility public) 
	     ) 
  (slot GCflag 
	; TRUE for candidate of deletion  
	; when doing the garbage collection 
	(default TRUE) 
	(allowed-symbols TRUE FALSE) 
	(override-message set-gc) 
	(create-accessor write) 
	) 
  ) 
(defclass allowed-value-LIST 
  (is-a LIST) 
  (role concrete) 
) ;ref this prot-style constraint wherever needed
;add methods for specialized error handeling
; subclass further for project specific needs
 ;would consider make-list extra arg for subclass, still might not need:
;SLOT-ALLOWED-VALUES: Returns the allowed values for a slot in a  multifield value.
;(slot-allowed-values <class-name> <slot-name>) ;can get this w/jessTAB

;I'd like a subclass called a PROMISE, that has a func to generate the elts as needed
; a version for the contents of a file, could use (first (wc$ ?file)) to estimate length
 
;;; overloadings for the class LIST 
 
;;; generic functions 
(defgeneric length)  ;new
(defmethod length ((?n NUMBER)) (length (to-str ?n)))
(defmethod length ((?n SYMBOL)) (length (to-str ?n)))
(defgeneric str-length)  ;new
(defmethod str-length ((?n NUMBER)) (str-length (to-str ?n)))
;(defgeneric append)  moved above
(defgeneric nconc)  ;so left in 1st list
(defgeneric atom) 
(defgeneric consp) 
(defgeneric listp) 
(defgeneric list) 
(defgeneric null) 
(defgeneric member) ;new 
;(defgeneric write-to-string)  ;mved up
(defgeneric left-paren-p) 
(defgeneric right-paren-p) 
(defgeneric valid-list-p) 
;(defgeneric make-list) 
(defgeneric list-leveling) 
(defgeneric eval-name) 
(deffunction paren-p (?str)) 
(defgeneric remove) 
 
(deffunction is-class (?class $?specific) ;use class-existp
   ;(funcall eq (type ?class) ?specific) 
  (bind ?super (first-dflt ?specific USER))
  (subclassp ?class ?super)
);not really specific, still sub, &get err if not class, so fix
;new
;(deffunction mk-list () (make-instance (gensym) of LIST)) ;try
;(deffunction mk-list () (make-instance of LIST))
(deffunction mk-list ()  (instance-name (make-instance of LIST)))
;also took out instance-address below;might be slower, but easier to dbg..
;(defgeneric value)  ;mv up
(defmethod value ((?a LIST)) 
  (send ?a get-elements))
;(defmethod value (?a) ?a)

(deffunction rest-lv (?lv)
	(if (multifieldp ?lv) then (rest$ ?lv) else 
           (if (eq (type ?lv) LIST) then (rest$ (value ?lv))
            else ?lv))) ;then just value
 
;;; implementations 
(defmethod member (?a ?b) 
  (member$ ?a ?b ))
(defmethod member (?a (?b LIST)) 
  (member$ ?a (send ?b get-elements)))

;I don't like the cut&paste for nconc
(defmethod nconc ((?a LIST) (?b LIST)) 
  "like append, but kept in 1st list"
  (send (bind ?lst ?a) 
	non-processed-put 
	(send ?a get-elements) 
	(send ?b get-elements)) 
  (instance-address ?lst) 
  ) 
(defmethod nconc ((?a LIST) (?b LIST) ($?lists LIST)) 
  (send (bind ?lst ?a) 
	non-processed-put 
	(send ?a get-elements) 
	(send ?b get-elements) 
        (bind ?mbrs (create$)) 
	(progn$ (?lst ?lists) 
		(bind ?mbrs (create$ ?mbrs (send ?lst get-elements))))) 
  (instance-address ?lst) 
  ) 
 
 
(defmethod append ((?a LIST) (?b LIST)) 
  (send (bind ?lst (mk-list)) 
	non-processed-put 
	(send ?a get-elements) 
	(send ?b get-elements)) 
  (instance-address ?lst) 
  ) 
 
(defmethod append ((?a LIST) (?b LIST) ($?lists LIST)) 
  (send (bind ?lst (mk-list)) 
	non-processed-put 
	(send ?a get-elements) 
	(send ?b get-elements) 
        (bind ?mbrs (create$)) 
	(progn$ (?lst ?lists) 
		(bind ?mbrs (create$ ?mbrs (send ?lst get-elements))))) 
  (instance-address ?lst) 
  ) 
 
(defmethod list ($?atom) 
  (bind ?lst (instance-address (mk-list))) 
  (send ?lst non-processed-put (expand$ ?atom)) 
  ?lst 
  ) 
 
;(defmethod list (?a ?b $?lst) 
;  (bind ?ret (instance-address (mk-list))) 
;  (send ?ret non-processed-put 
;	 ?a ?b (expand$ ?lst)) 
;  ?ret 
;  ) 
 
(defmethod atom ((?lst LIST)) 
  (= (send ?lst length) 0)) 
 
(defmethod atom (?atom) 
  TRUE) 
 
(defmethod consp ((?lst LIST)) 
  (not (atom ?lst))) 
 
(defmethod consp (?any) 
  FALSE) 
 
(defmethod listp ((?lst LIST)) 
  TRUE) 
 
(defmethod listp ((?lst SYMBOL (or (eq ?lst nil) 
				   (eq ?lst NIL)))) 
  TRUE) 
 
(defmethod listp (?other-type) 
  FALSE) 
 
(defmethod null ((?lst LIST)) 
  (= (send ?lst length) 0)) 
 
(defmethod null ((?nil SYMBOL)) 
  (or (eq ?nil nil) 
      (eq ?nil NIL))) 
 
(defmethod null (?any) 
  FALSE) 
 
(defmethod length ((?lst LIST)) 
  (send ?lst length)) 
 
(defmethod length ((?lst SYMBOL )) 
  0) 
 
(defmethod car ((?lst LIST)) 
  (send ?lst car)) 
 
(defmethod car ((?lst SYMBOL (or (eq ?lst nil) 
				 (eq ?lst NIL)))) 
  nil) 
 
(defmethod cdr ((?lst LIST)) 
  (send ?lst cdr)) 
 
(defmethod cdr ((?lst SYMBOL (or (eq ?lst nil) 
				 (eq ?lst NIL)))) 
  nil) 
 
;;; Note the use of push and pop.  
;;  It is better to use cdr, cons and bind. 
(defmethod pop ((?lst LIST)) 
  (send ?lst pop)) 
 
(defmethod pop ((?lst SYMBOL (or (eq ?lst nil) 
				 (eq ?lst NIL)))) 
  nil) 
 
(defmethod push (?obj (?lst LIST)) 
  ;; the constant nil may not be altered by push. 
  (send ?lst push ?obj)) 
 
(defmethod nth ((?ith INTEGER (> ?ith 0)) (?lst LIST)) 
  ;; index starts from 1 for consistency in CLIPS 
  (send ?lst nth ?ith)) 
 
(defmethod nth ((?ith INTEGER (> ?ith 0)) 
		(?lst SYMBOL (or (eq ?lst nil) (eq ?lst NIL)))) 
  nil) 
 
(defmethod cons (?obj (?lst LIST)) 
  (send ?lst cons ?obj)) 
 
(defmethod cons (?obj (?lst SYMBOL (or (eq ?lst nil) 
				       (eq ?lst NIL)))) 
  (list ?obj)) 

;cons only2things.. 
;new
(defmethod cons (?obj ?obj2) 
  (make-list ?obj ?obj2))
;now:
;CLIPS>  (write-to-string  (cons a (cons b (cons d e))))
;"(a b d e)"
;in lisp: (cons 'a (cons 'b (cons 'd 'e)))
;(A B D . E)
;can use to build up interesting structures:
;CLIPS> (write-to-string  (make-list defclass class (cons is-a super)))
;"(defclass class (is-a super))"
;-then i can use: file2list/now called: (csv-)map-file_line2list &soon:map-db_rec2list
;Will have a column/table list of slotnames, to put @body

;was: (deffunction cons ($?args)  ?args)
(defmethod cons (?a $?args)  (create$ ?a ?args))
;so now:
;CLIPS> (cons a b c)
;(a b c)
;CLIPS> (cons a b)
;[gen2]   ;oh well, it won't kill the new list code now/kludgy though
;CLIPS> (cons a)
;(a)

(defmethod position (?s (?l LIST))
 ;(position ?s (slot-value ?l elements))
  (position$ ?s (slot-value ?l elements))
)
(defmethod car ((?s LEXEME))
  ?s)

(defgeneric map)  ;
(defmethod map (?fnc (?a LIST) ?b ?c $?args) 
  (map1 ?fnc (value ?a) ?b ?c ?args))
(defmethod map (?fnc (?a LIST) (?b LIST) ?c $?args) 
  (map2 ?fnc (value ?a) (value ?b) ?c ?args))
(defmethod map (?fnc (?a LIST) (?b LIST) (?c LIST) $?args) 
  (map3 ?fnc (value ?a) (value ?b) (value ?c) ?args))
(defmethod map (?fnc ?a ?b ?c $?args) 
  (map1 ?fnc a ?b ?c ?args))
(defgeneric mapcar)  ;
(defmethod mapcar (?fnc (?a LIST) $?args)  ;
  (map1 ?fnc ?a ?args))

(defgeneric dim)  ;Rinspired:
(defmethod dim ((?a LIST)) (length ?a))
(defmethod dim ((?a MULTIFIELD)) (length ?a))
 
(defmethod eql (?x ?y ) (eq ?x ?y)) 
;;; Here we keep case sensitive until equalp, instead the eq level 
(defmethod eql ((?x LIST (null ?x)) (?y SYMBOL (null ?y))) 
  TRUE) 
 
(defmethod eql ((?x SYMBOL) (?y LIST)) 
  (eq ?y ?x)) 
 
(defmethod equal ((?x LIST) (?y LIST)) 
  (if (eq ?x ?y) then 
    TRUE 
    else   
    (if (neq (bind ?l (length ?x)) (length ?y)) then 
      FALSE 
      else 
      (bind ?i 0) 
      (while (<= (bind ?i (+ ?i 1)) ?l) 
	(if (not (equal (nth ?i ?x) (nth ?i ?y))) then 
	  (return FALSE))) 
      TRUE) 
    )) 
 
(defmethod equal (?x ?y) 
  (eq ?x ?y)) 
 
(defmethod equalp ((?x SYMBOL) (?y SYMBOL)) 
  (or (eq ?x ?y) 
      (eq (upcase ?x) (upcase ?y)))) 
 
(defmethod equalp ((?x STRING) (?y STRING)) 
  (or (eq ?x ?y) 
      (eq (upcase ?x) (upcase ?y)))) 
 
(defmethod equalp ((?x LIST) (?y LIST)) 
  (if (eq ?x ?y) then 
    TRUE 
    else   
    (if (neq (bind ?l (length ?x)) (length ?y)) then 
      FALSE 
      else 
      (bind ?i 0) 
      (while (<= (bind ?i (+ ?i 1)) ?l) 
	(if (not (equalp (nth ?i ?x) (nth ?i ?y))) then 
	  (return FALSE))) 
      TRUE) 
    )) 
 
(defmethod equalp (?x ?y) 
  (equal ?x ?y)) 
 
(defmethod assoc (?item (?lst SYMBOL (or (eq ?lst nil) 
					 (eq ?lst NIL))) $?rest) 
  nil) 
 
(defmethod assoc (?item (?a-list LIST)) 
  (progn$ (?cons (send ?a-list get-elements)) 
	  (if (and (consp ?cons) 
		   (eq ?item (car ?cons))) then 
	    (return ?cons))) 
  nil) 
 
(defmethod assoc (?item 
		  (?a-list LIST  
			   (eq ?test (create$ :test 'equal))) 
		  $?test) 
  (progn$ (?cons (send ?a-list get-elements)) 
	  (if (and (consp ?cons) 
		   (equal ?item (car ?cons))) then 
	    (return ?cons))) 
  nil) 
 
(defmethod assoc (?item 
		  (?a-list LIST  
			   (eq ?test (create$ :test 'equalp))) 
		  $?test) 
  (progn$ (?cons (send ?a-list get-elements)) 
	  (if (and (consp ?cons) 
		   (equalp ?item (car ?cons))) then 
	    (return ?cons))) 
  nil) 
 
;;; Functions or methods for parsing input to a LIST. 
 
(defglobal 
  ?*lp-tag* = "=*=" 
  ?*len-lp-tag* = 4 ; plus the left or right parenthesis 
  ) 
 
(defmethod left-paren-p ((?str STRING)) 
  (or (eq "(" ?str) 
      (and (eq (str-index "(" ?str) 1) 
	   (eq (sub-string 2 ?*len-lp-tag* ?str) ?*lp-tag*))) 
  ) 
 
(defmethod left-paren-p (?other) 
  FALSE) 
 
(defmethod right-paren-p ((?str STRING)) 
  (or (eq ")" ?str) 
      (and (eq (str-index ")" ?str) 1) 
	   (eq (sub-string 2 ?*len-lp-tag* ?str) ?*lp-tag*))) 
  ) 
 
(defmethod right-paren-p (?other) 
  FALSE) 
 
(defmethod list-leveling ((?lst MULTIFIELD) (?start-level INTEGER)) 
  (bind ?level ?start-level) 
  (bind ?nfd 0) 
  (bind ?len 0) 
  (bind ?list (create$)) 
  (progn$ (?ifd ?lst) 
	  (bind ?nfd (+ ?nfd 1)) 
	  (bind ?list 
		(insert$  
		 ?list ?nfd 
		 (if (stringp ?ifd) then 
		   (if (left-paren-p ?ifd) then 
		     (bind ?level (+ ?level 1)) 
		     (str-cat "(" ?*lp-tag* (- ?level 1)) 
		     else 
		     (if (right-paren-p ?ifd) then 
		       (str-cat ")" ?*lp-tag* 
				(bind ?level (- ?level 1))) 
		       else 
		       ?ifd) 
		     ) 
		   else 
		   ?ifd 
		   ))) 
	  (and (= ?level (+ 1 ?start-level)) 
	       (bind ?len (+ ?len 1))) 
	  ) 
  (if (= ?level ?start-level) then 
    (return (create$ (- ?len 1) ?list)) 
    else 
    (printout t "Parentheses don't match!" crlf) 
    FALSE) 
  ) 
 
(defmethod list-leveling ((?list MULTIFIELD)) 
  (list-leveling ?list 0)) 
 
(defmethod list-leveling ($?list) 
  (list-leveling ?list 0)) 
 
(defmethod make-llist-list ((?leveled-list MULTIFIELD) 
                            (?gen-inst SYMBOL 
                                       (or (eq ?gen-inst TRUE) 
                                           (eq ?gen-inst FALSE)))) 
  (bind ?list (create$)) 
  (bind ?nfd 1) 
  (bind ?nlst 0) 
  (bind ?len-llist (length$ ?leveled-list)) 
  (while (< (bind ?nfd (+ ?nfd 1)) ?len-llist) 
    (bind ?fd (nth$ ?nfd ?leveled-list)) 
    (bind  
     ?list 
     (insert$ 
      ?list (bind ?nlst (+ ?nlst 1)) 
      (if (left-paren-p ?fd) then 
        (make-llist-list 
         (subseq$ 
          ?leveled-list 
          ?nfd 
          (bind ?nfd 
                (+ ?nfd (member$ 
                         (str-cat ")" (sub-string 2 (length ?fd) ?fd)) 
                         (subseq$ ?leveled-list (+ 1 ?nfd) 
				  (length ?leveled-list)))))) 
         TRUE) 
        else 
        ?fd)) 
     ) 
    ) 
  (if ?gen-inst then 
    (send (bind ?lst ;(instance-address 
			(mk-list)
		     ;) ;try
	  ) 
	  non-processed-put ?list) 
    ?lst 
    else 
    ?list) 
  ) 
 
(defmethod make-list ((?str STRING)) 
  (bind ?mf (explode$ ?str)) 
  (make-llist-list  
   (rest$ (list-leveling 
	   (if (left-paren-p (nth$ 1 ?mf)) then 
	     ?mf 
	     else 
	     (create$ "(" ?mf ")"))))  
   TRUE)) 
 
(defmethod make-list ((?mf MULTIFIELD) ?make-top-inst-p) 
  (bind ?lst (create$)) 
  (bind ?ifd 1) 
  (progn$ (?fd ?mf) 
	 (bind ?lst 
	       (insert$ ?lst ?ifd  
			(if (stringp ?fd) then 
			  (bind ?tlst (explode$ ?fd)) 
			  (bind ?dfd (length$ ?tlst)) 
			  ?tlst 
			  else 
			  (bind ?dfd 1) 
			  ?fd 
			  ))) 
	 (bind ?ifd (+ ?ifd ?dfd)) 
	 ) 
  (make-llist-list  
   (rest$ (list-leveling 
	   (if (left-paren-p (nth$ 1 ?lst)) then 
	     ?lst 
	     else 
	     (create$ "(" ?lst ")"))))  
   ?make-top-inst-p) 
  ) 
 
(defmethod make-list ($?mf) 
  (make-list ?mf TRUE)) 

;In the pvm version I could add C-code for a string stream, &/or maybe hack a clips version?
 
(deffunction read-list (?in) 
  "takes a stream and reads 1 list from it" ;i'd like to read all lists, &/or read from a str-stream
  (if (neq (bind ?ifd (read ?in)) "(") then 
    ; if token is not a list 
    (if (eq ?ifd ")") then 
      ; bad begining of a list or atom 
      (printout t "Error: read-list: starts from right parenthesis." crlf) 
      (return FALSE) 
      else 
      (return ?ifd))) 
  (bind ?level 1) 
  (bind ?nfd 1) 
  (bind ?len 0) 
  (bind ?list (create$ (str-cat "(" ?*lp-tag* 0))) 
  (while (and (> ?level 0) 
	      (neq (bind ?ifd (read ?in)) EOF)) 
    (bind ?nfd (+ ?nfd 1)) 
    (bind ?list 
	  (insert$  
	   ?list ?nfd 
	   (if (stringp ?ifd) then 
	     (if (left-paren-p ?ifd) then 
	       (bind ?level (+ ?level 1)) 
	       (str-cat "(" ?*lp-tag* (- ?level 1)) 
	       else 
	       (if (right-paren-p ?ifd) then 
		 (str-cat ")" ?*lp-tag* 
			  (bind ?level (- ?level 1))) 
		 else 
		 ?ifd) 
	       ) 
	     else 
	     ?ifd))) 
    (and (= ?level 1) 
	 (bind ?len (+ ?len 1))) 
    ) 
  (if (= ?level 0) then 
    (bind ?ret (make-llist-list ?list TRUE)) 
    ;(send ?ret set-gc FALSE)  
    ?ret 
    ;(return (create$ (- ?len 1) ?list)) 
    else 
    (printout t "Parentheses don't match!" crlf) 
    (bind ?ret (make-llist-list ?list TRUE))  ;new-try ?
    FALSE) 
  ) 

;consider a way/version where the insname was partially informative (take 1st symbol + gensymed)
(deffunction mklist ($?args)
 "easy wrapper to make a list instance" ;I could almost use an older implementation if easier2play w/
 (bind ?args (create$ "(" ?args ")" ))  ;lisp should still be much easier, &way faster
 (make-llist-list  ?args TRUE)
)


(defmethod mapcar ((?fun SYMBOL) ($?lists LIST)) 
  "Only element is one of the types: NUMBER LEXEME and INSTANCE. 
  Other type of objects must have their corresponding eval-name defmethod." 
  (bind ?minlen (length (nth$ 1 ?lists))) 
  (progn$ (?lst ?lists) 
	  (bind ?tlen (length ?lst)) 
	  (if (< ?tlen ?minlen) then  
	    (bind ?minlen ?tlen))) 
  (bind ?ttlst (create$)) 
  (bind ?i 0) 
  (while (<= (bind ?i (+ ?i 1)) ?minlen) 
    (bind ?tlst "") 
    (progn$ (?lst ?lists) 
	    (bind ?tlst 
		  (str-cat ?tlst " " 
			   (eval-name (nth ?i ?lst))))) 
    (bind ?ttlst 
	  (insert$ ?ttlst ?i  
		   (eval (str-cat "(" ?fun ?tlst ")"))))) 
  ;(instance-address 
   (make-instance of LIST 
		  (elements ?ttlst))
  ;) ;try
) 
;==
(defmethod delete ((?ins LIST) ?a ?b)
  (slot-delete$ ?ins elements ?a ?b) 
)
(defmethod delete ((?mf MULTIFIELD) ?a ?b)
  (delete$ ?mf ?a ?b) 
)
(defmethod delete ((?s STRING) ?a ?b)
  (bind ?n (length ?s))
  (bind ?a (in-range ?a 0 ?b))
  (bind ?b (in-range ?b ?a ?n))
  (str-cat (sub-string  0 ?a ?s) (sub-string  ?b  ?n ?s))
)
(defmethod delete (?t ?a ?b)
 (printout t crlf "warn:delete:" ?t "unhandled:" (type ?t) "[" ?a ":" ?b "]")
)
;==
(defglobal ?*nil* = (make-instance nil of LIST))
;==
;deffunction file2str-mf (?file $?skip)
(deffunction file2str-list (?file $?skip)
  "read file lines into MF of strings"
  (bind ?s (first-dflt ?skip ""))
  (if (not (open ?file tmp-f2sl "r")) then 
     (printout t "[bad:" ?file "]")
     (return))
 (bind ?lines (create$))
 (while (not (eq (bind ?line (readline tmp-f2sl)) EOF)) do
    ;(bind ?lines (insert$  ?lines  (+ 1 (length$ ?lines)) ?line))
    (if (neq ?line ?s) then
	    (bind ?lines (append ?lines (make-list ?line))) ;not good w/huge files -fix
    )                   ;was append/create$ above now
 )
 (close tmp-f2sl)
?lines)
(deffunction strmf2list ($?lines)
 "mf of strs, to mf of list instances"
     (map1 make-list ?lines)
)
(deffunction sys2-L (?cmd-str)
 (make-list (sys2l ?cmd-str))
 ) ;still doesn't get mult LISTs if it reads in a multline file
   ;will have to make it call something other than file2str-mf
(deffunction sys2L (?cmd-str)
 (bind ?tfile (tfile))
 (system (str-cat ?cmd-str "|cat>" ?tfile))  
 (bind ?l (file2str-list ?tfile))  ;could try file2list below, but think that is different
 (remove ?tfile)
 ?l)
(deffunction tab2com$JC (?file)
  "can get a file, w/tabs turned to ,'s"
 (sys2L (str-cat "cat " ?file "|~/bin/tab2cs|~/bin/jc "))
)

(deffunction file2list (?file)
  "read file lines into ?";I need to get more used to this package, look@/fix
  (if (not (open (to-str ?file) tmp-f2l "r")) then 
     (printout t "[bad:" ?file "]")
     (return))
  (bind ?ret (read-list tmp-f2l)) 
  (close tmp-f2l)
?ret)

;make tmp file w/parens around it
(deffunction file2lists (?file)
  "read file lines into ?";I need to get more used to this package, look@/fix
 ;should make a tmpfile func that checks for file-existp
 (bind ?tfile (tfile))
 (system (str-cat "echo \"(\" > " ?tfile))  
 (system (str-cat "echo " ?file " >> " ?tfile))  
 (system (str-cat "cat " ?file " >> " ?tfile))  
 (system (str-cat "echo \")\" >> " ?tfile))  
  (if (not (open (to-str ?tfile) tmp-f2 "r")) then 
     (printout t "[bad:" ?file "]")
     (return))
  (bind ?ret (read-list tmp-f2)) 
  (close tmp-f2)
 (remove ?tfile)
?ret)
;deffunction file2lists (?file) ;if more than 1big list in the file
(deffunction map-file_line2list (?file) ;if more than 1big list in the file
  (map1 make-list (file2str-mf ?file))
) ;could use in a method for a FILE class, might try to convert to other than LIST instances. eg.rdb
     ;could subclass LIST, and just add stuff (esp. for quick changing stuff)-so send in class name

(deffunction csv-make-list (?s)  (make-list (csv-line2mf ?s)))

;if there are tabs, call tab2com on the file 1st

(deffunction csv-map-file_line2list (?file) ;if more than 1big list in the file
  (map1 csv-make-list (file2str-mf ?file))
) ;so the 1st LIST in the mf ->class & rest to instances

(deffunction tsv-map-file_line2list (?file) ;if more than 1big list in the file
 "have a tab sepearte file, into  lists?"
  (map1 csv-make-list (tab2com$ ?file))
)

;have a version where the 1st line turns into a class & the rest into instances
;-could even subclass LIST so these lists could come from any source and be processed the same way.
;i could write something w/a frmt str, and all the args could come from $? by counting%s
;--
(deffunction print-slot (?sn $?stream) 
 ;(bind ?s (if (full$ ?stream) then (first ?stream) else crlf))
  (bind ?s (first-dflt  ?stream crlf)) ;i should call this get-opt, (there is 1for cmndline reads)
  (bind ?str (str-cat "(slot " (symbol ?sn) " (create-accessor read-write))"))
  ;(printout ?s ?str crlf) ;print and return
?str)
(deffunction slot-init-str (?sn ?val) ;new
 "clos slot init"
  (bind ?str (str-cat " :" (symbol ?sn) " " ?val " "))
)
(deffunction L2class (?L ?classname ?supername)
  "LIST values become single-slot s in a class"
;(printout t crlf "(defclass " ?classname  " (is-a " ?supername ") w/slots:" (value ?L) ")")  ;finish
 ;(str-cat "(defclass " ?classname  " (is-a " ?supername ")" (mapcar print-slot ?L) ")\n")
  (bind ?rs ;str-cat "(defclass " ?classname  " (is-a " ?supername ") " 
     (make-list "(defclass " (to-sym ?classname)  " (is-a " (to-sym ?supername) ") (role concrete) " 
         (to-str (map1 print-slot (value ?L))) ")"))
  (printout t crlf ?rs)
  (str2file (write-to-string ?rs) (str-cat ?classname ".clp"))
?rs)
;--
(deffunction print-slot-val (?sn ?val $?stream) 
  (bind ?s (first-dflt  ?stream crlf)) 
 ;(bind ?str (str-cat "(" (symbol ?sn) " " ?val ")")) ;w/val can eval w/? look again
  (bind ?str (str-cat "(" (symbol ?sn) " " (symbol ?val) ")")) ;won't have strs anymore, so fix for memo/etc
  ;(printout ?s ?str crlf) ;print and return
?str)

;deffunction L2ins (?L ?classname ?slotnames)
(deffunction L2ins (?L ?slotnames ?classname) ;so can call w/map2
  "LIST values become assoc w/slotnames in a ins of classname"
;(printout t crlf "(" ?L " of " ?classname " w/slot:" (value ?slotnames) " vals:" (value ?L) ")") ;h
 (bind ?name ?L) ;for now-fix
 ;mk a str to eval
 ;(str-cat "(" ?name " of " ?classname " " (mapcar print-slot-val ?slotnames ?L) ")\n")
;(bind ?slotnames (map1 str-strip-quote (value ?slotnames))) ;new, but won't get rid of string, make a strip
  (bind ?rs (str-cat "([" (sym-cat ?name) "] of " (sym-cat ?classname) " " 
     (to-str (map2 print-slot-val ?slotnames (value ?L))) ")"))
  (printout t crlf ?rs)
  (str2file ?rs (str-cat ?classname ".ins") "a")
?rs)
;--use above also4DB->class/ins and below for class/ins->DB too
; for saveing might be easiest to save constructs to file &sed
;--------
(deffunction print-class (?class $?file)
  ;(bind ?s (first-dflt  ?stream crlf)) 
  ;(bind ?slotnames (class-slots ?class inherit))
  (bind ?slotnames (slot-names ?class))
  (printout t crlf "print-class:" ?slotnames) ;dbg
  ;(funcall printout ?stream t "(defclass " ?class  " (is-a " (class-superclasses ?class)  ") " )  
  (if (full ?file) then ;to a file
    ;(open ts (first ?file) "w")
    (open (first ?file) ts  "a")
    (printout ts t "(defclass " ?class  " (is-a " (class-superclasses ?class)  ") " )  
    (progn$ (?sn ?slotnames) do (printout ts t "  (slot " ?sn " (create-accessor read-write))"))
    (printout ts ")")  
    (close ts)
   else ;to the screen
    (printout t crlf "(defclass " ?class  " (is-a " (class-superclasses ?class)  ") " )  
    (bind ?r (map1 print-slot ?slotnames))
    (printout t ")")  
  )
  ;can't pass streams
  ;(printout ?stream t "(defclass " ?class  " (is-a " (class-superclasses ?class)  ") " )  
  ;(bind ?r (map1 print-slot ?slotnames ?stream))
  ;(printout ?stream t ")")  
) ;-slotnames not quite working?  
;but don't really have to save classes as much as the instances , anyway
(deffunction save-class (?class)
 ;(print-class ?class (str-cat ?class ".clp")) ;maybe use .pont
  (print-class ?class (str-cat ?class ".pont")) ;maybe use .pont
)
(deffunction save-class-ins (?class $?file)
  "save class ins into class.ins"
 ;(bind ?insfile (first-dflt ?file (str-cat ?class ".ins")))
  (bind ?insfile (first-dflt ?file (str-cat ?class ".pins")))
  ;"local" or "visible"
  (save-instances ?insfile visible inherit ?class)
  ;-it might be better to have a filename longer than the class, saving extra info in orig file!
)
(deffunction save-class+instances (?cls)
  "make a class .pont & .pins files"
  (save-class ?cls)
  (save-class-ins ?cls)
)
(deffunction save-class-instances (?cl)
  "make a ins file for each class"
  (map1 save-class-ins ?cl)
)
;--------
; by using save-class(tbd) and  (save-instances <file-name> [local | visible [[inherit] <class>+])
(deffunction L2classinstances (?Lmf ?file $?opt-class)
  "1st row are col-name->class-slots, then rest are instances of that class w/those slots-vals"
   (bind ?slotnames FALSE)
  ;(bind ?Lmf (csv-map-file_line2list ?file))   ;;Just use file2list if can explode w/o a ,  
   ;could use file up2ext as classname for now
   (if (full$ ?opt-class) then  (bind ?classname (first ?opt-class))  
       (printout t crlf "L2 using passed in class:" ?classname)
    else
     (bind ?classname (file-base ?file)) ;too aggressive, should go to last .;use4classnaem but!file
   )
   (bind ?classfile (str-cat ?classname ".clp"))
   (bind ?class (to-sym ?classname))
   (bind ?insfile (replace-ext ?file "ins")) ;this will help
  (bind ?istrs "") ;new?
  ;(bind ?files (ls$))
  ;(bind ?have-classfile (member ?classfile ?files))
  ;(bind ?have-insfile (member ?insfile ?files))
   (bind ?have-classfile (file-existp ?classfile))
   (bind ?have-insfile (file-existp ?insfile))
   (printout t crlf "L2:" ?classfile "[" ?have-classfile "]:" ?insfile "[" ?have-insfile "]" crlf)
   (bind ?firstrowL (first ?Lmf))
   ;might skip this if there is already a class file, or really if class alrady there
  (if (not (class-existp ?class)) then 
    (if (and (file-existp ?classfile) (load ?classfile)) then 
        (printout t crlf "got:" ?classfile)
        (bind ?slotnames (slotnames ?class))
     else (printout t crlf "have to create the class(file)")
     (bind ?cstrs (L2class ?firstrowL ?classname "USER")) ;could make a csv class&insert file-path etc.
    )
    else (bind ?slotnames (slotnames ?classname)) ;set from sent-in class vs 1strow
  )
   ;(bind ?slotnames (send ?firstrowL get-elements)) 
   (if (not ?slotnames) then  ;if have class/file then already set
	   (bind ?slotnames ?firstrowL) 
   )
  (if (not ?have-insfile) then 
   (if (null$ ?slotnames) then (bind ?slotnames (slot-names ?class))) ;should of worked above
   (if (null$ ?slotnames) then (bind ?slotnames (range1 (length ?firstrowL)))) ;not going2work
   (bind ?istrs (map2 L2ins (rest-lv ?Lmf) ?slotnames ?classname)) 
  ;(bind ?istrs (map2 L2ins (rest$ ?Lmf) ?slotnames ?classname)) 
 ;where are the istrs written? fix-
  ;if (not ?have-insfile) then  ;mv up
    (if (full$ ?istrs) then (str2file ?istrs ?insfile) ;new
     else (printout t crlf "no:" ?insfile " and nothing to print:" ?istrs
              ",Lmf:" ?Lmf ",firstrowL:" ?firstrowL))
  )
   (bind ?r ?istrs)
  (if (not (class-existp (to-sym ?classname))) then  ;done in case it just had2be constructed&still not loaded?
   (bind ?r (cons ?cstrs ?istrs))
   (load ?classfile)
  )
  ;(load-instances (str-cat ?classname ".ins"))
  (if (file-existp ?insfile) then
   (load-instances ?insfile)
  else (printout t crlf "[WARN-ERROR:" ?insfile ", not loaded"))
?r)
;=many files have extra stuff at the top that I need to ba able to skip/deal w/.
; i also should parse ics headers
;-
(deffunction txt2classinstances (?file) ;use csv version instead
   (bind ?Lmf (make-list (file2list ?file)))   ;;Just use file2list if can explode w/o a ,  
   (printout t crlf "txt of:" ?Lmf)  ;(write-to-string ?Lmf)
   (L2classinstances ?Lmf ?file)
);maybe try the clips version of tr "\t" "," and save the problem of the diffs
 ;i think csv would of already worked for the text version, yes it does
(deffunction csv2classinstances (?file $?skip)
   (bind ?n (first-dflt ?skip 0))
   (bind ?Lmf (csv-map-file_line2list ?file))   ;;Just use file2list if can explode w/o a ,  
   (if (and ?n (numberp ?n) (posativep ?n) (> (length ?Lmf) ?n)) then 
	(bind ?Lmf (delete ?Lmf 1 ?n))) ;skip n lines
   (printout t crlf "csv of:" ?Lmf)  ;(write-to-string ?Lmf)
   (L2classinstances ?Lmf ?file (rest$ ?skip)) ;send class too
)
(deffunction tro () (csv2classinstances rois-out.csv)) ;for dbg
;I could get the ave length of the lines, and (let usr) skip lines noticably shorter--
;-easier to just make class file for each type by hand, then compare filename w/class-types (maybe classes.*)
(deffunction parse-alltxt2ins ($?filt)
  (map1 csv2classinstances (txt-files ?filt))
)
;---
(deffunction remove-last (?l)
 (if (numberp (bind ?n (length ?l))) then ;or just full$
  (delete$ ?l ?n ?n) else ?l)
)
(deffunction shuffle-w (?t ?l)
  (if (full$ ?l) then (create$ (first ?l) ?t (shuffle-w ?t (rest$ ?l)))
   else ?l))
;CLIPS> (shuffle-w , (create$ a b c))
;(a , b , c ,)
(deffunction splay-w (?t ?l)
 (remove-last (shuffle-w ?t ?l))
)
;CLIPS> (splay-w "," (create$ a b c)) (a "," b "," c)
(deffunction csv-implode (?l)
  "implode after splay-w ,"
  (implode$ (splay-w , ?l))
)
;for-R
(deffunction c (?l)
  "create a c(1,2,3) str for R"
 (str-cat "c(" (csv-implode ?l) ")")
)

;---
;--consider checking for class file & not writing it if it is already there.--
;deffunction csv-implode (?l)  "return a str w/,s in it"
;deffunction classinstances2csv (?class $?file)
; (bind ?csvfile (first-dflt ?file (str-cat ?class ".csv")))
; (bind ?slotnames (class-slots ?class inherit))
; (str2file (csv-implode ?slotnames) ?csvfile)
; now generate for all instances, and append all those strings to this file
; (str2file ?str-of-instances ?csvfile "a")
 
 
;;; List convertion 

 ;maybe change/overload to-str to call these..
 
(defmethod write-to-string ((?str STRING)) 
  (str-cat "\"" ?str "\"")) 
 
(defmethod write-to-string ((?obj SYMBOL NUMBER)) 
  (str-cat ?obj)) 
 
(defmethod write-to-string ((?lst SYMBOL (or (eq ?lst nil) 
					     (eq ?lst [nil]) ;new
					     (eq ?lst NIL))) ?dummy) 
  "nil") 
 
(defmethod write-to-string ((?lst LIST) ?dummy-arg) 
  (if (null ?lst) then 
    "" 
    else 
    (bind ?result (write-to-string (car ?lst))) 
    (bind ?i 1) 
    (while (<= (bind ?i (+ ?i 1)) (length ?lst)) 
      (bind ?ifd (send ?lst nth ?i)) 
      (bind ?result  
	    (str-cat  
	     ?result " " (write-to-string ?ifd))) 
      ) 
    ?result 
    )) 
 
(defmethod write-to-string ((?lst LIST)) 
  (str-cat "(" (write-to-string ?lst nil) ")")) 
 
(defmethod write-to-string ((?obj (instancep ?obj))) 
  (str-cat "[" (instance-name ?obj) "]")) 
 
;;; Misc. func. 
 
(deffunction str-strip (?str ?trimchar) 
  (bind ?str-len (length ?str)) 
  (if (bind ?q-idx (str-index ?trimchar ?str)) then 
    (str-cat  
     (sub-string 1 (- ?q-idx 1) ?str) 
     (str-strip (sub-string (+ ?q-idx 1) ?str-len ?str) ?trimchar)) 
    else 
    ?str)) ;CLIPS> (str-strip "  one two    " " ") "onetwo"

(deffunction trim (?str)
 "does more than trim, takes space in ceter now too"
  (str-strip ?str " ") 
)

(deffunction str-strip-quote (?str) 
  (bind ?str-len (length ?str)) 
  (if (bind ?q-idx (str-index "\"" ?str)) then 
    (str-cat  
     (sub-string 1 (- ?q-idx 1) ?str) 
     (str-strip-quote (sub-string (+ ?q-idx 1) ?str-len ?str))) 
    else 
    ?str)) 

(deffunction to-str ($?args) 
  (str-strip-quote (implode$ ?args)))

;(deffunction qis- (?str) (qis (str-strip-quote ?str)))
(deffunction qis- (?ps)
 "quote if a string"
 (if (stringp ?ps) then (str-cat "\"" (str-strip-quote ?ps) "\"") else ?ps)
)
;--SAVE-INS2--------------------------------------------------
;deffunction print-ins2 (?ins ?fn)
(deffunction save-ins2 (?ins ?fn)
  "save/append 1instance to a file"
  (open ?fn out "a")
   (if (instancep ?ins) then 
       (bind ?sn (slot-names ?ins))
       (bind ?sv (slot-values ?ins)) 
       (printout out crlf "(" (instance-name ?ins) " of  " (class ?ins) " ")               ;will loose inner quotes4now
       (progn$ (?v ?sv) (if (full ?v) then (printout out crlf " (" (nth$ ?v-index ?sn) " " (qis- ?v) ")"))) ;no recursion 
       (printout out crlf ")" ) 
        else (printout out "[Warning] save-ins2 got " ?ins crlf))
  (close out))
;deffunction print_ins2 (?ins ?fn)
(deffunction save_ins2 (?ins ?fn)
  "save/append 1 or a list of instances to a file"
  (if (multifieldp ?ins) then (progn$ (?i ?ins) (save-ins2 ?i ?fn)) 
   else (save-ins2 ?ins ?fn)))

 
(deffunction paren-p (?str) 
  (and (stringp ?str) 
       (or (left-paren-p ?str) 
           (right-paren-p ?str))) 
  ) 
 
(deffunction list-gc () 
  (do-for-all-instances 
   ((?list LIST))  
   (and ?list:GCflag (eq (sub-string 1 3 ?list) "gen")) 
  ;(and ?list:GCflag (eq (sub-string 1 3 ?list) "L")) ;try 
   (send ?list delete))) 
 
(deffunction list-set-gc (?lst ?flag) 
  (if (eq (type ?lst) LIST) then 
    (send ?lst set-gc ?flag) 
    (progn$ (?sublst (send ?lst get-elements)) 
	    (list-set-gc ?sublst ?flag) 
	    ) 
    )) 
 
(defmethod copy-list ((?lst LIST)) 
  "Copy top-level structure of a list" 
  ;; Thus conses are shared below the top-level 
  (duplicate-instance ?lst (GCflag TRUE))) 
 
(defmethod copy-list ((?lst SYMBOL (or (eq ?lst nil) 
				       (eq ?lst NIL)))) 
  nil) 
 
(defmethod copy-alist ((?lst LIST)) 
  "Copy first and second level structure of a list" 
  (bind ?dup (duplicate-instance ?lst (GCflag TRUE))) 
  (bind ?nfd 0) 
  (progn$ (?ifd (send ?lst get-elements)) 
	  (bind ?nfd (+ ?nfd 1)) 
	  (if (eq (type ?ifd) LIST) then 
	    (slot-replace$ 
	     ?dup elements ?nfd ?nfd 
	     (duplicate-instance ?ifd (GCflag TRUE)))) 
	  ) 
  ?dup) 
 
(defmethod copy-alist ((?lst SYMBOL (or (eq ?lst nil) 
					(eq ?lst NIL)))) 
  nil) 
 
(defmethod copy-tree ((?lst LIST)) 
  "Copy a tree of conses" 
  (bind ?dup (duplicate-instance ?lst (GCflag TRUE))) 
  (bind ?nfd 0) 
  (progn$ (?ifd (send ?lst get-elements)) 
	  (bind ?nfd (+ ?nfd 1)) 
	  (if (eq (type ?ifd) LIST) then 
	    (slot-replace$ 
	     ?dup elements ?nfd ?nfd 
	     (copy-tree ?ifd))) 
	  ) 
  ?dup) 
 
(defmethod copy-tree ((?lst SYMBOL (or (eq ?lst nil) 
				       (eq ?lst NIL)))) 
  nil) 
 
(defmethod delete-list ((?lst LIST)) 
  (progn$ (?ifd (send ?lst get-elements)) 
	  (if (eq (type ?ifd) LIST) then 
	    (delete-list ?ifd)) 
	  ) 
  (send ?lst delete)) 
 
(defmethod delete-list ((?lst SYMBOL (or (eq ?lst nil) 
					 (eq ?lst NIL)))) 
  nil) 
 
(defmethod eval-name ((?item NUMBER SYMBOL)) 
  ?item) 
 
(defmethod eval-name ((?item STRING)) 
  (str-cat "\"" ?item "\"")) 
 
(defmethod eval-name ((?item INSTANCE-ADDRESS)) 
  (str-cat "(instance-address [" (instance-name ?item) "])")) 
 
(defmethod eval-name ((?item INSTANCE-NAME)) 
  (str-cat "(instance-address [" ?item "])")) 
 
(defmethod forms-string ((?lst LIST) ?dummy) 
  "make a string of forms that is ready to the eval function." 
  (if (null ?lst) then 
    "nil" 
    else 
    (bind ?str "") 
    (progn$ (?elm (send ?lst get-elements)) 
	    (bind ?str 
		  (str-cat ?str  
			   (if (> ?elm-index 1) then " " else "") 
			   (eval-name ?elm) 
			   ))) 
    )) 
 
(defmethod forms-string ((?lst LIST)) 
  (str-cat "(" (forms-string ?lst nil) ")")) 
 
;;; Messages for the LIST class 
 
;;  Input conversion 
 
(defmessage-handler LIST set-gc (?bool) 
  (bind ?self:GCflag ?bool) 
  (progn$ (?mbr ?self:elements) 
	  (if (eq (type ?mbr) LIST) then 
	    (send ?mbr set-gc ?bool)) 
	  ) 
  ) 
 
(defmessage-handler LIST make-list ($?lst) 
  (bind ?self:elements (make-list ?lst FALSE))) 
 
(defmessage-handler LIST non-processed-put ($?lst) 
  (bind ?self:elements ?lst)) 
 
(defmessage-handler LIST length () 
  (length$ ?self:elements)) 
 
(defmessage-handler LIST null () 
  (= (length$ ?self:elements) 0)) 
 
(defmessage-handler LIST car () 
  (if (> (length$ ?self:elements) 0) then 
    (nth$ 1 ?self:elements) 
    else 
    nil)) 
 
(defmessage-handler LIST pop () 
  (if (> (length$ ?self:elements) 0) then 
    (bind ?fst (nth$ 1 ?self:elements)) 
    (slot-direct-delete$ elements 1 1) 
    ?fst 
    else 
    nil) 
  ) 
 
(defmessage-handler LIST cdr () 
  (if (= (length$ ?self:elements) 0) then 
    nil 
    else 
    ;(instance-address  
     (duplicate-instance  
      ?self 
      (elements (rest$ ?self:elements))) 
    ;) ;try
)) 
 
(defmessage-handler LIST nth (?ith) 
  "index starts from 1 for consistency in CLIPS" 
  (nth$ ?ith ?self:elements)) 
 
(defmessage-handler LIST cons (?list) 
  ;(instance-address 
   (duplicate-instance  
    ?self 
    (elements (insert$ ?self:elements 1 ?list))) 
  ;) ;try
) 
 
(defmessage-handler LIST push (?list) 
  (slot-direct-insert$ elements 1 ?list) 
  ;(instance-address ?self)
?self) ;try 
 
(defmessage-handler LIST list ($?list) 
  (list ?self (expand$ ?list))) 
 
(defmessage-handler LIST consp () 
  (not (send ?self null))) 
 
(defmessage-handler LIST append ($?list) 
  (append ?self (expand$ ?list))) 
 
(defmessage-handler LIST set-field (?i ?value) 
  (slot-direct-replace$ elements ?i ?i ?value) 
  ;(instance-address ?self)
?self) ;try 
 
(defmessage-handler LIST show () 
  (write-to-string ?self)) 
 
(defmessage-handler LIST list-print () 
  (write-to-string ?self)) 
 
(defmessage-handler LIST list-copy () 
  (bind ?dup (duplicate-instance ?self (GCflag TRUE))) 
  (bind ?nfd 0) 
  (progn$ (?ifd ?self:elements) 
	  (bind ?nfd (+ ?nfd 1)) 
	  (if (eq (type ?ifd) LIST) then 
	    (slot-replace$ 
	     ?dup elements ?nfd ?nfd (send ?ifd list-copy))) 
	  ) 
  ?dup) 
 
(defmessage-handler LIST list-delete () 
  (progn$ (?ifd ?self:elements) 
	  (if (eq (type ?ifd) LIST) then 
	    (send ?ifd list-delete)) 
	  ) 
  (send ?self delete)) 
 
(defmessage-handler LIST get-members () 
  "For backward compatibility." 
  ?self:elements) 
 
(defmessage-handler LIST print ()
  (write-to-string ?self))

(deffunction pg (?n)
  (send (symbol-to-instance-name (sym-cat "gen" ?n )) print)
)
;-alg    ;[maybe km next?]
(defglobal ?*kb-path* = "\"/Users/bobak/wrk/sci/cb/calbay-my.pprj\"")
(defglobal ?*kb-cnct* = "(tell ((:OR ((:IS-CLASS Experiments)) ;this test should work for all of them
         ((:USE-KB :KB1 Protege  \"/Users/bobak/wrk/sci/cb/calbay-my.pprj\")))))"
)
(deffunction pa-vcn (?class ?vn ?in $?strm)
  "alg print: start_ins: class varname insname" ;oft call w/vn=in
  (bind ?t (first-dflt ?strm crlf))
  (printout ?t t "(:add-instance (?" ?vn " " ?class ") (:name ?" ?vn " \"" ?in "\")")
)
(deffunction pa-svv (?sn ?val  ?vn $?strm)
  "alg print: slotval: Slotname Value  Var" ;call w/map2
  (bind ?t (first-dflt ?strm crlf)) ;might have to send type info in too, so to know when to quote it
  (printout ?t t "(" ?sn "  ?" ?vn " " ?val ")")
)
(defmessage-handler USER alg ($?strm)
  "output instance in algernon format"
  (bind ?t (first-dflt ?strm crlf))
  (bind ?in (instance-name-to-symbol (instance-name ?self)))
  (pa-vcn (class ?self) ?in ?in ?t)
  (map2 pa-svv (slot-names ?self) (map1 write-to-string (slot-values ?self)) ?in ?t) 
  (printout ?t t ")")   ;reminds me of a data-fnc
)
(deffunction alg1 (?i $?args)
  (send ?i alg)
)
(deffunction alg (?i ?strm)
  (send ?i alg ?strm)
)
;if above was a method I wouldn't need diff names
(deffunction alg-class (?cls $?args)
  (bind ?fn (first-dflt ?args ?cls))
  (bind ?fn (ending .lsp ?fn))
  (open ?fn strm "w")
  (printout strm t ?*kb-cnct*)
  (printout strm t "(tell (")
  (map-class alg ?cls strm)
  (printout strm t "))")
  (close strm)
)
(deffunction ins2alg-class (?cls $?args)
 (li ?cls)
 (alg-class ?cls ?args)
) ;then can map over a list of classes to put from sqlDSN output into protege-kb via algernon
;-or
(deffunction li2alg-all (?p) ;a clu4 starting pnt
  (printout t crlf "loadling all ins in:" ?p ",gives:" (li-all ?p) crlf)
  (printout t crlf "now create alg files from all these files:" (ls-ins ?p) "'s ins" crlf)
  (map1 alg-class (ls-ins-base ?p))
) ;in ide ?p=calbay should do it on the mac
;-hdf
(deffunction dir2hdf (?dir)
  "all txt files in a dir go to a dir.hdf file, ret list of files imported"
  (sys2l (str-cat "h5import -f " ?dir ".hdf " ?dir "\/*.txt"))
)
;-database
;sqlite3
(deffunction sq2 ($?args)
 "used by sqlite fncs"
  (system$ (str-cat sqlite3 " " (to-str $?args)))
)
(deffunction sq2tables (?db)
 "get sqlite tables in a db"
  (sq2 ?db " .table")
)
(deffunction sq2schema (?db)
 "get sqlite schema in a db"
  (sq2 ?db " .schema") ;could send in tables too
);CLIPS> (sq2schema test.db)
;("CREATE TABLE t1 (t1key INTEGER PRIMARY KEY,data TEXT,num double,timeEnter DATE );")
;use to make defclass, but will all be flat, psql would be more interesting ;protege easier

(deffunction sq2dump (?db)
 "get sqlite dump of a db"
  (sq2 ?db " .dump")
)
(deffunction sq2selectall (?db ?table)
 ;(sq2 ?db  (str-cat " \"select * from " (to-str ?table) "\""))
 ;(sq2 ?db  (format nil " \"select * from %s \"" (to-str ?table)))
  (sys1$  (format nil "sqlite3 %s \"select * from `%s`\""  ?db ?table))
);get rid of str-strip-quote call if it touches the added quotes, or just try format
;system$ still had to-str which did the stripping
;        CLIPS (V6.21 06/15/03)
;CLIPS>  (sq2selectall test.db t1)
;("1|This is sample data|3|" "2|More sample data|6|" "3|And a little more|9|")
;
(deffunction sq2insall (?db ?ins)
  (bind ?sns (csv-implode (slot-names ?ins)))
  (bind ?svs (csv-implode (slot-values ?ins)))
  (sys1$  (format nil "sqlite3 %s \"insert (%s) values (%s);\""  ?db ?sns ?svs))
)
;sqlite3 test.db  "insert into t1 (data,num) values ('This is sample data',3);"
;------------------------------------------------------------------
(deffunction sqlcmd-ins2insert (?db ?ins)
  (bind ?sns (csv-implode (slot-names ?ins)))
  (bind ?svs (csv-implode (slot-values ?ins)))
  (sys1$  (format nil "sqlcmd /DB %s /COMMAND \"insert into %s (%s) values (%s);\""  
		?db (class ?ins) ?sns ?svs))
)
(deffunction ins2insert (?db ?ins)
  (bind ?sns (csv-implode (slot-names ?ins)))
  (bind ?svs (csv-implode (slot-values ?ins)))
  (sys1$ (format nil "sqlDSN  %s  \"insert into %s (%s) values (%s);\""  ?db (class ?ins) ?sns ?svs))
)
(deffunction ins2sql (?db ?ins)
  (bind ?sns (csv-implode (slot-names ?ins)))
  (bind ?svs (csv-implode (slot-values ?ins)))
  (sys1$  
   (format nil "java sqlDSN  %s  \"insert into %s (%s) values (%s);\""  ?db (class ?ins) ?sns ?svs))
)
;-
(deffunction lif2 (?file ?filend) 
 "li if; load-instances if the file is there"
  (if (full$ (ls$ ?filend)) then ;same as file-existp
	  (li ?file)
   else (printout t crlf "ins-file:" ?filend ":missing" crlf))
)
;-
;will want a update version of this (will I need to know which slots have changed)?
; would I have this as a sort of after for a set-slot-value?
;-need MIN/MAX/AVE/STDEV/COUNT -related to diff WHERE w/cutoffs;faster than doing w/in clips or not?
(deffunction sqlDSNins (?db ?cmd $?files) ;if a file keep it around
  (bind ?file (first-dflt ?files (to-str (gensym) ".ins")))
  (bind ?sed (second-dflt ?files ""))
  (bind ?sed (to-str ?sed))
  (if (> (length ?sed) 2) then 
      (bind ?sed (str-cat "|sed -f " ?sed))
      (printout t crlf "postprocess w/:" ?sed crlf))
  (bind ?filend (ending ".ins" ?file))
  (printout t crlf "will archive to:" ?file crlf)
  (printout t crlf "Opening DSN:" ?db " to run query:" ?cmd crlf)
  ;could keep tmp option, but not for now
  ;(sys1$ (str-cat "java sqlDSN " ?db " \"" ?cmd "\"")) ;finish/fix
  (bind ?eg  "|egrep -v \"Exception|java|jdbc|sqlDSN\"") 
  (bind ?sys (str-cat "java sqlDSN " ?db " \"" ?cmd "\"" ?sed ?eg "|more>" ?filend)) 
  (printout t crlf "system:" ?sys crlf)
  ;(system$  ?sys)
  (system  ?sys)
  (lif2 ?file ?filend)
)
(deffunction sqlDSNclass-en (?db ?class ?e ?n) ;if a file keep it around
  "get rois instances w/expert and img-step#"
  (bind ?cmd 
    (format nil "select * from %s where experiment_name='%s' and image_set_number=%d" ?class ?e ?n))
  (printout t crlf "formattedSqlCmd:" ?cmd crlf)
  (sqlDSNins ?db ?cmd (sym-cat ?class _ ?e _ ?n) (sym-cat ?class .sed))
)
(deffunction sqlDSNrois-en (?db ?e ?n) ;if a file keep it around
  "get rois instances w/expert and img-step#"
  (bind ?cmd 
    (format nil "select * from rois where experiment_name='%s' and image_set_number=%d" ?e ?n))
  (printout t crlf "formattedSqlCmd:" ?cmd crlf)
  (sqlDSNins ?db ?cmd (sym-cat ?e _ ?n) rnum.sed)
)
(deffunction sqlDSNrois-v (?e ?n) ;if a file keep it around
 (sqlDSNrois-en mysql:VSOMExperimentDB ?e ?n)
)
;(deffunction sqlDSNrois-en (?db ?e ?n) 
; (sqlDSNclass-en ?db rois ?e ?n)) 
(deffunction sqlDSNicsHeader-en (?db ?e ?n) 
 (sqlDSNclass-en ?db icsHeader ?e ?n)) 
;------------------------------------------------------------------
;-------------------------------------old---EOF before array code:
;------------------------------------------------------------------
;----this will be moved to permanent scs-utils.clp in a bit--------
(defgeneric pprint) 
;get a bunch of gf-s to take anything to almost anything

;I'd like a 
(defgeneric coerce)
(defmethod coerce ((?ins USER) ?type)
  (switch ?type
    (case symbol then (instance-name ?ins))
    (case instance then ?ins)
    (default (progn (printout "bad type:" ?type) ?ins))
  )
)
;neither method seems to be working yet/fix
(defmethod coerce ((?sym SYMBOL) ?type)
  (switch ?type
    (case symbol then ?sym)
    (default (progn (printout "bad type:" ?type) ?sym))
  )
)

;tmp utils to be moved to scs-util.clp,  clips is in need (KnWorks would be nice:)
;(deffunction null$ (?lv)
;  (or (eq ?lv nil) (and (multifieldp ?lv) (= (length ?lv) 0))))

;(deffunction full$ (?lv)
;  (if (multifieldp ?lv) then (> (length ?lv) 0) else (neq ?lv nil)))

(deffunction first-non-nil$ (?l)
  "find first non nil in a list"
  (loop-for-count (?i 1 (length$ ?l)) do 
    (if (full$ (nth$ ?i ?l)) then (return (nth$ ?i ?l)))))

(deffunction collect-mask$ (?l ?mask)
  (bind ?ret (create$))
  (loop-for-count (?i 1 (length$ ?l)) do 
    (if (nth$ ?i ?mask) then (bind ?ret (append ?ret (nth$ ?i ?l)))))
?ret)

(deffunction to-inst-name (?s)
  (sym-to-ins ?s)
  ;(if (not (instance-namep ?s)) then (sym-to-ins ?s) else ?s)
)

(deffunction ins-to-sym (?i)
  (if (instancep ?i) then (bind ?i (instance-name ?i)))
  (if (instance-namep ?i) then (bind ?i (instance-name-to-symbol ?i)))
?i)

;(deffunction explode-str$ (?str)
;  "take each char of a str to a list, opposite fnc is sym-cat"
; (bind ?l (create$))
; (loop-for-count (?i 1 (length ?str)) do 
;  (bind ?l (append ?l (sub-string ?i ?i ?str))))
;?l)

(deffunction implode (?l)
  (if (multifieldp ?l) then (implode$ ?l)
   else (to-str ?l)))

(deffunction impl (?l) 
  (if (full$ ?l) then (funcall sym-cat ?l)
   else " "))

;(deffunction implode (?l) ;def above
;  "like implode$"
; (funcall sym-cat ?l)):wq
;-------

;-----for numeric lists
(deffunction sum$ (?l)
 (if (= (length ?l) 1) then (return (first$ ?l))
  else 
 ;;(+ (first$ ?l) (sum$ (rest$ ?l)))
 ;(funcall + (first$ ?l) (sum$ (rest$ ?l)))
  (funcall$ + ?l)
))

(deffunction ave$ (?l)
 (/ (sum$ ?l) (* 1.0 (length ?l))))

(deffunction ave ($?ll)
  (ave$ $?ll))

(deffunction sqdiff (?i ?a)
 (** (- ?i ?a) 2)
)
(deffunction stdev$ (?l)
  (bind ?a (ave$ ?l))
  (sqrt (ave$ (map1 sqdiff ?l ?a)))
)

(deffunction inv1 (?n)
 "1st cut @a fzy not"
  (- 1 -?n))
;-------
(defglobal ?*number-p* =
  (create$ 1 2 3 4 5 6 7 8 9 0 "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
(deffunction number-p (?n)
 (member$ ?n ?*number-p*))
(deffunction nnumberp (?n) (not (numberp ?n)))
(deffunction nnumber-p (?n) (not (number-p ?n)))

(deffunction numeric-only (?s)
  (eval (impl (collect-if  number-p (explode-str$ ?s)))))
(deffunction non-numeric-only (?s)
  (eval (impl (collect-if nnumber-p (explode-str$ ?s)))))
;------- 

;-rewrite generic slot, so if not a multislot, take the list/val to a multifield
(deffunction slot-append2 (?ins ?slot ?a)
  "slot append, if not a mulislot, so will make a mf, w/the value" ;test this
  (if (full$ (slot-cardinality (class ?ins) ?slot)) then
    (slot-append ?ins ?slot ?a)
   else
    (slot-put-value ?ins ?slot (append ?a (create$ (slot-value ?ins ?slot))))))

(deffunction slot-append-new (?ins ?slot ?a)
  (if (not (member$ ?a (slot-value ?ins ?slot))) then 
    (slot-append ?ins ?slot ?a)))

(deffunction slot-append-new2 (?ins ?slot ?a)
  (if (not (smember$ ?a (slot-value ?ins ?slot))) then 
    (slot-append2 ?ins ?slot ?a)))

(deffunction count-instances (?class)
  "number from that class"  ;inherits?
  (bind ?count 0)
  (do-for-all-instances ((?ins ?class)) TRUE
    (bind ?count (+ ?count 1)))
?count)
;or
  ;(length (find-all-instances ((?ins ?class)) TRUE))

; Six new functions similar to the instance set query functions
;have been added for determining and performing actions on sets of facts that satisfy
;user-defined queries (see section 12.9.12): any-factp, find-fact, find-all-facts, do-for-fact,
;do-for-all-facts, and delayed-do-for-all-facts. The GetNextFactInTemplate
;-might be even more useful w/fuzzyclips, as i'd use facts there, till wraped in objs

;-----------------------------------FIND$
(deffunction find$ (?it ?in)
  "find position of lexeme in another, or elt in multifiled, or T/F if subset"
  (if (multifieldp ?in) then 
    (if (multifieldp ?it) then (subsetp ?it ?in)
     else (member$ ?it ?in))
   else (str-index (to-str ?it) (to-str ?in))))

(deffunction find-in-2nd (?a ?l1 ?l2)  ;optional default?
  "if find a in l1 then return assoc elt in l2"
  (if (bind ?position (find$ ?a ?l1)) then (nth$ ?position ?l2)))

;------------------------------------------------------------------
(defclass bcnum  ;bc - An arbitrary precision calculator language
  (is-a USER)
  (role concrete)
 (slot value (create-accessor read-write))
 ;(multislot values (create-accessor read-write))
)
;------------------------------------------------------------------
(defclass scs-netdata
 "base class for scs-netdata.clp"
  (is-a USER)
  (role abstract)
) 
;------------------------------------------------------------------
;--list that you can embed in a mf, or other sub-list
(defclass sub-list  ;Use list code above instead
 "a 'list' you can put in a multifield w/out it dissapearing"
  (is-a scs-netdata)   ;(is-a shadwell-scs-netdata)
  (role concrete)
 (multislot values (create-accessor read-write)))

;-not really using array yet, was for final by cmpt,valtype channel array, but
; can get this by doing a for-all-instances and printing out an ordered slotval
(defclass array  
 "sub-list used as n?dim array, abstract"
  (is-a sub-list)   
  (role abstract) ;why?
)
;--The 2DA array varient seems easiest to use (less instances)&can see in a way
;--2d array, is a sub-list of sub-lists  (want inst rather than mf to start w/)
; --would be nice to have n-dim arrays, but will use this the most,& can have an
;  ndim array buit of n-1 dim arrays  (maybe)
(defclass 2d-array  ;just to have special methods on
 "sub-list filled w/sub-list instances"
  (is-a array)     ;w/values filled w/sub-lists
  (role concrete)
  (slot rows (create-accessor read-write))
  (slot cols (create-accessor read-write))
) ;num-row is length of it's values; num-cols is length of it's sublist's values

(defclass 2da-array  
 "sub-list filled accessable as a 2d array"
  (is-a array)   
  (role concrete)
  (slot rows (create-accessor read-write))
  (slot cols (create-accessor read-write))
)

(deffunction mk-2d (?name ?rows ?cols ?fill-item)
  "make 2d-array inst &n'rows' of sub-list instances in it, all w/fill-item in"
 (bind ?a (make-instance (sym-cat ?name ?rows "_" ?cols) of 2d-array 
            (rows ?rows) (cols ?cols)))
 ;put rest in an init after
 (loop-for-count (?i 1 ?rows) do 
   (slot-append ?a values 
     (bind ?r (make-instance (sym-cat ?name "row" ?i) of sub-list)))
   (loop-for-count (?c 1 ?cols) do (slot-append ?r values ?fill-item))
 )
?a)

(deffunction mk-2da (?name ?rows ?cols ?fill-item)
 "2d array varient constructor"
 (bind ?a (make-instance (sym-cat ?name ?rows "_" ?cols) of 2da-array 
            (rows ?rows) (cols ?cols)))
 ;put rest in an init after
 (loop-for-count (?i 1 (* ?rows ?cols)) do (slot-append ?a values ?fill-item))
?a)

(deffunction print-2d (?ins)
  (print-ins ?ins)
  (bind ?rows (send ?ins get-values))  ;mf of sub-lists for every row
  (printout t crlf "rows=" ?rows crlf) ;debug
  (loop-for-count (?r 1 (send ?ins get-rows)) do  
    (bind ?row (nth$ ?r ?rows))  ;sub-list w/'values' for that row
    (printout t crlf "r=" ?r " row=" ?row) ;debug
    (bind ?rvals (send ?row get-values))
    (printout t crlf "[")
    (loop-for-count (?c 1 (send ?ins get-cols)) do 
      (printout t " " (nth$ ?c ?rvals)))
    (printout t "]")                ;could use aref, but this is more efficient
))

(deffunction print-2da (?ins)
 "print out any 2d array instance"
  (print-ins ?ins)
  (bind ?cols (send ?ins get-cols))
  (bind ?values (send ?ins get-values))
  (loop-for-count (?r 1 (send ?ins get-rows)) do  
    (printout t crlf "[")
    (loop-for-count (?c 1 (send ?ins get-cols)) do 
      (printout t " " (nth$ (+ ?c (* (- ?r 1) ?cols)) ?values)))
    (printout t "]")) 
  (printout t crlf " ")
)

(defmethod pprint ((?ins 2d-array))
  (print-2d ?ins))
(defmethod pprint ((?ins 2da-array))
  (print-2da ?ins))

;-should turn into handler to print by row, so can use for spreadsheet too
; and have before putting the name out, 
;print-spreadsheet would just print col-names before doing a full print-2da
;-or could just write it this time..but better to use oop in general

;-write recursive print-ins or print-ins-2depth & use for print meth on 2d-array
; might have to play w/print mh, or have slot-values &a find instance-existp
;-easy enough to just iterate through for a print

;---------annotated 2d array
(defclass spreadsheet
 "2d-array w/names slot for col-names"
  (is-a 2da-array)   ;w/col-names in names slot
  (role concrete)
 ;(multislot col-names (create-accessor read-write))
 ;(multislot row-names (create-accessor read-write)) ;or just have in values
 (slot col-names (create-accessor read-write))
 (slot row-names (create-accessor read-write)) 
)  ;aref by row/col # or by names  (intermix?)

(deffunction mk-spreadsheet (?name ?rows ?cols ?fill-item)
 "like mk-2d but 2nd arg are the col-'names'"
 (bind ?i (mk-2da ?name (length ?rows) (length ?cols) ?fill-item))   ;cols should = length names
 (send ?i put-col-names ?cols)
 (send ?i put-row-names ?rows)
?i)
;----------

(defmessage-handler 2d-array aref (?row ?col)
  "ref 2d array by int row/col"
  (bind ?r (nth$ ?row (send ?self get-values)))
  (nth$ ?col (send ?r get-values)))

(defmessage-handler 2d-array set-aref (?row ?col ?val)
  "set 2d array by int row/col, val is last"
  (bind ?r (nth$ ?row (send ?self get-values)))
  (slot-replace ?self values ?col ?col ?val) ;wrong, replaces whole row inst;fix
 ;(slot-direct-replace$ ?col ?col ?val)
)  ;look at slot-replace  & fix above  ;using 2da anyway

(defmessage-handler 2da-array aref (?row ?col)
  "set ref 2da array by int row/col"
 ;(nth$ (+ ?col (* (- ?row 1) ?self:cols)) ?self:values)
  (nth$ (+ ?col (* (- ?row 1) (send ?self get-cols))) (send ?self get-values))
)

(defmessage-handler 2da-array set-aref (?row ?col ?val)
  "ref 2da array by int row/col"
  (bind ?position (+ ?col (* (- ?row 1) ?self:cols)))
  (printout t crlf "o=" (* (- ?row 1) ?self:cols) " p=" ?position crlf) ;debug
  (slot-replace ?self values ?position ?position ?val))

;----------------array generic-fncs should work for any array
(defgeneric aref)
(defgeneric set-aref)

;---specified only 2d so far (even works w/variend, as it passes to handler)
(defmethod aref ((?a array) (?row INTEGER) (?col INTEGER))
  "ref a 2d array by row/col using integers"
  (send ?a aref ?row ?col))

(defmethod set-aref ((?a array) (?row INTEGER) (?col INTEGER) ?val)
  "ref a 2d array by row/col using integers"
  (send ?a set-aref ?row ?col ?val))

(deffunction position$ (?v ?l)
  "like ae fnc"
 (find$ ?v ?l))

(defmethod set-aref ((?s spreadsheet) (?row LEXEME) (?col LEXEME) ?val)
  "set array/ss value via row & col names"
  (bind ?r (position$ ?row (send ?s get-rows)))
  (bind ?c (position$ ?col (send ?s get-cols)))
  (set-aref ?s ?r ?c ?val))

;-----------------------------------------------------------------ALIST/ASSOC
;--association lists  (easier than having a mf of small sub-lists)
(defclass alist
  (is-a sub-list)
  (role concrete)
 (multislot keys (create-accessor read-write)))

(defmessage-handler alist assoc primary (?item)
  (find-in-2nd ?item ?self:keys (send ?self get-values)))  
     ;used to be self, but inherits from sub-list now, strange?

(defmessage-handler alist mk-assoc primary (?key ?val)
  (slot-append ?self keys ?key)
  (slot-append ?self values ?val))

(defgeneric assoc)
(defgeneric mk-assoc)

(defmethod assoc (?item (?alst alist))
  (send ?alst assoc ?item))

(defmethod assoc (?item (?l MULTIFIELD))
  (printout t crlf "[WARN can't get " ?item " from " ?l "]")) 
 
(defmethod assoc ((?l MULTIFIELD) (?alst alist))
  (map1 assoc ?l ?alst)  ;or apply-r 
)

(defmethod mk-assoc (?key ?val (?alst alist))
  ;(printout t crlf "key=" ?key " val=" ?val " alst=" ?alst crlf) ;debug
  (send ?alst mk-assoc ?key ?val))

(deffunction mk-alist (?name) 
  (make-instance (sym-cat ?name) of alist))
;------------------------------------------------------------use list for a jess bag
;(deffunction delete-ins (?name)
;    (send (s2i ?name) delete-instance))
(deffunction bag-create (?name)
  (mk-alist ?name) ;(make-instance ?name of assoc)
)
(deffunction bag-set (?name ?prop ?val)
  (mk-assoc ?prop ?val (s2i ?name))
)
(deffunction bag-get (?name ?prop)
    (assoc ?prop (s2i ?name)))
(deffunction bag-delete (?name)
;  (delete-ins ?name)
)
;--------------------------------------------------------------EOF
(deffunction tlst-create (?n)
 (make-instance ?n of LIST)  ;(bag-create clos-tax)
)
(deffunction tlst-delete (?n)
;  (delete-ins ?n)
)
(deffunction tlst-set (?name ?val)
  (nconc (s2i ?name) ?val))
(deffunction tlst-get (?name ?val)
  (member ?val (s2i ?name) ))

(deffunction rr1 () (reset) (run 1))
;-----------------------------------------EOF
;(defglobal ?*R-DEVICE_PATH* = "\/Users\/bobak\/Documents\/downloads\/ai\/prot\/rdf\/R-DEVICE\/")
(defglobal ?*R-DEVICE_PATH* = "\/home\/bobak\/bin\/csd.auth.gr\/R-DEVICE\/")
(deffunction loadr-device ()
  "r-device rdf code loading"
 (load* (str-cat ?*R-DEVICE_PATH* "rdf.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "classes.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "auxiliary-functions.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "class-functions.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "aggregates.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "types.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "oo-querying.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "second-order.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "stratification.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "translation.CLP")) 
 (load* (str-cat ?*R-DEVICE_PATH* "translation-rules.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "main.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "rdf-auxiliary.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "load-rdf.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "import.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "export.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "triple-transformation.clp"))
 (reset)
) 
;(defglobal ?*o-DEVICE_PATH* = "\/home\/bobak\/bin\/csd.auth.gr\/o-device\/")
;--------------------------------------------------------------c-trans.clp
;here are some utils to move into gen utils:
;; getNumber  - requests a number in a given range
;;
;; ask-question - asks a question and gets answer from provided
;;                allowed responses
;;
;; yes-or-no-p - requests a yes or no answer to given question
;;               (uses ask-question)
;-
;Add a ask-numbered-question, that numbers the choices, & takes either the
; (choice or?) the number for it; if just num, just put names in str.
;Better (getNumber "1.channel-list 2.data 3.millivolts 4.quit" 1 4)
;to-have(getNumberedChoice $?choices) & construct the above.

(deffunction getNumber (?question ?from ?to)
  (printout t ?question " [" ?from " to " ?to "]: ")
  (bind ?answer (read))
  (while (not (and (numberp ?answer)
                   (>= ?answer ?from)
                   (<= ?answer ?to)
              )
         )
    do
     (printout t "Please enter a numeric answer within the specified range" crlf

                  ?question " [" ?from " to " ?to "]: ")
      (bind ?answer (read))
  )
  ?answer
)

(deffunction getNumberedChoice ($?choices)
  "enter all the choices and it turns into a nubered list for getNumber"
;I'd consider using map & interleave a gen number list, but can do this
  (printout t crlf)   ;this doesn't print to a str, but can be ok
  (loop-for-count (?i 1 (length$ ?choices))
     (printout t ?i "." (nth$ ?i ?choices))) ;err was ?1 &nth
  (getNumber ":" 1 (length$ ?choices)))

(deffunction ask-question (?question $?allowed-values)
   (printout t ?question)
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?question)
      (bind ?answer (read))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer))))
   ?answer)


(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then TRUE
       else FALSE))
;(defglobal ?*env* = (system$ env))  ;might want to updae this, so write a fnc (fix/finish)
(defglobal ?*env* = FALSE)  ;might want to updae this, so write a fnc (fix/finish)
(deffunction echo$ (?var)
 "given env var name, get a value string"
 (if (not ?*env*) then (bind ?*env* (system$ env)))
 (filter$ ?*env* (str-cat (upcase ?var) =))
)
(deffunction cmeclp2L ()  (wdf file2lists) (map1 file2lists  ?*cme-clp-files*))
;don't forget the other utile fncs appended to this, or the others in storage
; look@ auxiliary-functions.clp sometime &second-order.clp?
;------------------------------------------------------------
(deffunction mk1subof (?sub ?super)
  (printout t crlf "(defclass " ?sub " (is-a " ?super "))")
)
(deffunction mksubsof (?super ?subs)
  (map1 mk1subof ?subs ?super)
) ;(mksubsof super (create$ sub1 sub2 sub3))
;--------------------------------------------------------------EOF
;http://www.pulp.se/clips/regexp/regexp.clp

;;;; A small regexp library for Clips. Inspired by the first chapter of the
;;;; book 'Beautiful Code' (O'Reilly). Enjoy. - Johan Lindberg (johan@pulp.se)
;;;; 
;;;; 2007-10-04 .. 16   1st release. Implemented match and search
;;;; 2007-11-29 .. 30   2nd release. Implemented findall, split and sub(stitute)
;;;;                    also fixed a bug in search that sometimes caused the
;;;;                    ?string not to be properly scanned (ended to early).
;;;;                    
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation; either version 2 of the License, or (at your option)
;;;; any later version.

(defglobal ?*errors* = t)

(deffunction tokenize-expression (?regexp)
  "tokenize-expression is used to split a regular expression pattern into
   a multifield variable that can be used in the match function."
   
  (bind ?string-index 1)
  (bind ?string-anchor 1)   ; ?string-anchor stores the ?string-index at which
                            ; the last token was matched. It is used to
                            ; retrieve character sequences (literal matches)
                            ; from ?regexp.
                            
  (bind ?tokens (create$))
  
  (while (< ?string-index (+ (str-length ?regexp) 1))
    (bind ?c (sub-string ?string-index ?string-index ?regexp))
    
    ;; Special characters: wildcard, quantifier and anchors
    (if (or (eq ?c ".")     ; matches any character
            
            (eq ?c "*")     ; match the previous token 0..n times
               
            (eq ?c "^")     ; matches the beginning of the string
            (eq ?c "$"))    ; matches the end of the string
     then
       ;; Check that anchors don't appear in strange places
       ;; of the regexp pattern.
       (if (or (and (eq ?c "^")
                    (> ?string-index 1))
               (and (eq ?c "$")
                    (< ?string-index (str-length ?regexp))))
        then
          (printout ?*errors* "; ERROR: Illegal Regular Expression Pattern." crlf)
          (printout ?*errors* "; You cannot use an anchor such as " ?c
                              " in the wrong place in a pattern!" crlf)
          (return nil))
          
       ;; Tokenize the sub string from anchor to index
       (if (< ?string-anchor ?string-index)
        then (bind ?tokens (insert$ ?tokens
                                    (+ (length$ ?tokens) 1)
                                    (sub-string ?string-anchor
                                                (- ?string-index 1)
                                                ?regexp))))
                                                
       ;; Check that quantifier tokens are not used with the
       ;; empty string.
       (if (and (eq ?c "*")
                (eq (length$ ?tokens) 0))
        then
          (printout ?*errors* "; ERROR: Illegal Regular Expression Pattern." crlf)
          (printout ?*errors* "; You cannot apply a quantifier such as " ?c
                              " on an empty string!" crlf)
          (return nil))
          
       ;; Tokenize the wildcard and anchor character
       (bind ?tokens (insert$ ?tokens
                              (+ (length$ ?tokens) 1)
                              (sub-string ?string-index
                                          ?string-index
                                          ?regexp)))
                                          
       (bind ?string-index (+ ?string-index 1))
       (bind ?string-anchor ?string-index)
     else
       (bind ?string-index (+ ?string-index 1))))
       
  ;; Tokenize the sub string from anchor to the end of the string
  (if (< ?string-anchor ?string-index)
   then (bind ?tokens (insert$ ?tokens
                               (+ (length$ ?tokens) 1)
                               (sub-string ?string-anchor
                                           (- ?string-index 1)
                                           ?regexp))))
  (return ?tokens))
  
  
(deffunction match (?pattern ?string)
  "match returns the string that ?pattern matched (in the *beginning* of
   ?string) otherwise it returns nil. If you want to match anywhere
   in ?string use search."
  
  (bind ?regexp (tokenize-expression ?pattern))
  (bind ?string-index 1)
  
  (progn$ (?part ?regexp)
    (bind ?prev-part (nth$ (- ?part-index 1) ?regexp))
    (bind ?next-part (nth$ (+ ?part-index 1) ?regexp))
    (bind ?next-token-index nil)
    
    ;; Match anchors. These are non-consuming (the ?string-index isn't
    ;; incremented).
    (if (and (eq ?part "^")
             (neq ?string-index 1))
     then (return nil))
     
    (if (and (eq ?part "$")
             (neq ?string-index (+ (str-length ?string) 1)))
     then (return nil))
     
    (if (neq ?next-part "*")
     then
       ;; Match a literal character (sequence)
       (if (and (neq ?part "^")
                (neq ?part "$")
                (neq ?part ".")
                (neq ?part "*"))
        then
          (if (neq ?part (sub-string ?string-index
                                     (- (+ (str-length ?part) ?string-index) 1)
                                     ?string))
           then (return nil)
           else (bind ?string-index (+ (str-length ?part) ?string-index))))
          
       ;; Match any character
       (if (eq ?part ".")
        then (bind ?string-index (+ ?string-index 1))))
        
       ;; Match the previous token 0..n times
       (if (eq ?part "*")
        then
          (if (and (neq ?next-part "$")
                   (neq ?next-part ".")
                   (neq ?next-part nil))
           then
             ;; Check for ?next-part in the remaining ?string
             (bind ?str-index (str-index ?next-part
                                         (sub-string ?string-index
                                                     (str-length ?string)
                                                     ?string)))
                                                     
             (if (eq ?str-index FALSE)
              then
                (bind ?next-token-index nil)
                (if (neq (nth$ (+ ?part-index 2) ?regexp) "*")
                 then
                   ;; If we can't find the next token and if it's
                   ;; not quantified the match fails!
                   (return nil))
              else
                (bind ?next-token-index (+ ?str-index ?string-index)))
                
           else
             (if (eq ?next-part ".")
              then (bind ?next-token-index nil) ; this can't be decided here!
              else (bind ?next-token-index (str-length ?string))))
             
             
          (if (neq ?next-token-index nil)
           then
             (if (eq ?prev-part ".")
              then
                ;; If the previous token is a wildcard we'll just increment
                ;; the ?string-index to wherever the next token begins.
                (bind ?string-index (- ?next-token-index 1))
                
              else
                ;; If the previous token is a literal character sequence
                ;; we'll first see if it's possible to fit 0..n pieces of
                ;; that sequence into the match.
                (bind ?match-length (- ?next-token-index ?string-index))
                (if (neq (mod ?match-length (str-length ?next-part)) 0)
                 then
                   ;; If it's not we can fail here!
                   (return nil)
                   
                 else
                   ;; Otherwise we must compare the remaining ?string
                   ;; character sequence by character sequence.
                   (while (< ?string-index ?next-token-index)
                      (if (neq ?prev-part (sub-string ?string-index
                                                      (- (+ (str-length ?prev-part) ?string-index) 1)
                                                      ?string))
                       then
                         (return nil)
                         
                       else
                         (bind ?string-index (+ ?string-index (str-length ?prev-part)))))))
           else
             ;; Match as much as possible (greedy)!
             (bind ?match true)
             (while (eq ?match true)
                (if (neq ?prev-part (sub-string ?string-index
                                               (- (+ (str-length ?prev-part) ?string-index) 1)
                                               ?string))
                 then
                   (bind ?match false)
                 else
                   (bind ?string-index (+ ?string-index (str-length ?prev-part))))))))
                   
  (if (and (neq ?next-token-index nil)
           (< ?string-index ?next-token-index))
   then (return (sub-string 1 ?next-token-index ?string))
   else (return (sub-string 1 (- ?string-index 1) ?string))))
  
  
(deffunction search (?pattern ?string)
  "find and return information about the first occurance of ?pattern in
   ?string. Returns a multifield consisting of start-index, stop-index and
   the string that matched."
  
  (bind ?string-index 1)
  (bind ?match (match ?pattern ?string))    ; search is implemented
                                            ; using match
  (while (and (eq ?match nil)
              (> (str-length ?string) 0))
    (bind ?string-index (+ ?string-index 1))
    (bind ?string (sub-string 2 (str-length ?string) ?string))
    (bind ?match (match ?pattern ?string)))
    
  (if (eq ?match nil)
   then (return (create$ -1 -1 nil))
   else (return (create$ ?string-index
                         (+ ?string-index (str-length ?match) -1)
                         ?match))))
                         
                         
(deffunction findall (?pattern ?string)
  "find and return information about all occurances of ?pattern in ?string.
   Returns a multifield of imploded "multifields" where each value consists
   of start-index, stop-index and the string that matched."
  
  (bind ?result (create$))
  (bind ?string-index 0)
  (while true
    (bind ?part (search ?pattern ?string))  ; findall is implemented
                                            ; using search
    (if (neq (nth$ 3 ?part) nil)
     then
       (bind ?string (sub-string (+ (nth$ 2 ?part) 1)    ; keep everything in
                                 (str-length ?string)    ; ?string after the
                                 ?string))               ; last match.
                                 
        ;; add ?string-index to begin- and end-index for this match.
        (bind ?part (replace$ ?part 1 1 (+ ?string-index (nth$ 1 ?part))))
        (bind ?part (replace$ ?part 2 2 (+ ?string-index (nth$ 2 ?part))))
        
        (bind ?string-index (nth$ 2 ?part))
        (bind ?result (insert$ ?result (+ (length$ ?result) 1) (implode$ ?part)))
        
     else (break)))
     
  (return ?result))

(deffunction findalls (?patterns ?string) (map2 findall ?patterns ?string))

(deffunction find-ins-str (?class ?slot ?value)  ;new find-ins fnc
 ;(find-all-instances ((?t ?class)) (findall ?value (slot-value ?t ?slot)))
  (if (multifieldp ?value) then ;allows for egrep like abiility, if any of values hit
      (find-all-instances ((?t ?class)) (findalls ?value (slot-value ?t ?slot) ))
 else (find-all-instances ((?t ?class)) (findall ?value (slot-value ?t ?slot) ))))
;can use w/:
(deffunction find-ins-str-sv (?cls ?sn ?str) 
   (save_ins2 (find-ins-str ?cls ?sn ?str) (str-cat ?cls ".ins")))

;deffunction find-ins-str1dot (?dn ?str) 
(deffunction find-ins-str1dot-sv (?dn ?str) 
    ;split ?dn at dot into cls&sn=dn
    (bind ?cls (file-base ?dn))
    (find-ins-str-sv (sym-cat ?cls) ?dn ?str)) 
 ;but better to rm-duplicates
(deffunction find-ins-str1dot (?dn ?str)
    ;split ?dn at dot into cls&sn=dn
    (bind ?cls (file-base ?dn))
    (find-ins-str (sym-cat ?cls) ?dn ?str))
(deffunction find-ins-str1dots (?dnl ?str)
  (rm-duplicates (map1 find-ins-str1dot ?dnl ?str)))
(deffunction find-ins-str1dots-sv (?dnl ?str)
  (save_ins2 (find-ins-str1dots ?dnl ?str) (str-cat (underscore ?str) ".ins")))

  
(deffunction split (?pattern ?string $?max)
  "split ?string in parts delimited by ?pattern. Empty strings ("") are
   removed from the result. If $?max is used the resulting multifield
   value will have at most $?max + 1 values."
   
  (bind ?string-index (nth$ 1 (search ?pattern ?string)))   ; split is
                                                            ; implemented
                                                            ; using search
  (bind ?string-anchor 1)
  (bind ?result (create$))
  
  (if (eq (length$ $?max) 0)
   then (bind ?max 0)
   else
     (if (> (length$ $?max) 1)
      then 
        (printout ?*errors* "; ERROR: Too many parameters." crlf)
        (printout ?*errors* "; The split function takes either 2 or 3 parameters.")
        (printout ?*errors* " You used " (+ (length$ $?max) 2) "!" crlf)
        (return nil)
      else (bind ?max (nth$ 1 $?max))))
      
  (while (neq ?string-index -1)
    (if (or (eq ?max 0)
            (> ?max (length$ ?result)))
     then
       (bind ?part (sub-string ?string-anchor
                               (- (+ ?string-index
                                     ?string-anchor) 2)
                               ?string))
       (bind ?result (insert$ ?result
                              (+ (length$ ?result) 1)
                              ?part))
       (bind ?string-anchor (+ ?string-index
                               (- ?string-anchor 1)
                               (str-length ?pattern)))
       (bind ?string-index (nth$ 1 (search ?pattern
                                           (sub-string ?string-anchor
                                                       (str-length ?string)
                                                       ?string))))
     else
       (bind ?part (sub-string ?string-anchor
                               (str-length ?string)
                               ?string))
       (bind ?result (insert$ ?result
                              (+ (length$ ?result) 1)
                              ?part))
       (return ?result)))
       
  (bind ?part (sub-string ?string-anchor
                          (str-length ?string)
                          ?string))
  (bind ?result (insert$ ?result
                         (+ (length$ ?result) 1)
                         ?part))
                         
  (bind ?fixed-result (create$))
  (progn$ (?part ?result)
     (if (or (neq ?part "")
             (eq ?part-index 1)
             (eq ?part-index (length$ ?result)))
      then (bind ?fixed-result (insert$ ?fixed-result
                               (+ (length$ ?fixed-result) 1)
                               ?part))))
  (return ?fixed-result))
  
  
(deffunction sub (?pattern ?replacement ?string $?max)
  "substitute each non-overlapping occurance of ?pattern in ?string with
   ?replacement."
   
  (bind ?result "")
  (if (eq (length$ $?max) 0)
   then (bind ?max 0)
   else
     (if (> (length$ $?max) 1)
      then 
        (printout ?*errors* "; ERROR: Too many parameters." crlf)
        (printout ?*errors* "; The sub function takes either 3 or 4 parameters.")
        (printout ?*errors* " You used " (+ (length$ $?max) 3) "!" crlf)
        (return nil)
      else (bind ?max (nth$ 1 $?max))))
      
  (bind ?string-parts (split ?pattern ?string ?max))    ; sub is implemented
                                                        ; using split
  (progn$ (?part ?string-parts)
    (bind ?result (str-cat ?result ?part))
    (if (< ?part-index (length$ ?string-parts))
     then (bind ?result (str-cat ?result ?replacement))))
     
  (return ?result))
  
;http://www.pulp.se/clips/unittest/unittest.clp   

;;;; unit test functions for Clips

(deffunction test-suite (?stream ?name $?test-cases)
   (printout ?stream crlf "Summary of test-suite: " ?name crlf)
   (progn$ (?test-case $?test-cases)
      (printout ?stream ?test-case crlf)))
      
(deffunction test-case (?name $?tests)
   (bind ?number-of-tests 0)
   (bind ?number-of-failed-tests 0)
   (bind ?messages (create$))
   
   (progn$ (?test $?tests)
      (if (eq ?test nil)
       then
         (bind ?number-of-tests (+ ?number-of-tests 1))
       else
         (bind ?number-of-tests (+ ?number-of-tests 1))
         (bind ?messages (insert$ ?messages (+ (length$ ?messages) 1) (str-cat "Test #" ?number-of-tests " failed: " ?test)))
         (bind ?number-of-failed-tests (+ ?number-of-failed-tests 1))))
   (return (create$ (str-cat ?number-of-failed-tests " of "
                             ?number-of-tests " unit tests failed in " ?name)
                    ?messages)))
                    
(deffunction test (?cmp ?result ?expected-result)
   (if (eq (type ?result) MULTIFIELD)
    then (bind ?r (str-cat "(create$ " (implode$ ?result) ")"))
    else (bind ?r ?result))
    
   (if (eq (type ?expected-result) MULTIFIELD)
    then (bind ?er (str-cat "(create$ " (implode$ ?expected-result) ")"))
    else (bind ?er ?expected-result))
   
   (if (eval (str-cat "(" ?cmp " " ?r " " ?er ")"))
    then (return nil)
    else 
      (if (eq (type ?result) MULTIFIELD)
       then (bind ?result (str-cat "(" (implode$ ?result) ")")))
       
      (if (eq (type ?expected-result) MULTIFIELD)
       then (bind ?expected-result (str-cat "(" (implode$ ?expected-result) ")")))
       
      (return (str-cat "(" ?cmp " " ?result " " ?expected-result ") -> FALSE"))))
      
;http://commentsarelies.blogspot.com/2007/11/added-some-functionality-in-clips.html 

(deffunction stringify (?value) ;not sure about htis one
  (if (not (stringp ?value))
     then
     (return ?value))
  (if (and (not (str-index "\"" ?value))
           (not (str-index "\\" ?value))) ;"
     then
     (return ?value))
  (bind ?new-string "")
  (bind ?length (str-length ?value))
  (loop-for-count (?i ?length)
     (bind ?char (sub-string ?i ?i ?value))
     (if (or (eq ?char "\"") (eq ?char "\\")) ;"
        then
        (bind ?new-string (str-cat ?new-string "\\" ?char)) ;"
        else
        (bind ?new-string (str-cat ?new-string ?char))))
  (return ?new-string))  ;???
;CLIPS> (stringify "abc") "abc"
;CLIPS> (stringify "a\\b\\c") "a\\b\\c"
;CLIPS> (stringify "a\b\c") "abc"

(deffunction str-cmp (?s1 ?s2)
  (str-compare (upcase ?s1) (upcase ?s2)))
(deffunction position- (?s1 ?s2)
  "case-insensative position w/args like str-cmp"
  (position$ (upcase ?s2) (upcase ?s1))) ;more like cmp
(deffunction rm-dsp (?s1)
  (str-rms ?s1 (create$ "-" " " "_")))
(deffunction position_ (?s1 ?s2)
  "case+sp/etc-insensative position w/args like str-cmp"
  (position- (rm-dsp ?s1) (rm-dsp ?s2))) ;more like cmp
(deffunction rm-comma (?s) (sub "," " "  ?s))
