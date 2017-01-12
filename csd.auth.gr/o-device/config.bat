;;; ######################################################################################
;;; ******* Modify these gloabals according to your configuration ******
;;; ######################################################################################

;*******************************
;*** Display infos, warnings ***
;*******************************
(defglobal ?*warn* = TRUE)
(defglobal ?*info* = TRUE)

;***************
;*** FOLDERS ***
;***************
;the folder path where the distribution folder of O-DEVICE exists
(defglobal ?*odevice-folder* = "c:/Users/George/Desktop/@work/_code/odevice/")

;the folder path with the source files of O-DEVICE 
(defglobal ?*src-folder* = (str-cat ?*odevice-folder* "src/"))

;the folder path where the generated files will be stored 
(defglobal ?*bundle-folder* = (str-cat ?*odevice-folder* "bundle/"))

;the folder path where the facts will be stored by the J2CF module
(defglobal ?*triple-folder* = (str-cat ?*odevice-folder* "triple-facts/"))



;*************
;*** FILES ***
;*************
;the file path where the dynamic rules will be stored
(defglobal ?*rule-file* = (str-cat ?*bundle-folder* "$rules.clp"))

;the file path where the facts of O-DEVICE will be stored 
(defglobal ?*fact-file* = (str-cat ?*bundle-folder* "$facts.clp"))

;the file path where the classes of O-DEVICE will be stored 
(defglobal ?*class-file* = (str-cat ?*bundle-folder* "$classes.clp"))

;the file path where the generated objects will be stored
(defglobal ?*object-file* = (str-cat ?*bundle-folder* "$objects.clp"))

;the file path where the triple-based facts will be stored
(defglobal ?*triple-facts* = (str-cat ?*triple-folder* "$triples.clp"))

;the path of the index file where the instances will be saved (restore-instances*).  
(defglobal ?*ins-idx* = (str-cat ?*odevice-folder* "$ins.idx"))





;************
;*** MISC ***
;************

;whether to use prefixes or complete namespaces. It is recommended to
;use TRUE since it results in faster execution
(defglobal ?*abbr* = TRUE)

;whether the imported ontologies should be processed or not
(defglobal ?*imports* = TRUE)

;Load the ontology vocabulary in the form of defglobal variables
(if ?*abbr*
	then (load* (str-cat ?*src-folder* "vocabulary-abbr.clp"))
	else (load* (str-cat ?*src-folder* "vocabulary.clp"))
)
