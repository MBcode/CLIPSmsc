;;; ######################################################################################
;;; Load the source files
;;; ######################################################################################
(load* (str-cat ?*src-folder* "global.clp"))
(load* (str-cat ?*src-folder* "functions.clp"))
(load* (str-cat ?*src-folder* "create-templates.clp"))
(load* (str-cat ?*src-folder* "order.clp"))
(load* (str-cat ?*src-folder* "create-classes.clp"))
(load* (str-cat ?*src-folder* "create-objects.clp"))
(load* (str-cat ?*src-folder* "rule-generator.clp"))

;;; ######################################################################################
;;; Define the addresses or the local paths of the ontologies
;;; *Note*: This function should not be called in the case where
;;; O-DEVICE is used through JAVA (using JO-DEVICE or in any other
;;; attempt to integrate O-DEVICE in JAVA applications). In these
;;; cases, the j2cf module should be called independenly in order
;;; to create the ?*fact-file*.
;;; ######################################################################################
;examples
;(load-ontology "http://127.0.0.1/1-ub-dl-univ0-dept0.owl")
;(load-ontology "http://www.loa-cnr.it/ontologies/DOLCE-Lite.owl")
;(load-ontology "file:c:/omconfig.owl" "ontology2" "ontology3" "ontology...")



;;; ######################################################################################
;;; Load the generated triple-based facts (from ?*triple-facts*)
;;; ######################################################################################
(batch* ?*triple-facts*)
(assert (UPDATE (refresh FALSE)))

;;; ######################################################################################
;;; Run and save O-DEVICE
;;; ######################################################################################
(run)
(save-o-device)
