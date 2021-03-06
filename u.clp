;This was at the end of my utils.clp at one point, from this work: http://lpis.csd.auth.gr/systems/r-device/manual.pdf
;-----------------------------------------EOF
(defglobal ?*R-DEVICE_PATH* = "\/Users\/bobak\/Documents\/downloads\/ai\/prot\/rdf\/R-DEVICE\/")
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
 (load* (str-cat ?*R-DEVICE_PATH* "translation.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "translation-rules.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "main.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "rdf-auxiliary.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "load-rdf.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "import.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "export.clp"))
 (load* (str-cat ?*R-DEVICE_PATH* "triple-transformation.clp"))
 (reset)
)
;--------------------------------------------------------- 
;This is still in utils.clp but not every machine has the other code.
;Even though Protege can work between formats, might still use this.
; 1plc would be to save gen ins ;as well as taking triples2frames
