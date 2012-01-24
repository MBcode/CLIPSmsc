(defclass ConsCell
 (is-a INITIAL-OBJECT)
 (role concrete) 
 (pattern-match reactive)

 (slot first (create-accessor read-write))
 (slot rest (create-accessor read-write))
)

