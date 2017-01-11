;load this file before my https://github.com/MBcode/CLIPSmsc/blob/master/utils.clp mike.bobak@gmail.com
;e.g. alias fclu  'rlwrap fz_clips -l ~/bin/fzutils.clp' where: cat f.clp utils.clp >fzutils.clp
;https://github.com/rorchard/FuzzyCLIPS seems to have been forked from an older clips version
(deffunction string-to-field (?in) (eval ?in)) ;added as fz_clips doesn't have it
;docs on it's use at: http://mma.perso.eisti.fr/HTML-SE/Programme/fzdocs.pdf 
;also http://thor.info.uaic.ro/~dcristea/cursuri/SE/fzdocs.pdf
;http://alumni.cs.ucr.edu/~vladimir/cs171/quickfuzzy.pdf
;http://math.haifa.ac.il/robotics/Presentations/pdf/Ch7_FuzzyLogic.PDF
;http://www.graco.unb.br/alvares/DOUTORADO/omega.enm.unb.br/pub/doutorado/disco2/ai.iit.nrc.ca/IR_public/fuzzy/fuzzyShower.html
;http://www.graco.unb.br/alvares/DOUTORADO/omega.enm.unb.br/pub/doutorado/disco2/ai.iit.nrc.ca/IR_public/fuzzy/fuzzyShowerJess.html
;http://www.graco.unb.br/alvares/DOUTORADO/omega.enm.unb.br/pub/doutorado/disco2/ai.iit.nrc.ca/IR_public/fuzzy/FuzzyTruck.html
;http://www.cs.dartmouth.edu/~spl/publications/fuzzy%20talk/FuzzyPendulum.html
