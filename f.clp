;load this file before my https://github.com/MBcode/CLIPSmsc/blob/master/utils.clp bobak@balisp.org
;e.g. alias fclu  'rlwrap fz_clips -l ~/bin/fzutils.clp' where: cat f.clp utils.clp >fzutils.clp
;https://github.com/rorchard/FuzzyCLIPS seems to have been forked from an older clips version
(deffunction string-to-field (?in) (eval ?in)) ;added as fz_clips doesn't have it
;docs on it's use at: http://mma.perso.eisti.fr/HTML-SE/Programme/fzdocs.pdf 
