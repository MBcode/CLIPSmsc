/*---------------------------------------------------------------------------
/*  FuzzyClips w/ PVM  (Model)/'Agent' code             includes:            
//  "C" Language Integrated Production System,          CLIPS Version 6.02   
//  Gary D. Riley, Software Technology Branch of NASA-Johnson Space Center   
//  Fuzzy reasoning extensions w/ certainty factors for facts and rules      
//                 Bob Orchard, NRCC - Nat'l Research Council of Canada      
//  PVM (Parallel Virtual Machine) communication extentions, Mike Bobak, ANL 
/----------------------------------------------------------------------------*/
/*  Use: ts-agt -r "(load util.clp)" -r "(load pvm.clp)"
      or ts-agt -r "(batch b)" where the file b has the above commands*/
/*  To include model specific code, add lines like those in pvm[ud]fncs.c    */

/*c-l-fncs.c c-misc-fncs.c c-pvm-fncs.c c-misc-defs.c c-pvm-defs.c ts-agent.c*/

#include "c-misc-fncs.c"
#include "c-pvm-fncs.c"
#include "c-l-fncs.c"
#include "clipsmain.c"

/*---------------------------------------------------------------USERFUNCTIONS*/
VOID UserFunctions()
{
#include "c-misc-defs.c"
#include "c-pvm-defs.c"
/*defines for c-l-fncs.c
DefineFunction2("tpn_n_out",'i',PTIF tpn_n_out,"tpn_n_out","44ikuik");
DefineFunction2("tpn_n_in",'i',PTIF tpn_n_in,"tpn_n_in","44ikuik");
*/
DefineFunction2("tpn_n_c",'i',PTIF tpn_n_c,"tpn_n_c","45ikuikk");
}
/*----------------------------------------------------------------EOF*/
