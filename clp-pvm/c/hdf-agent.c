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

/*c-misc-fncs.c c-pvm-fncs.c c-misc-defs.c c-pvm-defs.c ts-agent.c*/

#include "c-misc-fncs.c"
#include "c-pvm-fncs.c"
#include "c-hdf.c"
#include "clipsmain.c"

/*---------------------------------------------------------------USERFUNCTIONS*/
VOID UserFunctions()
{
#include "c-misc-defs.c"
#include "c-pvm-defs.c"

DefineFunction2("hdf_data",'i',PTIF hdf_data,"hdf_data","4*ikkxi"); 
DefineFunction2("hdf_nt",'i',PTIF hdf_nt,"hdf_nt","01kk"); 
DefineFunction2("hdf_dims",'i',PTIF hdf_dims,"hdf_dims","2*iki"); 
DefineFunction2("hdf_clear",'i',PTIF hdf_clear,"hdf_clear","00i"); 
DefineFunction2("hdf_strs",'i',PTIF hdf_strs,"hdf_strs","14iikkk"); 
DefineFunction2("hdf_dimscale",'i',PTIF hdf_dimscale,"hdf_dimscale","13ii"); 
DefineFunction2("hdf_setlengths",'i',PTIF hdf_setlengths,"hdf_setlengths","44iiiii"); 
DefineFunction2("hdf_range",'i',PTIF hdf_range,"hdf_range","02nnn"); 
DefineFunction2("hdf_cal",'i',PTIF hdf_cal,"hdf_cal","05nnnnni"); 

}
/*----------------------------------------------------------------EOF*/
