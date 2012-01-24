/*--this is the main loop for clips, to be included
//-this is the version that has libscheme embedded in it
//M. Bobak,  ANL
//---------------------------------------------------------*/
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                  A Product Of The                   */
   /*             Software Technology Branch              */
   /*             NASA - Johnson Space Center             */
   /*             CLIPS Version 6.00  05/12/93            */
   /*                     MAIN MODULE                     */
   /*******************************************************/
/*************************************************************/
/* Principal Programmer:    Gary D. Riley                    */
/* Contributing Programmer(s):                               */
/*      Bob Orchard (NRCC - Nat'l Research Council of Canada)*/
/*                  (Fuzzy reasoning extensions)             */
/*                  (certainty factors for facts and rules)  */
/*      Mike Bobak  (PVM extentions)                         */
/*************************************************************/
#if FUZZY_DEFTEMPLATES
#include "fuzzyutl.h"
#include "fuzzymod.h"
#endif
/*---------------------------------------------------------just added fuzzymod*/
/***********************************************************/
/* RerouteStdin: Reroutes stdin to read initially from the */
/*   file specified on the command line with -r option.    */
/***********************************************************/
globle VOID RerouteStdin2(int argc, char** argv)  /*int argc; char *argv[];*/
{
   int i;
   /* If no arguments return */
   if (argc < 3) { return; }
   /* If argv was not passed then forget it */
   if (argv == NULL) return;

   for (i = 1 ; i < argc ; i++)
   {   
      if (strcmp(argv[i],"-r") == 0)
      {
         if (i > (argc-1))
         {
            PrintErrorID("SYSDEP",1,CLIPS_FALSE);
            PrintCLIPS(WERROR,"No string found for -r option\n");
            return;
         }
         else
	 {
	    printf("Doing a: RouteCommand(%s)\n",argv[++i]); fflush(stdout);
	    RouteCommand(argv[i]);
	 }
      }
   }
}
/*---------------------------------------------------------
//RUN: Starts execution of rules.  Rules fire until agenda is empty or
//     the number of rule firings limit specified by the first argument
//     is reached (infinity if unspecified).
//     A fuzzyCLIPS extension assigns a special meaning to the rule limit
//     value -2 and values less than -2. For -2 the inference cycle will
//     continue forever (or until a break, control-C, is encountered).
//     Even when the agenda is empty the cycle will continue and any
//     functions added to the runtime list will be executed. If the
//     value is less than -2 then the cycle will continue until |limit|
//     rules have been fired even if the agenda becomes empty at some time.
//(run [<integer-expression>])
//---------------------------------------------------------*/

#include "scheme.h"

/*---------------------------------------------------------*/
int sc-eval()
{
      obj = scheme_read (scheme_stdin_port);  /*how to read 1st*/
      if (obj == scheme_eof)
    {
      printf ("\n; done\n");
      exit (0);
    }
      obj = SCHEME_CATCH_ERROR(scheme_eval (obj, global_env),0);
      if (obj)
    {
      scheme_write (obj, scheme_stdout_port); /*then how to get as clips obj*/
      printf ("\n");
    }
}
/*---------------------------------------------------------*/
#if defined(__cplusplus)
extern "C" {
#endif

#if ANSI_COMPILER
int main(int,char *[]);
VOID UserFunctions(void);
#else
int main();
VOID UserFunctions();
#endif

#if defined(__cplusplus)
}
#endif
/***************************************************************/
/* MAIN: Start execution of CLIPS.  This function must be      */
/*   redefined in order to embed CLIPS within another program. */
/*   Example of redefined main:                                */
/*     main()                                                  */
/*       {                                                     */
/*        InitializeCLIPS();                                   */
/*            .                                                */
/*            .                                                */
/*        ProcessData();                                       */
/*        RunCLIPS(-1);                                        */
/*        EvaluateData();                                      */
/*            .                                                */
/*            .                                                */
/*        FinalResults();                                      */
/*       }                                                     */
/***************************************************************/
#if defined(__cplusplus)
int main (int argc, char *argv[])
#else
int main(argc,argv)
  int argc;
  char *argv[] ;
#endif /* defined(__cplusplus) */
{
  Scheme_Env *global_env;
  Scheme_Object *obj, *in_port;
  int i;
/*FILE *fp;  blow of loading files from command line for now*/

  global_env = scheme_basic_env ();

   InitializeCLIPS();   
   RerouteStdin(argc,argv);   /*handles batch files (done in CommandLoop)*/
/* RerouteStdin2(argc,argv);  //my version -r "any command to route" (done now)
// the new lib has the -r option in RerouteStdin */
   CommandLoop();
   return(-1);
}
/*************************************************************/
/* UserFunctions:  The function which informs CLIPS of any   */
/*   user defined functions.  In the default case, there are */
/*   no user defined functions.  To define functions, either */
/*   this function must be replaced by a function with the   */
/*   same name within this file, or this function can be     */
/*   deleted from this file and included in another file.    */
/*   User defined functions may be included in this file or  */
/*   other files.                                            */
/*   Example of redefined UserFunctions:                     */
/*     UserFunctions()                                       */
/*       {                                                   */
/*        DefineFunction("fun1",'i',fun1,"fun1");            */
/*        DefineFunction("other",'f',other,"other");         */
/*       }                                                   */
/*************************************************************/
