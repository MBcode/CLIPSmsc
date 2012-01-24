/*misc functions to be included in the clips main file, M. Bobak,  ANL*/

#define ISMETH(m,ts,ac) (!strcasecmp((m),(ts)) && ((ac)+2)==get_ac())
/*---------------------------------------------------------INCLUDES*/
/*---------------------------------------------------------general*/
#include <stdio.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
/*---------------------------------------------------------extern C*/
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------clips*/
#include "clips.h"   /*has Rtn*fncs, so fnc can get args from clips*/
#include "setup.h"
#include "sysdep.h"
#include "extnfunc.h"
#include "commline.h"
#define PTIF2 (void (*)(VOID_ARG))
/*---------------------------------------------------------*/
#include "symbol.h"
#include "router.h"
#include "engine.h"
#include "argacces.h"
#include "prntutil.h"
/*---------------------------------------------------------*/
#ifdef __cplusplus
}
#endif
/*---------------------------------------------------------clipsmain*/
/*#include "incl/clipsmain.c"*/
/*the idea is to not have to store it in clips,*/
/*---------------------------------------------------------TYPELEN*/
/*DefineFunction2("typelen",'i',PTIF typelen,"typelen","11kk"); */
int typelen()
{
int r;
char c,type[14];
 sprintf(type,"%s",(char *)RtnLexeme(1));   /*type = *RtnLexeme(2);*/
 c = type[0];
 switch(tolower(c))
 {
  case 'b' :  r = 1;              break;
  case 'i' :  r = sizeof(int);    break;
  case 'f' :  r = sizeof(float);  break;
  case 'd' :  r = sizeof(double); break;
  case 'l' :  r = sizeof(long);   break;
  default  :  r = sizeof(float);  break;
 }
 return(r);
}
/*=======================================================--CLIPS fncs*/
/*might want to break out the fncs that don't have wrappers
  so they can be included by any file that does have wrappers
  and wants to use them  (not necc if all incl in 1 big file)
  (better to link in wrapper files seperately though)*/
  /*would be nice to have fncs to set/get vals from mf-s*/
/*=======================================================--internal fncs*/
/*----------------------------------------------------------PTR_TO_INT*/
/*DefineFunction2("ptr_to_int",'l',PTIF ptr_to_int,"ptr_to_int","11uu");*/
/*args: 1 ptr (accesible from get_ptr)*/
/*ret:  the long int version of the ptr(so does anything if started as a long)*/
long ptr_to_int()
{
    return((long)get_ptr(1));
}
/*---------------------------------------------------------*/
/*=======================================================-- clips fncs*/
/*---------------------------------------------------------ADDRUNFNC*/
/*DefineFunction2("addrunfnc",'i',PTIF addrunfnc,"addrunfnc","25iss"); */
/*engine.h:   LOCALE BOOLEAN     AddRunFunction(char *,VOID (*)(void),int);*/
/*eg. (addrunfnc "nrecv_route" "nrecv_route" 1)*/
int addrunfnc()
{
char str[99],fnc[99];
int priority=1,start=0,remove=0,cnt;
    cnt= RtnArgCount();
    sprintf(str,"%s",(char *)RtnLexeme(1));
    sprintf(fnc,"%s",(char *)RtnLexeme(2));
    if(cnt>2) priority 	= (int)RtnLong(3);
    if(cnt>3) start 	= (int)RtnLong(4);
    if(cnt>4) remove 	= (int)RtnLong(5);
    if(!remove)
    {
/*	if(start==1)	  return(AddRunStartFunction(str,PTIF fnc,priority));*/
/*	else if(start==2) return(AddRunStopFunction(str,PTIF fnc,priority));*/
/*	else		  */
/*bad argument  2 type for AddRunFunction(): int (*)() ( void (*)() expected)*/
	return(AddRunFunction(str,PTIF2 fnc,priority));
    } else
    {
/*	if(start==1)	  return(RemoveRunStartFunction(str));*/
/*	else if(start==2) return(RemoveRunStopFunction(str));*/
/*	else		  */
	return(RemoveRunFunction(str));
    } 
}
/*=======================================================--utility fncs*/
/*str-cat sym-cat sub-string str-index upcase lowcase p155*/
/*---------------------------------------------------------STR-CMP*/
/*DefineFunction2("str-cmp",'i',PTIF str-cmp,"str-cmp","24iss"); */
int str_cmp()
{
int cnt,len=0;
char s1[44],s2[44];
    sprintf(s1,"%s",(char *)RtnLexeme(1));
    sprintf(s2,"%s",(char *)RtnLexeme(2));
    cnt= RtnArgCount();
    if(cnt>2)
    {
	len = (int)RtnLong(3);
	if(len>0)
	{
	    if(cnt>3)	return(strncasecmp(s1,s2,len));
	    else	return(strncmp(s1,s2,len));
	} else
	{
	    if(cnt>3)	return(strcasecmp(s1,s2));
	    else	return(strcmp(s1,s2));
	}
    } else		return(strcmp(s1,s2));
} /*there is already a str-compare*/
/*---------------------------------------------------------ATOI*/
/*DefineFunction2("atoi",'i',PTIF catoi,"catoi","11s"); */
int catoi()
{
char s1[44];
    sprintf(s1,"%s",(char *)RtnLexeme(1));
    return(atoi(s1));
}
/*---------------------------------------------------------ATOF*/
/*DefineFunction2("atof",'f',PTIF catof,"catof","11s"); */
float catof()
{
char s1[44];
    sprintf(s1,"%s",(char *)RtnLexeme(1));
    return(atof(s1));
}
/*moved memeory fncs to c-ary.c*/
/*---------------------------------------------------------*/
/*---------------------------------------------------------EOF*/
