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
/*=======================================================--CLIPS access fncs*/
/*might want to break out the fncs that don't have wrappers
  so they can be included by any file that does have wrappers
  and wants to use them  (not necc if all incl in 1 big file)
  (better to link in wrapper files seperately though)*/
  /*would be nice to have fncs to set/get vals from mf-s*/
/*=======================================================--internal fncs*/
/*---------------------------------------------------------ADDSYMB*/
/*if want this from CLIPS use sym-cat*/
VOID *AddSymb(char *str)
{
char *t;
VOID *ret;
    t=strdup(str);
    ret=AddSymbol(t);
    free(t);
    return(ret);
}
/*---------------------------------------------------------*/
/*--------------------------------------------------------wrapper for Rtn-fncs*/
/*---------------------------------------------------------GET_STR*/
char *get_str(int n,char *m) /*a shorthand for returning a string*/
{ sprintf(m,"%s",(char *)RtnLexeme(n)); return(m); }
/*---------------------------------------------------------GET_CHAR*/
char get_char(int n)
{ 
char tmpstr[22];
 sprintf(tmpstr,"%s",(char *)RtnLexeme(n)); return(m); 
 return(tmpstr[0]);
}
/*----------------------------------------------------------*/
float get_ac()          { return(       RtnArgCount());}
/*----------------------------------------------------------*/
/*a shorthand for returning a float,int  (inline sometime)*/
/*---------------------------------------------------------GET_FLOAT*/
float  get_float(int n)	{ return((float)RtnDouble(n)); }
double get_double(int n){ return(       RtnDouble(n)); }
/*---------------------------------------------------------GET_INT*/
int    get_int(int n)  	{ return(  (int)RtnLong(n));   }
long   get_long(int n) 	{ return(       RtnLong(n));   }
/*might have these use RtnUnknown like in get_ptr*/
/*---------------------------------------------------------GET_INT_ARRAY*/
void   get_int_array(int start, int *array)
{      
int i;
      for(i=start;  i<RtnArgCount(); i++)  array[i-start] = (int)RtnLong(i);
}
/*---------------------------------------------------------GET_LONG_ARRAY*/
void   get_long_array(int start, long *array)
{      
int i;
      for(i=start;  i<RtnArgCount(); i++)  array[i-start] = RtnLong(i);
}
/*---------------------------------------------------------GET_FLOAT*/
float  get_float_if(int n,float *fp)  { 
       if(get_ac()>=n) return(*fp=(float)RtnDouble(n));  else return(0.0); }
double get_double_if(int n,double *dp){ 
       if(get_ac()>=n) return(*dp=       RtnDouble(n));  else return(0.0); }
/*---------------------------------------------------------GET_INT*/
int    get_int_if(int n,int *ip)  	{ 
       if(get_ac()>=n) return(*ip=(int)RtnLong(n));  else return(0);   }
long   get_long_if(int n,long *lp)	{ 
       if(get_ac()>=n) return(*lp=     RtnLong(n));  else return(0);   }
/*----------------------------------------------------------GET_PTR*/
VOID *get_ptr(int num)
{
DATA_OBJECT tmp;
VOID *ret=(VOID *)NULL;
long i;
    RtnUnknown(num,&tmp);
    switch(GetType(tmp))
    {
	case INTEGER:		
	    i=DOToLong(tmp);
	    if(i<999) printf("[bad int for ptr = %d]\n",i); 
	    else ret = (VOID *)i;
	    break;
	case EXTERNAL_ADDRESS:  ret = (VOID *)DOToPointer(tmp);  break;
	case SYMBOL:		
	case INSTANCE_NAME:		
	printf("will take ins ptr and use DirectGetSlot(ins,sn,&tmp)\n");
/*	case INSTANCE:		*/
	break;
    }
    return(ret);
}
/*----------------------------------------------------------wrap unk returns*/
/*like AddSymb, but for numbers, &symb*/
/*----------------------------------------------------------set_float*/
VOID set_float(DATA_OBJECT_PTR ptr,float f)
{
    SetpType(ptr,FLOAT);
    SetpValue(ptr,AddDouble((double)f));
    return;
}
/*----------------------------------------------------------set_double*/
VOID set_double(DATA_OBJECT_PTR ptr,double f)
{
    SetpType(ptr,FLOAT);
    SetpValue(ptr,AddDouble(f));
    return;
}
/*----------------------------------------------------------set_int*/
VOID set_int(DATA_OBJECT_PTR ptr,int i)
{
    SetpType(ptr,INTEGER);
    SetpValue(ptr,AddLong((long)i));
    return;
}
/*----------------------------------------------------------set_long*/
VOID set_long(DATA_OBJECT_PTR ptr,long i)
{
    SetpType(ptr,INTEGER);
    SetpValue(ptr,AddLong(i));
    return;
}
/*----------------------------------------------------------set_symb*/
VOID set_symb(DATA_OBJECT_PTR ptr,char *s)
{   /*might want a tmp str like w/ addsymb  (just using addsymb now)*/
    SetpType(ptr,SYMBOL);
    SetpValue(ptr,AddSymb(s));
    return;
}
/*----------------------------------------------------------*/
/*---------------------------------------------------------*/
/*tpn_to-mf could almost be used as a subfnc*/
/*---------------------------------------------------------*/
/*---------------------------------------------------------EOF*/
