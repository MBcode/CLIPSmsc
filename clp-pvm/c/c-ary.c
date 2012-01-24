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
/*----------------------------------------------------------PTR_TO_INT*/
/*DefineFunction2("ptr_to_int",'l',PTIF ptr_to_int,"ptr_to_int","11uu");*/
/*args: 1 ptr (accesible from get_ptr)*/
/*ret:  the long int version of the ptr(so does anything if started as a long)*/
long ptr_to_int()
{
    return((long)get_ptr(1));
}
/*---------------------------------------------------------*/
/*DefineFunction2("tpn_to_mf",'m',PTIF tpn_to_mf,"tpn_to_mf","25ikui"); */
/*args: type,ptr to memory,number to put to a m.f.*/
/*-deref or pk_tpn can fill malloced space, then can use this to get to a m.f.*/
VOID tpn_to_mf(DATA_OBJECT_PTR rp)
{
int num=1,stride=1,*pi,offset=0,i;
float *pf;
double *pd;
char tstr[9],type,*pc,t1[2];
VOID *mfp;
    t1[1]='\0';
    /*get the type*/
    sprintf(tstr,"%s",(char *)RtnLexeme(1)); 
    type = tolower(tstr[0]);
    if(type!='i' && type!='f' && type!='d' && type!='b')
    {	
	printf("[1st arg=type:i or f or d]");
	return;
    }

    /*get number (& offset & stride) to put to a m.f.*/
    if(RtnArgCount() > 2) num=(int)RtnLong(3);
    if(RtnArgCount() > 3) offset=(int)RtnLong(4);
    if(RtnArgCount() > 4) stride=(int)RtnLong(5);
    /*could use SetMultifieldErrorValue(rp); return;*/
    mfp = CreateMultifield(num);

    /*get the ptr, and set the MF*/
    switch(type)
    {
	case 'i' :  pi = (int *)get_ptr(2);  
		    for(i=0; i<num; i++)
		    {
			SetMFType(mfp,i,INTEGER);
			SetMFValue(mfp,i,AddLong(pi[offset+i]));
		    }
		    break;
	case 'f' :  pf = (float *)get_ptr(2);  
		    for(i=0; i<num; i++)
		    {
			printf("%f to mf,",pf[offset+i]); fflush(stdout);
			SetMFType(mfp,i,FLOAT);
			SetMFValue(mfp,i,AddDouble((double)pf[offset+i]));
		    }
		    break;
	case 'd' :  pd = (double *)get_ptr(2);  
		    for(i=0; i<num; i++)
		    {
			SetMFType(mfp,i,FLOAT);
			SetMFValue(mfp,i,AddDouble(pd[offset+i]));
		    }
		    break;
	/*this one could go per char or by stride or by space breaks*/
	/*go by char for now*/
	case 'b' :  pc = (char *)get_ptr(2);  
		    for(i=0; i<num; i++)
		    {
			SetMFType(mfp,i,SYMBOL);
			t1[0]=pc[offset+i];
			SetMFValue(mfp,i,AddSymbol(t1));
		    }
		    break;
    } /*gets past this and dies when printing out the result-
	-looks liked capped ok though*/
    SetpType(rp,MULTIFIELD);
    SetpValue(rp,mfp);
    SetpDOBegin(rp,1);
    SetpDOEnd(rp,num);
    return;
}
/*---------------------------------------------------------*/
/*DefineFunction2("tpn_mf_mirror",'m',PTIF tpn_mf_mirror,"tpn_mf_mirror","35ikui"); */
/*args: type,ptr to mem,number to put to a m.f. (by DO ptr to the array locs)*/
/*might be dangerous, maybe trasfer to from mf instead*/
VOID tpn_mf_mirror(DATA_OBJECT_PTR rp)
{
int *pi,i,num,offset=0,stride=1;
float *pf;
double *pd;
char tstr[9],type,t1[2];
VOID *mfp;
    t1[1]='\0';
    /*get the type*/
    sprintf(tstr,"%s",(char *)RtnLexeme(1)); 
    type = tolower(tstr[0]);
    if(type!='i' && type!='f' && type!='d' && type!='b')
    {	
	printf("[1st arg=type:i or f or d]");
	return;
    }

    /*get number (& offset & stride) to put to a m.f.*/
    if(RtnArgCount() > 2) num=(int)RtnLong(3);
    if(RtnArgCount() > 3) offset=(int)RtnLong(4);
    if(RtnArgCount() > 4) stride=(int)RtnLong(5);
    /*could use SetMultifieldErrorValue(rp); return;*/
    mfp = CreateMultifield(num);

    /*get the ptr, and set the MF*/
    switch(type)
    {
	case 'i' :  pi = (int *)get_ptr(2);  
		    for(i=0; i<num; i++)
		    {
			printf(",%d ",
			 SetMFType(mfp,i,INTEGER));
			/*SetMFValue(mfp,i,AddLong(pi[offset+i]));
			  set the Data_Object ptr to the address*/
		    }
		    break;
	case 'f' :  pf = (float *)get_ptr(2);  
		    for(i=0; i<num; i++)
		    {
			printf("%f to mf,",pf[offset+i]);
			fflush(stdout);
			printf(",%d ",
			 SetMFType(mfp,i,FLOAT));
			fflush(stdout);
			/*SetMFValue(mfp,i,AddDouble((double)pf[offset+i]));
			  set the Data_Object ptr to the address*/
		    }
		    break;
	case 'd' :  pd = (double *)get_ptr(2);  
		    for(i=0; i<num; i++)
		    {
			SetMFType(mfp,i,FLOAT);
			/*SetMFValue(mfp,i,AddDouble(pd[offset+i]));
			  set the Data_Object ptr to the address*/
		    }
		    break;
    }
    SetpType(rp,MULTIFIELD);
    SetpValue(rp,mfp);
    SetpDOBegin(rp,1);
    SetpDOEnd(rp,num);
    return;
}
/*---------------------------------------------------------*/
/*DefineFunction2("tpppno",'m',PTIF tpppno,"tpppno","66ikuuuik"); */
/*args: type,2X ptr to memory,number to do calc on, an operator to use*/
/*return: int version of ptr to the output array*/
int tpppno()
{
int num=1,stride=1,*pi1,*pi2,*piout,offset=0,i,rp=-1;
float *pf1,*pf2,*pfout;
double *pd1,*pd2,*pdout;
char tstr[9],type,t1[2],op;
    t1[1]='\0';
    /*get the type*/
    sprintf(tstr,"%s",(char *)RtnLexeme(1)); 
    type = tolower(tstr[0]);
    if(type!='i' && type!='f' && type!='d' && type!='b')
    {	
	printf("[1st arg=type:i or f or d]");
	return(-1);
    }
    sprintf(tstr,"%s",(char *)RtnLexeme(6)); 
    op = tolower(tstr[0]);

    if(RtnArgCount() > 4) num=(int)RtnLong(5);
    switch(type)
    {
	case 'i' :  
	    pi1 = (int *)get_ptr(2);  
	    pi2 = (int *)get_ptr(3);  
	    piout = (int *)get_ptr(4);  
	    switch(op)
	    {
	     case '+': for(i=0; i<num; i++) piout[i] = pi1[i] + pi2[i]; break;
	     case '-': for(i=0; i<num; i++) piout[i] = pi1[i] - pi2[i]; break;
	     case '*': for(i=0; i<num; i++) piout[i] = pi1[i] * pi2[i]; break;
	     case '/': for(i=0; i<num; i++) piout[i] = pi1[i] / pi2[i]; break;
	    }
	    rp = (int)piout;
	    break;
	case 'f' :
	    pf1 = (float *)get_ptr(2);  
	    pf2 = (float *)get_ptr(3);  
	    pfout = (float *)get_ptr(4);  
	    switch(op)
	    {
	     case '+': for(i=0; i<num; i++) pfout[i] = pf1[i] + pf2[i]; break;
	     case '-': for(i=0; i<num; i++) pfout[i] = pf1[i] - pf2[i]; break;
	     case '*': for(i=0; i<num; i++) pfout[i] = pf1[i] * pf2[i]; break;
	     case '/': for(i=0; i<num; i++) pfout[i] = pf1[i] / pf2[i]; break;
	    }
	    rp = (int)pfout;
	    break;
	case 'd' :  
	    pd1 = (double *)get_ptr(2);  
	    pd2 = (double *)get_ptr(3);  
	    pdout = (double *)get_ptr(4);  
	    switch(op)
	    {
	     case '+': for(i=0; i<num; i++) pdout[i] = pd1[i] + pd2[i]; break;
	     case '-': for(i=0; i<num; i++) pdout[i] = pd1[i] - pd2[i]; break;
	     case '*': for(i=0; i<num; i++) pdout[i] = pd1[i] * pd2[i]; break;
	     case '/': for(i=0; i<num; i++) pdout[i] = pd1[i] / pd2[i]; break;
	    }
	    rp = (int)pdout;
	    break;
    } 
    return(rp);
}
/*---------------------------------------------------------*/
/*=======================================================-- clips fncs*/
/*these could have been in c-msc, but are related to c-ary.c*/
/*---------------------------------------------------------MALLOC*/
/*DefineFunction2("malloc",'a',PTIF mallocn,"mallocn","11ii"); */
/*args: int num of bytes to malloc*/
/*ret:  clips extern ptr*/
VOID *mallocn()
{
int n;
    n = (int)RtnLong(1);
    if(n<1) n=100;
    return((VOID *) malloc(n));
}
/*---------------------------------------------------------IMALLOC*/
/*DefineFunction2("imalloc",'i',PTIF malloci,"malloci","11ii"); */
/*args: int num of bytes to malloc*/
/*ret:  int ptr*/
int malloci()
{
int n;
    n = (int)RtnLong(1);
    if(n<1) n=64;
    printf("[malloc %d]",n);
    return((int) malloc(n));
}
/*---------------------------------------------------------FREE*/
/*DefineFunction2("free",'i',PTIF freei,"freei","11uu"); */
/*args: ptr to memory to free*/
int freei()
{
  free((void *)get_ptr(1));
  return(1);
}
/*---------------------------------------------------------MEM_COPY*/
/*DefineFunction2("memcopy",'i',PTIF mem_copy,"mem_copy","33uuui"); */
int mem_copy() /*bcopy( a, b, n )*/
{
int n;
char *a,*b;
    a=(char *)get_ptr(1);
    b=(char *)get_ptr(2);
    n = (int)RtnLong(3);
    if(!a && !b) {memcpy(b,a,n); return(0);}
    else printf("[memcopy nil ptr]");
    return(1);
}
/*---------------------------------------------------------MEM_SET*/
/*DefineFunction2("memset",'i',PTIF mem_set,"mem_set","23uuik"); */
int mem_set() /*bzero( a, n )*/
{
int n;
char *a,b='\0',z[9];
    a=(char *)get_ptr(1);
    n = (int)RtnLong(2);
    if(RtnArgCount() >2) {
	sprintf(z,"%s",(char *)RtnLexeme(3));
	b=z[0];
    }
    if(!a && !b) {memset(a,b,n); return(0);}
    else printf("[memset nil ptr]");
    return(1);
}
/*---------------------------------------------------------DEREF*/
/*DefineFunction2("deref",'u',PTIF deref,"deref","2*ukuu"); */
/*send in int memory location, if send in an num set it, otherwise return it*/
/*int deref()*/
/*args:  type/ptr/opt- numbers*/
/*ret:   1st element at the ptr memory location*/
VOID deref(DATA_OBJECT_PTR rp)
{
int n,i,j,*npi,ret=0,t;
float *npf;
char tstr[89],type,*sp;
    n = RtnArgCount(); 
    printf("[ac=%d]",n);
    sprintf(tstr,"%s",(char *)RtnLexeme(1)); 
    type = tolower(tstr[0]);
    switch(type)
    {
	case 'i':  npi = (int *)get_ptr(2);  
		   break;
	case 'f':  npf = (float *)get_ptr(2); 
		   break;
	case 'b':  sp = (char *)get_ptr(2); 
		   break;
        default:   printf("bad type %s\n",tstr);
		   break;
    }
    if(n > 2)   /*then set the values*/
    {
	for(i=3; i<=n; i++)
	{
	  switch(type)
	  {
	    case 'i':  printf("[set%d at%d]",(npi[i-3] = get_int(i)),i);  break;
	    case 'f': printf("[set%f at%d]",(npf[i-3] = get_float(i)),i); break;
	    case 'b':  get_str(i,tstr);     /*fix-finish*/
			t = strlen(tstr);
			for(j=0; j<t; j++)
			 printf("[%c]",(sp[j] = tstr[j])); 
			break;
	  }
	}
	switch(type)  /*if setting want ptr, which might be newly malloced*/
	{
	  case 'i' : set_long(rp,(long)npi);	break;
	  case 'f' : set_long(rp,(long)npf);	break;
	  case 'b' : set_long(rp,(long)sp);	break;
	}
    } 
    else
    switch(type)
    {
      case 'i' : set_int(rp,(int)npi[0]);	break;
      case 'f' : set_float(rp,(float)npf[0]);	break;
      case 'b' : printf("[str=%s]",sp); 
		 set_symb(rp,(char *)sp);	break;
    }
    return;
}
/*---------------------------------------------------------*/
/*maybe transfer arrays 1slice at a time w/offset between C(==) & FORTRAN*/
/*have *VARA wrap some malloced array space instead of the global buf rtns*/
/*---------------------------------------------------------*/
/*could have a clips setable debug flag for the prints in the functions*/
/*---------------------------------------------------------*/
/*---------------------------------------------------------EOF*/
