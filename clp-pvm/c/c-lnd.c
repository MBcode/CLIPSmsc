/*clips glenda (Linda using PVM) fncs   MTB*/
/* #include "../gts/gluser.c" 
   might compile like gts?*/
#include <stdio.h>
#include <stdarg.h>
#include "glenda.h"

#if defined(__cplusplus)
extern "C" {
#endif
extern int gl_out(char*,...);
extern int gl_in(char*,...);
extern int gl_inp(char*,...);
extern int gl_rd(char*,...);
extern int gl_rdp(char*,...);
#if defined(__cplusplus)
}
#endif

/*-----------------------------------------------------------------*/
/*DefineFunction2("tpn_n_out",'i',PTIF tpn_n_out,"tpn_n_out","45ikuikk"); */
/*args: type,ptr to memory,
	# of elts to put into OR max# to take out of the tuple space, tuple name
        and one of 5 commands: out=O in=I inp=i rd=R rdp=r*/
/*return: #of elts actually recieved/sent*/
/*VOID tpn_n_out(DATA_OBJECT_PTR rp) be able to return a mf 
  if ever want to send >1 array in a tuple*/
int tpn_n_c()
{
int num=1,*pi,rnum;
float *pf;
double *pd;
char tstr[49],type,t1[2],cmnd;
    t1[1]='\0';
    /*get the type of the array*/
    sprintf(tstr,"%s",(char *)RtnLexeme(1)); 
    type = tolower(tstr[0]);
    if(type!='i' && type!='f' && type!='d' && type!='b')
    { 
      printf("[1st arg=type:i or f or d]");
      return(-1);
    }
    /*figure out which command is being executed*/
    if(RtnArgCount()>4) 
    {
	sprintf(tstr,"%s",(char *)RtnLexeme(5)); 
	     if(!strncasecmp(tstr,"out",3)) cmnd='O';
	else if(!strncasecmp(tstr,"inp",3)) cmnd='i';
	else if(!strncasecmp(tstr,"in",2))  cmnd='I';
	else if(!strncasecmp(tstr,"rdp",3)) cmnd='r';
	else if(!strncasecmp(tstr,"rd",2))  cmnd='R';
	else		                    cmnd='O';
    }
    else		cmnd='O';
    /*get the name of the tupel*/
    sprintf(tstr,"%s",(char *)RtnLexeme(4)); 

    /*get the number to put out or take in*/
    if(RtnArgCount() > 2) num=(int)RtnLong(3);

    printf("[tpn_n_c:%c for %s with %d elts]",cmnd,tstr,num);

    switch(type)
    {
	case 'i': pi = (int *)get_ptr(2);  
	printf("[pi=%d]",(int)pi);
	switch(cmnd)
	{
	  case 'O': gl_out(tstr,A_INT,num,pi,NULL);  rnum=num;	break;
          case 'I': gl_in(tstr,A_INT,num,pi,&rnum,NULL);	break;
          case 'i': gl_inp(tstr,A_INT,num,pi,&rnum,NULL);	break;
          case 'R': gl_rd(tstr,A_INT,num,pi,&rnum,NULL);	break;
          case 'r': gl_rdp(tstr,A_INT,num,pi,&rnum,NULL);	break;
	}
        break;
	case 'f': pf = (float *)get_ptr(2);  
	printf("[pf=%d]",(int)pf);
	switch(cmnd)
	{
	  case 'O': gl_out(tstr,A_FLOAT,num,pf,NULL); rnum=num;	break;
          case 'I': gl_in(tstr,A_FLOAT,num,pf,&rnum,NULL);	break;
          case 'i': gl_inp(tstr,A_FLOAT,num,pf,&rnum,NULL);	break;
          case 'R': gl_rd(tstr,A_FLOAT,num,pf,&rnum,NULL);	break;
          case 'r': gl_rdp(tstr,A_FLOAT,num,pf,&rnum,NULL);	break;
	}
        break;
	case 'd': pd = (double *)get_ptr(2);  
	printf("[pd=%d]",(int)pd);
	switch(cmnd)
	{
	  case 'O': gl_out(tstr,A_DOUBLE,num,pd,NULL); rnum=num; break;
          case 'I': gl_in(tstr,A_DOUBLE,num,pd,&rnum,NULL);	break;
          case 'i': gl_inp(tstr,A_DOUBLE,num,pd,&rnum,NULL);	break;
          case 'R': gl_rd(tstr,A_DOUBLE,num,pd,&rnum,NULL);	break;
          case 'r': gl_rdp(tstr,A_DOUBLE,num,pd,&rnum,NULL);	break;
	}
        break;
    }
    return(rnum);
}
/*presently this only puts 1 array into the tuple-space
  so for now, every array will have to go w/ a different tuple*/
/*-----------------------------------------------------------------*/
/*-------------------------------------------------------------------EOF--*/
