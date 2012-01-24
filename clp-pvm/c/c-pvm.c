/*pvm functions to be included in the clips main file, M. Bobak,  ANL*/

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
/*---------------------------------------------------------PVM include*/
#include "incl/pvm3.h"
/*---------------------------------------------------------PVM util fnc*/
/*---------------------------------------------------------PVMTYPE*/
/*the idea is to not have to store it in clips,*/
int pvmtype(char c)
{
int r;
 switch(tolower(c))
 {
  case 's' :  r = PVM_STR;  break;
  case 'b' :  r = PVM_BYTE; break;
  case 'i' :  r = PVM_INT; break;
  case 'f' :  r = PVM_FLOAT; break;
  case 'd' :  r = PVM_DOUBLE; break;
  case 'l' :  r = PVM_LONG; break;
  default  :  r = PVM_FLOAT; break;
 }
 return(r);
}
/*---------------------------------------------------------*/
/*=======================================================--start PVM fncs*/
/*mytid (or any pvm-sys-fnc will enroll process into pvm*/
/*=======================================================--process cntrl fncs*/
/*---------------------------------------------------------MYTID*/
/*DefineFunction2("mytid",'i',PTIF mytid,"mytid","00i"); */
int mytid()			/*returns the task id (tid) of this process*/
{
    return(pvm_mytid());
}
/*---------------------------------------------------------EXIT_PVM*/
/*DefineFunction2("exit_pvm",'i',PTIF exit_pvm,"exit_pvm","00i"); */
int exit_pvm()			/*un-enrolls this process w/local pvmd*/
{
    return(pvm_exit());
}
/*---------------------------------------------------------HALT*/
/*DefineFunction2("halt_pvm",'i',PTIF halt_pvm,"halt_pvm","00i"); */
int halt_pvm()			/*shuts down entire pvm system*/
{
    return(pvm_halt());
}
/*---------------------------------------------------------KILL_PVM*/
/*DefineFunction2("kill_pvm",'i',PTIF kill_pvm,"kill_pvm","11ii"); */
int kill_pvm()			/*kill task w/ given tid*/
{
int tid;
    tid 	= (int)RtnLong(1);
    return(pvm_kill(tid));
}
/*---------------------------------------------------------SPAWN*/
/*DefineFunction2("spawn",'i',PTIF spawn,"spawn","56issisii"); */
/*eg. (spawn "pvm-agt" "unused" 1 "boston" 1) */
int spawn()		       /*starts ntask copies of exec file task w/argv(s)*/
{			      /*flag(Task Default/Host/Arch/Debug/Trace/Compl)*/
char task[155],argv_[9][90],where[155],**tmp=(char**)NULL;
int flag,ntask,tids[9],rt=0;
    sprintf(task,"%s",(char *)RtnLexeme(1));
    sprintf(argv_[0],"%s",(char *)RtnLexeme(2));  /*assume 1 for now*/
    flag 	= (int)RtnLong(3);
    sprintf(where,"%s",(char *)RtnLexeme(4));
    ntask 	= (int)RtnLong(5);			/*assume 1 for now*/
    /*tids[0]	= (int)RtnLong(6);			//assume 1 for now*/
    rt = pvm_spawn(task, tmp ,flag,where,ntask,tids);
    /*would like to get an arg in instead of tmp  -fix*/
    printf("[spawn %s on %s rt= %d tids=%d]\n",task,where,rt,tids[0]);
    fflush(stdout);
    return(tids[0]);		/*would ret #spawned, but only1, so ret tid*/
/*can put vargs under ?$rest, then check numargs*/
/*can use return value w/ (tasks ?tid) to create an instance*/
}
/*---------------------------------------------------------CATCHOUT*/
/*DefineFunction2("catchout",'i',PTIF catchout,"catchout","01ss"); */
/*catch output of a child process in a file*/
int catchout()
{
FILE* fp;
char file[99];
    if(RtnArgCount() >0) sprintf(file,"%s",(char *)RtnLexeme(1)); 
    else		 sprintf(file,"out-%d",pvm_mytid());
    if((fp = fopen(file,"w")) == NULL) { 
	printf("[could not open % for catchout]",file); fflush(stdout); 
	return(0);
    } else return((int)fp);
}
/*---------------------------------------------------------ADDHOSTS  (finish)*/
/*DefineFunction2("addhosts",'i',PTIF addhosts,"addhosts","19ss"); */
/*eg. (addhosts "boston")*/
int addhosts()			/*adds host(s) to the virtual machine*/
{
char hosts[9][99];
int nhosts=1,i,infos[9],rt=0;
    nhosts= RtnArgCount();
    for(i=0; i<nhosts; i++) sprintf(hosts[i],"%s",(char *)RtnLexeme(i));
    /*rt= addhosts(hosts,nhosts,infos);*/
    if(rt!=nhosts) {
	printf("[Error: addhost, host not added %d]\n",rt);
	for(i=0; i<nhosts; i++) printf("%d,",infos[i]);
    }
    return(rt);
}
/*---------------------------------------------------------DELHOSTS  (finish)*/
/*DefineFunction2("delhosts",'i',PTIF delhosts,"delhosts","19ss"); */
int delhosts()			/*dels host(s) from the virtual machine*/
{
char hosts[9][99];
int nhosts=1,i,infos[9],rt=0;
    nhosts= RtnArgCount();
    for(i=0; i<nhosts; i++) sprintf(hosts[i],"%s",(char *)RtnLexeme(i));
    /*rt= delhosts(hosts,nhosts,infos);*/
    if(rt!=nhosts) { 
	printf("[Error: delhost, host not deleted %d]\n",rt);
	for(i=0; i<nhosts; i++) printf("%d,",infos[i]);
    }
    return(rt);
}   /*could make add/del 1 fnc, and make 'del' the optional last arg*/
/*=======================================================--info fncs*/
/*---------------------------------------------------------SETOPT*/
/*DefineFunction2("setopt",'i',PTIF setopt,"setopt","02iii"); */
int setopt()
{
int what=2,val=3;  /*default is debug level of 2*/
    if(RtnArgCount() >0) what 	= (int)RtnLong(1);
    if(RtnArgCount() >1) val 	= (int)RtnLong(2);
    printf("[setopt(%d,%d)]\n",what,val); fflush(stdout);
    return( pvm_setopt(what,val) );
} /*route,debug,autoerr,outputtid,outputcode,tracetid,tracecode,fragsize,resvtis*/
/*---------------------------------------------------------PARENT*/
/*DefineFunction2("parent",'i',PTIF parent,"parent","00i"); */
int parent()			/*get tid of process that spawned this one*/
{
    return(pvm_parent());
}
/*---------------------------------------------------------TIDHOST*/
/*DefineFunction2("tidtohost",'i',PTIF tidtohost,"tidtohost","11ii"); */
int tidtohost()		/*returns the host tdid on which the task w/tid is running*/
{
int tid;
    tid 	= (int)RtnLong(1);
    return(pvm_tidtohost(tid));
}
/*---------------------------------------------------------PSTAT*/
/*DefineFunction2("pstat",'i',PTIF pstat,"pstat","11ii"); */
int pstat()			/*get status of task tid (Ok/NoTask/BadParam)*/
{
int tid;
    tid 	= (int)RtnLong(1);
    return(pvm_pstat(tid));
}
/*---------------------------------------------------------MSTAT*/
/*DefineFunction2("mstat",'i',PTIF mstat,"mstat","11ii"); */
int mstat()			/*gets host status of (Ok/HostFail/NoHost)*/
{
char host[99];
    sprintf(host,"%s",(char *)RtnLexeme(1));
    return(pvm_mstat(host));
}	/*could put all info fncs into 1 fnc*/
/*---------------------------------------------------------CONFIG*/
/*DefineFunction2("config",'i',PTIF config,"config","00i"); */
int config()
{
int nhost,narch,info;
struct pvmhostinfo **hostp=0;
    info= pvm_config(&nhost,&narch,hostp);
    printf("[nhost=%d,narch=%d,]\n",nhost,narch);
    /*print rest latter*/
    fflush(stdout);
    return(info);
}
/*---------------------------------------------------------TASKS*/
/*DefineFunction2("tasks",'s',PTIF tasks,"tasks","02iik"); */
VOID* tasks()
{
/*VOID *ret;*/
int where=0,info,ntask,i;
char tstr[255]; /**t;*/
char str[255],name[30];
struct pvmtaskinfo *tl; 
    if(RtnArgCount() > 0) {where = (int)RtnLong(1); sprintf(name,"%d",where); }
    if(RtnArgCount() > 1) sprintf(name,"%s",(char *)RtnLexeme(2)); 
    info= pvm_tasks(where,&ntask,&tl);
    printf("[n-tasks=%d,where=%d,info=%d]\n",ntask,where,info); fflush(stdout);
    if(info<0) { printf("[BAD info=%d]\n",info); return(AddSymbol("")); }
    /*if given a task id, return a str to be evaluted to the assoc instance*/
    if(where)
    {
	sprintf(str,"(make-instance %s of TASK (tid %d) (ptid %d) (host %d) (flag %d) (Name %s) (pid %d))",
		   name,  tl[0].ti_tid, tl[0].ti_ptid, tl[0].ti_host, 
			  tl[0].ti_flag, tl[0].ti_a_out, tl[0].ti_pid); 
	puts(str);  fflush(stdout);
    }
    else	/*do for all tasks, but just return the tids (& get sep latter)*/
    {
	if(ntask<1) 
	{
	    printf("No Tasks\n"); fflush(stdout);
	    return(AddSymbol(""));
	}
	strcpy(str,"");
	printf("%d Tasks\n",ntask); fflush(stdout);
	for(i=0; i<ntask; i++)
	{
	    sprintf(tstr,"%d ",tl[i].ti_tid);
	    puts(tstr);  fflush(stdout);
	    strcat(str,tstr);
	}
    }
    puts(str);  fflush(stdout);
    return(AddSymb(str));
    /*t=strdup(str); ret=AddSymbol(t); free(t); return(ret);*/
}
/*struct pvmtaskinfo *tip;*/
/*if (!pvm_tasks(host, &ntask, &tip)) */
/*if (tip[i].ti_tid == mytid && !xflg)*/
/*struct pvmtaskinfo *tip; print_task_rec(&tip[0], 0);*/
/*---------------------------------------------------------BUFINFO*/
/*DefineFunction2("bufinfo",'i',PTIF bufinfo,"bufinfo","02iis"); */
int bufinfo()
{
int bufid,bytes,type,source,info=0,ret=1;
char str[9];
    if(RtnArgCount() >1) sprintf(str,"%s",(char *)RtnLexeme(1));
    if(RtnArgCount() >0) bufid = (int)RtnLong(1);
    else		 bufid = pvm_getrbuf();
    if(bufid<0)		 bufid = pvm_getrbuf();
    info= pvm_bufinfo(bufid,&bytes,&type,&source);
    printf("[bufid=%d,bytes=%d,type=%d,source=%d,info=%d]",
				bufid,bytes,type,source,info);
    switch(tolower(str[0]))
    {
	case 's': ret = source; break;
	case 'm': 
	case 't': ret = type; break;
	case 'i': ret = info; break;
        default:  ret = type; break;
    }
    return(ret);
}
/*---------------------------------------------------------PERROR*/
/*DefineFunction2("perror",'i',PTIF perror,"perror","00i"); */
/*used to print out the 'info' of the last pvm call*/
int perror_pvm()
{
char pre[33] ={"pvm-error="};
    return(pvm_perror(pre));
}
/*=======================================================--message buffer fncs*/
/*---------------------------------------------------------MKBUF*/
/*DefineFunction2("mkbuf",'i',PTIF mkbuf,"mkbuf","01ii"); */
int mkbuf()			/*makes a new buffer, w/encoding*/
{				/*(DataDefault/DataRaw/DataInPlace)*/
int encoding=2;
/*need to check # of args to see if need to read it*/
    if(RtnArgCount() >0) encoding 	= (int)RtnLong(1);
	return(pvm_mkbuf(encoding));
}
/*---------------------------------------------------------INITSEND*/
/*DefineFunction2("initsend",'i',PTIF initsend,"initsend","01ii"); */
int initsend()			/*clear send buf & makes a new 1*/
{
int encoding=2;  /*2?*/
/*need to check # of args to see if need to read it*/
    if(RtnArgCount() >0) encoding 	= (int)RtnLong(1);
	return(pvm_initsend(encoding));
}
/*---------------------------------------------------------FREEBUF*/
/*DefineFunction2("freebuf",'i',PTIF freebuf,"freebuf","11ii"); */
int freebuf()			/*frees buffer w/ bufid*/
{
int bufid;  /*could have a default, but not much used*/
    bufid 	= (int)RtnLong(1);
	return(pvm_freebuf(bufid));
}
/*---------------------------------------------------------GETSBUF*/
/*DefineFunction2("getsbuf",'i',PTIF getsbuf,"getsbuf","00i"); */
int getsbuf()			/*returns active send bufid*/
{
	return(pvm_getsbuf());
}
/*---------------------------------------------------------GETRBUF*/
/*DefineFunction2("getrbuf",'i',PTIF getrbuf,"getrbuf","00i"); */
int getrbuf()			/*returns active receive bufid*/
{
	return(pvm_getrbuf());
}
/*maybe add set(sr)buf fncs*/
/*=======================================================--sendin fncs*/
/*---------------------------------------------------------PKSTR*/
/*DefineFunction2("pkstr",'i',PTIF pkstr,"pkstr","11sk"); */
int pkstr()			/*fill buffer w/ string*/
{
char str[999];
    sprintf(str,"%s",(char *)RtnLexeme(1));
    printf("[pkstr=%s]\n",str);
    return(pvm_pkstr(str));
}
/*---------------------------------------------------------PKBYTE*/
/*DefineFunction2("pkbyte",'i',PTIF pkbyte,"pkbyte","13iuii"); */
int pkbyte()			/*fill buffer w/ an array of bytes*/
{
int nitem=1,stride=1;
char *p;
    p = (char *)get_ptr(1);
    if(RtnArgCount() >1) nitem = (int)RtnLong(2);
    if(RtnArgCount() >2) stride = (int)RtnLong(3);
    return(pvm_pkbyte(p,nitem,stride));
}
/*---------------------------------------------------------PKBYTE_STR*/
/*DefineFunction2("pkbyte_str",'i',PTIF pkbyte_str,"pkbyte_str","12ssi"); */
int pkbyte_str()			/*fill buffer w/ char array*/
{
char str[199];
int nitem=1;
    sprintf(str,"%s",(char *)RtnLexeme(1));
    if(RtnArgCount() >1) nitem = (int)RtnLong(2);
    return(pvm_pkbyte(str,nitem,1));
}
/*---------------------------------------------------------PKINT*/
/*DefineFunction2("pkint",'i',PTIF pkint,"pkint","1*ii"); */
/*could malloc up an array of length cnt*/
int pkint()			/*fill buffer w/ ints*/
{
int num[999],i,cnt;
    cnt= RtnArgCount();
    printf("[packing:");
    for(i=0; i<cnt; i++)
    {
	num[i] 	= (int)RtnLong(i+1);
	printf("%d,",num[i]);
    }
    printf("]\n");
    return(pvm_pkint(num,cnt,1));
}
/*---------------------------------------------------------PK1INT*/
/*DefineFunction2("pk1int",'i',PTIF pk1int,"pk1int","11ii"); */
int pk1int()			/*fill buffer w/ 1 int*/
{				/*pkint subsumes this fnc -del soon*/
int num[1];
    num[0] 	= (int)RtnLong(1);
    return(pvm_pkint(num,1,1));
}
/*---------------------------------------------------------PKFLOAT*/
/*DefineFunction2("pkfloat",'i',PTIF pkfloat,"pkfloat","1*ff"); */
/*DefineFunction2("pkfloat",'i',PTIF pkfloat,"pkfloat","1*nn"); */
int pkfloat()			/*fill buffer w/ int floats*/
{
float num[999];
int i,cnt;
    cnt= RtnArgCount();
    printf("[packing:");
    for(i=0; i<cnt; i++)
    {
	num[i] 	= (float)RtnDouble(i+1);
	printf("%f,",num[i]);
    }
    printf("]\n");
    return(pvm_pkfloat(num,cnt,1));
}
/*---------------------------------------------------------PKDOUBLE*/
/*DefineFunction2("pkdouble",'i',PTIF pkdouble,"pkdouble","1*dd"); */
int pkdouble()			/*fill buffer w/ int double*/
{
double num[999];
int i,cnt;
    cnt= RtnArgCount();
    for(i=0; i<cnt; i++)  num[i] 	= RtnDouble(i+1);
    return(pvm_pkdouble(num,cnt,1));
}
/*---------------------------------------------------------PKNUM*/
/*DefineFunction2("pknum",'i',PTIF pknum,"pknum","2*nkn"); */
int pknum()			/*fill buffer w/ numbers*/
{
int inum[999],i,cnt,info;
float fnum[999];
double dnum[999];
char tstr[49],type;
    cnt= RtnArgCount()-1;
    sprintf(tstr,"%s",(char *)RtnLexeme(1)); 
    type = tstr[0];
    if(type!='i' && type!='f' && type!='d')
    {	
	printf("[1st arg=type:i or f or d]");
	return(0);
    }
    printf("[packing:");
    for(i=0; i<cnt; i++)
    {
	switch(type)
	{
	    case 'i' :  inum[i] = (int)RtnLong(i+2);  break;
	    case 'f' :  fnum[i] = (float)RtnDouble(i+2);  break;
	    case 'd' :  dnum[i] = RtnDouble(i+2);  break;
	}
    }
    printf("]\n");
    switch(type)
    {
        case 'i' : info= pvm_pkint(inum,cnt,1); break;
        case 'f' : info= pvm_pkfloat(fnum,cnt,1); break;
        case 'd' : info= pvm_pkdouble(dnum,cnt,1); break;
    }
    printf("[packing:cnt=%d,type=%c,info=%d:",cnt,type,info);
    return(cnt);
}
/*---------------------------------------------------------*/
/*look into a fnc that moves things from recv to send buffers or vs.*/
/*---------------------------------------------------------SEND_*/
/*DefineFunction2("send_",'i',PTIF send_,"send_","12iii"); */
int send_()			/*send buf to task w/tid*/
{
int tid,msgtag=0;
    tid 	= (int)RtnLong(1);
    if(RtnArgCount() >1) msgtag = (int)RtnLong(2);
    return(pvm_send(tid,msgtag));
}
/*---------------------------------------------------------MCAST*/
/*DefineFunction2("mcast",'i',PTIF mcast,"mcast","2*ii"); */
/*inputs are the msgtag, then a list of tids, */
/*which could come from (explode$ (task))*/
int mcast()			/*send to task(s) w/tid(s)*/
{
int tids[20],ntasks,msgtag;
int cnt,i=0;
char str[199];
/*the rest of the args will be tids*/
    cnt= RtnArgCount();
    ntasks = cnt-1;
    msgtag 	= (int)RtnLong(1);
    for(i=1; i<cnt; i++) tids[i-1] = (int)RtnLong(i);
    return(pvm_mcast(tids,ntasks,msgtag));
}
/*---------------------------------------------------------SEND_STR*/
/*DefineFunction2("send_str",'i',PTIF send_str,"send_str","33iiis"); */
int send_str()			/*send str to task w/tid*/
{
int tid,msgtag;
char str[499];
    tid 	= (int)RtnLong(1);
    msgtag 	= (int)RtnLong(2);
    sprintf(str,"%s",(char *)RtnLexeme(3));
    return(pvm_psend(tid,msgtag,(void *)str,strlen(str)+1,0));
}
/*---------------------------------------------------------PSEND*/
/*DefineFunction2("psend",'i',PTIF psend,"psend","45iuikii"); */
/*args:buf-ptr #elts datatype tid msgtag */
int psend()
{
int tid,msgtag,num,type;
char tstr[9];
   num = (int)RtnLong(2);
   sprintf(tstr,"%s",(char *)RtnLexeme(3)); 
   type = pvmtype(tstr[0]);
   tid = (int)RtnLong(4);
   if(RtnArgCount()>4) msgtag = (int)RtnLong(5);
   else 	       msgtag = type;
   return(pvm_psend(tid,msgtag,(void *)get_ptr(1),num,type));
}
/*---------------------------------------------------------PRECV*/
/*DefineFunction2("precv",'i',PTIF precv,"precv","35iuikii"); */
/*args:buf-ptr #elts datatype tid msgtag */
int precv()
{
int tid=-1,msgtag,num,type,atid,atag,alen,info;
char tstr[9];
   num = (int)RtnLong(2);
   sprintf(tstr,"%s",(char *)RtnLexeme(3)); 
   type = pvmtype(tstr[0]);
   if(RtnArgCount()>3) tid    = (int)RtnLong(4);
   if(RtnArgCount()>4) msgtag = (int)RtnLong(5);
   else 	       msgtag = type;
   info=pvm_precv(tid,msgtag,(void *)get_ptr(1),num,type,&atid,&atag,&alen);
   printf("[precv from tid=%d msgtag=%d len=%d]\n",atid,atag,alen);
   return(info);
}
/*---------------------------------------------------------PK-TPN*/
/*DefineFunction2("pk_tpn",'i',PTIF pk_tpn,"pk_tpn","25ikui"); */
/*args: type/ptr to memory/number to pack or unpack*/
/*-all of this should be able to be done by deref & (u)pk fncs that take ptrs*/
int pk_tpn()
{
int num=1,stride=1,info,*pi,upk=0,offset=0;
float *pf;
double *pd;
char tstr[9],type,*pc;
    /*get the type*/
    sprintf(tstr,"%s",(char *)RtnLexeme(1)); 
    type = tolower(tstr[0]);
    if(type != tstr[0]) upk =1;
    if(type!='i' && type!='f' && type!='d' && type!='b')
    {	
	printf("[1st arg=type:i or f or d]");
	return(0);
    }
    /*get the ptr*/
    switch(type)
    {
	case 'i' :  pi = (int *)get_ptr(2);  break;
	case 'f' :  pf = (float *)get_ptr(2);  break;
	case 'd' :  pd = (double *)get_ptr(2);  break;
	case 'b' :  pc = (char *)get_ptr(2);  break;
    }
    /*get number to (un)pack*/
    if(RtnArgCount() > 2) num=(int)RtnLong(3);
    if(RtnArgCount() > 3) offset=(int)RtnLong(4);
    if(RtnArgCount() > 4) stride=(int)RtnLong(5);

  if(!upk)
  {
      switch(type)
      {
        case 'i' : info= pvm_pkint(&pi[offset],num,stride); break;
        case 'f' : info= pvm_pkfloat(&pf[offset],num,stride); break;
        case 'd' : info= pvm_pkdouble(&pd[offset],num,stride); break;
        case 'b' : info= pvm_pkbyte(&pc[offset],num,stride); break;
      }
    printf("[packed array into pvm buffer:%d",info);
  }
  else 	/*if(upk)*/
  {			
      switch(type)
      {
        case 'i' : info= pvm_upkint(&pi[offset],num,stride); break;
        case 'f' : info= pvm_upkfloat(&pf[offset],num,stride); break;
        case 'd' : info= pvm_upkdouble(&pd[offset],num,stride); break;
        case 'b' : info= pvm_upkbyte(&pc[offset],num,stride); break;
      }
    printf("[un-packed array from pvm buffer:%d",info);
  }
    /*could put a warning print if info looks bad*/
    return(info);   /*might return the sent pointer*/
}
/*---------------------------------------------------------*/
/*---------------------------------------------------------*/

/*=======================================================--receivin fncs*/
/*---------------------------------------------------------PROBE*/
/*DefineFunction2("probe",'i',PTIF probe,"probe","22iii"); */
/*returns the bufid if msg has arrived*/
/*---------------------------------------------------------UPKSTR*/
/*DefineFunction2("upkstr",'s',PTIF upkstr,"upkstr","00i"); */
/*eg. (recv_ -1 -1)  (bind ?str (upkstr))*/
VOID* upkstr()			/*get string from buffer*/
{
/*VOID *ret;*/
char str[999]; /**t;*/
    pvm_upkstr(str);
    printf("[upkstr=%s]\n",str);
    return(AddSymb(str));
    /*t= strdup(str); ret=AddSymbol(t); free(t); return(ret);*/
}
/*---------------------------------------------------------UPKBYTE*/
/*DefineFunction2("upkbyte",'l',PTIF upkbyte,"upkbyte","13iuii"); */
long upkbyte()			/*get bytes from buffer*/
{
int nitem=1,stride=1;
char *p=(char *)0;
    p = (char *)get_ptr(1);
    if(RtnArgCount() >1) nitem = (int)RtnLong(2);
    if(RtnArgCount() >2) stride = (int)RtnLong(3);
    pvm_upkbyte(p,nitem,stride);
    return((long)p);
}
/*---------------------------------------------------------UPKBYTE_V*/
/*DefineFunction2("upkbyte_v",'x',PTIF upkbyte_v,"upkbyte_v","02iii"); */
VOID* upkbyte_v()			/*get string from buffer*/
{
int nitem=1,stride=1;
char *p=(char *)0;
    if(RtnArgCount() >1) nitem = (int)RtnLong(1);  /*check these*/
    if(RtnArgCount() >2) stride = (int)RtnLong(2);
    pvm_upkbyte(p,nitem,stride);
    return((VOID *)p);
}
/*---------------------------------------------------------UPKBYTE_STR*/
/*DefineFunction2("upkbyte_str",'s',PTIF upkbyte_str,"upkbyte_str","01ii"); */
/*eg. (recv_ -1 -1)  (bind ?str (upkbyte_str))*/
VOID* upkbyte_str()			/*get string from buffer*/
{
/*VOID *ret;*/
char str[199]; /**t;*/
int nitem=1;
    if(RtnArgCount() >0) nitem = (int)RtnLong(1);
    pvm_upkbyte(str,nitem,1);
    printf("[upkbyte-str=%s]\n",str);
    return(AddSymb(str));
    /*t= strdup(str); ret=AddSymbol(t); free(t); return(ret);*/
}
/*---------------------------------------------------------UPKINT*/
/*DefineFunction2("upkint",'i',PTIF upkint,"upkint","01ii"); */
int upkint()
{
int i,info,nitem=1,inum[999];
    if(RtnArgCount() >0) nitem = (int)RtnLong(1);
    info= pvm_upkint(inum,nitem,1); 
    printf("[upkint ");
    for(i=0;i<nitem;i++) printf("%d,",inum[i]);
    printf(",nitem=%d,1  = %d\n",nitem,1);
    return(inum[0]);
}
/*---------------------------------------------------------UPKTOSTR*/
/*DefineFunction2("upktostr",'s',PTIF upktostr,"upktostr","22iik"); */
/*can explode$ to make a multifield/list or send/use as a string*/
VOID* upktostr()		/*retriev int # of type, ret as str*/
{
/*VOID *ret;*/
int inum[199];
float fnum[99];
double dnum[99];
char type,tstr[49]; /**t;*/
char str[255];
int i,nitem,info;
    nitem 	= (int)RtnLong(1);
    sprintf(tstr,"%s",(char *)RtnLexeme(2));   /*type = *RtnLexeme(2);*/
    type = tstr[0];
    if(type!='i' && type!='f' && type!='d')
    {	
	printf("[2nd arg=type:i or f or d]");
	return(AddSymbol(""));
    }
    switch(type)
    {
        case 'i' : info= pvm_upkint(inum,nitem,1); break;
        case 'f' : info= pvm_upkfloat(fnum,nitem,1); break;
        case 'd' : info= pvm_upkdouble(dnum,nitem,1); break;
    }
    printf("[upacking:nitem=%d,type=%c,info=%d:",nitem,type,info);
    strcpy(str,"");
    for(i=0; i<nitem; i++)
    {
	switch(type)
	{
	    case 'i' : sprintf(tstr," %d",inum[i]); break;
	    case 'f' : sprintf(tstr," %f",fnum[i]); break;
	    case 'd' : sprintf(tstr," %lf",dnum[i]); break;
	}
	strcat(str,tstr);
	/*printf("%s,",tstr);*/
	printf("%s, %s\n",tstr,str);
    }
    printf("]\n");
    return(AddSymb(str));
    /*t= strdup(str); ret=AddSymbol(t); free(t); return(ret);*/
}
/*---------------------------------------------------------RECV_*/
/*DefineFunction2("recv_",'i',PTIF recv_,"recv_","02iii"); */
int recv_()		/*blocking receive waits for a msg w/msgtag from tid*/
{			/*-1 is used as a wildcard*/
int tid=-1,msgtag=-1;
    if(RtnArgCount() >0) tid 	= (int)RtnLong(1);
    if(RtnArgCount() >1) msgtag = (int)RtnLong(2);
    return(pvm_recv(tid,msgtag));
}
/*---------------------------------------------------------NRECV_*/
/*DefineFunction2("nrecv_",'i',PTIF nrecv_,"nrecv_","02iii"); */
int nrecv_()			/*nonblocking receive returns bufid=0 if no msg*/
{				/*-1 is used as a wildcard*/
int tid=-1,msgtag=-1;
    if(RtnArgCount() >0) tid 	= (int)RtnLong(1);
    if(RtnArgCount() >1) msgtag = (int)RtnLong(2);
    return(pvm_nrecv(tid,msgtag));
}
/*---------------------------------------------------------TRECV*/
/*DefineFunction2("trecv",'i',PTIF trecv,"trecv","04iiiii"); */
int trecv()			/*recv w/ a timeout */
{				/*preferable to nrecv, because then can reroute*/
int tid=-1,msgtag=-1,sec=1,usec=0;
struct timeval tmout;
    if(RtnArgCount() >0) tid 	= (int)RtnLong(1);
    if(RtnArgCount() >1) msgtag = (int)RtnLong(2);
    if(RtnArgCount() >2) sec 	= (int)RtnLong(3);
    if(RtnArgCount() >3) usec 	= (int)RtnLong(4);
    tmout.tv_sec = sec;
    tmout.tv_usec = usec;
    return(pvm_trecv(tid,msgtag,&tmout));
}
/*=======================================================--signaling fncs*/
/*---------------------------------------------------------SENDSIG*/
/*DefineFunction2("sendsig",'i',PTIF sendsig,"sendsig","22iii"); */
int sendsig()			/*send signum to task w/tid*/
{
int tid,signum;
    tid 	= (int)RtnLong(1);
    signum 	= (int)RtnLong(2);
    return(pvm_sendsig(tid,signum));
}
/*---------------------------------------------------------NOTIFY*/
/*DefineFunction2("notify",'i',PTIF notify,"notify","44iii"); */
int notify()			/*notify if see (TaskExit/HostDelete/HostAdd)*/
{
int what,msgtag,cnt,tids[9];
    what 	= (int)RtnLong(1);
    msgtag 	= (int)RtnLong(2);
    cnt 	= (int)RtnLong(3);
    tids[0] = (int)RtnLong(4);
    return(pvm_notify(what,msgtag,cnt,tids));
}
/*=======================================================-- group fncs*/
/*---------------------------------------------------------JOINGROUP*/
/*DefineFunction2("joingroup",'i',PTIF joingroup,"joingroup","11kk");*/
int joingroup()
{
char group[40];
    sprintf(group,"%s",(char *)RtnLexeme(1));
    return(pvm_joingroup(group));  /*returns int instance# (inum)*/
}
/*---------------------------------------------------------LVGROUP*/
/*DefineFunction2("lvgroup",'i',PTIF lvgroup,"lvgroup","11kk");*/
int lvgroup()
{
char group[40];
    sprintf(group,"%s",(char *)RtnLexeme(1));
    return(pvm_lvgroup(group));  /*returns info*/
}
/*---------------------------------------------------------GETTID*/
/*DefineFunction2("gettid",'i',PTIF gettid,"gettid","22iki");*/
int gettid()
{
char group[40];
int inum;
    sprintf(group,"%s",(char *)RtnLexeme(1));
    inum 	= (int)RtnLong(2);
    return(pvm_gettid(group,inum));  /*returns tid*/
}
/*---------------------------------------------------------GETINST*/
/*DefineFunction2("getinst",'i',PTIF getinst,"getinst","22iki");*/
int getinst()
{
char group[40];
int tid;
    sprintf(group,"%s",(char *)RtnLexeme(1));
    tid 	= (int)RtnLong(2);
    return(pvm_getinst(group,tid));  /*returns inst*/
}
/*---------------------------------------------------------GSIZE*/
/*DefineFunction2("gsize",'i',PTIF gsize,"gsize","11kk");*/
int gsize()
{
char group[40];
    sprintf(group,"%s",(char *)RtnLexeme(1));
    return(pvm_gsize(group));  /*returns group size*/
}
/*---------------------------------------------------------BARRIER*/
/*DefineFunction2("barrier",'i',PTIF barrier,"barrier","22iki");*/
int barrier()  /*wait till count members call, not much used*/
{
char group[40];
int count;
    sprintf(group,"%s",(char *)RtnLexeme(1));
    count 	= (int)RtnLong(2);
    return(pvm_barrier(group,count));  /*returns info*/
}
/*---------------------------------------------------------BCAST*/
/*DefineFunction2("bcast",'i',PTIF bcast,"bcast","22iki");*/
int bcast()  
{
char group[40];
int msgtag=0;
    sprintf(group,"%s",(char *)RtnLexeme(1));
    if(RtnArgCount()>1) msgtag = (int)RtnLong(2);
    return(pvm_bcast(group,msgtag));  /*returns info*/
}
/*---------------------------------------------------------REDUCE*/
/*DefineFunction2("reduce",'i',PTIF reduce,"reduce","66ikiiiki");*/
int reduce()   /*this might have to run off the global buffers  */
{		/*so the type & number will also have to be chosen*/
char fnc[40],group[40];
int nitem,datatype,msgtag,root,info;
float dummy[2]; /*fix/(finish)*/
    sprintf(fnc,"%s",(char *)RtnLexeme(1));
    nitem 	= (int)RtnLong(2);
    datatype 	= (int)RtnLong(3);
    msgtag 	= (int)RtnLong(4);
    sprintf(group,"%s",(char *)RtnLexeme(5));
    root 	= (int)RtnLong(6);
    if(!strncasecmp(fnc,"Max",3)) 
	info = pvm_reduce(PvmMax,dummy,nitem,datatype,msgtag,group,root);  
    if(!strncasecmp(fnc,"Min",3)) 
	info = pvm_reduce(PvmMin,dummy,nitem,datatype,msgtag,group,root);  
    if(!strncasecmp(fnc,"Sum",3)) 
	info = pvm_reduce(PvmSum,dummy,nitem,datatype,msgtag,group,root);  
    if(!strncasecmp(fnc,"Prod",4)) 
	info = pvm_reduce(PvmProduct,dummy,nitem,datatype,msgtag,group,root);  
    return(info);
}
/*--------------------------------------------------------- */
/*--------------------------------------------------------- */
/*=======================================================-- end  PVM fncs*/
/*=========================================================get rid of these*/
/*---------------------------------------------------------PKFARRAY*/
/*DefineFunction2("pkfarray",'i',PTIF pkfarray,"pkfarray","22ixi"); */
int pkfarray()
{
int cnt;
float *num;
    num= (float *)RtnLong(1);
    cnt= (int)RtnLong(2);
    return(pvm_pkfloat(num,cnt,1));
}
/*---------------------------------------------------------UPKFARRAY*/
/*DefineFunction2("upkfarray",'x',PTIF upkfarray,"upkfarray","11ii"); */
VOID *upkfarray()
{
int cnt;
float *num=(float *)0;
    cnt= (int)RtnLong(1);
    pvm_upkfloat(num,cnt,1);
    return((VOID *)num);
}
/*---------------------------------------------------------*/
/*maybe transfer arrays 1slice at a time w/offset between C(==) & FORTRAN*/
/*have *VARA wrap some malloced array space instead of the global buf rtns*/
/*---------------------------------------------------------*/
/*could have a clips setable debug flag for the prints in the functions*/
/*---------------------------------------------------------*/
/*---------------------------------------------------------EOF*/
