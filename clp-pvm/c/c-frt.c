#define PTIF (int (*)(VOID_ARG))
#define VPTIF (void (*)(VOID_ARG))
/* DefineFunction2("DF2"",'i',PTIF DF2,"DF2","45iskuss"); */ 
int
DF2()
{
char c1, chr1[9] ,str1[99] ,str2[99] ,str3[99];
/*PTIF fncptr;  at the worst might have to give the return type &do a switch*/
int (*fncptr)();   /*might only be wrapping fnc that rtn ints anyway*/
    fncptr = PTIF get_ptr(3);
    if((int)fncptr < 999) printf("[fncptr=%d]",(int)fncptr); /*return(0);*/
    sprintf(str1,"%s",(char *)RtnLexeme(1));
    sprintf(chr1,"%s",(char *)RtnLexeme(2));
    sprintf(str2,"%s",(char *)RtnLexeme(4));
    c1= chr1[0];
    printf("[DefineFunction2 for:%s with type=%c]\n",str1,c1);
    if(RtnArgCount()>4)
    {
     sprintf(str3,"%s",(char *)RtnLexeme(5));
     DefineFunction2(str1,c1,PTIF fncptr,str2,str3); 
    }
    else DefineFunction(str1,c1,PTIF fncptr,str2); 
    return(1);
}
/*if this could be done interactively then a compiled model could print out
  a batch file that would desribe all the fnc(in C) directly as clips fncs
  -it might still be a good idea to have instances to call the fncs
   it would at least save putting ifs in, allowing for direct calling*/
/*problem is turning the cmndline version of the fnc into the fnc ptr
  don't think it will work.  the only way is if all the possible functions
  where compiled in  extern&all, in a big switch  -then there is the opt
  to make it a clips deffunction or if something is linked in use that
  ---could be done on arg types.. or vararg wrappers to Cfncs---??*/

  /*could have any obj files print out the fnc ptrs in a SUBROUTINE inst
  then DF2 could be called with this #, as part of a handler call
  -this might even be able to done w/ fortran code  w/out having to use f2c*/
  /*start including val_ptr slots in the subroutines and try DF2 w/ this #*/

/*can at least use these fnc ptrs for some basic in C array ops*/

/*Don't need to use DF2, can just call using the ptr to the fnc/sub &
  all the ptrs to the args,  (all wrapped in a fnc/sub clips inst)*/

/* DefineFunction2("cf0i"",'i',PTIF cf0i,"cf0i","11ii"); */ 
int
cf0i()
{
int (*fncptr)();   /*might only be wrapping fnc that rtn ints anyway*/
int i;
    fncptr = PTIF get_ptr(1);
    printf("[cf0i:calling %d]\n",(int)fncptr);
    i=fncptr();
    return(i);
}
/* DefineFunction2("cf0v"",'i',PTIF cf0v,"cf0v","11ii"); */ 
int
cf0v()
{
void (*fncptr)();   /*might only be wrapping fnc that rtn ints anyway*/
    fncptr = VPTIF get_ptr(1);
    printf("[cf0v:calling %d]\n",(int)fncptr);
    fncptr();
    return(1);
}
/*will want a version that can handle a arbitrary number of arg ptrs*/

/* (DF2 "tst" i tst "tst" "11ik"); */ 
/* (DF2 "srrf" i  #  "srrf" "00i"); */ 
int tst() {
char str1[99];
    sprintf(str1,"%s",(char *)RtnLexeme(1));
    printf("[test fnc tst can print out:%s]\n",str1);
    return(1);
}

extern int ftst_();

int ctst()
{
    printf("(ftst of FUNC (val_ptr %d))",(int)ftst_);
    fflush(stdout);
}

/* DefineFunction2("ftst",'i',PTIF ftst_,"ftst","00i"); */
/* DefineFunction2("ctst",'i',PTIF ctst,"ctst","00i"); */
/*----------------------------------------------------------------EOF*/
