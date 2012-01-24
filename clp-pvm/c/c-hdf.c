/*
;have taken HDF user-guide file: HDF3.2.4.asc and started to turn it into
;   clips wrappers for the HDF functions
|*/
|#include "hdf.h"
|/*
;for returning unkn types use:
;//DefineFunction2("Proj",'u',PTIF Proj,"Proj","2*uuk");
;VOID Proj(DATA_OBJECT_PTR rp)                   
;{ set_int(rp);  or  set_symb(rp);  or set_long(rp); }
;-for mf output (or input) could be done w/ a string   explode$/implode$
;if a seperate file include clips.h &
;might be including an acc_fncs.c 
|*/
|/*
|after the fortran & C examples might even put in a CLIPS example (when it works)
|Storing Rectangular Gridded Arrays of Scientific Data	4.1 March 1993
|4.1	NCSA HDF Calling Interfaces and Utilities
|Storing Rectangular Gridded Arrays of Scientific Data	4.1
|National Center for Supercomputing Applications March 1993
|Chapter 4	Storing Rectangular Gridded Arrays of Scientific Data
|Chapter Overview
|Scientific Datasets
|Reasons to Use Scientific Datasets
|Header File
|Writing Scientific Datasets to a File
|Writing Entire Arrays to an HDF File
|The “Set” Routines:  Preparing to Write an 
|SDS
|Writing an SDS to a File
|Writing Parts of an SDS
|Reading Scientific Datasets from a File
|Reading and Entire Array
|Getting Other Information about an SDS
|Reading Parts of an SDS
|Other SDS  Routines
|How SDS Routines Store and Convert 
|Scientific Data
|How HDF Normally Stores Arrays
|How HDF Normally Represents Numbers
|Backward Compatibility
|Sample Programs
|A FORTRAN Program
|A C Program
|
|Chapter Overview
|This chapter describes the routines that are available for storing 
|and retrieving scientific datasets.
|
|
|Scientific Datasets
|A Scientific Data Set (SDS) is an HDF set that stores rectangular 
|gridded arrays of data, together with information about the data. 
|Specifically, a SDS is a set of tags and associated information 
|about scientific data.  Assuming that a user of scientific data 
|often will want information about the data, an SDS might contain 
|the following information.  (The first three items are required in 
|every SDS; the rest are optional.)
|•	The actual data values.
|•	The number of dimensions (rank) and the size of each dimension 
|•	The number type of the data. 
|•	Scales to be used along the different dimensions when 
|interpreting or displaying the data
|•	Labels for all dimensions (each label can be thought of as the name of
|an independent variable) and for the data (the dependent variable)
|•	Units for all dimensions and for the data
|•	Format specifications to be used when displaying values for the 
|dimensions and for the data
|•	A range, attributing maximum and minimum values for the data set.
|•	Calibration information including an offset and scale factor.
|•	A fill value for representing missing data in a data set.
|•   The coordinate system to be used when interpreting or displaying the data
|Figure 4.1 shows a conceptual view of a sample scientific dataset. 
|The actual 2D array of data is only one element in the set. Other 
|elements include the number of dimensions (rank), the sizes of the 
|dimensions, identifying information about the data and axes, and 
|scales for the axes.
|Figure  4.1   HDF File with Scientific Dataset
|A file can contain many SDSs.  It can also contain other HDF 
|objects, such as raster image sets and annotations, together in the 
|same file with SDSs.
|
|The HDF library provides an SDS interface with routines for 
|storing and retrieving scientific data sets. This user interface lets 
|you (a) build an SDS and (b) extract data from an SDS. These 
|routines can be called  from C and FORTRAN programs that have 
|access to the library.  All routines are functions of type integer.
|
|Table 4.1 lists the C and FORTRAN names of SDS routines 
|currently contained in the HDF library. The following sections 
|provide descriptions and examples of these calling routines.
|
|
|Table 4.1	Scientific Dataset Routines in the HDF Library
|--------------------------------------------------------------
|C	FORTRAN
|Name	Name	Function
|
|DFSDadddata	dsadata	appends the data to the file, not 
|overwriting other file contents.
|
|DFSDputdata	dspdata	writes the data to a new file, 
|truncating old file if it exists.
|
|DFSDsetNT	dssnt	tells which number type is to be 
|used for next DFSDadddata or 
|DFSDputdata
|
|DFSDsetdims	dssdims	sets the rank and dimension  
|sizes for succeeding  SDSs
|
|DFSDclear	dsclear	clears all possible set values.
|
|DFSDsetdimstrs	dssdist	sets label, unit,  and format 
|specifications for a dimension 
|and its scale.
|
|DFSDsetdimscale	dssdisc	sets the scale for a dimension.
|
|
|DFSDsetdatastrs	dissdast	sets label, unit, and format 
|specifications for the data.
|
|DFSDsetlengths	dsslens	sets maximum lengths for 
|strings that will hold labels, 
|units, formats, and the name of 
|the coordinate system.
|
|DFSDsetrange	dssrang	sets maximum and minimum 
|data values.
|
|DFSDsetcal	dsscal	sets calibration information 
|associated with data
|
|DFSDstartslice	dssslc	prepares system to write part of 
|a dataset to a file.
|
|DFSDputslice	dspslc	writes part of a dataset to a file.
|
|DFSDendslice	dseslc	indicates write completion for 
|all parts of a dataset.
|
|DFSDgetdata	dsgdata	reads the next dataset in the 
|file.
|
|DFSDgetNT	dsgnt	gets the number type of data 
|that will be read with 
|DFSDgetdata.
|
|
|DFSDgetdims	dsgdims	gets the number of dimensions 
|of the dataset and the sizes of 
|the dimensions for the next SDS 
|in the file.
|
|DFSDgetdimstrs	dsgdist	reads the label, unit, and format 
|for a dimension and its scale.
|
|DFSDgetdimscale	dsgdisc	reads the scale for a dimension.
|
|DFSDgetdatastrs	dsgdast	reads the label, unit, and format  
|specification for the data.
|
|DFSDgetrange	dsgrang	reads the maximum and 
|minimum values.
|
|DFSDgetcal	dsgcal	gets calibration information 
|associated with data
|
|DFSDreadref	dsrref	sets the reference number of the 
|SDS to get next
|
|DFSDrestart	dsfirst	sets the next get command to 
|read from the first SDS in the 
|file, rather than the next.
|
|DFSDgetslice	dsgslc	reads part of a dataset.
|
|DFSDnumber		returns the number of SDSs in 
|the file.
|
|DFSDlastref	dslref	returns the value of the last 
|reference number of an SDS read 
|from or written to the file.
|
|DFSDpre32sdg	dsp32sd	tests whether the SDS with a 
|given ref was created by an HDF 
|library that precedes HDF3.2.
|
|
|Header File
|
|The header file dfsd.h contains the declarations and definitions 
|that are used by the routines listed here. This file can, if needed, 
|be included with your C source code.
|
|
|Writing Scientific Datasets to a File
|
|SDS information is written to a file in two steps. The first involves 
|execution of a series of "set" calls, which put information about 
|the actual data into a special structure in primary memory. To set 
|information associated with an SDS, you usually first invoke 
|DFSDsetdims, then execute whatever set routines you need. If you 
|do not wish to specify a certain item, you need not invoke its 
|corresponding set call.
|
|The second phase involves actually writing the data to a file, along 
|with the information that has been set. You can write data arrays 
|to an HDF file in two basic ways: 
|
|1.	by writing out the entire array with a single call 
|(DFSDadddata or DFSDputdata), and 
|2.	by writing the array in smaller pieces, or "slices." 
|
|Method #1 is covered in the section "Writing Entire Arrays to an 
|HDF file".  Method #2 is covered in the section "Writing Parts of a 
|Scientific Dataset".
|
|In general, you perform these same two steps for each dataset you 
|want to write to your file. However, it is not usually necessary to 
|perform all set calls for every dataset you wish to write out. For 
|example, if the rank and dimensions of all datasets are exactly the 
|same, you only have to call the routine DFSDsetdims before writing 
|out the first set. Thereafter, you need only call 
|DFSDputdata/DFSDadddata to write out the different data sets. The 
|HDF software remembers the rank and dimension values and 
|associates them with all subsequent data arrays that are written to 
|the same file, unless you change them.
|
|In other words, once an item has been set, it does not normally go 
|away even after a DFSDputdata or DFSDadddata call. The associated 
|information is only cleared if the new dataset  has different rank 
|or dimensions, or if DFSDclear is called. (The only exception to 
|this are that the values set by DFSDsetrange and DFSDsetcal are 
|cleared after they are written to a file.)
|
|A note about how to describe the dimensions of array:  Most 
|current Fortran compilers read/write data into/from an array in 
|column major order.  (See the section "How SDS Routines Store and 
|Convert Scientific Data" for details.)  For a given data stream, if 
|the order of the dimensions of the Fortran array is the reverse of 
|that of the C array, then the HDF files written by C or Fortran 
|programs will be the same.
|
|All of the Fortran examples in this chapter use arrays with 
|reversed dimensions from their counterparts in C. For instance, if 
|a 3D Fortran array is declared as fdata(500,800,10), the 
|corresponding  C array would be declared as cdata[10][800][500]. 
|The data can be imagined as 10 planes, each plane having 800 
|lines and each line having 500 points. We would denote this array 
|as a 10x800x500 array. The size of the first dimension of fdata is 
|500 while that of cdata is 10. The size of the third dimension of 
|fdata is 10 while cdata is 500. The same interpretation applies for 
|2D arrays.
|
|Writing Entire Arrays to an HDF File
|-hdf4 will have ansi declares*/
|/*
|DFSDadddata
|FORTRAN: INTEGER FUNCTION dsadata(filename, rank, dimsizes, data)
|CHARACTER*(*) filename	-	name of file to store SDS in
|INTEGER	 rank	-	number of dimensions of data 
|			array to be stored
|INTEGER	dimsizes(rank)	-	array that holds sizes of 
|			dimensions of the data array
|<valid type>	data(*)	-	array holding data to be stored
|
;====--------------------------------------------====HDF_DATA====
;C: int DFSDadddata(filename, rank, dimsizes, data)     ='a'
;& C: int DFSDputdata(filename, rank, dimsizes, data)   ='w'
;& C: int DFSDgetdata(filename, rank, dimsizes, data)   ='r'
|might have to call w/ funcall if dimsizes is sent a a mf
|was "5*ikkixi"  before realized could get rank from dimsizes list
;DefineFunction2("hdf_data",'i',PTIF hdf_data,"hdf_data","4*ikkxi"); 
;input: filename, 'a'or'w'or'r', data-ptr, dimsizes (rank inferred)
|====--------------------------------------------====ADDDATA====now is
====--------------------------------------------====HDF_DATA====*/
int hdf_data()
{
char	filename[80];	/*	name of file to store SDS in char *filename; */
int	rank;	/*	number of dimensions of data array to be stored */
int32	dimsizes[5];   /*array that holds sizes of dimensions of the data array
			int32	dimsizes[];*/
void *data;	      /*array holding data to be stored <valid type> *data;*/
int *ip;
float *fp;
double *dp;
char type;
int i,rep,ret,itype;

    get_str(1,filename);
    /*if can get the type after being set once don't need to send again*/
    rep = get_char(2);
    /*replace type w/ 'a' or 'p' for add or put*/
    rank = get_ac() - 4;
    data = (void *)get_ptr(3);
    get_long_array(4,dimsizes);

   /*could switch on itype 
   do you ever want to put data other than type set -NO
   itype = (int)DFSDgetNT();
   switch(itype) 
   {
    case DFNT_INT8    :    ip = (int *)data;
    if(rep=='w')  ret = DFSDputdata(filename, rank, dimsizes, ip); else
    if(rep=='r')  ret = DFSDgetdata(filename, rank, dimsizes, ip);
    else          ret = DFSDadddata(filename, rank, dimsizes, ip); break;
    case DFNT_FLOAT32 :    fp = (float *)data;
    if(rep=='w')  ret = DFSDputdata(filename, rank, dimsizes, fp); else
    if(rep=='r')  ret = DFSDgetdata(filename, rank, dimsizes, fp);
    else          ret = DFSDadddata(filename, rank, dimsizes, fp); break;
    case DFNT_FLOAT64 :    dp = (double *)data;
    if(rep=='w')  ret = DFSDputdata(filename, rank, dimsizes, dp); else
    if(rep=='r')  ret = DFSDgetdata(filename, rank, dimsizes, dp);
    else          ret = DFSDadddata(filename, rank, dimsizes, dp); break;
   }*/

   if(rep=='w')  ret = DFSDputdata(filename, rank, dimsizes, (char *)data); else
   if(rep=='r')  ret = DFSDgetdata(filename, rank, dimsizes, (char *)data);
   else          ret = DFSDadddata(filename, rank, dimsizes, (char *)data);
   return(ret);
}
/*====--------------------------------------------====HDF_DATA====
;Purpose:  To add to an HDF file the data in the multidimensional 
;array data, as well as all other information that has previously 
;been set (see "Set Routines" below.)
;-the 'w' option overwrites & the 'r' option reads in data
|
|Returns:  0 on success; -1 on failure.
|
|The argument rank gives the number of dimensions of the array 
|data.  The array dimsizes contains the sizes of the dimensions.  
|The length of the array dimsizes is rank.
|
|The array "data" can be of any valid type (see discussion of 
|DFSDsetNT below).  If no number type has been set by DFSDsetNT, 
|it is assumed that the data is of type float32.
|
|The invocation of DFSDadddata triggers the writing of the entire 
|SDS.  That is, when DFSDadddata is called, all information that has 
|been set by the "DFSDset..." calls (covered below) is written to the 
|file, along with the data array itself.
|
|Example:  Writing an Array as a Scientific Dataset.  
|This example shows a call that stores a 5x20x5000 array of type 
|float32s in an SDS . The SDS is to be stored in a file called 
|'myfile.hdf', with no labels, scales, or other information.
|
|FORTRAN:
|      INTEGER	dsadata
|      REAL	points(5000,20,5)
|      INTEGER	dims(3), ret
|      ...
|      dims(1) = 5000
|      dims(2) = 20
|      dims(3) = 5
|      
|      ret = dsadata('myfile.hdf',3, dims, points)
|      ...
|
|C: #include "hdf.h"
|    ...
|    float32	points[5][20][5000];
|    int	dims[3];
|    ...
|    dims(0) = 5;
|    dims(1) = 20;
|    dims(2) = 5000;
|    
|    DFSDadddata("myfile.hdf",3, dims, points);
|    ...
|
|CLIPS: (hdf_data "myfile.hdf" 'a' (get-ptr [points]) 5 20 5000)
|or     (hdf_data "myfile.hdf" 'a' (get-ptr [points]) (get-dims [points]))
|or     (send [points] hdf_data "myfile.hdf" 'a')
|or     (send [points] hdf_data  myfile.hdf   a)
|where  (make-instance points of ARRAY (dims 5 20 5000)) was run
|
|DFSDputdata
|FORTRAN: INTEGER FUNCTION dspdata(filename, rank, dimsizes, data)
|CHARACTER*(*)	filename	-	name of file to store SDS in
|INTEGER	rank	-	number of dimensions of data 
|			array to be stored
|INTEGER	dimsizes(rank)	-	array that holds sizes of 
|				dimensions
|<valid type>	data(*)	-	array holding data to be stored
|
;C: int DFSDputdata(filename, rank, dimsizes, data)
|char	*filename;	/-	name of file to store SDS in -/
|int	rank;	/-	number of dimensions of data array to 
|			be stored -/
|int32	dimsizes[];	/-	array that holds sizes of dimensions -/
|<valid type> *data;	/-	array holding data to be stored -/
|
;Purpose:  DFSDputdata does the same thing that DFSDadddata does, 
;except that it overwrites the previous contents of the file, whereas 
;DFSDadddata appends the scientific dataset to the file.
|
|Returns:  0 on success; -1 on failure.
|
|NOTE: DFSDputdata destroys whatever was in the HDF file before 
|it was called.  Use it with caution.
|
|
|The "Set" Routines: Preparing to Write an SDS
|---------------------------------------------
|
|DFSDsetNT
|FORTRAN: INTEGER FUNCTION dssnt(numbertype)
|INTEGER*4	numbertype	- number type of data to be written
|
;====--------------------------------------------====HDF_NT====
;DefineFunction2("hdf_nt",'i',PTIF hdf_nt,"hdf_nt","01kk"); 
;C: int DFSDsetNT(numbertype) &  int DFSDgetNT(numbertype)
;input: type char
;output: side effect set the hdf numbertype, or ret the#
====--------------------------------------------====HDF_NT====*/
int hdf_nt()
{
int32	numbertype;	/* number type of data to be written*/
char type;
int ret;
    if(get_ac() > 1) 
    {
     type = get_char(1);
     switch(type)
     {
	/*  8-bit signed integer	DFNT_INT8	20
	    16-bit signed integer	DFNT_INT16	22
	    32-bit float		DFNT_FLOAT32 	5
	    64-bit float		DFNT_FLOAT64	6 */
	  case 'i' :  numbertype = DFNT_INT8;   break;
	  case 'l' :  numbertype = DFNT_INT16;   break;
	  case 'f' :  numbertype = DFNT_FLOAT32;   break;
	  case 'd' :  numbertype = DFNT_FLOAT64;   break;
     }
     ret=DFSDsetNT(numbertype);
    }
    else ret=(int)DFSDgetNT(&numbertype);
    return(ret);
}
/*====--------------------------------------------====HDF_NT====
;Purpose:  To set the number type to be used for data to be 
;written out by the next DFSDadddata or DFSDputdata..
|
|Returns:  0 on success; -1 on failure.
|
|DFSDsetNT must be called if a number type other than float32 is 
|to be stored.  DFSDsetNT and DFSDsetdims can be called in any 
|order, but they should be called before any other  "DFSDset" 
|functions and before DFSDputdata or DFSDadddata.
|
|Valid parameter values for DFSDsetNT (e.g. DFNT_INT8) are of the 
|general form "DFNT_<numbertype>", all capital letters.  The 
|following table gives the number types currently supported.  If 
|you include the file hdf.h in your program, you can use the 
|symbolic names of the number types.  Number types that are 
|available in only the C-interface are marked with and asterisk (*).
|	symbolic 
|type	name	value
|32-bit float	DFNT_FLOAT32 	5
|64-bit float	DFNT_FLOAT64	6
|8-bit signed integer	DFNT_INT8	20
|8-bit unsigned integer*	DFNT_UINT8	21
|16-bit signed integer	DFNT_INT16	22
|16-bit unsigned integer*	DFNT_UINT16	23
|32-bit signed integer	DFNT_INT32	24
|32-bit unsigned integer*	DFNT_UINT32	25
|
|For other information how HDF stored numbers, see the section 
|"How HDF Stores Numbers in SDSs" elsewhere.
|
|Example.  Assuming that DFNT_INT8 has been defined and 
|i8data is an array with 8-bit integer data, the following code 
|fragments write out 8-bit integers to an SDS.
|
|FORTRAN: iret  = dssnt(DFNT_INT8)
|         iret = dsadata('myfile.hdf', rank, dims, i8data)
|
|C:    DFSDsetNT(DFNT_INT8);
|      DFSDadddata("myfile.hdf", rank, dims, i8data);
|
|CLIPS: (hdf_nt l)
|       (send [i8data] hdf_data myfile.hdf a)
|
|DFSDsetdims
|FORTRAN: INTEGER FUNCTION dssdims(rank, dimsizes)
|
|INTEGER	rank	-	number of dimensions
|INTEGER	dimsizes(rank)	-	dimensions of the SDS
|
;====--------------------------------------------===HDF_DIMS=====
;C: int DFSDsetdims(rank,dimsizes)& DFSDgetdims(filename,rank,dimsizes,maxrank)
;DefineFunction2(hdf_dims",'x',PTIF hdf_dims,"hdf_dims","2*iki"); 
;input: filename or "set",  rank,  then dimensizes or maxrank
;input: filename, rank, maxrank        or "set"  and the dimsizes
;output: print-str/mf of the dimsizes  or bool
====--------------------------------------------===HDF_DIMS=====*/
int hdf_dims()
{
int	rank;	       /* number of dimensions*/
int32	dimsizes[5];   /*dimsizes[];	dimensions of the SDS */
int i,ret,maxrank= 5;
char method[22],filename[22];

   get_str(1,method);
   if(ISMETH(method,"set",1))
   {
       strcpy(filename,method);
       rank = get_int(2);
       maxrank = get_int(3);
       ret =  DFSDgetdims(filename, &rank, dimsizes, maxrank);
       for(i=0; i<maxrank; i++) printf("[%d]",dimsizes[i]);
   }
   else  /* 'set' was sent */
   {
       get_long_array(2,dimsizes);
       rank = get_ac() - 2;
       ret = DFSDsetdims(rank, dimsizes);
   }
   return(ret);
}
/*====--------------------------------------------===HDF_DIMS=====
;Purpose:  To set the rank and dimension sizes for subsequent 
;scientific datasets that are written to the file.
|
|Returns:  0 on success; -1 on failure.
|
|This routine must be called before calling any other set routines, 
|except DFSDsetNT.  DFSDsetdims need not be called if other set 
|routines are not called and the correct dimensions are supplied in 
|DFSDputdata or DFSDadddata.
|
|If rank or dimension sizes change, all previous set calls are 
|cleared.
|
|Examples of the use of DFSDsetdims can be found in connection 
|with other "set" routines described below.
|
|
|DFSDclear
|FORTRAN: INTEGER FUNCTION dsclear()
|
;====--------------------------------------------===HDF_CLEAR=====
;C: int DFSDclear()
;DefineFunction2(hdf_clear",'i',PTIF hdf_clear,"hdf_clear","00i"); 
====--------------------------------------------===HDF_CLEAR=====*/
int hdf_clear()
{
  return(DFSDclear());
}
/*====--------------------------------------------===HDF_CLEAR=====
;Purpose:  To cause all possible 'set' values to be cleared.
|
|Returns:  0 on success; -1 on failure.
|
|After a call to DFSDclear, numbertype, rank, dimensions and other 
|values that were set by the "set" calls will not be written unless 
|they have been set again.
|
|
|DFSDsetdimstrs
|FORTRAN: INTEGER FUNCTION dssdist(dim, label, unit, format)
|INTEGER	dim		-	dimension this label, unit and format refer to
|CHARACTER*(*)	label	-	label that describes this dimension
|CHARACTER*(*)	unit	-	unit to be used with this dimension
|CHARACTER*(*)	format	-	format to be used in displaying scale 
|				for this dimension
|
;====--------------------------------------------===HDF_STRS=====
;C: int DFSDsetdimstrs(dim, label, unit, format) & get version
;  & C: int DFSDsetdatastrs(label, unit, format, coordsys) & get version
;DefineFunction2(hdf_strs",'i',PTIF hdf_strs,"hdf_strs","14iikkk"); 
;input: dim label unit format or dim (if 5th str(& dim=0 setdatastrs else get))
;output:side-eff of setting   or get those strings back   (get datastrs)
====--------------------------------------------===HDF_STRS=====*/
int hdf_strs()
{
int	dim;	/*	dimension this label, unit and format refer to*/
char	label[80];    /* *label;  label that describes this dimension*/
char	unit[80];     /* *unit;	  unit to be used with this dimension*/
char	format[80];   /* *format; format to be used to display scale */
char	coordsys[80]; /* *coordsys; coordinate system, for datastrs only*/
int ret;

   dim = get_int(1);
   /*could have it if send in a - dim then it gets rather than sets*/
  if(get_ac()>3)
  {
     get_str(2,label);
     get_str(3,label);
     get_str(4,format);
     if(get_ac()>4)
     {
       get_str(5,coordsys);
       ret=DFSDsetdatastrs(label, unit, format, coordsys);
     }
     else
     ret=DFSDsetdimstrs(dim, label, unit, format);
  }
  else
  {
    if(dim>0)
    {
     ret=DFSDgetdimstrs(dim, label, unit, format);
     printf("[got dimstr%d: label=%s unit=%s format=%s]",dim,label,unit,format);
    } else
    {
     ret=DFSDgetdatastrs(label, unit, format, coordsys);
     printf("[got datastrs: label=%s unit=%s format=%s coordsys=%s]",
			    label,unit,format,coordsys);
    }
     /*latter will return in a mf, at least a str which can be explode$d*/
  }
   return(ret);
}
/*====--------------------------------------------===HDF_STRS=====
;Purpose:  To set the items corresponding to dimension dim that 
;are to be stored as strings in the SDS, namely label, unit, and format.
|
|Returns:  0 on success; -1 on failure.
|
|Note:  In both C and Fortran programs, dim=1 for the first 
|dimension; dim=2, for the second dimension, and so forth.
|
|Example:  Writing a Scientific Dataset with Dimension 
|Information.  In this example a 200x300 data array is written 
|to a file called 'myfile.hdf', together with label, unit, and format 
|information about each dimension.
|
|FORTRAN: INTEGER   dssdims, dssdist, dsadata
|      REAL      press1(300,200)
|      INTEGER   dims(2), ret
|      ...
|      dims(1) = 300
|      dims(2) = 200
|      
|      ret = dssdims(2, dims)
|      ret = dssdist(1,'position','cm','F10.2')
|      ret = dssdist(2,'height','m','F10.3')
|      ret = dsadata('myfile.hdf', 2, dims, press1)
|      ...
|
|C: float32  press1[200][300];
|    int       dims[2];
|    ...
|    dims[0] = 200;
|    dims[1] = 300;
|
|    DFSDsetdims(2, dims);
|    DFSDsetdimstrs(1,"height", "m", "F10.3");
|    DFSDsetdimstrs(2,"position", "cm", "F10.2");
|    DFSDadddata("myfile.hdf", 2, dims, press1);
|
|CLIPS: (hdf_dims set 200 300)
|       (hdf_strs 1 height m F10.3)
|       (hdf_strs 2 position cm F10.2)
|       (send [press1] hdf_data  myfile.hdf a)
|
|DFSDsetdimscale
|FORTRAN: INTEGER FUNCTION dssdisc(dim, dimsize, scale)
|INTEGER	dim	-	dimension this scale goes with
|INTEGER	dimsize	-	size of scale
|<same type as data> scale(dimsize)	-	the scale
|
;====--------------------------------------------===HDF_DIMSCALE=====
;C: int DFSDsetdimscale(dim, dimsize, scale)
;DefineFunction2(hdf_dimscale",'i',PTIF hdf_dimscale,"hdf_dimscale","13ii"); 
====--------------------------------------------===HDF_DIMSCALE=====*/
int hdf_dimscale()
{
int	dim;		/* dimension this scale corresponds to */
int32	dimsize;	/* size of scale */
void	*scale;		/* the scale */
int ret;
    dim = get_int(1);
    dimsize = (int32)get_int(2);
    scale   = (void *)get_ptr(3);
    ret = DFSDsetdimscale(dim, dimsize, scale);
    /*might try if -min then do a get
      might take/return the scale#s seperately  rather than in an array
      unless there is actually a scale array inst*/
    return(ret);
}
/*====--------------------------------------------===HDF_DIMSCALE=====
;Purpose:  To set the scale corresponding to dimension dim by 
;taking it from the array scale.
|
|Returns:  0 on success; -1 on failure.
|
|A scale is a 1D array whose values describe reference points along 
|one of the dimensions of the SDS data.  For example, a 2D SDS 
|representing points on a map could have two scales, one 
|representing points of latitude, and the other points of longitude..
|
|Note:  In both C and Fortran programs, dim=1 for the first 
|dimension; dim=2, for the second dimension, and so forth.
|
|Example:  Writing a Scientific Dataset with Dimension 
|Scales.  In this example a 200 x 300 data array is written to a 
|file called 'myfile.hdf', together with scales for each dimension.  
|It is assumed that the arrays xscale and yscale have been assigned 
|values that define the corresponding scales.
|
|FORTRAN: INTEGER   dssdims, dssdisc, dsadata
|      REAL      press1(300,200)
|      INTEGER   dims(2), ret
|      REAL      latscale(200), lngscale(300)
|      ...
|      dims(1) = 300
|      dims(2) = 200
|      
|      ret = dssdims(2, dims)
|      ret = dssdisc(1, dims(1), lngscale)
|      ret = dssdisc(2, dims(2), latscale)
|      ret = dsadata('myfile.hdf', 2, dims, press1)
|      ...
|C: float32  press1[200][300];
|    float32   latscale[200], lngscale[300];
|    int      dims[2];
|    ...
|    dims[0] = 200;
|    dims[1] = 300;
|
|    DFSDsetdims(2, dims);
|    DFSDsetdimscale(1, dims[0], latscale);
|    DFSDsetdimscale(2, dims[1], lngscale);
|    DFSDadddata("myfile.hdf", 2, dims, press1);
|    ...
|
|CLIPS: (hdf_dims 200 300)
|       (hdf_dimsale 1 200 (get-ptr [latscale]))
|       (hdf_dimsale 2 300 (get-ptr [lngscale]))
|       (send [press1] hdf_data  myfile.hdf a)
|or the hdf_data hndlr will check if there are any instances in a scale slot&exec
|the hdf_data handler could also do the hdf_dims call, so just the last line
|
|DFSDsetdatastrs
|FORTRAN: INTEGER FUNCTION dssdast(label,unit,format,coordsys)
|CHARACTER*(*)  label	-	label that describes the data
|CHARACTER*(*)  unit	-	unit to be used with the data
|CHARACTER*(*)  format	-	format to be used in displaying data
|CHARACTER*(*)  coordsys	-	coordinate system
|
;C: int DFSDsetdatastrs(label, unit, format, coordsys)
;trying to integreate w/ hdf_dimstrs  then call it hdf_strs
;could do it if give rank of 0 &/or when extra string is sent
|char	*label;	/-	label that describes the data -/
|char	*unit;	/-	unit to be used with the data -/
|char	*format;	/-	format to be used in displaying the data -/
|char	*coordsys;	/-	coordinate system -/
|
;Purpose:  To set the items corresponding to the data that are to be stored as
;strings in the SDS, namely label, unit, format, & coordsys (coordinate system).
|
|Returns:  0 on success; -1 on failure.
|
|Note:  HDF writes strings to an HDF file with a terminator 
|appended at the end. The number of bytes in the HDF file is 1 byte 
|more than the number of ASCII characters in the string. 
|
|Example:  Writing a Scientific Dataset with Data 
|Information.  In this example a 200 x 300 data array is written 
|to a file called 'myfile.hdf', together with label, unit, and format 
|information about the data.  In this example we assume that the 
|coordsys parameter is of no interest to the user, so the empty 
|string (' ') is given as the fourth argument to DFSDsetdatastrs.
|
|FORTRAN: INTEGER   dssdims, dssdast, dsadata
|      REAL      press1(300,200)
|      INTEGER   dims(2), ret
|      ...
|      dims(1) = 300
|      dims(2) = 200
|      
|      ret = dssdims(2, dims)
|      ret = dssdast('pressure 1','Pascals','E15.9','')
|      ret = dsadata('myfile.hdf', 2, dims, press1)
|      ...
|
|C: float32  press1[200][300];
|    int      dims[2];
|    ...
|    dims[0] = 200;
|    dims[1] = 300;
|
|    DFSDsetdims(2, dims);
|    DFSDsetdatastrs("pressure 1","Pascals","E15.9","");
|    DFSDadddata("myfile.hdf", 2, dims, press1);
|    ...
|
|CLIPS: (hdf_dims 200 300)
|       (hdf_strs 0 "pressure 1" Pascals E15.9 "")  ;the # is ignored
|       (send [press1] hdf_data  myfile.hdf a)
|
|
|DFSDsetlengths
|FORTRAN: INTEGER FUNCTION dsslens(maxlen_label, maxlen_unit,maxlen_format, maxlen_coordsys)
|INTEGER	maxlen_label	-	max length of any label
|INTEGER	maxlen_unit	-	max length of any unit
|INTEGER	maxlen_format	-	max length of any format
|INTEGER	maxlen_coordsys	-	max length of any coordsys
|
|
;====--------------------------------------------===HDF_SETLENGTHS=====
;C: int DFSDsetlengths(maxlen_label, maxlen_unit,maxlen_format, maxlen_coordsys)
;DefineFunction2(hdf_setlengths",'i',PTIF hdf_setlengths,"hdf_setlengths","44iiiii"); 
====--------------------------------------------===HDF_SETLENGTHS=====*/
int hdf_setlengths()
{
int	maxlen_label;	/* max length of any label*/
int	maxlen_unit;	/* max length of any unit*/
int	maxlen_format;	/* max length of any format*/
int	maxlen_coordsys;	/* max length of any coordsys */
int     ret;
    maxlen_label = get_int(1);
    maxlen_unit = get_int(2);
    maxlen_format = get_int(3);
    maxlen_coordsys = get_int(4);
   ret=DFSDsetlengths(maxlen_label, maxlen_unit,maxlen_format, maxlen_coordsys);
    return(ret);
}
/*====--------------------------------------------===HDF_SETLENGTHS=====
;Purpose: To set, optionally, the maximum lengths for the strings that will hold
;labels, units, formats, and the name of the coordinate system.
|
|Returns:  0 on success; -1 on failure.
|
|These lengths are used by the routines DFSDgetdimstrs and 
|DFSDgetdatastrs to determine the maximum lengths of strings that 
|they get from the file.
|
|Normally, DFSDsetlengths is not needed. If it is not called, default 
|maximum lengths of 255 are used for all strings.
|
|
|DFSDsetrange1
|FORTRAN: INTEGER FUNCTION dssrang(max, min)
|<same type as data>	max	- high value in range
|<same type as data>	min	- low value in range
|
;====--------------------------------------------===HDF_RANGE=====
;C: int DFSDsetrange(pmax, pmin)  & get version
;input: min & max   or nothing
;output;  set range or get range
;DefineFunction2(hdf_range",'i',PTIF hdf_range,"hdf_range","02nnn"); 
====--------------------------------------------===HDF_RANGE=====*/
int hdf_range()
{
void	*pmax;	/*	pointer to the high value in range */
void	*pmin;	/*	pointer to the low value in range */
float f1,f2;
int     ret;
    if(get_ac()>1)
    {
      f1 = get_float(1);
      f2 = get_float(2);
      /*determine NT & set pmax/min accordingly 
      switch(DFSDgetNT())
      {
	case 'DFNT_INT8'    :  
	ret = DFSDsetrange(&((int)f1), &((int)f2));		break
	case 'DFNT_FLOAT32' :  
	ret = DFSDsetrange(&f1, &f2);				break;
	case 'DFNT_FLOAT64' :  
	ret = DFSDsetrange(&((double)f1), &((double)f2));	break;
      }*/
	ret = DFSDsetrange((void *)&f1, (void *)&f2);
    }
    else
    {
      ret = DFSDgetrange((char *)pmax, (char *)pmin);
      /*printf("[got range %f %f]",(float)*pmax,(float)*pmin);  fix*/
      /*latter will return as a mf or as a 2part array
	array might be easier for putting back into other fncs?*/
    }
    return(ret);
}
/*====--------------------------------------------===HDF_RANGE=====
;Purpose:  To set maximum and minimum data values.
|
|Returns:  0 on success; -1 on failure.
|
|Since the max/min values are supposed to relate to the data itself, 
|it is assumed that the type of max/min is the same as the type of 
|the data.  Note that in the C version of DFSDsetrange the arguments 
|are pointers, rather than simple variables, whereas in the 
|FORTRAN version they are simple variables of the same type as 
|the data array.
|
|This routine does not compute the maximum and minimum values. 
|It merely stores the values it is given.
|
|NOTE:  When the maximum and minimum values are written to a 
|file, the HDF element that holds these values is cleared, because it 
|is assumed that subsequent datasets will have different values for 
|max and min.
|
|
|Example.  In this example 16-bit data is written to an HDF file.   
|Notice that max and min must be the same data type as the SDS array 
|data.
|
|FORTRAN: integer*2    max, min, data(100,100)
|  ...
|  ret = dssdims(rank, dims)
|  ret = dssrang(max, min)
|  ret = dsadata('myfile.hdf', rank, dims, data)
|
|C:
|  int16 max, min, data[100][100];  
|   ...
|  DFSDsetdims(rank, dims);
|  DFSDsetrange((void *)&max, (void *)&min);
|  DFSDadddata("myfile.hdf", rank, dims, data);
|
|
|DFSDsetcal
|FORTRAN: INTEGER FUNCTION dsscal(cal, cal_err, ioff, ioff_err, cal_type)
|real*8	cal	- calibration factor
|real*8	cal_err	- calibration error (tolerance)
|real*8	ioff	- uncalibrated offset
|real*8	ioff_err	- uncalibrated offset error (tolerance)
|integer*4	cal_type	- number type of uncalibrated data
|
|
;====--------------------------------------------===HDF_CAL=====
;C: int DFSDsetcal(cal, cal_err, ioff, ioff_err, cal_type) & get version
;DefineFunction2(hdf_cal",'i',PTIF hdf_cal,"hdf_cal","05nnnnni"); 
;input: cal fact/err  uncal off/offerr #type
====--------------------------------------------===HDF_CAL=====*/
int hdf_cal()
{
float64	cal;		/* calibration factor */
float64	cal_err;	/* calibration error */
float64	ioff;		/* uncalibrated offset */
float64	ioff_err;	/* uncalibrated offset error */
int32	cal_type;	/* number type of uncalibrated data */
int ret;
    cal      = get_float(1);
    cal_err  = get_float(2);
    ioff     = get_float(3);
    ioff_err = get_float(4);
    cal_type = get_long(5);
    if(get_ac()>0) ret=DFSDsetcal(cal, cal_err, ioff, ioff_err, cal_type);
    else {         ret=DFSDgetcal(&cal, &cal_err, &ioff, &ioff_err, &cal_type);
 printf("[cal=%f %f ioff=%f %f t=%d]",cal, cal_err, ioff, ioff_err, cal_type);}
    return(ret);
}
/*====--------------------------------------------===HDF_CAL=====
;Purpose:  To provide calibration information about the data in an SDS.
|
|Returns:  0 on success; -1 on failure.
|
|This routine attaches to the SDS a record containing four 64-bit 
|floating point values followed by a 32-bit integer, to be 
|interpreted as follows:
|
|      cal       calibration factor
|      cal_err   calibration error
|      ioff      uncalibrated offset
|      ioff_err  uncalibrated offset error
|      cal_type  numbertype of uncalibrated data
|
|The relationship between a value 'iy' stored in an SDS and the 
|actual value 'y' is defined as:
|
|    y = cal * (iy - ioff)
|
|The variable ioff_err contains a potential error of ioff, and 
|cal_err contains  a potential error of cal.  
|
|Currently the calibration record is provided for information only.   
|The SDS interface performs no operations on the data based on the 
|calibration  tag.
|
|DFSDsetcal works like other SDS 'set' routines, with one exception: 
|calibration information is automatically cleared after call to  
|DFSDputdata or DFSDadddata.  Hence, DFSDsetcal must be called 
|anew for each  data set that is to be written.
|
|Example. In the following example a 100x100 array of 16-bit 
|integers is stored in an  SDS, together with calibration information 
|indicating that the values in the  array are intended to be 
|converted to 32-bit floats, and each value should be  multiplied by 
|10.0 then added to 16.75.
|
|FORTRAN:
|      integer dsadata, dssnt, dsscal
|      integer DFNT_FLOAT32, DFNT_INT16
|      real*8 cal, cale,  ioff,  ioffe
|      integer*4 ctype
|      integer*2 i16(100,100)
|
|      DFNT_INT16 = 22
|      DFNT_FLOAT32 = 5
|      ...
|      cal   = 10.0
|      cale  = 0.0
|      ioff  = 16.75
|      ioffe = 0.8
|      ctype = DFNT_FLOAT32
|C
|C  Write array and calibration information to file
|C
|      ret = dssnt(DFNT_INT16)
|      ret = dsscal(cal, cale, ioff, ioffe, ctype)
|      ret = dsadata('of.hdf', rank, dims, i16)
|
|C:
|  int16   i16[100][100];
|  float64 cal, cale, ioff, ioffe;
|  int32   calNT;
|  ...
|  cal   = 10.0;
|  cale  =  0.0;
|  ioff  = 16.75;
|  ioffe = 0.8;
|  calNT = DFNT_FLOAT32;
|  ...
|  DFSDsetNT(DFNT_INT16);
|  DFSDsetcal(cal, cale, ioff, ioffe, calNT);
|  DFSDputdata("myfile.hdf", rank, dims, i16);
|
|
|
|Writing Parts of an SDS
|-----------------------
|The routines DFSDstartslice, DFSDputslice, and DFSDendslice let 
|you write an SDS in pieces, or slices. A slice is a rectilinear 
|subarray of elements that can be stored in an SDS. 
|
|In the current implementation of HDF (HDF 3.2) you are restricted 
|in the order in which you write slices to an SDS: slices must be 
|written in such a way that each slice is physically contiguous in 
|the file to the slice that was written before it.
|
|For example, suppose you wish to write a 7 x 12 SDS in a series of 
|slices as illustrated in Figure 4.2.  You would write the slices in 
|order, as follows:
|
|•	The first slice to be written begins at the origin, writing all 
|elements from (1,1) to (2,12). 
|•	The second slice extends from (3,1) to (6,12).
|•	The third slice covers only part of row 7, from (7,1) to (7,4).
|•	The fourth slice covers the rest of row 7, filling out the 
|array.
|
|Figure 4.2	A 7 x 12 SDS 
|Divided into 
|Slices of Varying 
|Sizes
|                                           
|
|
|Calling Sequence for the Slice-Writing Routines
|-----------------------------------------------
|To store an array in slices, make calls to DFSDstartslice, 
|DFSDputslice, and DFSDendslice in the following order:
|
|DFSDstartslice(filename)
|DFSDputslice(windims, data, dims)
|DFSDputslice(windims, data, dims)
|...
|DFSDputslice(windims, data, dims)
|DFSDendslice()
|
|You must call DFSDstartslice before either of the other routines. 
|Thereafter, DFSDputslice may be called many times to write several 
|contiguous slices. DFSDendslice must be called to complete the 
|write. Besides DFSDputslice, no other HDF routines may be called 
|between the calls to DFSDstartslice and DFSDendslice. These three 
|routines are discussed below in more detail.
|
|
|DFSDstartslice
|FORTRAN: INTEGER FUNCTION dssslc(filename)
|CHARACTER*(*) filename	-	name of HDF file
|
|C: int DFSDstartslice(filename)
|char *filename;	/- name of HDF file -/
|
|
|Purpose:  To prepare the system to write a slice to a file.
|
|Returns:  0 on success; -1 on failure.
|
|Before calling DFSDstartslice, you must call DFSDsetdims to specify 
|the dimensions of the dataset to be written to the file. 
|DFSDstartslice always appends a new dataset to an existing file. 
|
|Remember, you must call DFSDstartslice before calling 
|DFSDputslice or DFSDendslice.
|
|DFSDputslice
|FORTRAN: INTEGER FUNCTION dspslc(windims, source, dims)
|INTEGER      windims(*) 	-	dimensions of slice
|<valid type> source(*)	-	array containing slice
|INTEGER	 dims(*)	-	dimensions of array source
|
|C: int DFSDputslice(windims, source, dims)
|int32        windims[];	/- dimensions of slice-/
|<valid type> *source;	/- array for storing slice-/
|int32        dims[];		/- dimensions of array source-/
|
|Purpose:  To write a slice to an SDS.
|
|Returns:  0 on success; -1 on failure.
|
|DFSDputslice takes some part of an array in memory and stores it 
|as part of the SDS array last specified by DFSDsetdims. Slices must 
|be stored contiguously. 
|
|Array windims ("window dimensions") specifies the size of the 
|slice to be written. windims has as many elements as there are 
|dimensions in the entire SDS array. source is the array in memory 
|containing the slice. dims is an array containing the dimensions of 
|the array source.  
|
|Notice that windims and dims need not be the same.  Windims could 
|refer to a subarray of source, in which case only a portion of 
|source is written to the SDS array.
|
|See this chapter's section "Restrictions on Slices To Be Written" 
|for restrictions on what may constitute a slice.
|
|
|DFSDendslice
|FORTRAN: INTEGER FUNCTION dseslc()
|
|C: int DFSDendslice()
|
|
|Purpose:  To specify that the entire dataset has been written.
|
|Returns:  0 on success; -1 on failure.
|
|DFSDendslice must call after all the slices are written. It checks to 
|ensure that the entire dataset has been written, and if it has not, 
|returns an error code.
|
|
|Example:  Writing Slices
|Suppose we want to create an 8x12 SDS array like the array in Fig. 
|4.3, and we want to write it using the slices shown in the figure.  
|Suppose also that the arrays from which we get our data are 2x12 
|arrays in memory called source1, source2, source3 and source4.  
|Fig. 4.4 contains sample C code to accomplish the write.
|
|Figure 4.3	Writing 
|Slices from a four 
|arrays to a 7x12 
|SDS.
|
|
|                                                       
|
|
|Figure 4.4	Writing Slices from a four arrays to a 7x12 SDS.
|/-***************************************************
|*
|* Example C code:  Write out slices of different sizes 
|*                  from a four 2 x 12 arrays to one 8 x 12
|*                  SDS.
|*
|***************************************************-/
|
|...
|
|	int rank;
|	int SDSdims[2], sourcedims[2], windims[2];
|	float source1[2][12];
|	float source2[2][12];
|	float source3[2][12];
|	float source4[2][12];
|
|	/- code that builds the array source goes here -/
|	...
|
|	SDSdims[0]=8;
|	SDSdims[1]=12;
|
|	sourcedims[0]=2;
|	sourcedims[1]=12;
|
|	DFSDsetdims(2,SDSdims);
|
|	/- write out scientific data set in slices -/
|	DFSDstartslice("myfile.hdf");
|
|	windims[0]=2; windims[1]=12;	/- {(1,1) to (2,12)} -/
|	DFSDputslice(windims, source1, sourcedims);
|
|	windims[0]=2; windims[1]=12;	/- {(3,1) to (4,12)} -/
|	DFSDputslice(windims, source2, sourcedims);
|
|	windims[0]=2; windims[1]=12;	/- {(5,1) to (6,4)} -/
|	DFSDputslice(windims, source3, sourcedims);
|
|	windims[0]=2; windims[1]=12;	/- {(7,1) to (8,12)} -/
|	DFSDputslice(windims, source4, sourcedims);
|
|	DFSDendslice();
|
|...
|
|
|
|Restrictions on Slices To Be Written
|It is important to recognize that you cannot write just any 
|subarray from an SDS. The subarray must satisfy the restriction 
|that when written to an SDS it must be written in a physically 
|contiguous sequence in the file. This restriction must hold 
|because the current implementation of HDF only allows data 
|elements to be written in contiguous chunks. (Reading slices from 
|SDSs is less restrictive.)
|
|For example, notice in the example in Figure 4.2 that the fourth 
|slice extends from the middle of the row to the end. The array 
|windims is {1,8}. The slice does not, and cannot,  extend to the 
|eighth row. For example, windims could not be {2,7}, for if it were, 
|then there would have to be a gap between the places in the file 
|where the two partial rows were stored, and this would violate the 
|restriction that slices must be written in a physically contiguous 
|chunk. 
|
|So far we have assumed implicitly that the array is stored by row 
|(row-major order), rather than by column. In C programs,  SDS 
|arrays are stored by row by default, but in FORTRAN programs, 
|arrays are stored in column-major order. In that case, the roles of 
|row and column are switched. If a slice starts in the middle of a 
|column, then it may not spill over into the next column.
|
|As long as you write out data in chunks that encompass an entire 
|dimension (a row or column at a time, a "slab" at a time, etc.), this 
|restriction is unnecessary. It is when you have to write 
|unequally-sized chunks that you have to worry about the 
|restriction.
|
|For higher dimensional arrays, the same rule applies, although its 
|implications may be more confusing. A more general statement of 
|the restriction for higher dimensions  can be described as follows:
|
|	dimsizes = {n1, n2, ...ni, ...nrank}
|	windims  = {w1, w2, ...wi, ...wrank}
|	startpos = {p1, p2, ...pi, ...prank}
|
|Case (a)
|Array elements are to be stored in row-major order (the default 
|order; the last dimension varies fastest). If wi is the first element 
|of windims that is greater than 1 but less than ni, then all previous 
|elements of windims must be 1, and all succeeding elements of 
|windims must be equal to their counterparts in dimsizes. 
|
|That is, if there is an i such that  1<wi<=ni, then it must be the 
|case that for all j, j<i, wj=1, and for all k, k>i, wk=nk.
|
|In other words, in a multidimensional array with
|
|		dimsizes={n1, n2, ...  ni, ni+1, ...nrank} 
|and
|		1<wi<=ni,  pi-1+ wi <= ni,
|then
|		windims ={ 1, 1, ...1, wi, ni+1, ...nrank}, and
|
|		startpos={(don't care)... pi, pi+1=1, ...prank=1}
|
|where startpos represents the start position of the slice in the 
|dataset. 
|
|Case (b)
|Array elements are to be stored in column-major order (first 
|dimension varies fastest). If wi is an element of windims that is 
|greater than 1 but less than ni, then all previous elements of 
|windims must be equal to their counterparts in dimsizes, and all 
|succeeding elements of windims must be 1. 
|
|That is, if there is an i such that  1<wi<=ni, then it must be the 
|case that for all j, j<i, wk=nk, and for all k, k>i, wk=1.
|
|In other words, in a multidimensional array with 
|
|	dimsizes={n1, n2, ...  ni, ni+1, ...nrank} 
|and
|	1<wi<=ni,  pi-1+ wi <= ni,
| then
|	windims ={n1, n2,...ni-1, wi, 1, 1, ...1}.
|
|		startpos={ p1=1, ...pi-1=1, pi,...(don't care)}
|
|
|3D Example
|Suppose you want to write a 5 x 6 x 9 array in 
|row-major order. An acceptable sequence of slices might be:
|
|slice	windims
|(1,1,1) to (1,6,9)	{1,6,9}	{1st slab}
|(2,1,1) to (2,2,9)	(1,2,9)	{1st 2 rows of 2nd slab}
|(2,3,1) to (2,6,9)	(1,4,9)	{next 3 rows, completing the 2nd slab}
|(3,1,1) to (3,1,5)	(1,1,4)	{5 elements of 1st row of 3rd slab}
|(3,1,6) to (3,1,9)	(1,1,5)	{next 4 elements, completing the 1st row}
|(3,2,1) to (3,6,9)	(1,5,9)	{next 5 rows, completing 3rd slab}
|(4,1,1) to (5,6,9)	(2,6,9)	{next 2 slabs, completing the array}
|
|
|
|Reading Scientific Datasets from a File
|---------------------------------------
|
|You can read an SDS from a file by executing a series of get calls. 
|Each call retrieves one or more pieces of information associated 
|with the SDS.  As in the case of writing, there are two types of data 
|that you can read from an SDS: (a) the data array itself, and (b) 
|other information associated with the SDS, such as labels, valid 
|range, etc.
|
|There are two ways to read in the data array itself:
|
|1.	by reading in the entire array with a single call to 
|DFSDgetdata, and 
|2.	by reading the array in smaller pieces, or "slices." 
|
|Method #1 is covered in the section "Reading in an Entire Array".  
|Method #2 is covered in the section "Reading Parts of an SDS".
|
|The other information associated with an SDS is read in using 
|"DFSDget..." calls that mirror the "DFSDset..." calls used to write 
|the same information.
|
|When reading an SDS, you must invoke at least one of  two 
|routines, DFSDgetdims or DFSDgetdata, before calling any of the 
|others.  The first call of either of these routines will find the first 
|SDS in the file and perform initializing operations for the SDS. 
|Once this initialization is done, the other routines can be called 
|in any order and as many times as desired. In the rest of the 
|chapter, we will refer the initialized SDS as the current SDS. 
|After the data array is read, the next DFSDgetdims or DFSDgetdata 
|call will make the next SDS in the file the current SDS.
|Reading in an Entire Array
|--------------------------
|DFSDgetdata
|FORTRAN: INTEGER FUNCTION dsgdata(filename, rank, dimsizes, data)
|CHARACTER*(*) filename	-	name of file with SDS
|INTEGER	rank	-	number of dimensions
|INTEGER*4	dimsizes(rank)	-	array that holds dimensions of 
|				buffer that will hold the data
|<valid type>	data(*)	-	array for holding the data
|
|C: int DFSDgetdata(filename, rank, dimsizes, data)
|char	*filename;	/- name of file with SDS -/
|int	rank;	/- number of dimensions -/
|int32	dimsizes[];	/- array that holds dimensions of buffer
|			   that will hold the data -/
|<valid type> data[];  /- array for holding the data -/
|
|Purpose:  To get the dataset from the next SDS in the file and 
|store it in the array data.
|
|Returns:  0 on success; -1 on failure.
|
|If you do not know the values of rank or dimsizes, you must call 
|DFSDgetdims (described below) to get them and then use them to 
|provide the right amount of space for the array data.  If you do not 
|know the number type of the data in an SDS, you can call DFSDgetNT 
|(described below) to find it out.
|
|Each new call to DFSDgetdata (or to DFSDgetdims and DFSDgetdata) 
|reads from the SDS that succeeds the last one read. For example, if 
|DFSDgetdata is called three times in succession, the third call 
|reads data from the third SDS in the file. Of course, if you do not 
|know the values of rank or dimsizes, you must call DFSDgetdims to 
|get them and then use them to provide the right amount of space 
|for the array data.
|
|If DFSDgetdims or DFSDgetdata is called and there are no more 
|scientific datasets left in the file, an error code is returned and 
|nothing is read. DFSDrestart (see below) can be used to override 
|this convention.
|
|Example: Reading in an SDS. The following is code to read in 
|an array whose dimensions are known to be 100x200, and whose 
|number type is known to be int16 (16-bit integer).
|
|FORTRAN: INTEGER     dsgdata
|      INTEGER*2   density(200, 100)
|      INTEGER     sizes(2), ret
|      
|      sizes(1) = 200
|      sizes(2) = 100
|      ret = dsgdata('myfile.hdf', 2, sizes, density)
|        ...
|
|C: unit16   density[100][200];
|    int32    sizes[2], ret;
|    
|    sizes[0] = 100;
|    sizes[1] = 200;
|    ret = DFSDgetdata("myfile.hdf", 2, sizes, density);
|      ...
|
|Getting Other Information about an SDS
|--------------------------------------
|DFSDgetNT
|FORTRAN: INTEGER FUNCTION dsgnt(numbertype)
|
|INTEGER*4	numbertype	- number type of data in SDS
|
|
|C: int DFSDgetNT(pnumbertype)
|int32	*pnumbertype;	/- number type of data in SDS -/
|
|
|Purpose:  To get the number type of the current SDS.
|
|Returns:  0 on success; -1 on failure.
|
|Since DFSDgetNT gets the number type of the current
|data set,  DFSDgetdims or DFSDgetdata should usually be called 
|before calling DFSDgetNT.
|
|See the description of DFSDsetNT for a listing of the valid values for 
|numbertype and their meanings.
|
|
|DFSDgetdims
|FORTRAN: INTEGER FUNCTION dsgdims(filename, rank, dimsizes, maxrank)
|CHARACTER*(*) filename	-	name of file with SDS
|INTEGER	rank	-	number of dimensions
|INTEGER*4	dimsizes(maxrank)	-	array for holding dimensions of
|			data set in the file
|INTEGER	maxrank	-	size of array dimsizes
|
|C: int DFSDgetdims(filename, rank, dimsizes, maxrank)
|char	*filename;	/-	name of file with SDS -/
|int	*rank;	/-	number of dimensions -/
|int32	dimsizes[];	/-	array for holding dimensions of
|			data set in the file  -/
|int	maxrank;	/-	size of array dimsizes -/
|
|
|Purpose:  To get the rank (number of dimensions) of the dataset 
|and the dimsizes of each dimension in the next SDS in the file.
|
|Returns:  0 on success; -1 on failure.
|
|The input argument maxrank tells DFSDgetdims the size of the array 
|that is allocated for storing the array of dimension sizes. The 
|value of rank cannot exceed the value of maxrank.
|
|The allocation of space for reading in the SDS should correspond to 
|the values read in by DFSDgetdims. 
|
|
|DFSDgetdimstrs
|FORTRAN: INTEGER FUNCTION dsgdist(dim, label, unit, format)
|INTEGER	dim	-	dimension this label, unit and 	format refer to
|CHARACTER*(*)	label	-	label that describes this dimension
|CHARACTER*(*)	unit	-	unit to be used with this dimension
|CHARACTER*(*)	format	-	format to be used in displaying 	
|			scale for this dimension
|
|C: int DFSDgetdimstrs(dim, label, unit, format)
|int	dim;	/-	dimension this label, unit and format refer to -/
|char	*label;	/-	label that describes this dimension -/
|char	*unit;	/-	unit to be used with this dimension -/
|char	*format;	/-	format to be used in displaying scale 
|			for this dimension -/
|
|
|Purpose:  To get the items corresponding to the dimension dim 
|that are stored as strings in the SDS, namely label, unit, and 
|format.
|
|Returns:  0 on success; -1 on failure.
|
|Note:  dim=1 for the first dimension, dim=2 for the 2nd dimension, 
|and so forth.
|
|The space allocated for label, unit and format should be at least 1 
|byte bigger than the length of the string. If the length is unknown 
|when the program is written, one may declare the array size as 
|1+maxlen_label, _unit, or _format which were set by 
|DFSDsetlengths. The default maxlength for those strings is 255.
|
|
|Example: Reading an SDS with dimension information. 
|In this example a float32 array of size 800x500 is read from an 
|HDF file, together with  label, unit, and format information about 
|each dimension.
|
|FORTRAN: INTEGER         dsgdims, dsgdata, dsgdist
|      INTEGER         rank, dimsizes(2), ret
|      CHARACTER*256   lnglabel, lngunit, lngfmt
|      CHARACTER*256   latlabel, latunit, latfmt
|      REAL*4          pressure(500,800)
|      
|      ret = dsgdims('myfile.hdf', rank, dimsizes, 2)
|      ret = dsgdist(1, lnglabel, lngunit, lngfmt)
|      ret = dsgdist(2, latlabel, latunit, latfmt)
|      ret = dsgdata('myfile.hdf', rank, dimsizes, pressure)
|        ...
|
|C: int     rank;
|    int32   dimsizes[2];
|    char    lnglabel[256], lngunit[256], lngfmt[256],
|            latlabel[256], latunit[256], latfmt[256];
|    float32 pressure[800][500];
|    
|    DFSDgetdims("myfile.hdf", &rank, dimsizes, 2);
|    DFSDgetdimstrs(1, latlabel, latunit, latfmt);
|    DFSDgetdimstrs(2, lnglabel, lngunit, lngfmt);
|    DFSDgetdata("myfile.hdf", rank, dimsizes, pressure);
|      ...
|
|
|DFSDgetdimscale
|FORTRAN: INTEGER FUNCTION dsgdist(dim, size, scale)
|INTEGER	dim	-	dimension this scale corresponds to
|INTEGER	size	-	size of scale
|REAL	scale(size)	-	the scale
|
|C: int DFSDgetdimscale(dim, size, scale)
|int  	dim;	/-	dimension this scale corresponds to -/
|int32	size;	/-	size of scale -/
|float32	scale[];	/-	the scale -/
|
|Purpose:  To get the scale corresponding to the dimension dim 
|and store it in the floating-point array scale.
|
|Returns:  0 on success; -1 on failure.
|
|Note: dim=1 for the first dimension.
|
|In the current implementation of HDF, dimension scales must be of 
|the same number type as the corresponding data.  (There are plans 
|to allow scales to have their own number type in a future release.)
|
|Example: Reading an SDS with dimension scales. In this 
|example a 800 x 500 data array is read from 'myfile.hdf', together 
|with scales for each dimension.  The scales are assumed to be of 
|type float32.
|
|FORTRAN: INTEGER         dsgdims, dsgdisc
|      INTEGER         rank, dimsizes(2), ret
|      REAL            lngscale(500), latscale(800)
|      REAL*4          pressure(500,800)
|      
|      ret = dsgdims('myfile.hdf', rank, dimsizes, 2)
|      ret = dsgdisc(1, dimsizes(1), lngscale)
|      ret = dsgdisc(2, dimsizes(2), latscale)
|      ret = dsgdata('myfile.hdf', rank, dimsizes, pressure)
|        ...
|
|C: int     rank;
|    int32   dimsizes[2];
|    float32 latscale[800], lngscale[500];
|    float32 pressure[800][500];
|    
|    DFSDgetdims("myfile.hdf", &rank, dimsizes, 2);
|    DFSDgetdimscale(1, dimsizes[0], latscale);
|    DFSDgetdimscale(2, dimsizes[1], lngscale);
|    DFSDgetdata("myfile.hdf", rank, dimsizes, pressure);
|      ...
|
|
|DFSDgetdatastrs
|FORTRAN: INTEGER FUNCTION dsgdast(label, unit, format, coordsys)
|CHARACTER*(*)	label	-	label that Describes the data
|CHARACTER*(*)	unit	-	unit to be used with the data
|CHARACTER*(*)	format	-	format to be used in displaying data
|CHARACTER*(*)	coordsys	-	coordinate system
|
|C: int DFSDgetdatastrs(label, unit, format, coordsys)
|char	*label;	/-	label that describes the data -/
|char	*unit;	/-	unit to be used with the data -/
|char	*format;	/-	format to be used in displaying data -/
|char	*coordsys;	/-	coordinate system -/
|
|
|Purpose:  To get information about the data itself from all 
|strings.
|
|Returns:  0 on success; -1 on failure.
|
|The parameter coordsys gives the coordinate system that is to be 
|used for interpreting the dimension information.
|
|The space allocated for label, unit and format should be at least 1 
|byte bigger than the length of the string. If the length is unknown 
|when the program is written, one may declare the array size as 
|1+maxlen_label, _unit, or _format which were set by 
|DFSDsetlengths. The default maxlength for those strings is 255.
|
|
|Example:  Reading a Scientific Dataset with Data 
|Information.  In this example a 800 x 500 data array is read 
|from a n HDF, together with label, unit, and format information 
|about the data.
|
|FORTRAN: INTEGER         dsgdims, dsgdata, dsgdast
|      INTEGER         rank, dimsizes(2), ret
|      CHARACTER*256   datalabel, dataunit, datafmt, coordsys
|      REAL            pressure(500,800), density(500,800)
|      
|      dimsizes(1) = 500
|      dimsizes(2) = 800
|      ret = dsgdims('myfile.hdf', rank, dimsizes, 2)
|      ret = dsgdata('myfile.hdf', rank, dimsizes, pressure)
|      ret = dsgdast(datalabel, dataunit, datafmt, coordsys)
|        ...
|
|C: int     rank;
|    int32   dimsizes[2];
|    char    datalabel[256], dataunit[256], datafmt[256],
|            coordsys[256];
|    float32 pressure[800][500];
|    
|    dimsizes[0] = 800;
|    dimsizes[1] = 500;
|    DFSDgetdims("myfile.hdf", &rank, dimsizes, 2);
|    DFSDgetdata("myfile.hdf", rank, dimsizes, pressure);
|    DFSDgetdatastrs(datalabel, dataunit, datafmt, coordsys);
|      ...
|
|
|DFSDgetrange
|FORTRAN: INTEGER FUNCTION dsgrang(max, min)
|<valid type>   max	-	high value stored with SDS
|<valid type>   min	-	low value stored with SDS
|
|C: int DFSDgetrange(pmax, pmin)
|<valid type>  *pmax;	/- high value stored with SDS -/
|<valid type>  *pmin;	/- low value stored with SDS -/
|
|
|Purpose:  To get maximum and minimum values stored with the 
|current SDS   .
|
|DFSDgetdims or DFSDgetdata should be called before 
|DFSDgetrange is called.
|
|Returns:  0 on success; -1 on failure or if there are no max or 
|min values.
|
|Since the max/min values are supposed to relate to the data itself, 
|it is assumed that the type of max/min is the same as the type of 
|the data.  One implication of this is that in the C version of 
|DFSDgetrange the arguments are pointers, rather than simple 
|variables, whereas in the FORTRAN version they are simple 
|variables of the same type as the data array.
|
|NOTE:  These values need to have been set by a user via a call to 
|DFSDsetrange. They are not automatically stored.
|
|Example.  In this example 16-bit data is read from an HDF file.   
|Notice that max and min must be the same data type as the SDS array 
|data.
|
|FORTRAN: integer*2    max, min, data(100,100)
|  ...
|  ret = dsgdata('myfile.hdf', rank, dims, data)
|  ret = dsgrang(max, min)
|
|C: int16 max, min, data[100][100];  
|   ...
|  DFSDgetdata("myfile.hdf", rank, dims, data);
|  DFSDgetrange(&max, &min);
|  
|
|DFSDgetcal
|FORTRAN: INTEGER FUNCTION dsgcal(cal, cal_err, ioff, ioff_err, cal_type)
|real*8	cal	- calibration factor
|real*8	cal_err	- calibration error
|real*8	ioff	- uncalibrated offset
|real*8	ioff_err	- uncalibrated offset error
|integer*4	cal_type	- number type of uncalibrated data
|
|
|C: int DFSDgetcal(cal, cal_err, ioff, ioff_err, cal_type)
|float64	*cal	/- calibration factor -/
|float64	*cal_err	/- calibration error -/
|float64	*ioff	/- uncalibrated offset -/
|float64	*ioff_err	/- uncalibrated offset error -/
|int32	*cal_type	/- number type of uncalibrated data -/
|
|
|Purpose:  To get calibration information about the data in the 
|current SDS.
|
|Returns:  0 on success; -1 on failure.
|
|This routine reads the calibration record, if there is one, attached 
|to an SDS.  A calibration record contains four 64-bit floating point 
|values followed by a 32-bit integer, to be interpreted as follows:
|
|      cal       calibration factor
|      cal_err   calibration error
|      ioff      uncalibrated offset
|      ioff_err  uncalibrated offset error
|      cal_type  numbertype of uncalibrated data
|
|The relationship between a value 'iy' stored in an SDS and the 
|actual value 'y' is defined as:
|
|    y = cal * (iy - ioff)
|
|The variable ioff_err contains a potential error of ioff, and 
|cal_err contains  a potential error of cal.  
|
|Currently the calibration record is provided for information only. 
|The SDS interface performs no operations on the data based on the 
|calibration  tag.
|
|
|DFSDreadref
|FORTRAN: INTEGER FUNCTION dsrref(name, ref)
|character*(*) name     - name of file containing SDS
|integer       ref      - reference number for next dsgdata
|
|C: int DFSDreadref(filename, ref)
|char *filename;     /- file containing SDS -/
|uint16 ref;         /- reference number for next DFSDgetdata -/
|
|
|Purpose:  To specify the reference number of the SDS to be read 
|when DFSDgetdims or DFSDgetdata is next called.
|
|Returns:  0 on success; -1 on failure.
|
|This routine is most likely to be used in conjunction with 
|DFANgetlablist, which returns a list of labels for a given tag 
|together with their reference numbers. It provides a sort of 
|random access to SDSs.
|
|NOTE: There is no guarantee that reference numbers appear in 
|sequence in an HDF file, so it is not generally safe to assume that 
|a reference number is a sequence number for an SDS.
|
|
|DFSDrestart
|FORTRAN: INTEGER FUNCTION dsfirst()
|
|C: int DFSDrestart()
|
|
|Purpose:  To cause the next get to read from the first SDS in the 
|file, rather than the SDS following the one that was most recently 
|read.
|
|Returns:  0 on success; -1 on failure.
|
|
|
|Example: Reading Two SDSs from a File. 
|In this example, two SDSs having the same dimension and other 
|information, except the values of the data arrays, are read from a 
|file.
|
|The interface remembers from the first call to the second that one 
|SDS has already been accessed, so on the second call it gets the 
|second SDS.
|
|FORTRAN: INTEGER   dsgdims, dsgdata, dsgdast
|      INTEGER   rank, dimsizes(2), ret
|      CHARACTER*256  datalabel, dataunit, datafmt, coordsys
|      REAL           pressure(300,200), density(300,200)
|
|      ret = dsgdata('myfile.hdf', rank, dimsizes, pressure)
|      ret = dsgdast(datalabel, dataunit, datafmt, coordsys)
|
|      ret = dsgdata('myfile.hdf', 2, dimsizes, density)
|      ret = dsgdast(datalabel, dataunit, datafmt,coordsys)
|      ...
|
|C: int      rank;
|    int32    dimsizes[2];
|    char     datalabel[256], dataunit[256], datafmt[256],
|             coordsys[256];
|    float32  pressure[200][300], density[200][300];
|
|    DFSDgetdata("myfile.hdf", rank, dimsizes, pressure);
|    DFSDgetdatastrs(datalabel, dataunit, datafmt, coordsys);
|
|    DFSDgetdata("myfile.hdf", 2, dimsizes, density);
|    DFSDgetdatastrs(datalabel, dataunit, datafmt, coordsys);
|      ...
|
|
|Reading Parts of an SDS
|-----------------------
|The routine DFSDgetslice lets you read in a slice from the current 
|SDS. A slice  is any subarray, or "hypercube", of the SDS from 
|which it is read. (Slices do not have to be read in contiguous order, 
|so the rules for reading slices are more general than they are for 
|writing slices.)
|
|A slice can be described with two one-dimensional arrays, one 
|containing the coordinates of the corner that is nearest to the 
|origin and the other containing the sizes of the slices dimensions. 
|For example, suppose you wish to read the shaded slices from the 
|10 x 12 SDS shown in Figure 4.5. The relevant corner of the first 
|slice is {3,4} and its dimensions are {4,6}. The second slice begins 
|at {1,10} and has dimensions {10,2}.
|
|Figure 4.5	A 10 x 12 
|SDS Showing  
|Two Slices That 
|Are To Be Read. 
|                                           
|
|DFSDgetslice
|FORTRAN: INTEGER FUNCTION dsgslc(filename, winst, windims, dest, dims)
|CHARACTER*(*) filename	-	name of HDF file
|INTEGER	winst	-	array with coordinates of
|			start of slice
|INTEGER	windims	-	array with dimensions of slice
|<valid type> dest(*)	-	array for returning slice
|INTEGER	dims	-	dimensions of array dest
|
|C: int DFSDgetslice(filename, winst, windims, dest,  dims)
|char	*filename	-	name of HDF file
|int32	winst[]	-	array with coordinates of start of slice
|int32	windims[]	-	array with dimensions of slice
|<valid type> *dest	-	array for returning slice
|int32	dims	-	dimensions of array dest
|
|Purpose:  To read part of an SDS from a file.
|
|Returns:  0 on success; -1 on failure.
|
|DFSDgetslice accesses the dataset last accessed by DFSDgetdims. If 
|DFSDgetdims has not been called, DFSDgetslice gets a slice from the 
|next dataset in the file.
|
|Array winst specifies the coordinates of the start of the slice. 
|Array windims gives the size of the slice. The number of elements 
|in winst and windims must be equal to the rank of the dataset. For 
|example, if the file contains a 3D dataset, winst may contain the 
|values {2, 4, 3}, while windims contains the values {3, 1, 4}. This 
|will extract a 3 x 4, two-dimensional slice, containing the 
|elements between (2, 4, 3) and (4, 4, 6) from the original dataset. 
|
|dest is the array into which the slice is read. It must be at least as 
|big as the desired slice.
|
|dims is an array containing the actual dimensions of the array 
|dest. The user assigns values to dims before calling DFSDgetslice. 
|
|NOTE:  In both C and Fortran the minimum value of winst[i] is 1. 
|For example, if the 3D slice starts at the origin, winst has values 
|{1,1,1} instead of {0,0,0}
|
|
|Example: Reading Slices
|The C code in Figure 4.6 shows how you could read the two slices 
|shown in Figure 4.5.
|Figure 4.6	Reading 
|Slices from a 
|10x12 SDS in 
|Reverse Order
|/-****************************************************
|* Example C code:       Read in slices from a 10 x 12 array.
|****************************************************-/
|#include "hdf.h"
|
|main()
|{
|        int  i, rank;
|        int32 dimsizes[2];
|
|        DFSDgetdims("myfile.hdf", &rank, dimsizes, 2);
|
|        /- starting at (3,4) read 4 x 6 window -/
|        /- Note: (3,4) rather than (2,3) because -/
|        /- FORTRAN-style indexing is used.       -/
|        getit("myfile.hdf", 3,4,4,6);
|
|        /- starting at (1,10) read 8 x 2 window -/
|        getit("myfile.hdf", 1,10,8,2);
|}
|
|getit(filename, st0, st1, rows, cols)
|int st0, st1, rows, cols;
|char *filename;
|{
|        int i, j;
|        int32 winst[2], windims[2], dims[2];
|        float32 data[500];
|
|        winst[0]=st0; winst[1]=st1;
|        dims[0] = windims[0] = rows;
|        dims[1] = windims[1] = cols;
|        DFSDgetslice(filename, winst, windims, data,dims);
|
|        for (i=0; i<rows; i++)  {
|                printf("\n");
|                for (j=0; j<cols; j++)
|                 printf("%5.0f%c",data[i*cols+j], ' ');
|        }
|        printf("\n");
|}
|
|Other SDS  Routines
|-------------------
|
|DFSDnumber
|FORTRAN: (not yet implemented in FORTRAN)
|
|C: int DFSDnumber(filename)
|char *filename  /- name of HDF file containing SDSs   -/
|
|Purpose:  To get the number of scientific datasets in the file.
|
|Returns:  Number of data sets on success; -1 on failure.
|
|
|DFSDlastref
|FORTRAN: INTEGER FUNCTION DFSDlastref()
|
|C: int DFSDlastref()
|
|Purpose:  To get the most recent reference number used in 
|writing or reading an SDS.
|
|Returns:  Reference number on success; -1 on failure.
|
|This routine is primarily used for annotations. See Chapter 5 
|"Annotating Data Objects and Files" for examples.
|
|DFSDpre32sdg
|FORTRAN: INTEGER FUNCTION dsp32sd(filename, ref, ispre32)
|character*(*) filename	-	file the SDG/ref resides in
|integer       ref	-	ref number of SDG 
|integer       ispre32	-	pre-HDF 3.2 flag: 
|			          1 if SDS is pre-HDF 3.2 
|			          0 otherwise 
|
|C: int DFSDpre32sdg(filename, ref, pispre32)
|char 	*filename	/- file the SDG/ref resides in       -/
|uint16	ref		/- ref number of SDG                 -/
|int	*pispre32	/- pre-HDF 3.2 flag:                 -/
|				/-          1 if SDS is pre-HDF 3.2  -/
|				/-          0 otherwise              -/
|
|
|Purpose:  To test if the SDG with given ref was created by an HDF 
|library that precedes HDF 3.2, and if this is the case to set 
|ispre32 (or the contents of pispre32) to 1; otherwise, set it to 0.
|
|Returns:  0 on success; -1 on failure. 
|
|This routine is for programmers who need to know whether the SDS 
|was written by an earlier version of the HDF library than HDF3.2.  
|Based on this information, their program can decide whether or 
|not to transpose the corresponding array.  (See the section 
|"Backward Compatibility" for details on this potential problem.)
|
|
|How SDS Routines Store and Convert Scientific Data
|--------------------------------------------------
|
|How HDF Stores Arrays
|---------------------
|When you write an array to an SDS using HDF version 3.2 (or 
|later), the numbers in your array are written to the HDF file in 
|exactly the same order in which they are stored in your computer.  
|(This was not always the case.  See the section "Backward 
|Compatibility" below for details.)  
|
|This convention can cause some confusion when data is written by 
|a C program, then read back by a FORTRAN program (or vice versa) 
|because the ways we dimension arrays in the two languages differ 
|relative to how arrays are stored in memory. C stores arrays in 
|memory in row major order, whereas FORTRAN stores them in 
|column major order. 
|
|In row major order, the mapping between the array dimensions and 
|physical storage order is one in which the last dimension varies 
|fastest.  Consider, for instance, the following 3x4 array:
|
|1   2   3   4
|5   6   7   8
|9  10  11  12
|
|If this array were declared in C as "A[3][4]" its values would be 
|stored in memory as:
|
|1   2   3   4   5   6   7   8   9   10  11  12
|A11 A12 A13 A14 A21 A22 A23 A24 A31 A32 A33 A34
|
|In reading through these numbers and indexing them as we would 
|in C, the last dimension varies fastest.
|
|If this array were stored the same way in memory by a FORTRAN 
|program, which uses column major order, it would have to be 
|declared as "A(4,3)" and the first  dimension would vary fastest.  
|In reading through the numbers and indexing them the way we do 
|in FORTRAN, the mapping would be as follows:
|
|1   2   3   4   5   6   7   8   9   10  11  12
|A11 A21 A31 A41 A12 A22 A32 A42 A13 A23 A33 A43
|
|This reversal of dimensions applies not only to dimension 
|declarations, but to scales, labels, units and formats.  For all of 
|these, dimension are written to HDF files in C order, from slowest 
|changing dimension to the fastest changing dimension. It follows 
|from this that if you want to maintain compatibility between C and 
|FORTRAN (for instance, when an SDS is written out by a C 
|program, then read in by a FORTRAN program), you'll need to 
|reverse, in Fortran programs, the dimension specifications for 
|dimension-related information (array dimensions, scales, labels, 
|units, and formats) from what they were in the corresponding C 
|program.
|
|To clarify further, consider the following three-dimensional 
|example.  In the following two program segments, one in C and the 
|other in FORTRAN, the C code writes an array to an SDS, and the 
|FORTRAN code reads back the same array.
|
|C code writes array:
|
|   int i,j,k;
|   float data[4][5][6];
|   int dimsizes[3];
|   char label1[20], unit1[20], format1[20];
|
|   dimsizes[0] = 4;
|   dimsizes[1] = 5;
|   dimsizes[2] = 6;
|
|   strcpy(label1, "pressure");
|   strcpy(unit1,"pascals");
|   strcpy(format1,"e15.7");
|   ...
|   DFSDsetdims(3, dimsizes);
|   DFSDsetdimstrs(1, label1, unit1, format1);
|   DFSDadddata("myfile.hdf", 3, dimsizes, data);
|
|FORTRAN code reads back the same array:
|
|      real*4  data(6,5,4)
|      integer dimsizes(3)
|      character*30 label3, unit3, format3
|
|      dimsizes(1) = 6
|      dimsizes(2) = 5
|      dimsizes(3) = 4
|
|      ret = dsgdata('myfile.hdf', 3, dimsizes, data)
|      ret = dsgdist(3, label3, unit3, format3)
|
|
|Notice that DFSDsetdimstrs assigns label1 ("pressure") to 
|dimension 1.  In order to read back that same label, the FORTRAN 
|program calls dsgdist to read in dimension 3.  After calling 
|dsgdist, the value assigned to label is "pressure."
|
|How HDF Stores Numbers in SDSs Standard HDF representation.
|-----------------------------------------------------------
|One of the primary goals of HDF is to provide portability of files 
|among different machine architectures.  In the case of SDS this 
|means that numbers that are represented one way on one machine 
|must be converted to a different representation of a different 
|machine. Before writing numbers to an HDF file, HDF converts 
|them from the native format of the host machine to a standard HDF 
|format.  When reading numbers in, to converts them from the 
|standard format to the native format of whatever machine they are 
|being read into.  This means that there are routines in the HDF 
|library that convert to and from every machine architecture that 
|HDF supports.  
|
|The standard representations that HDF uses for floating point 
|numbers are the IEEE floating point formats. For integers, the 
|standard used is the big-endian format.
|
|The user should be aware that converting to and from a standard 
|format can result in low order inaccuracies in the data. Data that 
|has been converted from 64-bit to 32-bit floating-point 
|representation is accurate to about 10-7.
|
|Native format
|In many instances it does not matter to a user how data is stored 
|or what conversions it must undergo. However, sometimes these 
|conversions cannot be tolerated, either because they slow down 
|processing too much or because they introduce intolerable 
|inaccuracies. For those instance, HDF provides a "native format" 
|option, which tells HDF not to convert numbers before writing 
|them to an HDF file. 
|
|The choice of the native format option can diminish the portability 
|of HDF files, since HDF does not support conversion routines that 
|can convert directly from every machine's format to every other.  
|
|In the next release HDF will support conversion from the little 
|endian native format (e.g. IBM PC native format) to any other 
|supported machine format.
|
|
|See the routine DFSDsetNT for details on how to choose the native 
|format option
|
|
|
|
|Backward compatibility
|In earlier (pre-HDF 3.2) versions of the HDF library the SDS 
|interface would, by default, transpose arrays written by FORTRAN 
|programs so that matrices that were declared the same way in C 
|and FORTRAN programs would be stored with the same physical 
|ordering in an HDF file.  This caused a great deal of confusion 
|among HDF users, and often resulted in very inefficient program
| execution, so it was decided with release 3.2 to eliminate array 
|transposition.  
|
|In order to maintain backward compatibility with files that 
|predate HDF 3.2, HDF 3.2 reads SDSs from such files in the same 
|way that pre-HDF3.2 versions did, so that old files can be read by 
|the new library without any extra programming.  This means that 
|aberrations in the way HDF 3.1 read old, transposed files continue 
|in HDF 3.2.  This is done to maintain consistency for those users 
|who wrote their code specifically to accommodate these 
|aberrations.  
|
|Despite this strategy for maintaining backward compatibility, 
|there are still cases in which users' programs need to know 
|whether an SDS was written by a pre-HDF 3.2 library.  For those 
|users, the routine DFSDpre32sdg, described elsewhere, has been 
|added to the HDF library.
|
|NOTE: Although the HDF developers have made their best effort 
|with HDF 3.2 to maintain backward compatibility, some of these 
|changes may still cause difficulty, especially when old programs 
|need to be changed to accommodate the new ordering.  Please 
|contact NCSA if you need help in smoothing the transition to HDF 
|3.2.
|
|
|Sample Programs
|
|Two complete sample programs, the first in FORTRAN, the second 
|in C, are presented below.
|
|A FORTRAN Program
|This program (Figure 4.7) does the following, in order:
|
|•	 Generates an array called pressure of random numbers, sets the 
|maximum and minimum values, and sets scale values for the 
|dimensions.
|
|•	Writes the contents of pressure to an HDF file called 
|"testsds.df", together with scales, label, unit, format and 
|max/min information
|
|•	Reads back the array from "testsds.df", together with the 
|associated information
|
|•	Compares the contents of the information read with the original 
|information
|
|Note that if this program were run on the Cray as is, the values 
|that were written to the file would not, in general, be equal to the 
|numbers that are read back in due to the loss of precision on the 
|write. See the section on "How SDS Routines Store and Convert 
|Scientific Data" for a discussion of this problem.
|
|
|Figure 4.7	FORTRAN Program Dealing with Scientific Datasets
|FORTRAN: PROGRAM SDex5
|
|      INTEGER           dssdims, dssdast, dssdist, dssdisc
|      INTEGER           dssrang, dspdata, dsgdims, dsgdata
|      INTEGER           dsgdast, dsgdist, dsgdist, dsgrang
|      INTEGER           ret, i, j
|      INTEGER           rank, inRank
|      INTEGER           shape(2), inShape(2)
|      REAL              pressure(36,18), inPressure(36,18)
|      REAL              ltscales(18), iltscale(18)
|      REAL              lgscales(36), ilgscale(36)
|      REAL              maxpressure, inMaxpressure
|      REAL              minpressure, inMinpressure
|      REAL              epsi
|      CHARACTER*20      datalabel,dataunit,datafmt
|      CHARACTER*256     inDataunit, inDatalabel, inDatafmt
|      CHARACTER*20      dimlabels(2), dimunits(2),dimfmts(2)
|      CHARACTER*256     inDimunits(2), inDimlabels(2)
|      CHARACTER*256     inDimfmts(2), inDummy
|
|      epsi = 0.0001
|      rank = 2
|      shape(1) = 36
|      shape(2) = 18
|
|      datalabel         = 'Pressure 1'
|      dataunit          = 'Pascals'
|      datafmt           = 'E15.9'
|      dimlabels(1)      = 'longitude'
|      dimunits(1)       = 'km'
|      dimfmts(1)        = 'F10.2'
|      dimlabels(2)      = 'latitude'
|      dimunits(2)       = 'km'
|      dimfmts(2)        = 'F10.2'
|
|C      call getpressure(pressure, 100)
|C      call findMaxMin(pressure, 100, maxpressure, minpressure)
|
|      do 110 i = 1, 18
|         do 100 j = 1, 36
|            pressure(j,i) = i*10 + j*0.1 
|100      continue
|110   continue
|
|      maxpressure = 183.6
|      minpressure = 10.1
|      do 120 i = 1, 18
|         ltscales(i) = i
|120   continue
|
|      do 130 j = 1, 36
|         lgscales(j) = j
|130   continue
|
|
|C  Write data to file
|      ret = dssdims(rank, shape)
|      ret = dssdast(datalabel, dataunit, datafmt, '')
|      ret = dssdist(1, dimlabels(1), dimunits(1), dimfmts(1))
|      ret = dssdist(2, dimlabels(2), dimunits(2), dimfmts(2))
|      ret = dssdisc(1, shape(1), lgscales)
|      ret = dssdisc(2, shape(2), ltscales)
|      ret = dssrang(maxpressure, minpressure)
|      ret = dspdata('testsds.df', 2, shape, pressure)
|C  Read data back
|      ret = dsgdims('testsds.df', inRank, inShape, 2)      
|      ret = dsgdata('testsds.df', 2, inShape, inPressure)
|      ret = dsgdast(inDatalabel, inDataunit, inDatafmt, inDummy)
|      ret = dsgdist(1, inDimlabels(1), inDimunits(1), inDimfmts(1))
|      ret = dsgdist(2, inDimlabels(2), inDimunits(2), inDimfmts(2))
|      ret = dsgdisc(1, inShape(1), ilgscale)
|      ret = dsgdisc(2, inShape(2), iltscale)
|      ret = dsgrang(inMaxpressure, inMinpressure)
|
|C  Compare information read in with original information   :
|      print *, 'Input rank is :', inRank
|      print *, 'Input shape is :', inShape(1), ',', inShape(2)
|
|      do 200 i = 1, 18
|         do 210 j = 1, 36
|            if (abs(pressure(j,i)-inPressure(j,i)) .gt. epsi) then
|               print *, 'Array position ', j, ',', i, 'is different'
|            end if
| 210     continue
| 200  continue
|
|      print *, 'Input datalabel is :', inDatalabel
|      print *, 'Input dataunit is :', inDataunit
|      print *, 'Input datafmt is :', inDatafmt
|      print *, 'Input dimlabels(1) is :', inDimlabels(1)
|      print *, 'Input dimunits(1) is :', inDimunits(1)
|      print *, 'Input dimfmts(1) is :', inDimfmts(1)
|      print *, 'Input dimlabels(2) is :', inDimlabels(2)
|      print *, 'Input dimunits(2) is :', inDimunits(2)
|      print *, 'Input dimfmts(2) is :', inDimfmts(2)
|
|      do 300 i = 1, 18
|         if (abs(ltscales(i)-iltscale(i)) .gt. epsi) then
|            print *, 'ltscales is different at position ', i
|         end if
|300   continue
|
|      do 310 j = 1, 36
|         if (lgscales(j) .ne. ilgscale(j)) then
|            print *, 'lgscales is different at position ', j
|         end if
|310   continue
|
|      print *, 'Output maxpressure is :', maxpressure
|      print *, 'Input maxpressure is :', inMaxpressure
|
|      print *, 'Output minpressure is :', minpressure
|      print *, 'Input minpressure is :', inMinpressure
|
|      print *, 'Check completed.'
|      stop
|      end
|
|A C Program
|This program (Figure 4.8) does the following, in order:
|
|•	Generates an array called pressure of random numbers, sets the 
|maximum and minimum values, and sets scale values for the 
|dimensions.
|
|•	Writes the contents of pressure to an HDF file called 
|"testsds.df", together with scales, label, unit, format and 
|max/min information.
|
|•	Reads back the array from "testsds.df", together with the 
|associated information.
|
|•	Compares the contents of the information read with the original 
|information.
|Figure 4.8	C Program Dealing with 
|Scientific Datasets
|C:
|#include "hdf.h"
|#include <stdio.h>
|
|#define MAX_ROW 18
|#define MAX_COL 36
|#define SIZE_ARRAY (MAX_ROW * MAX_COL)
|
|main()
|{
|  int ret, i, j;
|  int rank, inRank;
|  int32 shape[2], inShape[2];  
|  float32 pressure[MAX_ROW][MAX_COL], latscales[MAX_ROW], 
|          lngscales[MAX_COL], inPressure[MAX_ROW][MAX_COL], 
|          inlatscales[MAX_ROW], inlngscales[MAX_COL],
|          maxpressure, inMaxpressure, minpressure, inMinpressure,
|          epsi;
|  char *datalabel, *dataunit, *datafmt, 
|       inDatalabel[256], inDataunit[256], inDatafmt[256],
|       *dimlabels[2], *dimunits[2], *dimfmts[2], 
|       inDimlabels[2][256], inDimunits[2][256], inDimfmts[2][256],
|       inDummy[256];
|
|  epsi = 0.0001;
|  rank 	= 2;
|  shape[0] 	= MAX_ROW;
|  shape[1] 	= MAX_COL;
|
|  datalabel	= "Pressure 1";
|  dataunit	= "Pascals";
|  datafmt	= "E15.9";
|  dimlabels[0]	= "latitude_label";
|  dimlabels[1]	= "longitude_label";
|  dimunits[0]	= "km";
|  dimunits[1]	= "km";
|  dimfmts[0]	= "F10.2";
|  dimfmts[1]	= "F10.2";
|
|/-  getpressure(pressure, SIZE_ARRAY);
|  findMaxMin(pressure, SIZE_ARRAY, &maxpressure, &minpressure);
|-/
|
|  for (i=0;i<MAX_ROW;i++)
|      for (j=0; j<MAX_COL; j++)
|          pressure[i][j] = (i+1)*10 + (j+1)*0.1;  /- to make the
|                values  look the same as those written by Fortran -/
|   maxpressure = 183.6;
|   minpressure = 10.1;
|  for(i=0;i<MAX_ROW;i++)
|    latscales[i] = i+1;
|  for(i=0;i<MAX_COL;i++)
|    lngscales[i] = i+1;
|
|  printf("Writing out SDS\n");
|  DFSDsetdims(rank, shape);
|  DFSDsetdatastrs(datalabel, dataunit, datafmt, "");
|  DFSDsetdimstrs(1, dimlabels[0], dimunits[0], dimfmts[0]);
|  DFSDsetdimstrs(2, dimlabels[1], dimunits[1], dimfmts[1]);
|  DFSDsetdimscale(1, shape[0], latscales);
|  DFSDsetdimscale(2, shape[1], lngscales);
|  DFSDsetrange(&maxpressure, &minpressure);
|  DFSDputdata("testsds.df", 2, shape, pressure);
|
|  printf("Reading in SDS\n");
|  DFSDgetdims("testsds.df", &inRank, inShape, 2);
|  DFSDgetdata("testsds.df", 2, inShape, inPressure);
|  DFSDgetdatastrs(inDatalabel, inDataunit, inDatafmt, inDummy);
|  DFSDgetdimstrs(1, inDimlabels[0], inDimunits[0], inDimfmts[0]);
|  DFSDgetdimstrs(2, inDimlabels[1], inDimunits[1], inDimfmts[1]);
|  DFSDgetdimscale(1, inShape[0], inlatscales);
|  DFSDgetdimscale(2, inShape[1], inlngscales);
|  DFSDgetrange(&inMaxpressure, &inMinpressure);  
|  printf("Checking input vs output:\n");
|
|  printf("  Checking pressure\n");
|  for(i=0;i<MAX_ROW;i++)
|    for(j=0;j<MAX_COL;j++)
|      if (abs(pressure[i][j] - inPressure[i][j]) > epsi)
|          printf("Array position %d, %d is different\n", i, j);
|
|  printf("  ITEM         OUTPUT         INPUT\n");
|  printf("  rank         %-15d%d\n", rank, inRank);
|  printf("  shape[0]     %-15d%d\n", shape[0], inShape[0]);
|  printf("  shape[1]     %-15d%d\n", shape[1], inShape[1]);
|  printf("  datalabel    %-15s%s\n", datalabel, inDatalabel);
|  printf("  dataunit     %-15s%s\n", dataunit, inDataunit);
|  printf("  datafmt      %-15s%s\n", datafmt, inDatafmt);
|  printf("  dimlabels[0] %-15s%s\n", dimlabels[0], inDimlabels[0]);
|  printf("  dimunits[0]  %-15s%s\n", dimunits[0],inDimunits[0]);
|  printf("  dimfmts[0]   %-15s%s\n", dimfmts[0], inDimfmts[0]);
|  printf("  dimlabels[1] %-15s%s\n", dimlabels[1], inDimlabels[1]);
|  printf("  dimunits[1]  %-15s%s\n", dimunits[1],inDimunits[1]);
|  printf("  dimfmts[1]   %-15s%s\n", dimfmts[1], inDimfmts[1]);
|  printf("  maxpressure  %-15f%f\n", maxpressure,inMaxpressure);
|  printf("  minpressure  %-15f%f\n", minpressure,inMinpressure);
|
|  for(i=0;i<MAX_ROW;i++)
|    if (abs(latscales[i] - inlatscales[i]) > epsi)
|      printf("latscales is different at position %d\n", i);
|  for(i=0;i<MAX_COL;i++)
|    if (abs(lngscales[i] - inlngscales[i]) > epsi)
|      printf("lngscales is different at position %d\n", i);
|  printf("Check Completed\n\n");
|}
|
|
|1The obsolete routines DFSDsetmaxmin and DFSDgetmaxmin assumed that all numbers were 
|floating point.  Now that the maximum and minimum values can be of different number types, it 
|is necessary to redefine the parameters in the C versions of the routines from 'floats' to pointers.  
|To avoid confusion, and possible undetected errors, these routines have been replaced by 
|DFSDsetrange and DFSDgetrange.
==============================------------------------------====EOF====*/
