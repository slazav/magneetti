#include <mex.h>
#include <string.h>
#include <stdio.h>

/* Matlab interface to the magneetti program.
 *
 *       slazav, 2014.
 */

/**********************************************************/
/* global constants */
/* dimension for the current distribution in shield parts */
#define NN 200
/* the maximum number of solenoids and shield parts */
#define MAX 30

/**********************************************************/
/* we are going to use these functions: */
extern void curren_( /* Calculates the current distribution in the shield. */
   int    *nn,      /* dimension of the vector CURSHI */
   int    *max,     /* the maximum number of solenoids and shield parts */
   int    *nshi,    /* the number of shield parts */
   bool   *difshi,  /* [max] .TRUE.  if the shield dimensions have been changed */
   double *lenshi,  /* [max] the length of the shield part (0 for a disk) */
   double *radshi,  /* [max] the radius of the shield part */
   double *censhi,  /* [max] the z-coordinate of the center of the shield part */
   double *rhole,   /* [max] the radius of the hole in the disk */
   bool   *symshi,  /* [max] .TRUE.  if there is a similar part at z=-CENSHI(I) */
   double *lensol,  /* [max] the length of the solenoid */
   double *radsol,  /* [max] the inner radius of the solenoid */
   double *censol,  /* [max] the z-coordinate of the center of the solenoid */
   int    *layers,  /* [max] the number of layers of wire in the solenoid */
   double *loops,   /* [max] the  number of turns of wire in one layer of the solenoid */
   bool   *symsol,  /* [max] .TRUE.  if there is a similar solenoid at z=-CENSOL(I) */
   double *cur,     /* [max] the current in the wire of the solenoid */
   double *wirdia,  /* the diameter of the wire used to wind the solenoids */
   double *foilth,  /* the thickness of the insulating foil between the layers */
   double *bext,    /* the external magnetic field */
   double *traflx,  /* [max] the flux trapped in the center of the shield part */
   double *M,       /* [nn,nn] */
   double *A,       /* [nn] */
   double *X        /* [nn] */
);
extern void field_( /* Calculates the magnetic field in the point (r, z). */
   double *z,       /* z coordinate */
   double *r,       /* r coordinate */
   int    *nn,      /* dimension of the vector CURSHI */
   int    *max,     /* the maximum number of solenoids and shield parts */
   int    *nshi,    /* the number of shield parts */
   double *lenshi,  /* [max] the length of the shield part (0 for a disk) */
   double *radshi,  /* [max] the radius of the shield part */
   double *censhi,  /* [max] the z-coordinate of the center of the shield part */
   double *rhole,   /* [max] the radius of the hole in the disk */
   double *curshi,  /* [nn]  current distribution in all shields */
   double *censol,  /* [max] the z-coordinate of the center of the solenoid */
   double *lensol,  /* [max] the length of the solenoid */
   double *radsol,  /* [max] the inner radius of the solenoid */
   double *loops,   /* [max] the  number of turns of wire in one layer of the solenoid */
   int    *layers,  /* [max] the number of layers of wire in the solenoid */
   double *wirdia,  /* the diameter of the wire used to wind the solenoids */
   double *foilth,  /* the thickness of the insulating foil between the layers */
   double *cur,     /* [max] the current in the wire of the solenoid */
   double *bext,    /* the external magnetic field */
   double *bz,      /* out: magnetic field z component */
   double *br       /* out: magnetic field r component */
);
extern void flux_( /* Calculates the vector potential in the point (r, z). */
   double *z,       /* z coordinate */
   double *r,       /* r coordinate */
   int    *nn,      /* dimension of the vector CURSHI */
   int    *max,     /* the maximum number of solenoids and shield parts */
   int    *nshi,    /* the number of shield parts */
   double *lenshi,  /* [max] the length of the shield part (0 for a disk) */
   double *radshi,  /* [max] the radius of the shield part */
   double *censhi,  /* [max] the z-coordinate of the center of the shield part */
   double *rhole,   /* [max] the radius of the hole in the disk */
   double *curshi,  /* [nn]  current distribution in all shields */
   double *censol,  /* [max] the z-coordinate of the center of the solenoid */
   double *lensol,  /* [max] the length of the solenoid */
   double *radsol,  /* [max] the inner radius of the solenoid */
   double *loops,   /* [max] the  number of turns of wire in one layer of the solenoid */
   int    *layers,  /* [max] the number of layers of wire in the solenoid */
   double *wirdia,  /* the diameter of the wire used to wind the solenoids */
   double *foilth,  /* the thickness of the insulating foil between the layers */
   double *cur,     /* [max] the current in the wire of the solenoid */
   double *bext,    /* the external magnetic field */
   double *magflu   /* out: the magnetic flux through the ring of radius R located at z = Z */
);


/**********************************************************/
/* set a field to matlab/octave structure from a double array */
void
set_field(mxArray *mst, int nfld, int N, double *arr){
  int i;
  double *arr1 = mxMalloc(N*sizeof(double));
  mxArray *mxarr = mxCreateNumericMatrix(N, 1, mxDOUBLE_CLASS, mxREAL);
  for (i=0; i<N; i++) arr1[i]=arr[i];
  mxSetData(mxarr, arr1);
  mxSetFieldByNumber(mst, 0, nfld, mxarr);
}
/* set a field to matlab/octave structure from an integer array */
void
set_field_i(mxArray *mst, int nfld, int N, int *arr){
  int i;
  double *arr1 = mxMalloc(N*sizeof(double));
  mxArray *mxarr = mxCreateNumericMatrix(N, 1, mxDOUBLE_CLASS, mxREAL);
  for (i=0; i<N; i++) arr1[i]=(double)arr[i];
  mxSetData(mxarr, arr1);
  mxSetFieldByNumber(mst, 0, nfld, mxarr);
}

/* get a field from matlab structure into a double array */
void
get_field(const mxArray *mst, int nfld, int N, double *arr){
  int i;
  mxArray *v;
  double *arr1;
  v = mxGetFieldByNumber(mst, 0, nfld);
  if (!mxIsDouble(v))
    mexErrMsgTxt("Not a floating point array in a structure field");
  if (mxGetNumberOfElements(v)!=N)
    mexErrMsgTxt("Wrong size of array in a structure field");
  arr1 = mxGetData(v);
  for (i=0;i<N;i++) arr[i] = arr1[i];
}
/* get a field from matlab structure into an integer array */
void
get_field_i(const mxArray *mst, int nfld, int N, int *arr){
  int i;
  mxArray *v;
  double *arr1;
  v = mxGetFieldByNumber(mst, 0, nfld);
  if (!mxIsDouble(v))
    mexErrMsgTxt("Not an array in a structure field");
  if (mxGetNumberOfElements(v)!=N)
    mexErrMsgTxt("Wrong size of array in a structure field");
  arr1 = mxGetData(v);
  for (i=0;i<N;i++) arr[i] = (int)arr1[i];
}

/**********************************************************/
/* Convert c structure to matlab.
   List of field is read from FIELDS file. */
/*
mxArray *
pars_c2mat(PARS_T *cst){
  int i;
  const char *keys[256]; //max number of fields
  i=0;
#define INT(name) keys[i]=#name; i++;
#define DBL(name) keys[i]=#name; i++;
#define ARR(name) keys[i]=#name; i++;
#include FIELDS
#undef INT
#undef DBL
#undef ARR
  mxArray * mst = mxCreateStructMatrix(1,1,i, keys);
  i=0;
#define INT(name)  set_field_i(mst, i, 1, &cst->name); i++;
#define DBL(name)  set_field(mst, i, 1, &cst->name); i++;
#define ARR(name)  set_field(mst, i, cst->n, cst->name); i++;
#include FIELDS
#undef INT
#undef DBL
#undef ARR
  return mst;
}
*/
/* Convert matlab structure to c.
   List of field is read from FIELDS file. */
/*
void
pars_mat2c(PARS_T *cst, const mxArray *mst){
  int i;
  const char * k;

  if (!mxIsStruct(mst))
    mexErrMsgTxt("Structure expected");
  if (mxGetNumberOfElements(mst)!=1)
    mexErrMsgTxt("One element in StructureArray is needed");

  for (i = 0; i < mxGetNumberOfFields(mst); i++){
    k = mxGetFieldNameByNumber(mst, i);
#define INT(name)\
    if (strcmp(k, #name)==0){ get_field_i(mst, i, 1, &cst->name); continue;}
#define DBL(name)\
    if (strcmp(k, #name)==0){ get_field(mst, i, 1, &cst->name); continue;}
#define ARR(name)\
    if (strcmp(k, #name)==0){ get_field(mst, i, cst->n, cst->name); continue;}
#include FIELDS
#undef INT
#undef DBL
#undef ARR
    mexErrMsgTxt("Unknown field");
  }
}
*/

/**********************************************************/
void
mexFunction(int nlhs, mxArray *plhs[],
            int nrhs, const mxArray *prhs[]){

  int i,j;
  double in[6];


  /* Parse arguments */
/*
  if (nlhs != 1)
    mexErrMsgTxt("output argument is needed");
  if (nrhs != 7)
    mexErrMsgTxt("Usage: [Bz, Br, cu] = mangeetti_field(z,r, shields, solenoids, wire, b_ext, traflx)");
*/
  /* Check arg types */
/*
  if (!mxIsStruct(mst))
    mexErrMsgTxt("Structure expected");
  if (mxGetNumberOfElements(mst)!=1)
    mexErrMsgTxt("One element in StructureArray is needed");

    if (!mxIsDouble(prhs[i]))
      mexErrMsgTxt("non-numeric argument");
    if (mxGetM(prhs[i])!=1 || mxGetM(prhs[i])!=1)
      mexErrMsgTxt("non-scalar argument");


  for (i=0; i<6; i++){
    if (i==4 && nrhs < 5){ in[i]=MAXN; continue; }
    if (i==5 && nrhs < 6){ in[i]=0; continue; }
    if (!mxIsDouble(prhs[i]))
      mexErrMsgTxt("non-numeric argument");
    if (mxGetM(prhs[i])!=1 || mxGetM(prhs[i])!=1)
      mexErrMsgTxt("non-scalar argument");
    in[i] = *mxGetPr(prhs[i]);
  }
  i = (int)in[4]; // double -> int
  j = (int)in[5]; // double -> int
  FUNC(&in[0], &in[1], &in[2], &in[3], &i, &j);
  plhs[0] = pars_c2mat(&PARS);
*/

  /* Run the calculateion */
  int nn = NN;
  int max = MAX;

  int   nshi, nsol;    /* number of solenoids and shield parts */
  double lenshi[MAX], radshi[MAX], censhi[MAX], rhole[MAX];
  double lensol[MAX], radsol[MAX], censol[MAX], loops[MAX], cur[MAX];
  int    layers[MAX];
  double curshi[NN];
  double wirdia, foilth, bext, traflx;
  double magflu, br, bz;
  double M[NN*NN], A[NN], X[NN];
  double z,r;
  bool   difshi[MAX], symshi[MAX], symsol[MAX];

  /* Initialize data
     for all unused solenoids should be lensol=0
   */
  memset(lenshi, 0, MAX*sizeof(double));
  memset(radshi, 0, MAX*sizeof(double));
  memset(censhi, 0, MAX*sizeof(double));
  memset(rhole,  0, MAX*sizeof(double));
  memset(lensol, 0, MAX*sizeof(double));
  memset(radsol, 0, MAX*sizeof(double));
  memset(censol, 0, MAX*sizeof(double));
  memset(loops,  0, MAX*sizeof(double));
  memset(cur,    0, MAX*sizeof(double));
  memset(layers, 0, MAX*sizeof(int));
  memset(curshi, 0, NN*sizeof(double));
  memset(M,      0, NN*NN*sizeof(double));
  memset(A,      0, NN*sizeof(double));
  memset(X,      0, NN*sizeof(double));

  curren_(
     &nn, &max, &nshi, difshi, lenshi, radshi, censhi, rhole,
     symshi, lensol, radsol, censol, layers, loops,
     symsol, cur, &wirdia, &foilth, &bext, &traflx, M, A, curshi);

  field_(&z, &r, &nn, &max, &nshi, lenshi, radshi, censhi, rhole,
     curshi, censol, lensol, radsol, loops, layers,
     &wirdia, &foilth, cur, &bext, &bz, &br);

  flux_(&z, &r, &nn, &max, &nshi, lenshi, radshi, censhi, rhole,
     curshi, censol, lensol, radsol, loops, layers,
     &wirdia, &foilth, cur, &bext, &magflu);

  return;
}
