#include <stdlib.h>
#include <assert.h>
#include <string.h>

/* C definitions for magneetti program.
 *
 *       slazav, 2014-2016.
 */
const int max = 30;
const int nn =  150;
const int nmem = nn*nn*2 + nn*4;
const int nmem_lim = 2500;


/**********************************************************/
/* we are going to use these functions: */
/* Si units: sizes in [m], field in [T] */
extern void curren_( /* Calculates the current distribution in the shield. */
   int    *nn,      /* dimension of the vector CURSHI */
   int    *max,     /* the maximum number of solenoids and shield parts */
   int    *nshi,    /* the number of shield parts */
   int    *difshi,  /* .TRUE.  if the shield dimensions have been changed */
   double *lenshi,  /* [max] the length of the shield part (0 for a disk) */
   double *radshi,  /* [max] the radius of the shield part */
   double *censhi,  /* [max] the z-coordinate of the center of the shield part */
   double *rhole,   /* [max] the radius of the hole in the disk */
   int    *symshi,  /* [max] .TRUE.  if there is a similar part at z=-CENSHI(I) */
   double *lensol,  /* [max] the length of the solenoid */
   double *radsol,  /* [max] the inner radius of the solenoid */
   double *censol,  /* [max] the z-coordinate of the center of the solenoid */
   int    *layers,  /* [max] the number of layers of wire in the solenoid */
   double *loops,   /* [max] the  number of turns of wire in one layer of the solenoid */
   int    *symsol,  /* [max] .TRUE.  if there is a similar solenoid at z=-CENSOL(I) */
   double *cur,     /* [max] the current in the wire of the solenoid */
   double *wirdia,  /* the diameter of the wire used to wind the solenoids */
   double *foilth,  /* the thickness of the insulating foil between the layers */
   double *bext,    /* the external magnetic field */
   double *traflx,  /* [max] the flux trapped in the center of the shield part */
   double *M,       /* [nn,nn] */
   double *A,       /* [nn] */
   double *curshi   /* [nn] */
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

// data structure
typedef struct {
  int   nn,max;
  int   nshi;    /* number of shield parts */
  double *lenshi, *radshi, *censhi, *rhole;
  double *lensol, *radsol, *censol, *loops, *cur;
  int    *layers;
  double *curshi;
  double wirdia, foilth, bext, *traflx;
  double *M, *A;
  int    difshi, *symshi, *symsol;
} magnet_data_t;

// allocate memory and set it to zero
void
magnet_data_init(magnet_data_t *d){
  d->nn  = nn;
  d->max = max;
  d->nshi = 0;
  d->wirdia = 0;
  d->foilth = 0;
  d->bext   = 0;
  d->difshi = 0;
  size_t ds = sizeof(double);
  size_t is = sizeof(int);
  d->lenshi = (double *)calloc(ds, max);   assert(d->lenshi);
  d->radshi = (double *)calloc(ds, max);   assert(d->radshi);
  d->censhi = (double *)calloc(ds, max);   assert(d->censhi);
  d->rhole  = (double *)calloc(ds, max);   assert(d->rhole);
  d->traflx = (double *)calloc(ds, max);   assert(d->traflx);
  d->lensol = (double *)calloc(ds, max);   assert(d->lensol);
  d->radsol = (double *)calloc(ds, max);   assert(d->radsol);
  d->censol = (double *)calloc(ds, max);   assert(d->censol);
  d->loops  = (double *)calloc(ds, max);   assert(d->loops);
  d->cur    = (double *)calloc(ds, max);   assert(d->cur);
  d->layers =    (int *)calloc(is, max);   assert(d->layers);
  d->curshi = (double *)calloc(ds, nn);    assert(d->curshi);
  d->M      = (double *)calloc(ds, nn*nn); assert(d->M);
  d->A      = (double *)calloc(ds, nn);    assert(d->A);
  d->symshi =    (int *)calloc(is, max);   assert(d->symshi);
  d->symsol =    (int *)calloc(is, max);   assert(d->symsol);
}

// free memory
void
magnet_data_free(magnet_data_t *d){
  free(d->lenshi);
  free(d->radshi);
  free(d->censhi);
  free(d->rhole);
  free(d->traflx);
  free(d->lensol);
  free(d->radsol);
  free(d->censol);
  free(d->loops);
  free(d->cur);
  free(d->layers);
  free(d->curshi);
  free(d->M);
  free(d->A);
  free(d->symshi);
  free(d->symsol);
}


extern void iwkin_(int *N); // allocate memory
struct {double RWKSP[nmem];} worksp_;

// wrappers for fortran functions

void curren(magnet_data_t *d){
  int N = nmem;
  if (nmem>nmem_lim) iwkin_(&N);
  curren_(
     &d->nn, &d->max, &d->nshi, &d->difshi,
     d->lenshi, d->radshi, d->censhi, d->rhole, d->symshi,
     d->lensol, d->radsol, d->censol, d->layers, d->loops, d->symsol,
     d->cur, &d->wirdia, &d->foilth, &d->bext, d->traflx,
     d->M, d->A, d->curshi);
}

void field(double z, double r, magnet_data_t *d,
           double *bz, double *br){
  field_(&z, &r, &d->nn, &d->max, &d->nshi,
     d->lenshi, d->radshi, d->censhi, d->rhole, d->curshi,
     d->censol, d->lensol, d->radsol, d->loops, d->layers,
     &d->wirdia, &d->foilth, d->cur, &d->bext, bz, br);
}

void flux(double z, double r, magnet_data_t *d, double *magflu){
  flux_(&z, &r, &d->nn, &d->max, &d->nshi,
     d->lenshi, d->radshi, d->censhi, d->rhole, d->curshi,
     d->censol, d->lensol, d->radsol, d->loops, d->layers,
     &d->wirdia, &d->foilth, d->cur, &d->bext, magflu);
}

