////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// random.cc
//
// High quality random number generators
//
// Taken from "Numerical Recipes in C" 2nd Edition
////////////////////////////////////////////////////////////////////////////
#include <time.h>
#include <math.h>
#include "random.h"

// Defines for ran1()
//
#define IA     16807
#define IM     2147483647
#define AM     (1.0/IM)
#define IQ     127773
#define IR     2836
#define NTAB   32
#define NDIV   (1+(IM-1)/NTAB)

#define EPS    1e-5
#define RNMX   (1.0-EPS)

// Return a uniformly distributed random deviate
double ran1(void)
{
  int            j;
  long           k;
  static long    iy=0;
  static long    iv[NTAB];
  double         temp;
  static long    idum;
  time_t         t;
   
  if(!iy || idum <= 0)
  {
     time(&t);
     idum=(*(unsigned long *)&t);

     for(j=NTAB+7; j>=0; j--)
     {
       k=idum/IQ;
       idum=IA*(idum-k*IQ)-IR*k;
       if(idum<0) idum+=IM;
       if(j<NTAB) iv[j]=idum;
     }
     iy=iv[0];
  }
  k=idum/IQ;
  idum=IA*(idum-k*IQ)-IR*k;
  if(idum<0) idum+=IM;
  j=iy/NDIV;
  iy=iv[j];
  iv[j]=idum;
  if((temp=AM*iy) > RNMX) return RNMX;
  else                    return temp;
}

// Return a Gaussian distributed random deviate
double gasdev(void)
{
  static int      iset=0;
  static double   gset;
  double          fac, rsq, v1, v2;
   
  if(iset==0)
  {
    do
    {
      v1=2.0*ran1()-1.0;
      v2=2.0*ran1()-1.0;
      rsq=v1*v1+v2*v2;
    } while(rsq >= 1.0 || rsq == 0.0);
      
    fac=sqrt(-2.0*log(rsq)/rsq);
    gset=v1*fac;
    iset=1;
    return v2*fac;
  }
  else
  {
    iset=0;
    return gset;
  }
}
