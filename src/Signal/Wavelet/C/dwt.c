#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "dwt.h"

#include <stdio.h>

double* c_dwt( double* ls, int ln, double* xs, int xn) {
  int lm = 0;
  return c_dwt_worker( lm, ls, ln, xs, xn ); 
}


double* c_idwt( double* ls, int ln, double* xs, int xn) {
  int lm = 1 - (ln - ((ln >> 1) << 1));
  return c_dwt_worker( lm, ls, ln, xs, xn ); 
}


double* c_dwt_worker( int lm, double* ls, int ln, double* xs, int xn) {
  double sin_, cos_;

  double* ds = malloc( xn * sizeof( double ) );

  if ( ln == 0 || xn == 0) {
    return (double*) memcpy( (void*) ds, (void*) xs, xn * sizeof( double ) );
  }

  sin_ = sin( ls[ 0 ] );
  cos_ = cos( ls[ 0 ] );
  lm = lattice( lm, xs, ds, xn, sin_, cos_ );

  for( int lnc = 1; lnc < ln; lnc++) {
    sin_ = sin( ls[ lnc ] );
    cos_ = cos( ls[ lnc ] );
    lm = lattice( lm, ds, ds, xn, sin_, cos_ );
  }

  return ds;
}


inline int lattice( int lm, double* inArr, double* outArr, int arrLen,
             double sin_,  double cos_ ) {
  int xi, yi;
  double x, y;

  for ( int xnc = 0; xnc < (arrLen - (lm << 1)); xnc += 2 ) {
    xi = xnc + lm;
    yi = xi + 1;
    x  = inArr[ xi ];
    y  = inArr[ yi ];
    outArr[ xi ] = x * cos_ + y * sin_;
    outArr[ yi ] = x * sin_ - y * cos_;
  }

  if (lm == 1) {
    xi = arrLen - 1;
    yi = 0;
    x  = inArr[ xi ];
    y  = inArr[ yi ];
    outArr[ xi ] = x * cos_ + y * sin_;
    outArr[ yi ] = x * sin_ - y * cos_;
  }
  
  return 1 - lm;
}
