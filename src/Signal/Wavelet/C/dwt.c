#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "dwt.h"


double* c_dwt( double* ls, int ln, double* xs, int xn) {
  int xi, yi;
  double sin_, cos_;
  double x, y;

  double* ds = malloc( xn * sizeof( double ) );

  if ( ln == 0 ) {
    return (double*) memcpy( (void*) ds, (void*) xs, xn * sizeof( double ) );
  }

  sin_ = sin( ls[ 0 ] );
  cos_ = cos( ls[ 0 ] );

  for ( int xnc = 0; xnc < xn; xnc += 2 ) {
    xi = xnc % xn;
    yi = ( xi +   1 ) % xn;
    x  = xs[ xi ];
    y  = xs[ yi ];
    ds[ xi ] = x * cos_ + y * sin_;
    ds[ yi ] = x * sin_ - y * cos_;
  }


  for( int lnc = 1; lnc < ln; lnc++) {
    sin_ = sin( ls[ lnc ] );
    cos_ = cos( ls[ lnc ] );

    for ( int xnc = 0; xnc < xn; xnc += 2 ) {
      xi = ( xnc + lnc ) % xn;
      yi = (  xi +   1 ) % xn;
      x  = ds[ xi ];
      y  = ds[ yi ];
      ds[ xi ] = x * cos_ + y * sin_;
      ds[ yi ] = x * sin_ - y * cos_;
    }
  }
  

  return ds;
}


double* c_idwt( double* ls, int ln, double* xs, int xn) {
  int xi, yi;
  double sin_, cos_;
  double x, y;

  double* ds = malloc( xn * sizeof( double ) );

  if ( ln == 0 ) {
    return (double*) memcpy( (void*) ds, (void*) xs, xn * sizeof( double ) );
  }

  sin_ = sin( ls[ 0 ] );
  cos_ = cos( ls[ 0 ] );

  for ( int xnc = 0; xnc < xn; xnc += 2 ) {
    xi = (xnc + ln - 1) % xn;
    yi = (xi + 1) % xn;
    x  = xs[ xi ];
    y  = xs[ yi ];
    ds[ xi ] = x * cos_ + y * sin_;
    ds[ yi ] = x * sin_ - y * cos_;
  }

  for( int lnc = 1; lnc < ln; lnc++) {
    sin_ = sin( ls[ lnc ] );
    cos_ = cos( ls[ lnc ] );

    for ( int xnc = 0; xnc < xn; xnc += 2 ) {
      xi = ( xnc + (ln - lnc - 1) ) % xn;
      yi = (  xi +   1 ) % xn;
      x  = ds[ xi ];
      y  = ds[ yi ];
      ds[ xi ] = x * cos_ + y * sin_;
      ds[ yi ] = x * sin_ - y * cos_;
    }
  }

  return ds;
}
