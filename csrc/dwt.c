#include <math.h>
#include "dwt.h"


void dwt( double* ls, int ln, double* xs, int xn) {
  int xi, yi;
  double sin_, cos_;
  double x, y;

  for( int lnc = 0; lnc < ln; lnc++) {
    sin_ = sin( ls[ lnc ] );
    cos_ = cos( ls[ lnc ] );

    for ( int xnc = 0; xnc < xn; xnc += 2 ) {
      xi = ( xnc + lnc ) % xn;
      yi = (  xi +   1 ) % xn;
      x  = xs[ xi ];
      y  = xs[ yi ];
      xs[ xi ] = x * cos_ + y * sin_;
      xs[ yi ] = x * sin_ - y * cos_;
    }
  }
}


void idwt( double* ls, int ln, double* xs, int xn) {
  int xi, yi;
  double sin_, cos_;
  double x, y;

  for( int lnc = 0; lnc < ln; lnc++) {
    sin_ = sin( ls[ lnc ] );
    cos_ = cos( ls[ lnc ] );

    for ( int xnc = 0; xnc < xn; xnc += 2 ) {
      xi = ( xnc - lnc ) % xn;
      if (xi < 0) { xi += xn; }
      yi = (  xi +   1 ) % xn;
      if (yi < 0) { yi += xn; }
      x  = xs[ xi ];
      y  = xs[ yi ];
      xs[ xi ] = x * cos_ + y * sin_;
      xs[ yi ] = x * sin_ - y * cos_;
    }
  }
}
