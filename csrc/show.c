#include <stdio.h>
#include "show.h"

void showDArray( double* ds, int dn ) {
  printf( "[" );
  for( int i = 0; i < dn; i++ ) {
    printf("%f, ", ds[ i ] );
  }
  printf( "]\n" );
}
