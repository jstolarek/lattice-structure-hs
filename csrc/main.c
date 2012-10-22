#include <stdlib.h>
#include <string.h>
#include "dwt.h"
#include "show.h"

int main() {
  int sigSize = 16;
  int lsSize  = 3;
  double* sig = (double*) malloc( sigSize * sizeof( double ) );
  double* ls  = (double*) malloc(  lsSize * sizeof( double ) );

  memcpy( ls,  (double[]) {0.5235987755982988, \
        0.4363323129985824, 0.6981317007977318}, lsSize * sizeof( double ));
  memcpy( sig, (double[]) {1,2,2,4,-3,5,0,1,1,-1,-2,2,4,5,6,3}, 
          sigSize * sizeof( double ));

  showDArray( sig, sigSize );

  dwt( ls, lsSize, sig, sigSize );

  showDArray( sig, sigSize );

  memcpy( ls,  (double[]) {0.6981317007977318, \
        0.4363323129985824, 0.5235987755982988}, lsSize * sizeof( double ));

  idwt( ls, lsSize, sig, sigSize );

  showDArray( sig, sigSize );  

  free( ls  );
  free( sig );

  return 0;
}
