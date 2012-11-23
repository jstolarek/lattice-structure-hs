#ifndef _DWT_H_
#define _DWT_H_

double* c_dwt(  double* ls, int ln, double* xs, int xn);
double* c_idwt( double* ls, int ln, double* xs, int xn);
double* c_dwt_worker( int lm, double* ls, int ln, double* xs, int xn);
double* c_lattice( int lm, double sin_, double cos_, double* inArr, 
                   int arrLen );
int c_lattice_worker( int lm, double* inArr, double* outArr, int arrLen,
                      double sin_,  double cos_ );
#endif 
