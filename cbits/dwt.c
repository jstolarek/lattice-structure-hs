#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#ifdef USE_SSE3
#include <x86intrin.h>
#include <emmintrin.h>
#include <pmmintrin.h>
#endif
#include "dwt.h"

// Believe it or not, but stdlib.h does not specify function prototypes!
// So this one here is to avoid warning about implicit function declarations
// produced when it sees the call to posix_memalign for the first time.
int posix_memalign(void **memptr, size_t alignment, size_t size);

/* Forward and inverse wavelet transform. These are wrappers around
 * c_dwt_worker.
 *
 * ls - array containing lattice structure coefficients
 *      represented as angles (in radians)
 * ln - length of ls array
 * xs - signal to transform. Must be even length. No assertions
 *      are made to verify this!
 * xn - length of input signla
 *
 * return value - transformed signal
 */
double* c_dwt( double* ls, int ln, double* xs, int xn ) {
  int lm = 0;
  return c_dwt_worker( lm, ls, ln, xs, xn );
}


double* c_idwt( double* ls, int ln, double* xs, int xn ) {
  int lm = 1 - (ln % 2);
  return c_dwt_worker( lm, ls, ln, xs, xn );
}


/* Worker function that calculates both forward and inverse wavelet transform
 *
 * lm - layer modifier that decides the shift of first input layer. In case of
 *      forward transform this is always 0. For inverse transform it is 1 if
 *      number of layers is even, otherwise 0.
 * ls, ln, ds, dn - the same as for c_dwt / c_idwt
 *
 * return value - transformed signal
 */
double* c_dwt_worker( int lm, double* ls, int ln, double* xs, int xn ) {
  double sin_, cos_;

  double* ds;
  posix_memalign( (void**) &ds, 16, xn * sizeof( double ) );

  // making sure that trival cases work
  if ( ln == 0 || xn == 0) {
    return (double*) memcpy( (void*) ds, (void*) xs, xn * sizeof( double ) );
  }

  sin_ = sin( ls[ 0 ] );
  cos_ = cos( ls[ 0 ] );
  // first layer reads from input xs array and writes to output ds array
  lm = c_lattice_worker( lm, xs, ds, xn, sin_, cos_ );

  for( int lnc = 1; lnc < ln; lnc++) {
    sin_ = sin( ls[ lnc ] );
    cos_ = cos( ls[ lnc ] );
    // subsequent layers operate in situ on output ds array
    lm = c_lattice_worker( lm, ds, ds, xn, sin_, cos_ );
  }

  return ds;
}

/* Wrapper for lattice function. Used when calling C implementation of lattice
 * from Haskell. C implementations of dwt/idwt call c_lattice_worker directly.
 * See c_lattice_worker below for parameter description.
 */
double* c_lattice( int lm, double sin_, double cos_, double* inArr,
                   int arrLen ) {
  double* outArr;
  posix_memalign( (void**) &outArr, 16, arrLen * sizeof( double ) );

  // do computations only if non-empty array was passed
  if ( arrLen != 0) {
    c_lattice_worker( lm, inArr, outArr, arrLen, sin_, cos_ );
  }

  return outArr;
}

/* One layer of a lattice structure. Processes elements in pairs.
 *
 * lm - layer modifier (description above)
 * inArr - input array
 * outArr - output array
 * arrLen - length of input and output arrays (it must be identical!). This
 *          must be even, though - as stated before - there is no assertion
 *          to verify this.
 * sin_, cos_ - weights of base operations in the layer. As name suggests
 *              these are values of trigonometric functions calculated by
 *              ecternal function.
 */
int c_lattice_worker( int lm, double* inArr, double* outArr, int arrLen,
             double sin_,  double cos_ ) {
  int xi, yi;
  double x, y;
#ifdef USE_SSE3
  __m128d _msin, _mcos;
  __m128d _m0, _m1;
  _msin = _mm_loaddup_pd( &sin_ );
  _mcos = _mm_loaddup_pd( &cos_ );
#endif

  // this handles the border case (if there is such)
  // probably not worth rewriting this using SSE3
  if (lm == 1) {
    xi = arrLen - 1;
    yi = 0;
    x  = inArr[ xi ];
    y  = inArr[ yi ];
    outArr[ xi ] = x * cos_ + y * sin_;
    outArr[ yi ] = x * sin_ - y * cos_;
  }

  // processing signal elements in pairs. If layer modifier lm is 1, then
  // loop makes one less iteration - it ignores the border case operating
  // on first and last elements of the signal

  for ( int xnc = lm; xnc < (arrLen - (lm << 1)); xnc += 2 ) {
#ifdef USE_SSE3
    _m0 = _mm_loadu_pd( &inArr[ xnc ] );
    _m1 = _mm_shuffle_pd( _m0, _m0, 0x1 );
    _m0 = _mm_mul_pd( _msin, _m0 );
    _m1 = _mm_mul_pd( _mcos, _m1 );
    _m0 = _mm_addsub_pd( _m0, _m1 );
    _m1 = _mm_shuffle_pd( _m0, _m0, 0x1 );
    _mm_storeu_pd( &outArr[ xnc     ], _m1 );
#else
    xi = xnc;
    yi = xi + 1;
    x  = inArr[ xi ];
    y  = inArr[ yi ];
    outArr[ xi ] = x * cos_ + y * sin_;
    outArr[ yi ] = x * sin_ - y * cos_;
#endif
  }

  // fliping the lm modifier
  return 1 - lm;
}
