#ifndef POLYNOMIAL_H_
#define POLYNOMIAL_H_

#include <stdbool.h>
#include <complex.h>

#ifdef __cplusplus
extern "C" {
#endif

void fit_poly(int order, bool thruzero, int npts, const double *x, 
    const double *y, double *c);
void eval_poly(int order, const double *c, int npts, const double *x, double *y);
void eval_poly_cmplx(int order, const double *c, int npts, const double complex *x, 
    double complex *y);
void poly_roots(int order, const double *c, double complex *rts);

#ifdef __cplusplus
}
#endif  // __cplusplus
#endif  // POLYNOMIAL_H_