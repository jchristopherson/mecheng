// c_polynomial_example_1.c

#include <stdio.h>
#include "polynomial.h"

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

int main() {
    // Local Variables
    const int npts = 21;
    const int order = 3;
    const int ncoeff = 4;
    double c[ncoeff], yc[npts];
    double complex rts[order], yrts[order];
    double x[21] = { 
        0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8,
        0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7,
        1.8, 1.9, 2.0
    };
    double y[21] = {
        1.216737514, 1.250032542, 1.305579195, 1.040182335,
        1.751867738, 1.109716707, 2.018141531, 1.992418729,
        1.807916923, 2.078806005, 2.698801324, 2.644662712,
        3.412756702, 4.406137221, 4.567156645, 4.999550779,
        5.652854194, 6.784320119, 8.307936836, 8.395126494,
        10.30252404
    };
    int i;
    double emax, emin, e;

    // Fit a 3rd order polynomial
    fit_poly(order, false, npts, x, y, c);
    printf("%f + (%f) x + (%f) x^2 + (%f) x^3\n",
        c[0], c[1], c[2], c[3]);

    // Evaluate the fitted polynomial
    eval_poly(order, c, npts, x, yc);

    // Compute the largest and smallest error terms
    for (i = 0; i < npts; ++i) {
        e = yc[i] -  y[i];
        if (i == 0) {
            emax = emin = e;
        }
        else {
            emax = MAX(emax, e);
            emin = MIN(emin, e);
        }
    }
    printf("Min Error: %f\nMax Error: %f\n", emin, emax);

    // Compute the roots of the polynomial
    poly_roots(order, c, rts);

    // Evaluate the polynomial at each root to verify the root
    eval_poly_cmplx(order, c, order, rts, yrts);
    for (i = 0; i < order; ++i) {
        printf("Root %i: (%f, %f)\tf(x) = (%f, %f)\n",
            i, creal(rts[i]), cimag(rts[i]), creal(yrts[i]), cimag(yrts[i]));
    }

    // End
    return 0;
}
