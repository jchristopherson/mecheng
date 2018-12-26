#ifndef GENANN_WRAPPER_H_
#define GENANN_WRAPPER_H_

#include "genann.h"

#ifdef __cplusplus
extern "C" {
#endif

void run_network(const genann *ann, const double *inputs, double *outputs);
void get_weights(const genann *ann, double *weights);
void set_weights(genann *ann, const double *weights);

#ifdef __cplusplus
}
#endif
#endif
