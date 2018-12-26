#ifndef GENANN_WRAPPER_H_
#define GENANN_WRAPPER_H_

#include "genann.h"

#ifdef __cplusplus
extern "C" {
#endif

void run_network(const genann *ann, const double *inputs, double *outputs);

#ifdef __cplusplus
}
#endif
#endif
