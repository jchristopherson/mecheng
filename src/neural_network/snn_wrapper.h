/* snn_wrapper.h */
#ifndef SNN_WRAPPER_H_
#define SNN_WRAPPER_H_

#include "snn.h"

#ifdef __cplusplus
extern "C" {
#endif


void snn_run_network(const network *obj, const double *inputs, double *outputs);
void snn_get_weights(const network *obj, double *x);
void snn_set_weights(network *obj, const double *x);

#ifdef __cplusplus
}
#endif
#endif
