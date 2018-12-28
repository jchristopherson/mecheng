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
void snn_gradient(const network *obj, const snn_cost_fcn_diff dcf, 
                  const double *x, const double *y, bool eval, double *g);
void snn_get_network_output_error(const network *obj, double *x);

#ifdef __cplusplus
}
#endif
#endif
