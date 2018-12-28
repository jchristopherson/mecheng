/* snn_wrapper.c */

#include "snn.h"


void snn_run_network(const network *obj, const double *inputs, double *outputs) {
    /* Local Variables */
    int i;
    double *ptr;

    /* Process */
    ptr = snn_eval_network(obj, inputs);

    /* Copy the contents at ptr into outputs */
    for (i = 0; i < obj->output_count; ++i) outputs[i] = ptr[i];
}

void snn_get_weights(const network *obj, double *x) {
    /* Local Variables */
    int i;

    /* Process */
    for (i = 0; i < obj->total_weight_count; ++i) x[i] = obj->weights[i];
}

void snn_set_weights(network *obj, const double *x) {
    /* Local Variables */
    int i;

    /* Process */
    for (i = 0; i < obj->total_weight_count; ++i) obj->weights[i] = x[i];
}