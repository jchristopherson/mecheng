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

void snn_gradient(const network *obj, const snn_cost_fcn_diff dcf, 
                  const double *x, const double *y, bool eval, double *g)
{
    /* Local Variables */
    int i;
    double *ptr;

    /* Process */
    ptr = snn_eval_gradient(obj, dcf, x, y, eval);

    /* Copy the contents at ptr to g */
    for (i = 0; i < obj->total_coefficient_count; ++i) g[i] = ptr[i];
}

void snn_get_network_output_error(const network *obj, double *x) {
    /* Local Variables */
    int i;
    double *ptr;

    /* Obtain a pointer to the last layer error array */
    ptr = obj->delta_pointers[obj->total_layer_count - 2];
    for (i = 0; i < obj->output_count; ++i) x[i] = ptr[i];
}
