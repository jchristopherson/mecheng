#include "genann_wrapper.h"

void run_network(const genann *ann, const double *inputs, double *outputs) {
    // Local Variables
    double *temp;
    int i, n;

    // Process
    temp = genann_run(ann, inputs);

    // Copy the results to the outputs array
    n = ann->outputs;
    for (i = 0; i < n; ++i) outputs[i] = temp[i];
}

void get_weights(const genann *ann, double *weights) {
    // Local Variables
    int i, n;

    // Process
    n = ann->total_weights;
    for (i = 0; i < n; ++i) weights[i] = ann->weight[i];
}

void set_weights(genann *ann, const double *weights) {
    // Local Variables
    int i, n;

    // Process
    n = ann->total_weights;
    for (i = 0; i < n; ++i) ann->weight[i] = weights[i];
}
