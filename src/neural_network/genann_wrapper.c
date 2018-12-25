#include "genann_wrapper.h"
#include <stdlib.h>

void run_network(const genann *ann, const double *inputs, double *outputs) {
    // Local Variables
    double *temp;
    int i, n;

    // Process
    temp = genann_run(ann, inputs);

    // Copy the results to the outputs array
    n = ann->outputs;
    for (i = 0; i < n; ++i) outputs[i] = temp[i];

    // Free memory
    free(temp);
}