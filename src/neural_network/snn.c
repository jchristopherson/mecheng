/* snn.c */

#include <math.h>
#include <stdlib.h>
#include <time.h>
#include "snn.h"

#define SQR(x) ((x) * (x))
#define INDEX(i, j, m) ((j) * (m) + (i))


network* snn_init_network(int nlayers, const int *node_counts, int *err) {
    /* Local Variables */
    int i, nin, nout, noffset;
    network *obj = NULL;

    /* Initialization */
    *err = SNN_NO_ERROR;

    /* Ensure proper layer structure */
    if (nlayers < 3) {
        *err = SNN_INVALID_INPUT_ERROR;
        return NULL;
    }

    /* Allocate memory for the network object */
    obj = (network*)malloc(sizeof(network));
    if (!obj) {
        *err = SNN_OUT_OF_MEMORY_ERROR;
        return NULL;
    }

    /* Establish version information */
    obj->version = 1;

    /* Initialize appropriate constants */
    obj->total_layer_count = nlayers;
    obj->input_count = node_counts[0];
    obj->output_count = node_counts[nlayers - 1];
    obj->hidden_layer_count = nlayers - 2;

    obj->total_neuron_count = 0;
    for (i = 0; i < nlayers; ++i) {
        obj->total_neuron_count += node_counts[i];
    }

    /* Allocate memory for the weighting factors */
    obj->total_weight_count = 0;
    for (i = 1; i < nlayers; ++i) {
        nin = node_counts[i-1];
        nout = node_counts[i];
        obj->total_weight_count += (nin * nout);
    }
    obj->weights = (double*)malloc((size_t)(obj->total_weight_count * sizeof(double)));
    if (!obj->weights) {
        *err = SNN_OUT_OF_MEMORY_ERROR;
        snn_free_network(obj);
        return NULL;
    }
    obj->weight_pointers = (double**)malloc((size_t)((nlayers - 1) * sizeof(double*)));
    if (!obj->weight_pointers) {
        *err = SNN_OUT_OF_MEMORY_ERROR;
        snn_free_network(obj);
        return NULL;
    }

    for (i = 1; i < nlayers; ++i) {
        nin = node_counts[i-1];
        nout = node_counts[i];
        obj->weight_pointers[i-1] = obj->weights + (i - 1) * (nin * nout);
    }

    /* Store the neuron-per-layer information */
    obj->neuron_per_layer_count = (int*)malloc((size_t)(nlayers * sizeof(int)));
    if (!obj->neuron_per_layer_count) {
        *err = SNN_OUT_OF_MEMORY_ERROR;
        snn_free_network(obj);
        return NULL;
    }
    for (i = 0; i < nlayers; ++i) obj->neuron_per_layer_count[i] = node_counts[i];

    /* Allocate memory for the bias terms - no bias terms on the inputs */
    obj->total_bias_count = obj->total_neuron_count - obj->input_count;
    obj->bias = (double*)malloc((size_t)(obj->total_bias_count * sizeof(double)));
    if (!obj->bias) {
        *err = SNN_OUT_OF_MEMORY_ERROR;
        snn_free_network(obj);
        return NULL;
    }
    obj->bias_pointers = (double**)malloc((size_t)((nlayers - 1) * sizeof(double*)));
    if (!obj->bias_pointers) {
        *err = SNN_OUT_OF_MEMORY_ERROR;
        snn_free_network(obj);
        return NULL;
    }
    noffset = 0;
    for (i = 0; i < nlayers - 1; ++i) {
        obj->bias_pointers[i] = obj->bias + noffset;
        noffset += node_counts[i+1];
    }

    /* Allocate memory for the output values */
    obj->output = (double*)malloc((size_t)(obj->total_neuron_count * sizeof(double)));
    if (!obj->output) {
        *err = SNN_OUT_OF_MEMORY_ERROR;
        snn_free_network(obj);
        return NULL;
    }
    obj->output_pointers = (double**)malloc((size_t)(nlayers * sizeof(double*)));
    if (!obj->output_pointers) {
        *err = SNN_OUT_OF_MEMORY_ERROR;
        snn_free_network(obj);
        return NULL;
    }
    noffset = 0;
    for (i = 0; i < nlayers; ++i) {
        obj->output_pointers[i] = obj->output + noffset;
        noffset += node_counts[i];
    }

    /* Randomize the weights and biases */
    snn_randomize_weights_and_biases(obj);

    /* Output */
    return obj;
}





void snn_free_network(network *obj) {
    if (obj) {
        if (obj->neuron_per_layer_count) free(obj->neuron_per_layer_count);
        if (obj->weights) free(obj->weights);
        if (obj->bias) free(obj->bias);
        if (obj->output) free(obj->output);
        if (obj->weight_pointers) free(obj->weight_pointers);
        if (obj->output_pointers) free(obj->output_pointers);
        if (obj->bias_pointers) free(obj->bias_pointers);
        obj->neuron_per_layer_count = NULL;
        obj->weights = NULL;
        obj->bias = NULL;
        obj->output = NULL;
        obj->weight_pointers = NULL;
        obj->output_pointers = NULL;
        obj->bias_pointers = NULL;
        obj->input_count = 0;
        obj->output_count = 0;
        obj->hidden_layer_count = 0;
        obj->total_layer_count = 0;
        obj->total_weight_count = 0;
        obj->total_neuron_count = 0;
        obj->total_bias_count = 0;
        free(obj);
    }
    obj = NULL;
}





void snn_randomize_weights_and_biases(network *obj) {
    /* Local Variables */
    int i;

    /* Seed the random generator */
    srand(time(0));

    /* Populate the weighting factors */
    for (i = 0; i < obj->total_weight_count; ++i) {
        obj->weights[i] = ((double)rand()) / RAND_MAX;
    }

    /* Populate the bias terms */
    for (i = 0; i < obj->total_bias_count; ++i) {
        obj->bias[i] = ((double)rand()) / RAND_MAX;
    }
}





double* snn_eval_network(const network *obj, const double *x) {
    /* Local Variables */
    int i, nin, nout;
    double *weights, *inputs, *outputs, *bias;

    /* Copy the inputs into the appropriate storage array */
    copy(obj->input_count, x, obj->output_pointers[0]);

    /* Evaluate each layer */
    nout = obj->input_count;
    for (i = 1; i < obj->total_layer_count; ++i) {
        /* Get pointers for the relevant layer parameters */
        inputs = obj->output_pointers[i-1];
        outputs = obj->output_pointers[i];
        weights = obj->weight_pointers[i-1];
        bias = obj->bias_pointers[i-1];

        /* Determine the number of inputs and outputs for the layer */
        nin = nout;
        nout = obj->neuron_per_layer_count[i];

        /* Evaluate the layer */
        evaluate_layer(nin, nout, inputs, weights, bias, outputs);
    }

    /* Output */
    return outputs;
}





static void evaluate_layer(int ninputs, int nouts, const double *x, const double *weights, 
                           const double *offsets, double *z)
{
    /* Local Variables */
    int i;

    /* Copy the offsets array to the output array */
    copy(nouts, offsets, z);

    /* Compute: Z = WEIGHTS * X + Z */
    mult_mtx(nouts, 1, ninputs, weights, x, 1.0, z);

    /* Evaluate the sigmoid function to determine the node output */
    for (i = 0; i < nouts; ++i) z[i] = sigmoid(z[i]);
}





static void mult_mtx(int m, int n, int k, const double *x, const double *y, double beta, 
                     double *z) 
{
    /* Local Variables */
    int i, j, ij;
    double val;

    /* Process */
    if (beta == 0.0) {
        for (j = 0; j < n; ++j) {
            for (i = 0; i < m; ++i) {
                val = 0.0;
                for (ij = 0; ij < k; ++ij) val += x[INDEX(i, ij, m)] * y[INDEX(ij, j, k)];
                z[INDEX(i, j, m)] = val;
            }
        }
    }
    else {
        for (j = 0; j < n; ++j) {
            for (i = 0; i < m; ++i) {
                val = z[INDEX(i, j, m)];
                for (ij = 0; ij < k; ++ij) val += x[INDEX(i, ij, m)] * y[INDEX(ij, j, k)];
                z[INDEX(i, j, m)] = val;
            }
        }
    }
}





static void hadamard_product(int n, const double *x, const double *y, double *z) {
    int i;
    for (i = 0; i < n; ++i) z[i] = x[i] * y[i];
}





inline static double sigmoid(double x) {
    return 1.0 / (1.0 + exp(-x));
}





inline static double diff_sigmoid(double x) {
    double ex = exp(-x);
    double denom = ex + 1.0;
    return ex / SQR(denom);
}





inline static double quadratic_cost_fcn(int n, const double *y, const double *a) {
    int i;
    double val, rst = 0.0;
    for (i = 0; i < n; ++i) {
        val = y[i] - a[i];
        rst += SQR(val);
    }
    return rst / 2.0;
}





inline static void copy(int n, const double *src, double *dst) {
    int i;
    for (i = 0; i < n; ++i) dst[i] = src[i];
}
