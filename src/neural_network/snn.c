/* snn.c */

#include <math.h>
#include <stdlib.h>
#include <time.h>
#include "snn.h"

#define SQR(x) ((x) * (x))
#define MAX(x, y) ((x) > (y) ? (x) : (y))
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
    if (!obj) goto CLEAN_UP_ON_ERROR;

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
    if (!obj->weights) goto CLEAN_UP_ON_ERROR;
    obj->weight_pointers = (double**)malloc((size_t)((nlayers - 1) * sizeof(double*)));
    if (!obj->weight_pointers) goto CLEAN_UP_ON_ERROR;

    for (i = 1; i < nlayers; ++i) {
        nin = node_counts[i-1];
        nout = node_counts[i];
        obj->weight_pointers[i-1] = obj->weights + (i - 1) * (nin * nout);
    }

    /* Store the neuron-per-layer information */
    obj->neuron_per_layer_count = (int*)malloc((size_t)(nlayers * sizeof(int)));
    if (!obj->neuron_per_layer_count) goto CLEAN_UP_ON_ERROR;
    for (i = 0; i < nlayers; ++i) obj->neuron_per_layer_count[i] = node_counts[i];

    /* Allocate memory for the bias terms - no bias terms on the inputs */
    obj->total_bias_count = obj->total_neuron_count - obj->input_count;
    obj->bias = (double*)malloc((size_t)(obj->total_bias_count * sizeof(double)));
    if (!obj->bias) goto CLEAN_UP_ON_ERROR;
    obj->bias_pointers = (double**)malloc((size_t)((nlayers - 1) * sizeof(double*)));
    if (!obj->bias_pointers) goto CLEAN_UP_ON_ERROR;
    noffset = 0;
    for (i = 0; i < nlayers - 1; ++i) {
        obj->bias_pointers[i] = obj->bias + noffset;
        noffset += node_counts[i+1];
    }

    /* Allocate memory for the output values */
    obj->output = (double*)malloc((size_t)(obj->total_neuron_count * sizeof(double)));
    if (!obj->output) goto CLEAN_UP_ON_ERROR;
    obj->output_pointers = (double**)malloc((size_t)(nlayers * sizeof(double*)));
    if (!obj->output_pointers) goto CLEAN_UP_ON_ERROR;
    noffset = 0;
    for (i = 0; i < nlayers; ++i) {
        obj->output_pointers[i] = obj->output + noffset;
        noffset += node_counts[i];
    }

    /* Allocate memory for the error values */
    obj->delta = (double*)malloc((size_t)(obj->total_bias_count * sizeof(double)));
    if (!obj->delta) goto CLEAN_UP_ON_ERROR;
    obj->delta_pointers = (double**)malloc((size_t)((nlayers - 1) * sizeof(double*)));
    if (!obj->delta_pointers) goto CLEAN_UP_ON_ERROR;
    noffset = 0;
    for (i = 0; i < nlayers - 1; ++i) {
        obj->delta_pointers[i] = obj->delta + noffset;
        noffset += node_counts[i+1];
    }

    /* Allocate memory for the gradient vector */
    obj->total_coefficient_count = obj->total_bias_count + obj->total_weight_count;
    obj->gradient = (double*)malloc((size_t)(obj->total_coefficient_count * sizeof(double)));
    if (!obj->gradient) goto CLEAN_UP_ON_ERROR;
    obj->gradient_weight_pointers = (double**)malloc((size_t)((nlayers - 1) * sizeof(double*)));
    if (!obj->gradient_weight_pointers) goto CLEAN_UP_ON_ERROR;
    obj->gradient_bias_pointers = (double**)malloc((size_t)((nlayers - 1) * sizeof(double*)));
    noffset = 0;
    for (i = 1; i < nlayers; ++i) {
        obj->gradient_weight_pointers[i-1] = obj->gradient + noffset;
        nin = node_counts[i-1];
        nout = node_counts[i];
        noffset += nin * nout;
    }
    for (i = 0; i < nlayers - 1; ++i) {
        obj->gradient_bias_pointers[i] = obj->gradient + noffset;
        noffset += node_counts[i+1];
    }

    /* Allocate a workspace array */
    obj->workspace_size = 0;
    for (i = 0; i < nlayers; ++i) 
        obj->workspace_size = MAX(obj->workspace_size, node_counts[i]);
    obj->workspace = (double*)malloc((size_t)(obj->workspace_size * sizeof(double)));
    if (!obj->workspace) goto CLEAN_UP_ON_ERROR;

    /* Randomize the weights and biases */
    snn_randomize_weights_and_biases(obj);

    /* Output */
    return obj;

CLEAN_UP_ON_ERROR:
    *err = SNN_OUT_OF_MEMORY_ERROR;
    snn_free_network(obj);
    return NULL;
}





void snn_free_network(network *obj) {
    if (obj) {
        if (obj->neuron_per_layer_count) free(obj->neuron_per_layer_count);
        if (obj->weights) free(obj->weights);
        if (obj->bias) free(obj->bias);
        if (obj->output) free(obj->output);
        if (obj->delta) free(obj->delta);
        if (obj->gradient) free(obj->gradient);
        if (obj->weight_pointers) free(obj->weight_pointers);
        if (obj->output_pointers) free(obj->output_pointers);
        if (obj->bias_pointers) free(obj->bias_pointers);
        if (obj->delta_pointers) free(obj->delta_pointers);
        if (obj->gradient_weight_pointers) free(obj->gradient_weight_pointers);
        if (obj->gradient_bias_pointers) free(obj->gradient_bias_pointers);
        if (obj->workspace) free(obj->workspace);
        obj->neuron_per_layer_count = NULL;
        obj->weights = NULL;
        obj->bias = NULL;
        obj->output = NULL;
        obj->delta = NULL;
        obj->gradient = NULL;
        obj->weight_pointers = NULL;
        obj->output_pointers = NULL;
        obj->bias_pointers = NULL;
        obj->delta_pointers = NULL;
        obj->gradient_weight_pointers = NULL;
        obj->gradient_bias_pointers = NULL;
        obj->workspace = NULL;
        obj->input_count = 0;
        obj->output_count = 0;
        obj->hidden_layer_count = 0;
        obj->total_layer_count = 0;
        obj->total_weight_count = 0;
        obj->total_neuron_count = 0;
        obj->total_bias_count = 0;
        obj->total_coefficient_count = 0;
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






double* snn_eval_gradient(const network *obj, const snn_cost_fcn_diff dcf, 
                          const double *x, const double *y, bool eval)
{
    /* Local Variables */
    int i, j, k, l, nin, nlayer, nnext;
    double *del, *z, *a, *weights, *delnext, *bias, *aprev, *g, dsig, val;

    /* Evaluate the network at X */
    if (eval) snn_eval_network(obj, x);

    /* Let Z use the workspace array */
    z = obj->workspace;

    /* Compute the error in the output layer of the network. */
    nnext = obj->output_count;
    nlayer = obj->neuron_per_layer_count[obj->total_layer_count - 2];   /* # of neurons in the last hidden layer */
    del = obj->delta_pointers[obj->total_layer_count - 2];              /* Pointer to overall output error vector */
    a = obj->output_pointers[obj->total_layer_count - 1];               /* a = sigma(z) - network output - NNEXT elements long */
    aprev = obj->output_pointers[obj->total_layer_count - 2];           /* aprev = sigma(z(l-1)) - last hidden layer output - NLAYER elements long */
    bias = obj->bias_pointers[obj->total_bias_count - 1];               /* NNEXT elements - bias terms from output layer */
    weights = obj->weight_pointers[obj->total_layer_count - 2];         /* NNEXT-by-NLAYER */
    copy(nnext, bias, z);                                               /* Store bias in Z */
    for (i = 0; i < obj->output_count; ++i) {
        /* Compute z = w * a + bias NOTE: bias is stored in Z*/
        mult_mtx(nnext, 1, nlayer, weights, aprev, 1.0, z);

        /* Compute the error of the output layer */
        dsig = diff_sigmoid(z[i]);
        val = dcf(y[i], a[i]);
        del[i] = val * dsig;
    }

    /* Compute the error in each previous layer */
    for (i = obj->total_layer_count - 2; i > 0; --i) {
        /* Define the size info */
        nnext = obj->neuron_per_layer_count[i+1];   /* # of neurons in the next higher layer */
        nlayer = obj->neuron_per_layer_count[i];    /* # of neurons in the current layer */
        nin = obj->neuron_per_layer_count[i-1];     /* # of neurons in the previous layer */

        /* Get the appropriate pointers */
        delnext = del;                              /* NNEXT elements - Pointer to the next higher layer's error results */
        del = obj->delta_pointers[i-1];             /* NLAYER elements - Pointer to where this layer's error values will be written */
        bias = obj->bias_pointers[i-1];             /* NLAYER elements - Pointer to this layer's bias terms */
        a = obj->output_pointers[i];                /* NLAYER elements - Pointer to this layer's output */
        aprev = obj->output_pointers[i-1];          /* NIN elements - Pointer to the previous layer's output */
        weights = obj->weight_pointers[i-1];        /* NLAYER-by-NIN - Pointer to the weighting matrix */

        /* Copy this layer's bias terms into the buffer Z */
        copy(nlayer, bias, z);

        /* Compute Z = WEIGHTS * APREV + Z */
        mult_mtx(nlayer, 1, nin, weights, aprev, 1.0, z);

        /* Compute the layer error by first computing WEIGHTS**T * DELNEXT */
        weights = obj->weight_pointers[i];  /* NNEXT-by-NLAYER - Pointer to the weighting matrix between this layer and the next higher layer */
        mult_trans_mtx(nlayer, 1, nnext, weights, delnext, 0.0, del);

        /* Now compute the Hadamard product with the derivative of the sigmoid function */
        hadamard_product(nlayer, z, del);
    }

    /* Construct the bias terms of the gradient using the error estimates */
    copy(obj->total_bias_count, obj->delta, obj->gradient_bias_pointers[0]);

    /* Construct the weighting factor terms of the gradient */
    nlayer = obj->input_count;
    for (i = 1; i < obj->total_layer_count; ++i) {
        /* Determine the array size information */
        nin = nlayer;
        nlayer = obj->neuron_per_layer_count[i];

        /* Get the appropriate pointers */
        aprev = obj->output_pointers[i-1];      /* NIN elements */
        del = obj->delta_pointers[i-1];         /* NLAYER elements */
        g = obj->gradient_weight_pointers[i-1]; /* NIN * NLAYER elements */

        /* Compute  aprev(k) * delta(j) */
        l = 0;
        for (k = 0; k < nin; ++k) {
            for (j = 0; j < nlayer; ++j) {
                g[l++] = aprev[k] * del[j];
            }
        }
    }

    /* Output */
    return obj->gradient;
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





static void evaluate_layer_error(int ncurrent, int nnext, const double *x, const double *del,
                                 const double *weights, double *err)
{
    /* Local Variables */
    int i;
    double val, dsig;

    /* Compute the matrix multiplication W**T * DEL, and then include the
     * Hadamard multiplication operation with the derivative of the sigmoid
     * function.
     */
    for (i = 0; i < ncurrent; ++i) {
        dsig = diff_sigmoid(x[i]);
        val = dot_product(nnext, &x[INDEX(i, 0, nnext)], del);
        err[i] = val * dsig;
    }
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
                val = beta * z[INDEX(i, j, m)];
                for (ij = 0; ij < k; ++ij) val += x[INDEX(i, ij, m)] * y[INDEX(ij, j, k)];
                z[INDEX(i, j, m)] = val;
            }
        }
    }
}





static void mult_trans_mtx(int m, int n, int k, const double *x, const double *y, 
                           double beta, double *z)
{
    /* Local Variables */
    int i, j, ij;
    double val;

    /* 
     * X is input as K-by-M
     * Y is input as K-by-N
     * Z is M-by-N
     */

    /* Process */
    if (beta == 0.0) {
        for (j = 0; j < n; ++j) {
            for (i = 0; i < m; ++i) {
                val = 0.0;
                for (ij = 0; ij < k; ++ij) val += x[INDEX(ij, i, k)] * y[INDEX(ij, j, k)];
                z[INDEX(i, j, m)] = val;
            }
        }
    }
    else {
        for (j = 0; j < n; ++j) {
            for (i = 0; i < m; ++i) {
                val = beta * z[INDEX(i, j, m)];
                for (ij = 0; ij < k; ++ij) val += x[INDEX(ij, i, k)] * y[INDEX(ij, j, k)];
                z[INDEX(i, j, m)] = val;
            }
        }
    }
}






static void hadamard_product(int n, const double *x, double *y) {
    int i;
    for (i = 0; i < n; ++i) y[i] = diff_sigmoid(x[i]) * y[i];
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





inline static double diff_quadratic_cost_fcn(double y, double a) {
    return a - y;
}





inline static void copy(int n, const double *src, double *dst) {
    int i;
    for (i = 0; i < n; ++i) dst[i] = src[i];
}




static double dot_product(int n, const double *x, const double *y) {
    int i;
    double val = 0.0;
    for (i = 0; i < n; ++i) val += x[i] * y[i];
    return val;
}