/* snn.h */
#ifndef SNN_H_
#define SNN_H_

/* SNN - A small neural-network library.
 *
 * SNN is intended to act as a small neural-network library written in C to 
 * allow use on pretty much any platform imaginable.  As such, it does not
 * rely upon any external dependencies.  Utilizing the library is as simple
 * as including snn.h and snn.c in your project.
 */

#ifdef __cplusplus
extern "C" {
#endif

/* Error Flag: No error has occurred. */
#define SNN_NO_ERROR                0
/* Error Flag: Invalid input error. */
#define SNN_INVALID_INPUT_ERROR     5000
/* Error Flag: Insufficient memory available. */
#define SNN_OUT_OF_MEMORY_ERROR     5001

typedef struct network_ {
    /* The version number of this structure. */
    short version;
    /* The number of input neurons. */
    int input_count;
    /* The number of output neurons. */
    int output_count;
    /* The number of hidden layers. */
    int hidden_layer_count;
    /* An array containing the number of neurons per layer 
     * (length = total_layer_count). 
     */
    int *neuron_per_layer_count;
    /* The total layer count, including the input and output layers. */
    int total_layer_count;
    /* The total number of weighting factors. */
    int total_weight_count;
    /* The total number of neurons, including input and output neurons. */
    int total_neuron_count;
    /* The total number of bias terms. */
    int total_bias_count;
    /* An array containing all weighting factors (length = total_weight_count). */
    double *weights;
    /* An array containing all the bias factors (length = total_bias_count). */
    double *bias;
    /* An array containing the outputs for each neuron (length = total_neuron_count). */
    double *output;
    /* An array containing pointers to the start of each layer's weighting factors
     * in the weights array (length = total_layer_count - 1). 
     * (-1 as there are no output layer weighting factors).
     */
    double **weight_pointers;
    /* An array containing pointers to the start of each layer's neuron outputs
     * (length = total_layer_count).
     */
    double **output_pointers;
    /* An array containing pointers to the start of each layer's bias factors
     * (length = total_layer_count - 1). (-1 as there are no input biases).
     */
    double **bias_pointers;
} network;

/* Initializes a new network object.
 *
 * - nlayers: The total number of layers, including the input and output layers.
 *      This value must be at least 3.
 * - node_counts: An array of length NLAYERS containing the number of neurons in each
 *      layer.  Each element must be positive, and non-zero.
 * - err: [output] An error code.  The following values are possible.
 *      - SNN_NO_ERROR: Successful operation.
 *      - SNN_INVALID_INPUT_ERROR: An input value was not valid.
 *      - SNN_OUT_OF_MEMORY_ERROR: There was insufficient memory available.
 * 
 * Returns: A pointer to the newly created network object.
 */
network* snn_init_network(int nlayers, const int *node_counts, int *err);

/* Deallocates the supplied network object.
 *
 * - obj: A pointer to the network object.
 */
void snn_free_network(network *obj);

/* Randomizes the stored weights and biases over the interval [0, 1].
 *
 * - obj: A pointer to the network object.
 */
void snn_randomize_weights_and_biases(network *obj);

/* Evaluates the supplied network given an input set.
 *
 * - obj: The network to evaluate.
 * - x: A pointer to an array containing the input values to the network.  There
 *      is expected to be one input for every input neuron.
 * Returns: A pointer to an array containing the output of the network.  Notice,
 *      the memory referenced by this pointer is internally controlled.  Do not
 *      attempt to free or release this array.
 */
double* snn_eval_network(const network *obj, const double *x);


/* Evaluates a single layer of the network.
 *
 * - ninputs: The number of input neurons providing input to this layer.
 * - nouts: The number of neurons in this layer.
 * - x: A pointer to the NINPUTS element input vector (outputs from the previous layer).
 * - weights: A pointer to the NOUTS-by-NINPUTS weighting factor matrix.
 * - offsets: A pointer to the NOUTS bias vector.
 * - z: [output] A pointer to the NOUTS element output vector.
 */
static void evaluate_layer(int ninputs, int nouts, const double *x, const double *weights, 
                           const double *offsets, double *z);

/* Multiplies two matrices such that: z = x * y + beta * z.
 * 
 * m: The number of rows in the output matrix.
 * n: The number of columns in the output matrix.
 * k: The number of columns in the left-hand matrix, and the number of rows
 *      in the right-hand matrix.
 * x: A pointer to the M-by-K left-hand matrix.
 * y: A pointer to the K-by-N right-hand matrix.
 * beta: A scaling factor.
 * z: [input, output] A pointer to the M-by-N output matrix.
 */
static void mult_mtx(int m, int n, int k, const double *x, const double *y, double beta,
                     double *z);

/* Computes the Hadamard product of two arrays.
 *
 * - n: The number of elements in either array.
 * - x: A pointer to the first array.
 * - y: A pointer to the second array.
 * - z: [output] A pointer to the output array.
 */
static void hadamard_product(int n, const double *x, const double *y, double *z);

/* Computes the sigmoid function.
 *
 * - x: The input argument.
 * Returns: The result of the function evaluation.
 */
inline static double sigmoid(double x);

/* Computes the first derivative of the sigmoid function.
 *
 * - x: The input argument.
 * Returns: The result of the function evaluation.
 */
inline static double diff_sigmoid(double x);

/* Computes the quadratic cost function: C = sum(y(j) - a(j))**2 / 2.
 *
 * - n: The number of elements in each array.
 * - y: An N-element array containing the desired outputs of the network.
 * - a: An N-element array containing the actual network outputs.
 * Returns: The value of the cost function.
 */
inline static double quadratic_cost_fcn(int n, const double *y, const double *a);

/* Copies the contents of one array to another.
 *
 * - n: The number of elements in the array.
 * - src: The source array.
 * - dst: The destination array.
 */
inline static void copy(int n, const double *src, double *dst);

#ifdef __cplusplus
}
#endif
#endif
