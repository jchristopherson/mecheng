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

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Error Flag: No error has occurred. */
#define SNN_NO_ERROR                0
/* Error Flag: Invalid input error. */
#define SNN_INVALID_INPUT_ERROR     5000
/* Error Flag: Insufficient memory available. */
#define SNN_OUT_OF_MEMORY_ERROR     5001

/* Computes the cost function of the network.
 *
 * - n: The number of elements in each array.
 * - y: An N-element array containing the desired outputs of the network.
 * - a: An N-element array containing the actual network outputs.
 * Returns: The value of the cost function.
 */
typedef double (*snn_cost_fcn)(int n, const double *y, const double *a);

/* Computes the derivative of the network cost function with respect to the
 * actual outputs (a).
 *
 * - n: The number of elements in the network output array.
 * - y: The desired output of the network at the j-th neuron.
 * - a: The actual network output at the j-th neuron.
 * Returns: The value of the cost function derivative at the j-th neuron.
 */
typedef double (*snn_cost_fcn_diff)(int n, double y, double a);

/* Computes the neural activation function.
 *
 * - z: The input to the function.  It is expected to be z = w * x + b.
 * Returns: The value of the function.
 */
typedef double (*snn_neural_function)(double z);

/* Computes the derivative of the neural activation function.
 *
 * - z: The input to the function.  It is expected to be z = w * x + b.
 * Returns: The value of the function.
 */
typedef double (*snn_nerual_function_derivative)(double z);

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
    /* The total number of weighting factors and bias terms. */
    int total_coefficient_count;
    /* An array containing all weighting factors (length = total_weight_count). */
    double *weights;
    /* An array containing all the bias factors (length = total_bias_count). */
    double *bias;
    /* An array containing the outputs for each neuron (length = total_neuron_count). */
    double *output;
    /* An array containing the error for each layer (length = total_bias_count). */
    double *delta;
    /* An array containing the gradient vector (length = total_coefficient_count). */
    double *gradient;
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
    /* An array containing pointers to the start of each layer's error array
     * (length = total_layer_count - 1).
     */
    double **delta_pointers;
    /* An array containing pointers to the start of each layer's weighting 
     * factor portion of the gradient array (length = total_layer_count - 1).
     */
    double **gradient_weight_pointers;
    /* An array containing pointers to the start of each layer's bias 
     * factor portion of the gradient array (length = total_layer_count - 1).
     */
    double **gradient_bias_pointers;
    /* A scratch workspace array equal in length to the largest neuron count
     * in a single layer.
     */
    double *workspace;
    /* The workspace array size. */
    int workspace_size;
    /* A list of activation functions.  One for each layer with the exception
     * of the input layer (length = total_layer_count - 1).
     */
    snn_neural_function *activation_functions;
    /* A list of activation function derivatives.  On for each layer with the
     * exception of the input layer (length = total_layer_count - 1).
     */
    snn_nerual_function_derivative *activation_derivatives;
} network;

/* Computes the quadratic cost function: C = sum(y(j) - a(j))**2 / 2.
 *
 * - n: The number of elements in each array.
 * - y: An N-element array containing the desired outputs of the network.
 * - a: An N-element array containing the actual network outputs.
 * Returns: The value of the cost function.
 */
double snn_quadratic_cost_fcn(int n, const double *y, const double *a);

/* Computes the derivative of the quadratic cost function
 * C = sum(y(j) - a(j))**2 / 2 with respect to the actual network outputs (a).
 * The derivative is expressed as dC/da(j) = a(j) - y(j)
 *
 * - n: The number of elements in the network output array.
 * - y: The desired output of the network at the j-th neuron.
 * - a: The actual network output at the j-th neuron.
 * Returns: The value of the cost function derivative at the j-th neuron.
 */
double snn_diff_quadratic_cost_fcn(int n, double y, double a);

/* Computes the cross-entropy cost function.
 * 
 * - n: The number of elements in each array.
 * - y: An N-element array containing the desired outputs of the network.
 * - a: An N-element array containing the actual network outputs.
 * Returns: The value of the cost function.
 */
double snn_entropy_cost_fcn(int n, const double *y, const double *a);

/* Computes the derivative of the cross-entropy cost function with
 * respect to the actual network outputs (a).
 * 
 * - n: The number of elements in the network output array.
 * - y: The desired output of the network at the j-th neuron.
 * - a: The actual network output at the j-th neuron.
 * Returns: The value of the cost function derivative at the j-th neuron.
 */
double snn_diff_entropy_cost_fcn(int n, double y, double a);

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

/* Computes the gradient vector of the network for a given cost function.
 * 
 * - obj: The network.
 * - dcf: The derivative of the cost function with respect to the network
 *      outputs.
 * - x: A poitner to an array containing the input values to the network.  There
 *      is expected to be one value for every input neuron.
 * - y: A pointer to an array containing the expected output from the network.
 *      There is expected to be one value for every output neuron.
 * - eval: Set to true if a network evaluation is necessary; else, false.  For
 *      the gradient calculations to be correct, the state of the network must
 *      be current such that it has been evaluated at X.  If the network
 *      has not been evaluated at X with snn_eval_network, set this parameter
 *      to true.  Only set this parameter to false if snn_eval_network has
 *      been called for X.
 * Returns: A pointer to an array containing the gradient vector of the network
 *      with respect to each weight and bias term.  Notice, the memory 
 *      referenced by this pointer is internally controlled.  Do not attempt
 *      to free or release this array.
 */
double* snn_eval_gradient(const network *obj, const snn_cost_fcn_diff dcf, 
                          const double *x, const double *y, bool eval);

/* Takes a single training step.
 * 
 * - dcf: The derivative of the cost function with respect to the network
 *      outputs.
 * - x: A poitner to an array containing the input values to the network.  There
 *      is expected to be one value for every input neuron.
 * - y: A pointer to an array containing the expected output from the network.
 *      There is expected to be one value for every output neuron.
 * - rate: The desired learning rate.
 */
void snn_training_step(const network *obj, const snn_cost_fcn_diff dcf,
                       const double *x, const double *y, double rate);

/* Evaluates a single layer of the network.
 *
 * - fcn: The activation function for each neuron in the layer.
 * - ninputs: The number of input neurons providing input to this layer.
 * - nouts: The number of neurons in this layer.
 * - x: A pointer to the NINPUTS element input vector (outputs from the previous layer).
 * - weights: A pointer to the NOUTS-by-NINPUTS weighting factor matrix.
 * - offsets: A pointer to the NOUTS bias vector.
 * - z: [output] A pointer to the NOUTS element output vector.
 */
static void evaluate_layer(snn_neural_function fcn, int ncurrent, int nnext, const double *x, 
                           const double *weights, const double *offsets, double *z);


// /* Evaluates the error for the current layer based upon the error in the next
//  * layer.
//  * 
//  * - ncurrent: The number of neurons in this layer.
//  * - nnext: The number of neurons in the next layer.
//  * - x: An NCURRENT length array containing the neuron outputs from this layer.
//  * - del: An NNEXT length array containing the error estimate from the next
//  *      layer up.
//  * - weights: An NNEXT-by-NCURRENT matrix containing the weighting factors
//  *      matrix applied to the output of this layer.
//  * - err: [output] An NCURRENT element array where the error estimate for this
//  *      layer will be written.
//  */
// static void evaluate_layer_error(int ninputs, int nouts, const double *x, const double *del,
//                                  const double *weights, double *err);


/* Multiplies two matrices such that: z = x * y + beta * z.
 * 
 * - m: The number of rows in the output matrix.
 * - n: The number of columns in the output matrix.
 * - k: The number of columns in the left-hand matrix, and the number of rows
 *      in the right-hand matrix.
 * - x: A pointer to the M-by-K left-hand matrix.
 * - y: A pointer to the K-by-N right-hand matrix.
 * - beta: A scaling factor.
 * - z: [input, output] A pointer to the M-by-N output matrix.
 */
static void mult_mtx(int m, int n, int k, const double *x, const double *y, double beta,
                     double *z);

/* Multiplies two matrices such that: z = x**T * y + beta * z.
 * - m: The number of rows in the output matrix.
 * - n: The number of columns in the output matrix.
 * - k: The number of rows in the left-hand-side matrix, and the number of rows
 *      in the right-hand-matrix.
 * - x: A pointer to the K-by-M left-hand matrix.
 * - y: A pointer to the K-by-N right-hand-matrix.
 * - beta: A scaling factor.
 * - z: [input, output] A pointer to the M-by-N output matrix.
 */
static void mult_trans_mtx(int m, int n, int k, const double *x, const double *y, 
                           double beta, double *z);

/* Computes the Hadamard product of two arrays while applying the derivative
 * of the sigmoid function on X such that: Y = D(x) * Y.
 *
 * - diff: The routine to use when computing the derivative of the activation function.
 * - n: The number of elements in either array.
 * - x: A pointer to the first array.
 * - y: [input, output] A pointer to the second array.
 */
static void hadamard_product(snn_nerual_function_derivative diff, int n, const double *x, double *y);

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

/* Copies the contents of one array to another.
 *
 * - n: The number of elements in the array.
 * - src: The source array.
 * - dst: The destination array.
 */
inline static void copy(int n, const double *src, double *dst);

/* Computes the dot product of two vectors.
 *
 * - n: The number of elements in either vector.
 * - x: The left-hand-side vector.
 * - y: The right-hand-side vector.
 * Returns: The dot product of x and y.
 */
static double dot_product(int n, const double *x, const double *y);

#ifdef __cplusplus
}
#endif
#endif
