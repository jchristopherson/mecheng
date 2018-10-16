! neural_network_core.f90


! REFERENCES:
! - http://neuralnetworksanddeeplearning.com/chap1.html

module neural_network_core
    use iso_fortran_env
    use ferror
    use collection_errors
    use collection_list
    implicit none
    private
    public :: NN_OUT_OF_MEMORY_ERROR
    public :: NN_INVALID_INPUT_ERROR
    public :: NN_INVALID_LAYER_ERROR
    public :: NN_NULL_POINTER_ERROR
    public :: NN_ARRAY_SIZE_ERROR
    public :: NN_UNINITIALIZED_ERROR
    public :: NN_INDEX_OUT_OF_RANGE_ERROR
    public :: cost_function
    public :: sigmoid
    public :: sigmoid_derivative
    public :: quadratic_cost_function
    public :: cross_entropy_cost_function
    public :: layer
    public :: network

! ******************************************************************************
! ERROR FLAGS
! ------------------------------------------------------------------------------
    !> An out-of-memory error flag.
    integer(int32), parameter :: NN_OUT_OF_MEMORY_ERROR = COLLECTION_OUT_OF_MEMORY_ERROR
    !> An invalid input error flag.
    integer(int32), parameter :: NN_INVALID_INPUT_ERROR = COLLECTION_INVALID_INPUT_ERROR
    !> An error flag denoting an invalid network construction.
    integer(int32), parameter :: NN_INVALID_NETWORK_ERROR = 1003
    !> An error flag denoting an invalid layer construction.
    integer(int32), parameter :: NN_INVALID_LAYER_ERROR = 1004
    !> A null pointer error flag.
    integer(int32), parameter :: NN_NULL_POINTER_ERROR = 1005
    !> An array size error flag.
    integer(int32), parameter :: NN_ARRAY_SIZE_ERROR = 1006
    !> An error flag denoting an uninitialized object error.
    integer(int32), parameter :: NN_UNINITIALIZED_ERROR = 1007
    !> An error flag indicating the supplied index was outside the bounds of the collection.
    integer(int32), parameter :: NN_INDEX_OUT_OF_RANGE_ERROR = 1008

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    interface
        !> @brief Defines a cost function interface.
        !!
        !! @param[in] n The total number of training examples.
        !! @param[in] a The output from the output layer neurons 
        !!  (\f$ a = \sigma \left( z \right) \f$).
        !! @param[in] y The expected training outputs.
        !! @return The output of the cost function.
        pure function cost_function(n, a, y) result(c)
            use iso_fortran_env
            integer(int32), intent(in) :: n
            real(real64), intent(in), dimension(:) :: a, y
            real(real64) :: c
        end function
    end interface

! ******************************************************************************
! NN_LAYER.F90
! ------------------------------------------------------------------------------
    !> @brief A single layer from a network.  The default neural function 
    !! utilized is a sigmoid function.
    type :: layer
    private
        !> A matrix containing the weights of each input to each neuron
        !! in the layer.  There is one row per neuron in the layer.
        real(real64), allocatable, dimension(:,:) :: m_weights
        !> An array containing the biases for each neuron in the layer.
        real(real64), allocatable, dimension(:) :: m_bias
    contains
        !> @brief Initializes the layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine initialize(class(layer) this, integer(int32) nInputs, integer(int32) nNeurons, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The layer object.
        !! @param[in] nInputs The number of inputs this layer can accept.  This value must be
        !!  greater than 1.
        !! @param[in] nNeurons The number of neurons in this layer.  This value must be greater
        !!  than 1.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_INVALID_INPUT_ERROR: Occurs if either @p nInputs or @p nNeurons is less than 1.
        !!  - NN_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory available.
        procedure, public :: initialize => lyr_init
        !> @brief Gets the number of inputs this layer can accept.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_input_count(class(layer) this)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @return The number of inputs this layer can accept.
        procedure, public :: get_input_count => lyr_get_input_count
        !> @brief Gets the number of neurons in the layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure integer(int32) function get_neuron_count(class(layer) this)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @return The number of neurons in the layer.
        procedure, public :: get_neuron_count => lyr_get_neuron_count
        !> @brief Evaluates the layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function evaluate(class(layer) this, real(real64) a(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @param[in] a An N-element array containing the output from the previous layer,
        !!  where N is equal to the number of inputs this layer can accept.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        !!  - NN_ARRAY_SIZE_ERROR: Occurs if @p a is not sized correctly.
        !!
        !! @return An M-element array containing the results from evaluating each of the
        !!  M neurons in this layer.
        !!
        !! @par Remarks
        !! This function evaluates each neuron as follows.
        !! @par
        !! \f$ a^{l} = \sigma \left( \mathbf{w} a^{l-1} + b^{l} \right) \f$,
        !! @par
        !! where \f$ \mathbf{w} \f$ is the weighting factors matrix, \f$ b^{l} \f$ is the bias
        !! vector, and \f$ a^{l-1} \f$ is the output vector from the previous layer.
        procedure, public :: evaluate => lyr_evaluate
        !> @brief Gets the weighting factor matrix where there is a row
        !! for each neuron, and a column for each input weighting factor.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64)(:,:) function get_weights(class(layer) this)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @return The matrix of weighting factors.
        procedure, public :: get_weights => lyr_get_weights
        !> @brief Gets an array of bias factors from the neurons in the
        !! layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64)(:) function get_bias_vector(class(layer) this)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @return The bias vector.
        procedure, public :: get_bias_vector => lyr_get_bias_vector
        !> @brief Gets the requested weighting factor.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function get_weight(class(layer) this, integer(int32) neuron, integer(int32) input, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @param[in] neuron The one-based index of the neuron of interest.
        !! @param[in] input The one-based index of the input of interest.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        !!  - NN_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p neuron or @p input are outside the bounds of the layer.
        !!
        !! @return The requested weighting factor.
        procedure, public :: get_weight => lyr_get_weight
        !> @brief Sets the specified weighting factor.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_weight(class(layer) this, integer(int32) neuron, integer(int32) input, real(real64) x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The layer object.
        !! @param[in] neuron The one-based index of the neuron of interest.
        !! @param[in] input The one-based index of the input of interest.
        !! @param[in] x The value to place into the collection.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        !!  - NN_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p neuron or @p input are outside the bounds of the layer.
        procedure, public :: set_weight => lyr_set_weight
        !> @brief Gets the requested neuron bias.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function get_bias(class(layer) this, integer(int32) neuron, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @param[in] neuron The one-based index of the neuron of interest.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        !!  - NN_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p neuron is outside the bounds of the layer.
        !!
        !! @return The requested bias value.
        procedure, public :: get_bias => lyr_get_bias
        !> @brief Sets the specified neuron bias.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_bias(class(layer) this, integer(int32) neuron, real(real64) x, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The layer object.
        !! @param[in] neuron The one-based index of the neuron of interest.
        !! @param[in] x The value to place into the collection.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        !!  - NN_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p neuron is outside the bounds of the layer.
        procedure, public :: set_bias => lyr_set_bias
        !> @brief Evaluates each neuron in the layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function eval_neural_function(class(layer) this, real(real64) z(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @param[in] z The N-element array resulting from the operation \f$ \mathbf{w} a^{l-1} + b^{l} \f$, 
        !!  where N is the number of neurons in this layer, \f$ \mathbf{w} \f$ is the weighting factors matrix, 
        !! \f$ b^{l} \f$ is the bias vector, and \f$ a^{l-1} \f$ is the output vector from the previous layer.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        !!  - NN_ARRAY_SIZE_ERROR: Occurs if @p z is not sized correctly.
        !!
        !! @return The N-element array containing the results from evaluating each of the
        !!  N neurons in this layer.
        procedure, public :: eval_neural_function => lyr_fcn
        !> @brief Evaluates the first derivative of each neuron in the layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function eval_neural_derivative(class(layer) this, real(real64) z(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @param[in] z The N-element array resulting from the operation \f$ \mathbf{w} a^{l-1} + b^{l} \f$, 
        !!  where N is the number of neurons in this layer, \f$ \mathbf{w} \f$ is the weighting factors matrix, 
        !! \f$ b^{l} \f$ is the bias vector, and \f$ a^{l-1} \f$ is the output vector from the previous layer.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        !!  - NN_ARRAY_SIZE_ERROR: Occurs if @p z is not sized correctly.
        !!
        !! @return The N-element array containing the results from evaluating the first derivative of
        !!  each of the N neurons in this layer.
        procedure, public :: eval_neural_derivative => lyr_diff
        !> @brief Evaluates the argument to pass to each neuron.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function eval_arguments(class(layer) this, real(real64) a(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The layer object.
        !! @param[in] a An N-element array containing the output from the previous layer,
        !!  where N is equal to the number of inputs this layer can accept.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        !!  - NN_ARRAY_SIZE_ERROR: Occurs if @p a is not sized correctly.
        !!
        !! @return An M-element array containing the results from evaluating the argument passed
        !!  to the neurons \f$ \mathbf{w} a^{l-1} + b^{l} \f$ where \f$ \mathbf{w} \f$ is the 
        !!  weighting factors matrix, \f$ b^{l} \f$ is the bias vector, and \f$ a^{l-1} \f$ is the 
        !!  output vector from the previous layer.
        procedure, public :: eval_arguments => lyr_eval_arg

        !> @brief Randomizes the layer weighting and neuron bias factors in the range (0, 1).
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine randomize(class(layer) this, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in,out] this The layer object.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the layer has not been initialized.
        procedure, public :: randomize => lyr_random
    end type

    interface
        module subroutine lyr_init(this, nInputs, nNeurons, err)
            class(layer), intent(inout) :: this
            integer(int32), intent(in) :: nInputs, nNeurons
            class(errors), intent(inout), target, optional :: err
        end subroutine

        pure module function lyr_get_input_count(this) result(x)
            class(layer), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function lyr_get_neuron_count(this) result(x)
            class(layer), intent(in) :: this
            integer(int32) :: x
        end function

        module function lyr_evaluate(this, a, err) result(ap)
            class(layer), intent(in) :: this
            real(real64), intent(in), dimension(:) :: a
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, dimension(:) :: ap
        end function

        pure module function lyr_get_weights(this) result(w)
            class(layer), intent(in) :: this
            real(real64), allocatable, dimension(:,:) :: w
        end function

        pure module function lyr_get_bias_vector(this) result(b)
            class(layer), intent(in) :: this
            real(real64), allocatable, dimension(:) :: b
        end function

        module function lyr_get_weight(this, neuron, input, err) result(x)
            class(layer), intent(in) :: this
            integer(int32), intent(in) :: neuron, input
            class(errors), intent(inout), target, optional :: err
            real(real64) :: x
        end function

        module subroutine lyr_set_weight(this, neuron, input, x, err)
            class(layer), intent(inout) :: this
            integer(int32), intent(in) :: neuron, input
            real(real64), intent(in) :: x
            class(errors), intent(inout), target, optional :: err
        end subroutine

        module function lyr_get_bias(this, neuron, err) result(x)
            class(layer), intent(in) :: this
            integer(int32), intent(in) :: neuron
            class(errors), intent(inout), target, optional :: err
            real(real64) :: x
        end function

        module subroutine lyr_set_bias(this, neuron, x, err)
            class(layer), intent(inout) :: this
            integer(int32), intent(in) :: neuron
            real(real64), intent(in) :: x
            class(errors), intent(inout), target, optional :: err
        end subroutine

        module function lyr_fcn(this, z, err) result(y)
            class(layer), intent(in) :: this
            real(real64), intent(in), dimension(:) :: z
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, dimension(:) :: y
        end function

        module function lyr_diff(this, z, err) result(y)
            class(layer), intent(in) :: this
            real(real64), intent(in), dimension(:) :: z
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, dimension(:) :: y
        end function

        module function lyr_eval_arg(this, a, err) result(z)
            class(layer), intent(in) :: this
            real(real64), intent(in), dimension(:) :: a
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, dimension(:) :: z
        end function

        module subroutine lyr_random(this, err)
            class(layer), intent(inout) :: this
            class(errors), intent(inout), target, optional :: err
        end subroutine
    end interface

! ******************************************************************************
! NN_NETWORK.F90
! ------------------------------------------------------------------------------
    !> @brief A generic feed-forward network.
    type :: network
    private
        !> The list of layer objects.
        type(persistent_list) :: m_layers
        !> The number of input neurons.
        integer(int32) :: m_nInputs = 0
    contains
        !> @brief Initializes the network.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in,out] this The network object.
        procedure, public :: initialize => net_init
        !> @brief Gets the requested layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in] this The network object.
        !!
        !! @par Remarks
        !! Layer 1 is considered the first hidden layer, and layer N - 1
        !! is the output layer assuming N is the total number of layers
        !! including the input layer.
        procedure, public :: get => net_get_layer
        !> @brief Sets the specified layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in,out] this The network object.
        !!
        !! @par Remarks
        !! Layer 1 is considered the first hidden layer, and layer N - 1
        !! is the output layer assuming N is the total number of layers
        !! including the input layer.
        procedure, public :: set => net_set_layer
        !> @brief Gets the total number of layers in the network, including
        !! the input layer.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in] this The network object.
        procedure, public :: get_count => net_get_count
        !> @brief Gets the number of inputs accepted by the network.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in] this The network object.
        procedure, public :: get_input_count => net_get_input_count
        !> @brief Gets the number of outputs for this network.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in] this The network object.
        procedure, public :: get_output_count => net_get_output_count
        !> @brief Evaluates the network given an input.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in] this The network object.
        procedure, public :: evaluate => net_evaluate
        !> @brief Backpropagates the error estimates in the network to arrive
        !! at a gradient vector for the network describing how the network
        !! changes with regards to each weighting factor and bias term.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in] this The network object.
        procedure, public :: backpropagate => net_backprop
        !> @brief Gets a count of all of the neurons in the network, including
        !! the input and output neurons.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in] this The network object.
        procedure, public :: get_neuron_count => net_get_neuron_count
        !> @brief Gets the total number of weighting factors and bias terms in
        !! the network.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! @endcode
        !!
        !! @param[in] this The network object.
        procedure, public :: get_weighting_factor_count => net_get_weight_count
    end type

    interface
        module subroutine net_init(this, layers, model, err)
            class(network), intent(inout) :: this
            integer(int32), intent(in), dimension(:) :: layers
            class(layer), intent(in) :: model
            class(errors), intent(inout), target, optional :: err
        end subroutine

        module function net_get_layer(this, i, err) result(x)
            class(network), intent(in) :: this
            integer(int32), intent(in) :: i
            class(errors), intent(inout), target, optional :: err
            class(layer), pointer :: x
        end function

        module subroutine net_set_layer(this, i, lyr, err)
            class(network), intent(inout) :: this
            integer(int32), intent(in) :: i
            class(layer), intent(in) :: lyr
            class(errors), intent(inout), target, optional :: err
        end subroutine

        pure module function net_get_count(this) result(x)
            class(network), intent(in) :: this
            integer(int32) :: x
        end function

        pure module function net_get_input_count(this) result(x)
            class(network), intent(in) :: this
            integer(int32) :: x
        end function

        module function net_get_output_count(this) result(x)
            class(network), intent(in) :: this
            integer(int32) :: x
        end function

        module function net_evaluate(this, inputs, err) result(x)
            class(network), intent(inout) :: this
            real(real64), intent(in), target, dimension(:) :: inputs
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, target, dimension(:) :: x
        end function

        module function net_backprop(this, cfcn, n, x, y, err) result(derivs)
            class(network), intent(in) :: this
            procedure(cost_function), intent(in), pointer :: cfcn
            integer(int32), intent(in) :: n
            real(real64), intent(in), dimension(:) :: x, y
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, dimension(:) :: derivs
        end function

        module function net_get_neuron_count(this) result(x)
            class(network), intent(in) :: this
            integer(int32) :: x
        end function

        module function net_get_weight_count(this) result(x)
            class(network), intent(in) :: this
            integer(int32) :: x
        end function

    end interface


contains

    !> @brief Computes the sigmoid function \f$ \sigma(x) = \frac{1}{1 + \exp(-x)} \f$.
    !!
    !! @param[in] x The independent variable.
    !! @return The result.
    pure elemental function sigmoid(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y
        y = 1.0d0 / (1.0d0 + exp(-x))
    end function

    !> @brief Computes the first derivative of the sigmoid function 
    !! \f$ \frac{d \sigma(x)}{dx} = \frac{\exp(-x)}{(1 + \exp(-x))^{2}} \f$.
    !!
    !! @param[in] x The independent variable.
    !! @return The result.
    pure elemental function sigmoid_derivative(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y
        y = exp(-x) / (1.0d0 + exp(-x))**2
    end function

    !> @brief Defines a quadratic cost function.
    !!
    !! @param[in] n The total number of training examples.
    !! @param[in] a The output from the output layer neurons 
    !!  (\f$ a = \sigma \left( z \right) \f$).
    !! @param[in] y The expected training outputs.
    !! @return The output of the cost function.
    pure function quadratic_cost_function(n, a, y) result(c)
        ! Arguments
        integer(int32), intent(in) :: n
        real(real64), intent(in), dimension(:) :: a, y
        real(real64) :: c

        ! Local Variables
        integer(int32) :: i, m

        ! Process
        m = min(size(a), size(y))
        c = 0.0d0
        do i = 1, m
            c = c + (y(i) - a(i))**2
        end do
        c = c / (2.0d0 * real(n, real64))
    end function

    !> @brief Defines a cross-entropy cost function.
    !!
    !! @param[in] n The total number of training examples.
    !! @param[in] a The output from the output layer neurons 
    !!  (\f$ a = \sigma \left( z \right) \f$).
    !! @param[in] y The expected training outputs.
    !! @return The output of the cost function.
    pure function cross_entropy_cost_function(n, a, y) result(c)
        ! Arguments
        integer(int32), intent(in) :: n
        real(real64), intent(in), dimension(:) :: a, y
        real(real64) :: c

        ! Local Variables
        integer(int32) :: i, m

        ! Process
        m = min(size(a), size(y))
        c = 0.0d0
        do i = 1, m
            c = c + y(i) * log(a(i)) + (1.0d0 - y(i)) * log(1.0d0 - a(i))
        end do
        c = -c / real(n, real64)
    end function
end module