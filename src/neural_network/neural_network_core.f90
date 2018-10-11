! neural_network_core.f90


! REFERENCES:
! - http://neuralnetworksanddeeplearning.com/chap1.html

module neural_network_core
    use iso_fortran_env
    use ferror
    use collection_errors
    implicit none
    private
    public :: NN_OUT_OF_MEMORY_ERROR
    public :: NN_INVALID_INPUT_ERROR
    public :: NN_INVALID_LAYER_ERROR
    public :: NN_NULL_POINTER_ERROR
    public :: NN_ARRAY_SIZE_ERROR
    public :: NN_UNINITIALIZED_ERROR
    public :: NN_INDEX_OUT_OF_RANGE_ERROR
    public :: sigmoid
    public :: sigmoid_derivative
    public :: layer

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
    end type

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
end module