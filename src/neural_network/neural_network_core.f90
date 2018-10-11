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
        procedure, public :: initialize => lyr_init
        procedure, public :: get_input_count => lyr_get_input_count
        procedure, public :: get_neuron_count => lyr_get_neuron_count
        procedure, public :: evaluate => lyr_evaluate
        procedure, public :: get_weights => lyr_get_weights
        procedure, public :: get_bias_vector => lyr_get_bias_vector
        procedure, public :: get_weight => lyr_get_weight
        procedure, public :: set_weight => lyr_set_weight
        procedure, public :: get_bias => lyr_get_bias
        procedure, public :: set_bias => lyr_set_bias
        procedure, public :: eval_neural_function => lyr_fcn
        procedure, public :: eval_neural_derivative => lyr_diff
        procedure, public :: eval_arguments => lyr_eval_arg
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