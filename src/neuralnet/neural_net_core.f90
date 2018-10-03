! neural_net_core.f90

! REFERENCES:
! - http://neuralnetworksanddeeplearning.com/chap1.html

module neural_net_core
    use iso_fortran_env
    use ferror
    use collection_list
    use neural_net_constants
    implicit none
    private
    public :: neuron
    public :: sigmoid_neuron
    public :: evaluate_neuron
    public :: layer
    public :: neural_network
    public :: evaluate_network
    public :: feedforward_network

! ******************************************************************************
! NERUAL_NET_NEURON.F90
! ------------------------------------------------------------------------------
    ! A generic neuron type.
    type, abstract :: neuron
    private
        ! The neuron bias
        real(real64) :: m_bias = 0.0d0
        ! The weights for each input
        real(real64), allocatable, dimension(:) :: m_weights
    contains
        procedure, public :: initialize => n_init
        procedure, public :: get_bias => n_get_bias
        procedure, public :: set_bias => n_set_bias
        procedure, public :: get_input_count => n_get_inputs
        procedure, public :: get_weights => n_get_weights
        procedure, public :: set_weights => n_set_weights
        procedure, public :: randomize => n_randomize
        procedure(evaluate_neuron), deferred, public :: evaluate
    end type

! --------------------------------------------------------------------------------------------------
    interface
        !> @brief Evaluates a neuron.
        !!
        !! @param[in] this The neuron object.
        !! @param[in] wx An array of input signals (unweighted).
        !! @return The results of the neuron decision function.
        pure function evaluate_neuron(this, wx) result(y)
            use iso_fortran_env, only : real64
            import neuron
            class(neuron), intent(in) :: this
            real(real64), intent(in), dimension(:) :: wx
            real(real64) :: y
        end function

        !> @brief Initializes the neuron object.
        !!
        !! @param[in,out] this The neuron object.
        !! @param[in] n The number of inputs accepted by the neuron.  This value must be
        !!  greater than or equal to 1.
        !! @param[in] rnd This is an optional input, that if specified, controls whether or
        !!  not to initialize the weighting factors randomly, or set them to unity.  The
        !!  default is to set the weighting factors to unity.
        !! @param[in,out] err An errors-based object used for error handling.  Possible
        !!  errors are as follows.
        !!  - NN_INVALID_INPUT_ERROR: Occurs if @p n is less than 1.
        !!  - NN_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        module subroutine n_init(this, n, rnd, err)
            class(neuron), intent(inout) :: this
            integer(int32), intent(in) :: n
            logical, intent(in), optional :: rnd
            class(errors), intent(inout), optional, target :: err
        end subroutine

        !> @brief Gets the bias of the neuron.
        !!
        !! @param[in] this The neuron object.
        !! @return The bias.
        pure module function n_get_bias(this) result(x)
            class(neuron), intent(in) :: this
            real(real64) :: x
        end function

        !> @brief Sets the bias of the neuron.
        !!
        !! @param[in,out] this The neuron.
        !! @param[in] x The bias.
        module subroutine n_set_bias(this, x)
            class(neuron), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        !> @brief Gets the number of inputs the neuron can accept.
        !!
        !! @param[in] this The neuron.
        !! @return The number of inputs.
        pure module function n_get_inputs(this) result(x)
            class(neuron), intent(in) :: this
            integer(int32) :: x
        end function

        !> @brief Gets an array of weights that are assigned to the neuron inputs.
        !!
        !! @param[in] this The neuron object.
        !! @return An array of the weights for each input.
        pure module function n_get_weights(this) result(x)
            class(neuron), intent(in) :: this
            real(real64), allocatable, dimension(:) :: x
        end function

        !> @brief Sets the weighting factor for each neuron input.
        !!
        !! @param[in,out] this The neuron object.
        !! @param[in] w An array containing the weights for each neuron.  If a different
        !!  number of weights are supplied than inputs available for the neuron, the neuron
        !!  is rescaled to accept the same number of neurons as values in this array.
        !! @param[in,out] err An errors-based object used for error handling.
        !!  Possible errors are as follows.
        !!  - NN_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        module subroutine n_set_weights(this, w, err)
            class(neuron), intent(inout) :: this
            real(real64), dimension(:) :: w
            class(errors), intent(inout), optional, target :: err
        end subroutine

        !> @brief Randomizes the weighting factors and bias with values ranging
        !!  between 0 and 1.
        !!
        !! @param[in,out] this The neuron.
        !! @param[in,out] err An errors-based object used for error handling.
        !!  Possible errors are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the neuron has not been properly
        !!      initialized.
        module subroutine n_randomize(this, err)
            class(neuron), intent(inout) :: this
            class(errors), intent(inout), target, optional :: err
        end subroutine
    end interface

! ******************************************************************************
! NEURAL_NET_SIGMOID.F90
! ------------------------------------------------------------------------------
    !> @brief A sigmoid type neuron.
    type, extends(neuron) :: sigmoid_neuron
    contains
        procedure, public :: evaluate => sn_eval
    end type

! ------------------------------------------------------------------------------
    interface
        !> @brief Evaluates a neuron.
        !!
        !! @param[in] this The sigmoid_neuron object.
        !! @param[in] wx An array of input signals (unweighted).
        !! @return The results of the neuron decision function.
        pure module function sn_eval(this, wx) result(y)
            class(sigmoid_neuron), intent(in) :: this
            real(real64), intent(in), dimension(:) :: wx
            real(real64) :: y
        end function
    end interface

! ******************************************************************************
! NEURAL_NET_LAYER.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a layer of neurons.
    type, extends(persistent_list) :: layer
    contains
        procedure, public :: get_neuron => layer_get
        procedure, public :: initialize => layer_init
        procedure, public :: evaluate => layer_evaluate
    end type

! ------------------------------------------------------------------------------
    interface
        !> @brief Gets a neuron from the layer.
        !!
        !! @param[in] this The layer object.
        !! @param[in] i The one-based index defining from where in the layer
        !!  the neuron should be retrieved.
        !! @return A pointer to the neuron object.
        !!
        !! @par Remarks
        !! This routine should be called istead of the more generic get 
        !! routine.  The get routine will return an unlimited polymorphic
        !! pointer which would require code similar to the following for the
        !! pointer to be useful.
        !! @par
        !! @code{.f90}
        !! class(*), pointer :: ptr
        !! ptr => layer%get(index)
        !! select type (ptr)
        !! class is (neuron)
        !!     ! Do something here with the pointer to the neuron
        !! end type
        !! @endcode
        module function layer_get(this, i) result(ptr)
            class(layer), intent(in) :: this
            integer(int32), intent(in) :: i
            class(neuron), pointer :: ptr
        end function

        !> @brief Initializes the layer with the specified number and type of neurons.
        !!
        !! @param[in,out] this The layer object.
        !! @param[in] n The number of neurons to create (must be at least 1).
        !! @param[in] model The model neuron type.
        !! @param[in,out] err An errors-based object used for error handling.  Possible
        !!  errors are as follows.
        !!  - NN_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - NN_INVALID_INPUT_ERROR: Occurs if @p n is less than 1.
        module subroutine layer_init(this, n, model, err)
            class(layer), intent(inout) :: this
            integer(int32), intent(in) :: n
            class(neuron), intent(in) :: model
            class(errors), intent(inout), target, optional :: err
        end subroutine

        !> @brief Evaluates the entire layer.
        !!
        !! @param[in] this The layer object.
        !! @param[in] x The array of inputs to pass to each neuron.
        !! @param[in,out] err An errors-based object used for error handling.  Possible
        !!  errors are as follows.
        !!  - NN_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
        !!  - NN_INVALID_LAYER_ERROR: Occurs if neurons are not yet defined for the layer.
        !!  - NN_NULL_POINTER_ERROR: Occurs if a pointer to an undefined neuron is found.
        module function layer_evaluate(this, x, err) result(y)
            class(layer), intent(in) :: this
            real(real64), intent(in), dimension(:) :: x
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, dimension(:) :: y
        end function
    end interface

! ******************************************************************************
! NEURAL_NET_NETWORK.F90
! ------------------------------------------------------------------------------
    type :: layer_pointer
        class(layer), pointer :: item => null()
    end type

    !> @brief A structure for a basic neural network type.
    type, extends(persistent_list), abstract :: neural_network
    contains
        procedure, public :: initialize => network_init
        procedure, public :: get_layer => network_get

        procedure, public :: randomize => network_randomize
        procedure(evaluate_network), deferred, public :: evaluate
    end type

! ------------------------------------------------------------------------------
    interface
        !> @brief Evaluates a network.
        !!
        !! @param[in] this The neural_network object.
        !! @param[in] x An N-element array containing the inputs to pass to the
        !!  input layer.  It is expected that this array is the same size as
        !!  the input layer.  If not, the typical behavior is to either 
        !!  truncate this array, in the event that x is larger than the number
        !!  of input nodes, or pad with zeros in the event that x is smaller 
        !!  than the number of input nodes.
        !! @param[in,out] err An errors-based object used for error handling. 
        !!  Error codes are specific to the actual implementation.
        function evaluate_network(this, x, err) result(y)
            use iso_fortran_env
            use ferror
            import neural_network
            class(neural_network), intent(in) :: this
            real(real64), intent(in), target, dimension(:) :: x
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, dimension(:) :: y
        end function

        !> @brief Initializes the neural_network object.
        !!
        !! @param[in,out] this The neural_network object.
        !! @param[in] lyrs An array containing the number of neurons
        !!  in each layer.  For instance, item 1 in this array defines
        !!  the number of neurons in layer 1, item 2 defines the number
        !!  of neurons in layer 2, etc.  There must be at least 3
        !!  items; hence, 3 layers, in this array (network).
        !! @param[in] lmodel The model layer type.
        !! @param[in] nmodel The model neuron type.
        !! @param[in,out] err An errors-based object used for error handling.
        !!  Possible errors are as follows.
        !!  - NN_ARRAY_SIZE_ERROR: Occurs if @p lyrs isn't at least 3 elements 
        !!      in size.
        !!  - NN_INVALID_INPUT_ERROR: Occurs if any of the values in
        !!      @p lyrs are less than 1 as each layer must have at least
        !!      1 neuron.
        !!  - NN_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        module subroutine network_init(this, lyrs, lmodel, nmodel, err)
            class(neural_network), intent(inout) :: this
            integer(int32), intent(in), dimension(:) :: lyrs
            class(layer), intent(in) :: lmodel
            class(neuron), intent(in) :: nmodel
            class(errors), intent(inout), target, optional :: err
        end subroutine

        !> @brief Gets a layer from the network.
        !!
        !! @param[in] this The neural_network object.
        !! @param[in] i The index of the layer to retrieve.  The input layer is
        !!  layer #1.
        !! @param[in,out] err An errors-based object used for error handling.  Possible
        !!  errors are as follows.
        !!  - NN_INDEX_OUT_OF_RANGE_ERROR: Occurs if @p i is outside the bounds of
        !!      the collection of layers.
        !! @return A pointer to the requested layer.
        module function network_get(this, i, err) result(ptr)
            class(neural_network), intent(in) :: this
            integer(int32), intent(in) :: i
            class(errors), intent(inout), target, optional :: err
            class(layer), pointer :: ptr
        end function

        !> @brief Randomizes the entire network such that each neuron 
        !! utilizes random weights and biases between 0 and 1.
        !!
        !! @param[in,out] this The neural_network object.
        !! @param[in,out] err  An errors-based object used for error handling.
        !!  Possible errors are as follows.
        !!  - NN_UNINITIALIZED_ERROR: Occurs if the network or any underlying
        !!      neurons have not been properly initialized.
        module subroutine network_randomize(this, err)
            class(neural_network), intent(inout) :: this
            class(errors), intent(inout), target, optional :: err
        end subroutine
    end interface

! ******************************************************************************
! NEURAL_NET_FF_NETWORK.F90
! ------------------------------------------------------------------------------
    !> @brief Defines a feed-forward neural network.
    type, extends(neural_network) :: feedforward_network
    contains
        procedure, public :: evaluate => ff_evaluate
    end type

! ------------------------------------------------------------------------------
    interface
        !> @brief Evaluates a network.
        !!
        !! @param[in] this The feedforward_network object.
        !! @param[in] x An N-element array containing the inputs to pass to the
        !!  input layer.  It is expected that this array is the same size as
        !!  the input layer.  If not, the typical behavior is to either 
        !!  truncate this array, in the event that x is larger than the number
        !!  of input nodes, or pad with zeros in the event that x is smaller 
        !!  than the number of input nodes.
        !! @param[in,out] err An errors-based object used for error handling.  
        !!  Possible errors are as follows.
        !!  - NN_INVALID_NETWORK_ERROR: Occurs if the network doesn't have at
        !!      least 3 layers.
        !!  - NN_INVALID_LAYER_ERROR: Occurs if a layer is not properly 
        !!      initialized (e.g. it contains invalid or null-valued neurons).
        !!  - NN_NULL_POINTER_ERROR: Occurs if a null-valued neuron or layer
        !!      is encountered.  This results from improper initialization of
        !!      the network.
        !!  - NN_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        module function ff_evaluate(this, x, err) result(y)
            class(feedforward_network), intent(in) :: this
            real(real64), intent(in), target, dimension(:) :: x
            class(errors), intent(inout), target, optional :: err
            real(real64), allocatable, dimension(:) :: y
        end function
    end interface

! ------------------------------------------------------------------------------
end module
