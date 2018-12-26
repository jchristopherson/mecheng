! neural_networks.f90

module neural_networks
    use iso_c_binding
    use iso_fortran_env
    use ferror
    implicit none
    private
    public :: neural_network
    public :: shuffle

! REF: https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/505505

    !> @brief Randomly shuffles a set of data.
    interface shuffle
        module procedure :: shuffle_array_dbl
        module procedure :: shuffle_mtx_dbl
        module procedure :: shuffle_array_int32
    end interface

    ! Wrapper type for the C GENANN structure (see genann.h)
    type, bind(C) :: genann
        integer(c_int) :: inputs
        integer(c_int) :: hidden_layers
        integer(c_int) :: hidden
        integer(c_int) :: outputs
        type(c_funptr) :: activation_hidden
        type(c_funptr) :: activation_output
        integer(c_int) :: total_weights
        integer(c_int) :: total_neurons
        type(c_ptr) :: weights
        type(c_ptr) :: output
        type(c_ptr) :: delta
    end type

    interface
        function c_genann_init(inputs, hidden_layers, hidden, outputs) result(rst) bind(C, name = "genann_init")
            use iso_c_binding
            import genann
            integer(c_int), intent(in), value :: inputs, hidden_layers, hidden, outputs
            type(c_ptr) :: rst
        end function

        subroutine c_genann_free(ann) bind(C, name = "genann_free")
            import genann
            type(genann), intent(inout) :: ann
        end subroutine

        subroutine c_genann_randomize(ann) bind(C, name = "genann_randomize")
            import genann
            type(genann), intent(in) :: ann
        end subroutine

        function c_genann_copy(ann) result(rst) bind(C, name = "genann_copy")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            type(c_ptr) :: rst
        end function

        subroutine c_genann_run(ann, inputs, outputs) bind(C, name = "run_network")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double), intent(in) :: inputs(*)
            real(c_double), intent(out) :: outputs(*)
        end subroutine

        subroutine c_genann_train(ann, inputs, outputs, rate) bind(C, name = "genann_train")
            use iso_c_binding
            import genann
            type(genann), intent(inout) :: ann
            real(c_double), intent(in) :: inputs(*), outputs(*)
            real(c_double), intent(in), value :: rate
        end subroutine

        subroutine c_get_weights(ann, weights) bind(C, name = "get_weights")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double) :: weights(*)
        end subroutine

        subroutine c_set_weights(ann, weights) bind(C, name = "set_weights")
            use iso_c_binding
            import genann
            type(genann), intent(inout) :: ann
            real(c_double) :: weights(*)
        end subroutine
    end interface


    type neural_network
    private
        type(genann), pointer :: m_network => null()
    contains
        final :: nn_clean
        procedure, public :: initialize => nn_init
        procedure, public :: get_input_count => nn_get_input_count
        procedure, public :: get_hidden_layer_count => nn_get_hidden_layer_count
        procedure, public :: get_node_count_per_hidden_layer => nn_get_hidden_node_count
        procedure, public :: get_output_count => nn_get_output_count
        procedure, public :: get_weight_count => nn_get_weight_count
        procedure, public :: get_neuron_count => nn_get_neuron_count
        procedure, public :: run => nn_run
        procedure, public :: training_step => nn_training_step
        procedure, public :: get_weights => nn_get_weights
        procedure, public :: set_weights => nn_set_weights
        procedure, public :: randomize_weights => nn_randomize_weights
    end type

contains
    !> @brief Initializes a new neural_network object.
    !!
    !! @param[in,out] this The neural_network object.
    !! @param[in] inputs The desired number of input nodes.
    !! @param[in] hidden_layers The desired number of hidden layers.
    !! @param[in] hidden The desired number of nodes per hidden layer.
    !! @param[in] outputs The desired number of output nodes.
    !!
    !! @par Remarks
    !! Notice, upon successful initialization, the network is assigned a
    !! series of random values for each node.
    subroutine nn_init(this, inputs, hidden_layers, hidden, outputs)
        ! Arguments
        class(neural_network), intent(inout) :: this
        integer(int32), intent(in) :: inputs, hidden_layers, hidden, outputs
        real(real64), allocatable, dimension(:) :: weights

        ! Local Variables
        type(c_ptr) :: ptr

        ! Ensure the network isn't already initialized
        if (associated(this%m_network)) call c_genann_free(this%m_network)

        ! Initialize the network, and obtain the GENANN object at the
        ! memory address returned by GENANN_INIT
        ptr = c_genann_init(inputs, hidden_layers, hidden, outputs)
        call c_f_pointer(ptr, this%m_network)

        ! Randomize the weighting factors
        call this%randomize_weights()
    end subroutine

    !> @brief Cleans up resources held by the neural_network object.
    !!
    !! @param[in,out] this The neural_network object.
    impure elemental subroutine nn_clean(this)
        type(neural_network), intent(inout) :: this
        if (associated(this%m_network)) call c_genann_free(this%m_network)
        nullify(this%m_network)
    end subroutine

    !> @brief Gets the number of input nodes.
    !!
    !! @param[in] this The neural_network object.
    !! @return The number of input nodes.
    pure function nn_get_input_count(this) result(n)
        class(neural_network), intent(in) :: this
        integer(int32) :: n
        if (associated(this%m_network)) then
            n = this%m_network%inputs
        else
            n = 0
        end if
    end function

    !> @brief Gets the number of hidden layers.
    !!
    !! @param[in] this The neural_network object.
    !! @return The number of hidden layers.
    pure function nn_get_hidden_layer_count(this) result(n)
        class(neural_network), intent(in) :: this
        integer(int32) :: n
        if (associated(this%m_network)) then
            n = this%m_network%hidden_layers
        else
            n = 0
        end if
    end function

    !> @brief Gets the number of nodes per hidden layer.
    !!
    !! @param[in] this The neural_network object.
    !! @return The number of nodes per hidden layer.
    pure function nn_get_hidden_node_count(this) result(n)
        class(neural_network), intent(in) :: this
        integer(int32) :: n
        if (associated(this%m_network)) then
            n = this%m_network%hidden
        else
            n = 0
        end if
    end function

    !> @brief Gets the number of output nodes.
    !!
    !! @param[in] this The neural_network object.
    !! @return The number of output nodes.
    pure function nn_get_output_count(this) result(n)
        class(neural_network), intent(in) :: this
        integer(int32) :: n
        if (associated(this%m_network)) then
            n = this%m_network%outputs
        else
            n = 0
        end if
    end function

    !> @brief Gets the number of weighting factors.
    !!
    !! @param[in] this The neural_network object.
    !! @return The number of weighting factors.
    pure function nn_get_weight_count(this) result(n)
        class(neural_network), intent(in) :: this
        integer(int32) :: n
        if (associated(this%m_network)) then
            n = this%m_network%total_weights
        else
            n = 0
        end if
    end function

    !> @brief Gets the total number of neurons in the network.
    !!
    !! @param[in] this The neural_network object.
    !! @return The number of neurons including the input, hidden, and output
    !!  neurons.
    pure function nn_get_neuron_count(this) result(n)
        class(neural_network), intent(in) :: this
        integer(int32) :: n
        if (associated(this%m_network)) then
            n = this%m_network%total_neurons
        else
            n = 0
        end if
    end function

    !> @brief Runs the feedforward algorithm to calculate the network's output.
    !!
    !! @param[in] this The neural_network object.
    !! @param[in] inputs An array of inputs.  There must be one input for each
    !!  input node.
    !! @param[in,out] err
    !!
    !! @return An array containing the outputs of the network.
    function nn_run(this, inputs, err) result(rst)
        ! Arguments
        class(neural_network), intent(in) :: this
        real(real64), intent(in), dimension(:) :: inputs
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:) :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: nin, nout, flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        
        ! Determine array sizes
        nin = this%get_input_count()
        nout = this%get_output_count()

        ! Input Check
        if (size(inputs) /= nin) then
            ! TO DO: Array size error
        end if

        ! Local Memory Allocation
        allocate(rst(nout), stat = flag)
        if (flag /= 0) then
            ! TO DO: Out of memory error
        end if

        ! Process
        call c_genann_run(this%m_network, inputs, rst)
    end function

    !> @brief Performs a single backpropagation training step.
    !!
    !! @param[in] this The neural_network object.
    !! @param[in] inputs An array of inputs.  There must be one input for each
    !!  input node.
    !! @param[in] desired An array containing the desired outputs for the given
    !!  inputs.
    !! @param[in] rate The learning rate.
    !! @param[out] delta An optional output array that, if supplied, is used to
    !!  return the difference between the actual network output, and the desired
    !!  network output.
    !! @param[in,out] err
    !!
    subroutine nn_training_step(this, inputs, desired, rate, delta, err)
        ! Arguments
        class(neural_network), intent(in) :: this
        real(real64), intent(in), dimension(:) :: inputs, desired
        real(real64), intent(in) :: rate
        real(real64), intent(out), dimension(:), optional :: delta
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: nin, nout

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        
        ! Determine array sizes
        nin = this%get_input_count()
        nout = this%get_output_count()

        ! Input Check
        if (size(inputs) /= nin) then
            ! TO DO: Array size error
        end if

        ! Compute the training step
        call c_genann_train(this%m_network, inputs, desired, rate)

        ! Compute the error estimate, if necessary
        if (present(delta)) then
            if (size(delta) /= nout) then
                ! TO DO: Array size error
            end if
            delta = this%run(inputs, errmgr) - desired
        end if
    end subroutine

    !> @brief Gets a vector containing each weighting factor.
    !!
    !! @param[in] this The neural_network object.
    !! @param[in,out] err
    !!
    !! @return An array containing the weighting factors.
    function nn_get_weights(this, err) result(x)
        ! Arguments
        class(neural_network), intent(in) :: this
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, target, dimension(:) :: x

        ! Local Variables
        integer(int32) :: n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Local Memory Allocation
        n = this%get_weight_count()
        allocate(x(n), stat = flag)
        if (flag /= 0) then
            ! TO DO: Out of memory error
        end if

        ! Retrieve the data
        call c_get_weights(this%m_network, x)
    end function

    !> @brief Sets the weighting factors for the network.
    !!
    !! @param[in,out] this The neural_network object.
    !! @param[in] x The array of weighting factors.
    !! @param[in,out] err
    !!
    subroutine nn_set_weights(this, x, err)
        ! Arguments
        class(neural_network), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the network is initialized
        n = this%get_weight_count()
        if (n == 0) then
            ! TO DO: Uninitialized network error
        end if

        ! Input Check
        if (size(x) /= n) then
            ! TO DO: Array size error
        end if

        ! Process
        call c_set_weights(this%m_network, x)
    end subroutine

    !> @brief Randomizes the value of each weighting factor over the set [0, 1].
    !!
    !! @param[in,out] this The neural_network object.
    !! @param[in,out] err
    !!
    subroutine nn_randomize_weights(this, err)
        ! Arguments
        class(neural_network), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        real(real64), allocatable, dimension(:) :: xrand

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the network is initialized
        n = this%get_weight_count()
        if (n == 0) then
            ! TO DO: Uninitialized network error
        end if

        ! Allocate an array and populate with random numbers
        allocate(xrand(n), stat = flag)
        if (flag /= 0) then
            ! TO DO: Out of memory error
        end if
        call random_number(xrand)

        ! Establish the array
        call c_set_weights(this%m_network, xrand)
    end subroutine

! ------------------------------------------------------------------------------
    subroutine shuffle_array_dbl(x)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x

        ! Local Variables
        integer(int32) :: i, k, n
        real(real64) :: rv, temp

        ! Process
        n = size(x)
        do i = 1, n
            call random_number(rv)
            k = int(rv * n, int32) + 1
            temp = x(i)
            x(i) = x(k)
            x(k) = temp
        end do
    end subroutine

! --------------------
    subroutine  shuffle_mtx_dbl_rowwise(x)
        ! Arguments
        real(real64), intent(inout), dimension(:,:) :: x

        ! Local Variables
        integer(int32) :: i, k, n
        real(real64) :: rv, temp(size(x, 2))

        ! Process
        n = size(x, 1)
        do i = 1, n
            call random_number(rv)
            k = int(rv * n, int32) + 1
            temp = x(i,:)
            x(i,:) = x(k,:)
            x(k,:) = temp
        end do
    end subroutine

! --------------------
    subroutine shuffle_mtx_dbl_columnwise(x)
        ! Arguments
        real(real64), intent(inout), dimension(:,:) :: x

        ! Local Variables
        integer(int32) :: i, k, n
        real(real64) :: rv, temp(size(x, 1))

        ! Process
        n = size(x, 2)
        do i = 1, n
            call random_number(rv)
            k = int(rv * n, int32) + 1
            temp = x(:,i)
            x(:,i) = x(:,k)
            x(:,k) = temp
        end do
    end subroutine

! --------------------
    subroutine shuffle_mtx_dbl(x, rowwise)
        ! Arguments
        real(real64), intent(inout), dimension(:,:) :: x
        logical, intent(in), optional :: rowwise

        ! Local Variables
        logical :: check
        
        ! Initialization
        if (present(rowwise)) then
            check = rowwise
        else
            check = .true.
        end if

        ! Process
        if (check) then
            call shuffle_mtx_dbl_rowwise(x)
        else
            call shuffle_mtx_dbl_columnwise(x)
        end if
    end subroutine

! --------------------
    subroutine shuffle_array_int32(x)
        ! Arguments
        integer(int32), intent(inout), dimension(:) :: x

        ! Local Variables
        integer(int32) :: i, k, n, temp
        real(real64) :: rv

        ! Process
        n = size(x)
        do i = 1, n
            call random_number(rv)
            k = int(rv * n, int32) + 1
            temp = x(i)
            x(i) = x(k)
            x(k) = temp
        end do
    end subroutine

! --------------------
end module
