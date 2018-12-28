! neural_networks.f90

module neural_networks
    use iso_c_binding
    use iso_fortran_env
    use ferror
    implicit none
    private
    public :: cost_function_derivative
    public :: neural_network
    public :: shuffle

! REF: https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/505505

    !> @brief Randomly shuffles a set of data.
    interface shuffle
        module procedure :: shuffle_array_dbl
        module procedure :: shuffle_mtx_dbl
        module procedure :: shuffle_array_int32
    end interface

    interface
        !> @brief Computes the derivative of the network cost function with
        !!  respect to the network outputs (a).
        !!
        !! @param[in] y The desired output of the network at the j-th output
        !!      neuron.
        !! @param[in] a The actual output of the network at the j-th output 
        !!      neuron.
        !! @return The value of the cost function derivative at the j-th
        !!      output neuron.
        function cost_function_derivative(y, a) result(rst)
            use iso_fortran_env
            real(real64), intent(in), value :: y, a
            real(real64) :: rst
        end function
    end interface

    ! Wrapper type for the C NETWORK structure (see snn.h)
    type, bind(C) :: snn_network
        integer(c_short) :: version
        integer(c_int) :: input_count
        integer(c_int) :: output_count
        integer(c_int) :: hidden_layer_count
        type(c_ptr) :: neuron_per_layer_count
        integer(c_int) :: total_layer_count
        integer(c_int) :: total_weight_count
        integer(c_int) :: total_neuron_count
        integer(c_int) :: total_bias_count
        integer(c_int) :: total_coefficient_count
        type(c_ptr) :: weights
        type(c_ptr) :: bias
        type(c_ptr) :: output
        type(c_ptr) :: delta
        type(c_ptr) :: gradient
        type(c_ptr) :: weight_pointers
        type(c_ptr) :: bias_pointers
        type(c_ptr) :: delta_pointers
        type(c_ptr) :: gradient_weight_pointers
        type(c_ptr) :: gradient_bias_pointers
        type(c_ptr) :: workspace
        integer(c_int) :: workspace_size
    end type

    interface
        function c_snn_quadratic_cost_fcn(n, y, a) result(rst) bind(C, name = "snn_quadratic_cost_fcn")
            use iso_c_binding
            integer(c_int), intent(in), value :: n
            real(c_double), intent(in) :: y(n), a(n)
            real(c_double) :: rst
        end function

        function c_snn_diff_quadratic_cost_fcn(y, a) result(rst) bind(C, name = "snn_diff_quadratic_cost_fcn")
            use iso_c_binding
            real(c_double), intent(in), value :: y, a
            real(c_double) :: rst
        end function

        function c_snn_init_network(nlayers, node_counts, err) result(obj) bind(C, name = "snn_init_network")
            use iso_c_binding
            import snn_network
            integer(c_int), intent(in), value :: nlayers
            integer(c_int), intent(in) :: node_counts(nlayers)
            integer(c_int), intent(out) :: err
            type(c_ptr) :: obj
        end function

        subroutine c_snn_free_network(obj) bind(C, name = "snn_free_network")
            import snn_network
            type(snn_network), intent(inout) :: obj
        end subroutine

        subroutine c_snn_randomize_weights_and_biases(obj) bind(C, name = "snn_randomize_weights_and_biases")
            import snn_network
            type(snn_network), intent(inout) :: obj
        end subroutine

        subroutine c_snn_run_network(obj, inputs, outputs) bind(C, name = "snn_run_network")
            use iso_c_binding
            import snn_network
            type(snn_network), intent(in) :: obj
            real(c_double), intent(in) :: inputs(*)
            real(c_double), intent(out) :: outputs(*)
        end subroutine

        subroutine c_snn_get_weights(obj, x) bind(C, name = "snn_get_weights")
            use iso_c_binding
            import snn_network
            type(snn_network), intent(in) :: obj
            real(c_double), intent(out) :: x(*)
        end subroutine

        subroutine c_snn_set_weights(obj, x) bind(C, name = "snn_set_weights")
            use iso_c_binding
            import snn_network
            type(snn_network), intent(inout) :: obj
            real(c_double), intent(in) :: x(*)
        end subroutine

        function c_cost_fcn_diff(y, a) result(z)
            use iso_c_binding
            real(c_double), intent(in), value :: y, a
            real(c_double) :: z
        end function

        subroutine c_snn_gradient(obj, dcf, x, y, eval, g) bind(C, name = "snn_gradient")
            use iso_c_binding
            import snn_network
            type(snn_network), intent(in) :: obj
            type(c_funptr), intent(in), value :: dcf ! c_cost_fcn_diff
            real(c_double), intent(in) :: x(*), y(*)
            logical(c_bool), intent(in), value :: eval
            real(c_double), intent(out) :: g(*)
        end subroutine

        subroutine c_snn_traning_step(obj, dcf, x, y, rate) bind(C, name = "snn_training_step")
            use iso_c_binding
            import snn_network
            type(snn_network), intent(in) :: obj
            type(c_funptr), intent(in), value :: dcf ! c_cost_fcn_diff
            real(c_double), intent(in) :: x(*), y(*)
            real(c_double), intent(in), value :: rate
        end subroutine

        subroutine c_snn_get_network_output_error(obj, x) bind(C, name = "snn_get_network_output_error")
            use iso_c_binding
            import snn_network
            type(snn_network), intent(in) :: obj
            real(c_double), intent(out) :: x(*)
        end subroutine
    end interface


    type neural_network
    private
        type(snn_network), pointer :: m_network => null()
    contains
        final :: nn_clean
        procedure, public :: initialize => nn_init_1
        procedure, public :: get_input_count => nn_get_input_count
        procedure, public :: get_hidden_layer_count => nn_get_hidden_layer_count
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
    subroutine nn_init_1(this, inputs, hidden_layers, hidden, outputs)
        ! Arguments
        class(neural_network), intent(inout) :: this
        integer(int32), intent(in) :: inputs, hidden_layers, hidden, outputs
        real(real64), allocatable, dimension(:) :: weights

        ! Local Variables
        type(c_ptr) :: ptr
        integer(int32) :: layers(hidden_layers + 2), i, flag

        ! Ensure the network isn't already initialized
        if (associated(this%m_network)) call c_snn_free_network(this%m_network)

        ! Initialize the network
        layers(1) = inputs
        layers(hidden_layers + 2) = outputs
        do i = 1, hidden_layers
            layers(i+1) = hidden
        end do
        ptr = c_snn_init_network(hidden_layers + 2, layers, flag)
        if (flag /= 0) then
            ! TO DO: Deal with the error conditions
        end if
        call c_f_pointer(ptr, this%m_network)
    end subroutine

    !> @brief Cleans up resources held by the neural_network object.
    !!
    !! @param[in,out] this The neural_network object.
    impure elemental subroutine nn_clean(this)
        type(neural_network), intent(inout) :: this
        if (associated(this%m_network)) call c_snn_free_network(this%m_network)
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
            n = this%m_network%input_count
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
            n = this%m_network%hidden_layer_count
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
            n = this%m_network%output_count
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
            n = this%m_network%total_weight_count
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
            n = this%m_network%total_neuron_count
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
        call c_snn_run_network(this%m_network, inputs, rst)
    end function

    !> @brief Performs a single backpropagation training step.
    !!
    !! @param[in] this The neural_network object.
    !! @param[in] inputs An array of inputs.  There must be one input for each
    !!  input node.
    !! @param[in] desired An array containing the desired outputs for the given
    !!  inputs.
    !! @param[in] rate The learning rate.
    !! @param[in] dcf An optional cost function derivative routine, that if
    !!  supplied, controls how the backpropagation algorithm computes the
    !!  network error and gradient terms.  The default is a quadratic error
    !!  function.
    !! @param[out] delta An optional output array that, if supplied, is used to
    !!  return the difference between the actual network output, and the desired
    !!  network output.
    !! @param[in,out] err
    !!
    subroutine nn_training_step(this, inputs, desired, rate, dcf, delta, err)
        ! Arguments
        class(neural_network), intent(in) :: this
        real(real64), intent(in), dimension(:) :: inputs, desired
        real(real64), intent(in) :: rate
        procedure(cost_function_derivative), intent(in), pointer, optional :: dcf
        real(real64), intent(out), dimension(:), optional :: delta
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: nin, nout
        type(c_funptr) :: cfcnptr
        procedure(c_cost_fcn_diff), pointer :: fcnptr

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
        if (size(desired) /= nout) then
            ! TO DO: Array size error
        end if

        ! Get a pointer to the cost function routine
        if (present(dcf)) then
            fcnptr => dcf
        else
            fcnptr => c_cost_diff
        end if
        cfcnptr = c_funloc(fcnptr)

        ! Compute the traning step
        call c_snn_traning_step(this%m_network, cfcnptr, inputs, desired, rate)

        ! Compute the error estimate, if necessary
        if (present(delta)) then
            if (size(delta) /= nout) then
                ! TO DO: Array size error
            end if
            call c_snn_get_network_output_error(this%m_network, delta)
        end if

    contains
        function c_cost_diff(yj, aj) result(cj)
            real(c_double), intent(in), value :: yj, aj
            real(c_double) :: cj
            cj = dcf(yj, aj)
        end function
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
        call c_snn_get_weights(this%m_network, x)
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
        call c_snn_set_weights(this%m_network, x)
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

        ! Randomize the weights and biases
        call c_snn_randomize_weights_and_biases(this%m_network)
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
