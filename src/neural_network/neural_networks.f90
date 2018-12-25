! neural_networks.f90

module neural_networks
    use iso_c_binding
    use iso_fortran_env
    use ferror
    implicit none
    private
    public :: neural_network

! REF: https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/505505

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

        ! Local Variables
        type(c_ptr) :: ptr

        ! Ensure the network isn't already initialized
        if (associated(this%m_network)) call c_genann_free(this%m_network)

        ! Initialize the network, and obtain the GENANN object at the
        ! memory address returned by GENANN_INIT
        ptr = c_genann_init(inputs, hidden_layers, hidden, outputs)
        call c_f_pointer(ptr, this%m_network)
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
    !! @retrun The number of input nodes.
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
    !! @retrun The number of hidden layers.
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
    !! @retrun The number of nodes per hidden layer.
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
    !! @retrun The number of output nodes.
    pure function nn_get_output_count(this) result(n)
        class(neural_network), intent(in) :: this
        integer(int32) :: n
        if (associated(this%m_network)) then
            n = this%m_network%outputs
        else
            n = 0
        end if
    end function


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

end module
