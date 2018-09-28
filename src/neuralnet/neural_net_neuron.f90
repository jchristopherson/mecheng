! neural_net_neuron.f90

submodule (neural_net_core) neural_net_neuron
contains
! --------------------------------------------------------------------------------------------------
    ! Initializes the neuron by specifying the number of inputs it may accept
    module subroutine n_init(this, n, rnd, err)
        ! Arguments
        class(neuron), intent(inout) :: this
        integer(int32), intent(in) :: n
        logical, intent(in), optional :: rnd
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (n < 1) then
            call errmgr%report_error("n_init", &
                "The number of inputs must be greater than or equal to 1.", &
                NN_INVALID_INPUT_ERROR)
            return
        end if

        ! Allocate the weighting vector
        if (allocated(this%m_weights)) deallocate(this%m_weights)
        allocate(this%m_weights(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("n_init", &
                "Insufficient memory available.", &
                NN_OUT_OF_MEMORY_ERROR)
            return
        end if
        if (present(rnd)) then
            if (rnd) then
                call random_number(this%m_weights)
            else
                this%m_weights = 1.0d0
            end if
        else
            this%m_weights = 1.0d0
        end if
    end subroutine




    ! Gets the bias of the specified neuron.
    pure module function n_get_bias(this) result(x)
        class(neuron), intent(in) :: this
        real(real64) :: x
        x = this%m_bias
    end function

    ! Sets the bias of the specified neuron.
    !
    ! x: The bias.
    module subroutine n_set_bias(this, x)
        class(neuron), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_bias = x
    end subroutine



    ! Gets the number of inputs accepted by the neuron
    pure module function n_get_inputs(this) result(x)
        class(neuron), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_weights)) then
            x = size(this%m_weights)
        else
            x = 0
        end if
    end function



    ! Gets the array of input weights
    pure module function n_get_weights(this) result(x)
        class(neuron), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x
        if (this%get_input_count() > 0) x = this%m_weights
    end function

    ! Sets the array of input weights.  If the supplied number of weights is different
    ! from what is allocated, the allocation will be changed to accomodate the supplied
    ! weights.
    module subroutine n_set_weights(this, w, err)
        ! Arguments
        class(neuron), intent(inout) :: this
        real(real64), dimension(:) :: w
        class(errors), intent(inout), optional, target :: err

        ! Ensure the allocated space is correctly sized for the input array
        if (size(w) /= this%get_input_count()) then
            call this%initialize(size(w), err = err)
            if (present(err)) then
                if (err%has_error_occurred()) return
            end if
        end if

        ! Copy the inputs to the storage array
        this%m_weights = w
    end subroutine


    ! Randomizes the existing weighting factors and bias.
    module subroutine n_randomize(this, err)
        ! Arguments
        class(neuron), intent(inout) :: this
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (this%get_input_count() == 0) then
            call errmgr%report_error("n_randomize", &
                "The neuron is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        ! Process
        call random_number(this%m_weights)
        call random_number(this%m_bias)
    end subroutine

end submodule
