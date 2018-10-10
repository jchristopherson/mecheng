! nn_layer.f90

submodule (neural_network_core) nn_layer
contains
    module subroutine lyr_init(this, nInputs, nNeurons, fcn, diff, err)
        ! Arguments
        class(layer), intent(inout) :: this
        integer(int32), intent(in) :: nInputs, nNeurons
        procedure(neural_function), intent(in), pointer, optional :: fcn
        procedure(neural_function_derivative), intent(in), pointer, optional :: diff
        class(errors), intent(inout), target, optional :: err

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

        ! Input Check
        if (nInputs < 1) then
            call errmgr%report_error("lyr_init", &
                "Expected at least 1 input.", &
                NN_INVALID_INPUT_ERROR)
            return
        end if
        
        if (nNeurons < 1) then
            call errmgr%report_error("lyr_init", &
                "Expected at least 1 neuron.", &
                NN_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        if (allocated(this%m_weights)) deallocate(this%m_weights)
        if (allocated(this%m_bias)) deallocate(this%m_bias)

        allocate(this%m_weights(nNeurons, nInputs), stat = flag)
        if (flag == 0) allocate(this%m_bias(nNeurons), stat = flag)

        if (flag /= 0) then
            call errmgr%report_error("lyr_init", &
                "Insufficient memory available.", &
                NN_OUT_OF_MEMORY_ERROR)
            return
        end if
        
        ! Assign pointers to the neural functions
        if (present(fcn)) then
            this%m_fcn => fcn
        else
            this%m_fcn => sigmoid
        end if

        if (present(diff)) then
            this%m_diff => diff
        else
            this%m_diff => sigmoid_derivative
        end if
    end subroutine

    pure module function lyr_get_input_count(this) result(x)
        class(layer), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_weights)) then
            x = size(this%m_weights, 2)
        else
            x = 0
        end if
    end function

    pure module function lyr_get_neuron_count(this) result(x)
        class(layer), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_weights)) then
            x = size(this%m_weights, 1)
        else
            x = 0
        end if
    end function
    
end submodule
