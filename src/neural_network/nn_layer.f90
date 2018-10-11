! nn_layer.f90

submodule (neural_network_core) nn_layer
contains
    module subroutine lyr_init(this, nInputs, nNeurons, err)
        ! Arguments
        class(layer), intent(inout) :: this
        integer(int32), intent(in) :: nInputs, nNeurons
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
    


    module function lyr_evaluate(this, a, err) result(ap)
        ! Arguments
        class(layer), intent(in) :: this
        real(real64), intent(in), dimension(:) :: a
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:) :: ap

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: nIn
        
        ! Initialization
        nIn = this%get_input_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (this%get_neuron_count() == 0) then
            call errmgr%report_error("lyr_evaluate", &
                "This layer has not yet been initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if
        if (nIn /= size(a)) then
            write(errmsg, '(I0AI0A)') size(a), &
                " inputs were found, but ", nIn, &
                " were expected."
            call errmgr%report_error("lyr_evaluate", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Evaluate the layer
        ap = this%eval_neural_function(this%eval_arguments(a, err))
    end function



    pure module function lyr_get_weights(this) result(w)
        class(layer), intent(in) :: this
        real(real64), allocatable, dimension(:,:) :: w
        w = this%m_weights
    end function




    pure module function lyr_get_bias_vector(this) result(b)
        class(layer), intent(in) :: this
        real(real64), allocatable, dimension(:) :: b
        b = this%m_bias
    end function



    module function lyr_get_weight(this, neuron, input, err) result(x)
        ! Arguments
        class(layer), intent(in) :: this
        integer(int32), intent(in) :: neuron, input
        class(errors), intent(inout), target, optional :: err
        real(real64) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        x = 0.0d0
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (.not.allocated(this%m_weights)) then
            call errmgr%report_error("lyr_get_weight", &
                "This layer is not initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if
        if (neuron < 1 .or. neuron > size(this%m_weights, 1)) then
            call errmgr%report_error("lyr_get_weight", &
                "The neuron index is outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if
        if (input < 1 .or. input > size(this%m_weights, 2)) then
            call errmgr%report_error("lyr_get_weight", &
                "The input index is outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        x = this%m_weights(neuron, input)
    end function

    module subroutine lyr_set_weight(this, neuron, input, x, err)
        ! Arguments
        class(layer), intent(inout) :: this
        integer(int32), intent(in) :: neuron, input
        real(real64), intent(in) :: x
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

        ! Input Check
        if (.not.allocated(this%m_weights)) then
            call errmgr%report_error("lyr_set_weight", &
                "This layer is not initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if
        if (neuron < 1 .or. neuron > size(this%m_weights, 1)) then
            call errmgr%report_error("lyr_set_weight", &
                "The neuron index is outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if
        if (input < 1 .or. input > size(this%m_weights, 2)) then
            call errmgr%report_error("lyr_set_weight", &
                "The input index is outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        this%m_weights(neuron, input) = x
    end subroutine



    module function lyr_get_bias(this, neuron, err) result(x)
        ! Arguments
        class(layer), intent(in) :: this
        integer(int32), intent(in) :: neuron
        class(errors), intent(inout), target, optional :: err
        real(real64) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        x = 0.0d0
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (.not.allocated(this%m_bias)) then
            call errmgr%report_error("lyr_get_bias", &
                "This layer is not initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if
        if (neuron < 1 .or. neuron > size(this%m_bias)) then
            call errmgr%report_error("lyr_get_bias", &
                "The neuron index is outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        x = this%m_bias(neuron)
    end function

    module subroutine lyr_set_bias(this, neuron, x, err)
        ! Arguments
        class(layer), intent(inout) :: this
        integer(int32), intent(in) :: neuron
        real(real64), intent(in) :: x
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

        ! Input Check
        if (.not.allocated(this%m_bias)) then
            call errmgr%report_error("lyr_get_bias", &
                "This layer is not initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if
        if (neuron < 1 .or. neuron > size(this%m_bias)) then
            call errmgr%report_error("lyr_get_bias", &
                "The neuron index is outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        this%m_bias(neuron) = x
    end subroutine



    module function lyr_fcn(this, z, err) result(y)
        ! Arguments
        class(layer), intent(in) :: this
        real(real64), intent(in), dimension(:) :: z
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:) :: y

        ! Initialization
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: n
        
        ! Initialization
        n = this%get_neuron_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (n == 0) then
            call errmgr%report_error("lyr_fcn", &
                "This layer has not been initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if
        if (size(z) /= n) then
            write(errmsg, '(AI0AI0A)') "The input array was expected to be of size ", n, &
                ", but was found to be of size ", size(z), "."
            call errmgr%report_error("lyr_fcn", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Evaluate the sigmoid function
        y = sigmoid(z)
    end function




    module function lyr_diff(this, z, err) result(y)
        ! Arguments
        class(layer), intent(in) :: this
        real(real64), intent(in), dimension(:) :: z
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:) :: y

        ! Initialization
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: n
        
        ! Initialization
        n = this%get_neuron_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (n == 0) then
            call errmgr%report_error("lyr_diff", &
                "This layer has not been initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if
        if (size(z) /= n) then
            write(errmsg, '(AI0AI0A)') "The input array was expected to be of size ", n, &
                ", but was found to be of size ", size(z), "."
            call errmgr%report_error("lyr_diff", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Evaluate the sigmoid derivative function
        y = sigmoid_derivative(z)
    end function
    


    module function lyr_eval_arg(this, a, err) result(z)
        ! Arguments
        class(layer), intent(in) :: this
        real(real64), intent(in), dimension(:) :: a
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:) :: z

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: n
        
        ! Initialization
        n = this%get_input_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (n == 0) then
            call errmgr%report_error("lyr_eval_arg", &
                "This layer has not been initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if
        if (size(z) /= n) then
            write(errmsg, '(AI0AI0A)') "The input array was expected to be of size ", n, &
                ", but was found to be of size ", size(z), "."
            call errmgr%report_error("lyr_eval_arg", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        z = matmul(this%m_weights, a) + this%m_bias
    end function
end submodule
