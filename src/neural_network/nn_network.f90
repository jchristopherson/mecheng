! nn_network.f90

submodule (neural_network_core) nn_network
contains
    module subroutine net_init(this, layers, model, err)
        ! Arguments
        class(network), intent(inout) :: this
        integer(int32), intent(in), dimension(:) :: layers
        class(layer), intent(in) :: model
        class(errors), intent(inout), target, optional :: err

        ! Initialization
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, n
        class(layer), pointer :: lyr
        
        ! Initialization
        n = size(layers)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (n < 3) then
            write(errmsg, '(AI0A)') "There must be at least 3 layers, including " // &
                "the input and output layers, but ", n, " were requested."
            call errmgr%report_error("net_init", trim(errmsg), NN_INVALID_INPUT_ERROR)
            return
        end if
        do i = 1, n
            if (layers(i) < 1) then
                write(errmsg, '(I0AI0A)') layers(i), " neurons requested for layer ", &
                    i, "; however, there must be at least one neuron for a valid layer."
                call errmgr%report_error("net_init", trim(errmsg), NN_INVALID_LAYER_ERROR)
                return
            end if
        end do

        ! Process
        if (this%m_layers%get_count() /= 0) call this%m_layers%clear()
        this%m_nInputs = layers(1)  ! Just store the number of inputs to the network
        do i = 2, n
            ! Push the layer onto the back of the network.  Notice, the push
            ! routine creates and stores a copy of model.
            call this%m_layers%push(model)
        end do

        ! Initialize each layer
        do i = 1, n - 1
            lyr => this%get(i)
            call lyr%initialize(layers(i), layers(i+1), errmgr)
            if (errmgr%has_error_occurred()) return
        end do
    end subroutine



    module function net_get_layer(this, i, err) result(x)
        ! Arguments
        class(network), intent(in) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), target, optional :: err
        class(layer), pointer :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(*), pointer :: ptr
        
        ! Initialization
        nullify(x)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (i < 1 .or. i > this%m_layers%get_count()) then
            call errmgr%report_error("net_get_layer", &
                "The supplied index is outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        ptr => this%m_layers%get(i)
        select type (ptr)
        class is (layer)
            x => ptr
        end select
    end function



    module subroutine net_set_layer(this, i, lyr, err)
        ! Arguments
        class(network), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(layer), intent(in) :: lyr
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
        if (i < 1 .or. i > this%m_layers%get_count()) then
            call errmgr%report_error("net_set_layer", &
                "The supplied index is outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        call this%m_layers%set(i, lyr)
    end subroutine



    pure module function net_get_count(this) result(x)
        class(network), intent(in) :: this
        integer(int32) :: x, n
        n = this%m_layers%get_count()
        if (n == 0) then
            x = n
        else
            x = n + 1   ! +1 accounts for the input layer
        end if
    end function



    pure module function net_get_input_count(this) result(x)
        class(network), intent(in) :: this
        integer(int32) :: x
        x = this%m_nInputs
    end function



    module function net_get_output_count(this) result(x)
        ! Arguments
        class(network), intent(in) :: this
        integer(int32) :: x

        ! Local Variables
        integer(int32) :: n
        class(layer), pointer :: ptr

        ! Process
        n = this%m_layers%get_count()
        if (n == 0) then
            x = 0
        else
            ptr => this%get(n)
            if (.not.associated(ptr)) then
                x = 0
                return
            end if
            x = ptr%get_neuron_count()
        end if
    end function



    module function net_evaluate(this, inputs, err) result(x)
        ! Arguments
        class(network), intent(inout) :: this
        real(real64), intent(in), target, dimension(:) :: inputs
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, target, dimension(:) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: nIn
        real(real64), pointer, dimension(:) :: xIn
        class(layer), pointer :: lyr
        
        ! Initialization
        nIn = this%get_input_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(inputs) /= nIn) then
            write(errmsg, '(AI0AI0A)') "Incorrect input array size.  Expected ", nIn, &
                " inputs, but found ", size(inputs), "."
            call errmgr%report_error("net_evaluate", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Evaluate each layer
        xIn => inputs
        do i = 1, this%get_count() - 1
            ! Get a pointer to the layer object
            lyr => this%get(i)
            if (.not.associated(lyr)) then
                write(errmsg, '(AI0A)') "Layer ", i, " points to a null reference."
                call errmgr%report_error("net_evaluate", trim(errmsg), NN_NULL_POINTER_ERROR)
                return
            end if

            ! Evaluate the layer
            x = lyr%evaluate(xIn, errmgr)
            if (errmgr%has_error_occurred()) return

            ! Update the pointer for the next time around
            xIn => x
        end do
    end function
end submodule
