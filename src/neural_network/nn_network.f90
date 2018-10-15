! nn_network.f90

submodule (neural_network_core) nn_network
    ! A container to allow collections of arrays.
    type array_container
        ! The array.
        real(real64), allocatable, dimension(:) :: x
    end type

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



    module function net_backprop(this, cfcn, n, x, y, err)
        ! Arguments
        class(network), intent(in) :: this
        procedure(cost_function), intent(in), pointer :: cfcn
        integer(int32), intent(in) :: n
        real(real64), intent(in), dimension(:) :: x, y
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, nLayers, flag
        class(layer), pointer :: lyr
        type(array_container), allocatable, dimension(:) :: z, sprime, a
        real(real64), allocatable, dimension(:) :: delta
        
        ! Initialization
        nLayers = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the network is initialized
        if (nLayers < 1) then
            call errmgr%report_error("net_backprop", &
                "The network has not been initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        ! Ensure the number of inputs and outputs jive with the
        ! input and output array sizes
        if (size(x) /= this%get_input_count()) then
            write(errmsg, '(AI0AI0A)') "Expected to find ", this%get_input_count(), &
                " elements in the input array, but found ", size(x), " instead."
            call errmgr%report_error("net_backprop", trim(errmsg), &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        if (size(y) /= this%get_output_count()) then
            write(errmsg, '(AI0AI0A)') "Expected to find ", this%get_output_count(), &
                " elements in the output array, but found ", size(y), " instead."
            call errmgr%report_error("net_backprop", trim(errmsg), &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Local Memory Allocation
        allocate(z(nLayers - 1), stat = flag)
        if (flag == 0) allocate(sprime(nLayers - 1), stat = flag)
        if (flag == 0) allocate(a(nLayers - 1), stat = flag)
        if (flag == 0) allocate(delta(size(y)), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("net_backprop", &
                "Insufficient memory available.", &
                NN_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Feed-Forward Process
        do i = 1, nLayers - 1
            ! Get a pointer to the layer
            lyr => this%get(i)
            if (.not.associated(lyr)) then
                write(errmsg, '(AI0A)') "Layer ", i + 1, " is not initialized properly."
                call errmgr%report_error("net_backprop", trim(errmsg), &
                    NN_NULL_POINTER_ERROR)
                return
            end if

            ! Compute z & sigma'(z)
            if (i == 1) then
                z(i)%x = lyr%eval_arguments(x, errmgr)
            else
                z(i)%x = lyr%eval_arguments(a(i-1)%x, errmgr)
            end if
            if (errmgr%has_error_occurred()) return

            sprime(i)%x = lyr%eval_neural_derivative(z(i)%x, errmgr)
            if (errmgr%has_error_occurred()) return

            ! Compute the layer output
            a(i)%x = lyr%eval_neural_function(z(i)%x, errmgr)
            if (errmgr%has_error_occurred()) return
        end do

        ! Compute the output error
        delta = compute_cost_gradient(cfcn, n, a(nLayers - 1)%x, y, errmgr) * &
            sprime(nLayers - 1)%x

        ! Backpropagate the error
        do i = nLayers - 1, 1, -1
        end do

        ! Compute the derivatives
    end function







! ******************************************************************************
! PRIVATE ROUTINES
! ------------------------------------------------------------------------------
    ! Private function for computing the gradient of the cost function
    ! with respect to the output layer results.
    function compute_cost_gradient(fcn, n, a, y, err) result(dc)
        ! Arguments
        procedure(cost_function), intent(in), pointer :: fcn
        integer(int32), intent(in) :: n
        real(real64), intent(in), dimension(:) :: a, y
        real(real64), allocatable, dimension(:) :: dc
        class(errors), intent(inout) :: err

        ! Local Variables
        integer(int32) :: i, nPts, flag
        real(real64) :: eps, epsmch, temp, h, c
        real(real64), allocatable, dimension(:) :: work

        ! Initialization
        nPts = size(a)
        epsmch = epsilon(epsmch)
        eps = sqrt(epsmch)
        work = a

        ! Local Memory Allocation
        allocate(dc(nPts), stat = flag)
        if (flag /= 0) then
            call err%report_error("compute_cost_gradient", &
                "Insufficient memory available.", &
                NN_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Evaluate the cost function at "A"
        c = fcn(n, a, y)

        ! Process
        do i = 1, nPts
            temp = work(i)
            h = eps * abs(temp)
            if (h <= 2.0d0 * epsmch) h = eps
            work(i) = temp + h
            dc(i) = (fcn(n, work, y) - c) / h
            work(i) = temp
        end do
    end function
end submodule
