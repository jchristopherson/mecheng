! neural_net_layer.f90

submodule (neural_net_core) neural_net_layer
contains
! ------------------------------------------------------------------------------
    module function layer_get(this, i) result(ptr)
        ! Arguments
        class(layer), intent(in) :: this
        integer(int32), intent(in) :: i
        class(neuron), pointer :: ptr

        ! Local Variables
        class(*), pointer :: genptr

        ! Get the stored object
        genptr => this%get(i)

        ! Convert to a neuron-based object
        select type (genptr)
        class is (neuron)
            ptr => genptr
        end select
    end function

! ------------------------------------------------------------------------------
    module subroutine layer_init(this, n, model, err)
        ! Arguments
        class(layer), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(neuron), intent(in) :: model
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: i

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (n < 1) then
            call errmgr%report_error("layer_init", &
                "There must be at least one neuron in the layer.", &
                NN_INVALID_INPUT_ERROR)
            return
        end if

        ! Clear the existing collection
        if (this%get_count() > 0) call this%clear()

        ! Process - Simply push the model onto the end of the collection
        ! as a copy of model will be stored, not the actual model
        if (this%get_capacity() < n) then
            call this%set_capacity(n, err = errmgr)
            if (errmgr%has_error_occurred()) return
        end if
        do i = 1, n
            call this%push(model)
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module function layer_evaluate(this, x, err) result(y)
        ! Arguments
        class(layer), intent(in) :: this
        real(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(neuron), pointer :: nrn
        character(len = 256) :: errmsg

        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (n < 1) then
            call errmgr%report_error("layer_evaluate", &
                "The layer must have at least one neuron.", &
                NN_INVALID_LAYER_ERROR)
            return
        end if

        ! Memory Allocation
        allocate(y(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("layer_evaluate", &
                "Insufficient memory available.", &
                NN_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Cycle over each neuron
        !
        ! TO DO: Investigate making a parallel loop
        do i = 1, n
            ! Obtain a pointer to the neuron
            nrn => this%get_neuron(i)

            ! Ensure the pointer references a valid object
            if (.not.associated(nrn)) then
                write(errmsg, '(AI0A)') "An invalid pointer to neuron ", i, " was found."
                call errmgr%report_error("layer_evaluate", &
                    trim(errmsg), &
                    NN_NULL_POINTER_ERROR)
                return
            end if

            ! Evaluate the neuron
            y(i) = nrn%evaluate(x)
        end do
    end function

! ------------------------------------------------------------------------------
end submodule
