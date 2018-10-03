! neural_net_network.f90

submodule (neural_net_core) neural_net_network
contains
! ------------------------------------------------------------------------------
    module subroutine network_init(this, lyrs, model, err)
        ! Arguments
        class(neural_network), intent(inout) :: this
        integer(int32), intent(in), dimension(:) :: lyrs
        class(neuron), intent(in) :: model
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        integer(int32) :: i, n, nin, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        class(neuron), allocatable :: nrn
        type(layer) :: lyr
        class(layer), pointer :: ptr

        ! Initialization
        n = size(lyrs)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (n < 3) then
            write(errmsg, '(AI0A)') &
                "3 layers were expected for this network type, but ", &
                n, " were found."
            call errmgr%report_error("network_init", trim(errmsg), &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Memory Allocation
        if (this%get_count() /= 0) call this%clear()
        if (this%get_capacity() < n) call this%set_capacity(n, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Establish the network structure
        do i = 1, n
            ! Ensure there is at least 1 neuron in the current layer
            if (lyrs(i) < 1) then
                write(errmsg, '(AI0AI0A)') "Layer ", i, &
                    " was expected to have at least 1 neuron, but ", &
                    lyrs(i), " were found."
                call errmgr%report_error("network_init", trim(errmsg), &
                    NN_INVALID_INPUT_ERROR)
                return
            end if

            ! Place the layer into the collection of layers.  The push
            ! method will create a copy of lyr, and store its copy.
            ! As such, it is irrelevant what we do with lyr after
            ! this call.
            call this%push(lyr, errmgr)
            if (errmgr%has_error_occurred()) return
        end do

        ! Set up each layer structure
        nin = 1 ! The number of inputs accepted by each neuron of the current layer
        do i = 1, n
            ! Update the input count for the model neuron for this layer
            if (allocated(nrn)) deallocate(nrn)
            allocate(nrn, source = model, stat = flag)
            if (flag /= 0) then
                call errmgr%report_error("network_init", &
                    "Insufficient memory available.", &
                    NN_OUT_OF_MEMORY_ERROR)
                return
            end if
            call nrn%initialize(nin, err = errmgr)
            if (errmgr%has_error_occurred()) return

            ! Define the layer structure
            ptr => this%get_layer(i)
            call ptr%initialize(lyrs(i), nrn, errmgr)

            ! Update the input number count
            nin = lyrs(i)
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module function network_get(this, i, err) result(ptr)
        ! Arguments
        class(neural_network), intent(in) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), target, optional :: err
        class(layer), pointer :: ptr

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(*), pointer :: genptr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        nullify(ptr)

        ! Error Checking
        if (i < 1 .or. i > this%get_count()) then
            call errmgr%report_error("network_get", &
                "The supplied index was outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        genptr => this%get(i)
        select type (genptr)
        class is (layer)
            ptr => genptr
        end select
    end function

! ------------------------------------------------------------------------------
    module subroutine network_randomize(this, err)
        ! Arguments
        class(neural_network), intent(inout) :: this
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        integer(int32) :: i, j, nlyrs
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(layer), pointer :: lyr
        class(neuron), pointer :: nrn
        character(len = 256) :: errmsg

        ! Initialization
        nlyrs = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (nlyrs < 1) then
            call errmgr%report_error("network_randomize", &
                "The network has not been properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        ! Process
        !
        ! TO DO: Investigate how to run this loop in parallel
        do i = 1, nlyrs
            ! Obtain a pointer to the layer
            lyr => this%get_layer(i)

            ! Ensure the layer pointer is valid
            if (.not.associated(lyr)) then
                write(errmsg, '(AI0A)') "Layer ", i, "is undefined."
                call errmgr%report_error("network_randomize", &
                    trim(errmsg), NN_NULL_POINTER_ERROR)
                return
            end if

            ! Cycle over each neuron in the layer
            do j = 1, lyr%get_count()
                ! Obtain a pointer to the neuron
                nrn => lyr%get_neuron(j)

                ! Ensure the neuron pointer is valid
                if (.not.associated(nrn)) then
                    write(errmsg, '(AI0AI0A)') "Neuron ", j, &
                        " in layer ", i, " is undefined."
                    call errmgr%report_error("network_randomize", &
                        trim(errmsg), NN_NULL_POINTER_ERROR)
                    return
                end if

                ! Randomize the neuron
                call nrn%randomize(errmgr)
                if (errmgr%has_error_occurred()) return
            end do
        end do
    end subroutine

! ------------------------------------------------------------------------------
end submodule
