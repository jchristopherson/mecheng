! neural_net_network.f90

submodule (neural_net_core) neural_net_network
contains
! ------------------------------------------------------------------------------
    module function network_get(this, i) result(ptr)
        ! Arguments
        class(neural_network), intent(in) :: this
        integer(int32), intent(in) :: i
        class(layer), pointer :: ptr

        ! Local Variables
        class(*), pointer :: genptr

        ! Get the stored object
        genptr => this%get(i)

        ! Convert to a layer-based object
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
