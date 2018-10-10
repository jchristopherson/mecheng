! neural_net_ff_network.f90

submodule (neural_net_core) neural_net_ff_network
contains
! ------------------------------------------------------------------------------
    module function ff_evaluate(this, x, err) result(y)
        ! Arguments
        class(feedforward_network), intent(in) :: this
        real(real64), intent(in), target, dimension(:) :: x
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:) :: y

        ! Local Variables
        type(errors), target :: deferr
        class(errors), pointer :: errmgr
        integer(int32) :: i, n, npts, flag
        real(real64), allocatable, target, dimension(:) :: temp, outs
        real(real64), pointer, dimension(:) :: ins
        class(layer), pointer :: lyrptr
        character(len = 256) :: errmsg

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        n = this%get_count()

        ! Ensure we've got at least 3 layers to work with
        if (n < 3) then
            call errmgr%report_error("ff_evaluate", &
                "The network must have at least 3 layers.", &
                NN_INVALID_NETWORK_ERROR)
            return
        end if

        ! Cycle over each layer, and evaluate each layer
        do i = 1, n
            ! Retrieve the layer
            lyrptr => this%get_layer(i)

            ! Ensure we have a valid pointer
            if (.not.associated(lyrptr)) then
                write(errmsg, '(AI0A)') &
                     "A null pointer was encountered when looking for layer ", &
                     i, "."
                call errmgr%report_error("ff_evaluate", &
                    trim(errmsg), &
                    NN_NULL_POINTER_ERROR)
                return
            end if

            ! Establish the input layer
            if (i == 1) then
                npts = lyrptr%get_count()
                if (size(x) == npts) then
                    ins => x
                else if (size(x) < npts) then
                    allocate(temp(npts), stat = flag)
                    if (flag /= 0) then
                        call errmgr%report_error("ff_evaluate", &
                            "Insufficient memory available.", &
                            NN_OUT_OF_MEMORY_ERROR)
                        return
                    end if
                    temp(1:size(x)) = x
                    temp(size(x)+1:npts) = 0.0d0
                    ins => temp
                else
                    ins => x(1:npts)
                end if
            else
                ins => outs
            end if

            ! Evaluate the layer
            outs = lyrptr%evaluate(ins, errmgr)
            if (errmgr%has_error_occurred()) return
        end do
        if (allocated(outs)) y = outs
    end function

! ------------------------------------------------------------------------------
end submodule
