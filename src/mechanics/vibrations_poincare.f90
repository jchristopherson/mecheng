! vibrations_poincare.f90

submodule (vibrations) vibrations_poincare
contains
! ------------------------------------------------------------------------------
    module function compute_poincare_section(t, x, v, period, err) result(rst)
        ! Arguments
        real(real64), intent(in), dimension(:) :: t, x, v
        real(real64), intent(in) :: period
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, npts, nsample, flag
        real(real64) :: maxt, mint
        real(real64), allocatable, dimension(:) :: samples
        type(linear_interp) :: interp
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        npts = size(t)

        ! Ensure the input arrays are properly sized
        if (size(x) /= npts) then
            write(errmsg, '(AI0AI0A)') &
                "The input array was expected to be of size ", &
                npts, ", but was found to be of size ", size(x), "."
            call errmgr%report_error("compute_poincare_section", &
                trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(v) /= npts) then
            write(errmsg, '(AI0AI0A)') &
                "The derivative array was expected to be of size ", &
                npts, ", but was found to be of size ", size(v), "."
            call errmgr%report_error("compute_poincare_section", &
                trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Ensure period is a positive, non-zero value
        if (period < epsilon(period)) then
            call errmgr%report_error("compute_poincare_section", &
                "The sampling period must be a positive value larger " // &
                "than machine precision.", MECH_INVALID_INPUT_ERROR)
            return
        end if

        ! Build the time array at which values are to be sampled
        maxt = maxval(t)
        mint = minval(t)
        nsample = int((maxt - mint) / period, int32) + 1
        allocate(samples(nsample), stat = flag)
        if (flag == 0) allocate(rst(nsample, 2), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("compute_poincare_section", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        samples(1) = mint
        do i = 2, nsample
            samples(i) = samples(i-1) + period
        end do

        ! Interpolate to obtain the values - use linear interpolation
        call interp%initialize(t, x, err = errmgr)
        if (errmgr%has_error_occurred()) return
        rst(:,1) = interp%interpolate(samples)

        call interp%initialize(t, v, err = errmgr)
        if (errmgr%has_error_occurred()) return
        rst(:,2) = interp%interpolate(samples)
    end function

! ------------------------------------------------------------------------------
end submodule
