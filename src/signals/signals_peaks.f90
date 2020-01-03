! signals_peaks.f90

submodule (signals) signals_peaks
contains
! ------------------------------------------------------------------------------
module function peak_detect(v, delta, err) result(rst)
    ! Arguments
    real(real64), intent(in), dimension(:) :: v
    real(real64), intent(in) :: delta
    class(errors), intent(inout), optional, target :: err
    type(peak_info) :: rst

    ! Local Variables
    integer(int32) :: i, j, k, n, mxpos, mnpos, flag
    integer(int32), allocatable, dimension(:) :: maxind, minind
    real(real64), allocatable, dimension(:) :: maxtab, mintab
    real(real64) :: mx, mn, val
    logical :: lookformax
    type(errors), target :: deferr
    class(errors), pointer :: errmgr

    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    lookformax = .true.
    n = size(v)
    allocate(maxind(n), stat = flag)
    if (flag == 0) allocate(minind(n), stat = flag)
    if (flag == 0) allocate(maxtab(n), stat = flag)
    if (flag == 0) allocate(mintab(n), stat = flag)
    if (flag /= 0) go to 100
    mx = v(1)
    mn = v(1)
    mxpos = 1
    mnpos = 1
    
    ! Process
    j = 0
    k = 0
    do i = 1, n
        val = v(i)
        if (val > mx) then
            mx = val
            mxpos = i
        end if
        if (val < mn) then
            mn = val
            mnpos = i
        end if

        if (lookformax) then
            if (val < mx - delta) then
                j = j + 1
                maxtab(j) = mx
                maxind(j) = mxpos
            end if
        else
            if (val > mn + delta) then
                k = k + 1
                mintab(k) = mn
                minind(k) = mnpos
            end if
        end if
    end do

    ! Collect the output
    allocate(rst%max_values(j), stat = flag)
    if (flag == 0) allocate(rst%max_value_indices(j), stat = flag)
    if (flag == 0) allocate(rst%min_values(k), stat = flag)
    if (flag == 0) allocate(rst%min_value_indices(k), stat = flag)
    if (flag /= 0) go to 100

    do i = 1, j
        rst%max_values(i) = maxtab(i)
        rst%max_value_indices(i) = maxind(i)
    end do
    do i = 1, k
        rst%min_values(i) = mintab(i)
        rst%min_value_indices(i) = minind(i)
    end do
    
    ! End
    return

    ! Error Handling - Memory Allocation Issues
100 continue
    call errmgr%report_error("peak_detect", &
        "Insufficient memory available.", &
        SIG_OUT_OF_MEMORY_ERROR)
    return
end function

! ------------------------------------------------------------------------------
end submodule
