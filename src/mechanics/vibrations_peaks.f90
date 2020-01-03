! vibrations_peaks.f90

submodule (vibrations) vibrations_peaks
    use curvefit_core, only : is_monotonic
contains
! ------------------------------------------------------------------------------
module function frf_peak_detect_1(freq, amp, ranges, err) result(rst)
    ! Arguments
    real(real64), intent(in), dimension(:) :: freq, amp
    class(frf_search_info), intent(in), dimension(:) :: ranges
    class(errors), intent(inout), optional, target :: err
    type(peak_info), allocatable, dimension(:) :: rst

    ! Local Variables
    integer(int32) :: i, start, finish, nranges, flag
    class(errors), pointer :: errmgr
    type(errors), target :: deferr

    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    nranges = size(ranges)

    ! Quick Return
    if (size(freq) < 2) return
    if (nranges == 0) return

    ! Ensure freq is monotonically increasing
    if (.not.is_monotonic(freq)) then
        ! ERROR: The array is not monotonic
        call errmgr%report_error("frf_peak_detect_1", &
            "The frequency array is not monotonic.", &
            MECH_NONMONOTONIC_ERROR)
        return
    end if
    if (freq(2) < freq(1)) then
        ! ERROR: The array is not monotonically increasing
        call errmgr%report_error("frf_peak_detect_1", &
            "The frequency array is not monotonically increasing.", &
            MECH_NONMONOTONIC_ERROR)
        return
    end if

    ! Ensure freq & amp are the same size
    if (size(freq) /= size(amp)) then
        ! ERROR: Array sizes must match
        call errmgr%report_error("frf_peak_detect_1", &
            "The frequency and amplitude arrays must be the same size.", &
            MECH_ARRAY_SIZE_ERROR)
        return
    end if

    ! Allocate space for the output
    allocate(rst(nranges), stat = flag)
    if (flag /= 0) then
        ! ERROR: Memory issue
        call errmgr%report_error("frf_peak_detect_1", &
            "Insufficient memory available.", &
            MECH_OUT_OF_MEMORY_ERROR)
        return
    end if

    ! Cycle over each range
    do i = 1, nranges
        ! Locate the start and finish indices
        start = find_first_index(freq, ranges(i)%min_frequency)
        finish = find_first_index(freq, ranges(i)%max_frequency)

        ! Search for max and min values over the requested range
        rst(i) = peak_detect(amp(start:finish), &
            ranges(i)%amplitude_threshold, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Offset the indices to account for the starting point
        rst(i)%max_value_indices = rst(i)%max_value_indices + start - 1
        rst(i)%min_value_indices = rst(i)%min_value_indices + start - 1
    end do
end function

! ------------------------------------------------------------------------------
module function frf_peak_detect_2(freq, amp, thrsh, err) result(rst)
    ! Arguments
    real(real64), intent(in), dimension(:) :: freq, amp
    real(real64) :: thrsh
    class(errors), intent(inout), optional, target :: err
    type(peak_info) :: rst

    ! Local Variables
    type(frf_search_info) :: ranges(1)
    type(peak_info) :: rsts(1)

    ! Initialization
    ranges(1)%min_frequency = freq(1)
    ranges(1)%max_frequency = freq(size(freq))
    ranges(1)%amplitude_threshold = thrsh

    ! Process
    rsts = frf_peak_detect(freq, amp, ranges, err)
    rst = rsts(1)
end function

! ------------------------------------------------------------------------------
! Finds the index of the first occurrence of the specified value. If
! the value isn't found, the first index of the closest value larger
! in magnitude.  It is assumed that x is monotonically increasing.
pure function find_first_index(x, val) result(ind)
    ! Arguments
    real(real64), intent(in), dimension(:) :: x
    real(real64), intent(in) :: val
    integer(int32) :: ind

    ! Process
    ind = 1
    do while (x(ind) < val)
        ind = ind + 1
        if (ind > size(x)) then
            ind = size(x)
            exit
        end if
    end do
end function

! ------------------------------------------------------------------------------
end submodule
