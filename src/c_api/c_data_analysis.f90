! c_data_analysis.f90

!> @brief A collection of routines for FRF data analysis purposes.
module c_data_analysis
    use iso_fortran_env
    use iso_c_binding
    use curvefit_regression
    use vibrations
    use signals
    use ferror
    implicit none

    !> @brief A C-compatible type for defining peak detect search ranges.
    type, bind(C) :: search_range
        !> @brief The minimum x-coordinate of the range.
        real(c_double) :: min_x
        !> @brief The maximum x-coordinate of the range.
        real(c_double) :: max_x
        !> @brief The minimum y deviation to determine a peak.
        real(c_double) :: min_y
    end type

contains
! *************************************************************************** !
    !> @brief Smooths the supplied data set.
    !!
    !! @param[in] x An N-element array containing the monotonically increasing
    !!  independent variable data.
    !! @param[in] y An N-element array containing the dependent variable data
    !!  corresponding to @p x.
    !! @param[in] factor A positive value defining the smoothing factor.
    !! @param[in,out] err An optional errors-based object that if provided 
    !!  can be used to retrieve information relating to any errors encountered
    !!  during execution. If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling. Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - CF_ARRAY_SIZE_ERROR: Occurs if x and y are not the same size.
    !!  - CF_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    !!
    !! @return The smoothed version of @p y.
    function smooth_data(x, y, factor, err) result(ys)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), intent(in) :: factor
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:) :: ys

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(lowess_smoothing) :: obj

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        call obj%initialize(x, y, .false., errmgr)
        if (errmgr%has_error_occurred()) return
        
        ys = obj%smooth(factor, errmgr)
    end function

! *************************************************************************** !
    !> @brief Locates the peaks in each specified range within the data set.
    !!
    !! @param[in] x An N-element array containing the monotonically increasing
    !!  independent variable data.
    !! @param[in] y An N-element array containing the dependent variable data
    !!  corresponding to @p x.
    !! @param[in] ranges An array containing search range information.
    !! @param[in,out] err An optional errors-based object that if provided 
    !!  can be used to retrieve information relating to any errors encountered
    !!  during execution. If not provided, a default implementation of the 
    !!  errors class is used internally to provide error handling. Possible 
    !!  errors and warning messages that may be encountered are as follows.
    !!  - MECH_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - MECH_ARRAY_SIZE_ERROR: Occurs if @p x and @p y are not the same
    !!      size.
    !!  - MECH_NONMONOTONIC_ERROR: Occurs if @p x is not monotonically 
    !!      increasing.
    !!
    !! @return An M-by-2 matrix containing the x and y peak information,
    !!  where M is the number of ranges.  The first column of the matrix
    !!  contains the x-coordinate of each peak, and the second column
    !!  contains the corresponding y-coordinate of each peak.
    function find_peaks(x, y, ranges, err) result(pks)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        type(search_range), intent(in), dimension(:) :: ranges
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: pks

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(frf_search_info), allocatable, dimension(:) :: info
        type(peak_info), allocatable, dimension(:) :: pklist
        integer(int32) :: i, nranges, ind

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Define range information
        nranges = size(ranges)
        allocate(info(nranges))
        do i = 1, nranges
            info(i)%min_frequency = ranges(i)%min_x
            info(i)%max_frequency = ranges(i)%max_x
            info(i)%amplitude_threshold = ranges(i)%min_y
        end do

        ! Compute the peaks
        pklist = frf_peak_detect(x, y, info, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Seperate out the peaks - only take the max from each range
        allocate(pks(nranges, 2))
        do i = 1, nranges
            ind = maxloc(pklist(i)%max_values, 1)
            pks(i,1) = x(pklist(i)%max_value_indices(ind))
            pks(i,2) = pklist(i)%max_values(ind)
        end do
    end function

! *************************************************************************** !
!                                    C API                                    !
! --------------------------------------------------------------------------- !
    function c_smooth_data(n, x, y, factor, ys) result(flag) &
            bind(C, name = "c_smooth_data")
        ! Arguments
        integer(c_int), intent(in), value :: n
        real(c_double), intent(in) :: x(n), y(n)
        real(c_double), intent(in), value :: factor
        real(c_double), intent(out) :: ys(n)
        integer(c_int) :: flag

        ! Local Variables
        type(errors) :: err

        ! Initialization
        flag = 0
        call err%set_exit_on_error(.false.)

        ! Process
        ys = smooth_data(x, y, factor, err)
        if (err%has_error_occurred()) flag = err%get_error_flag()
    end function

! *************************************************************************** !
    function c_find_peaks(n, x, y, nranges, ranges, pks) result(flag) &
            bind(C, name = "c_find_peaks")
        ! Arguments
        integer(c_int), intent(in), value :: n, nranges
        real(c_double), intent(in) :: x(n), y(n)
        type(search_range), intent(in) :: ranges(nranges)
        real(c_double), intent(out) :: pks(nranges, 2)
        integer(c_int) :: flag

        ! Local Variables
        type(errors) :: err

        ! Initialization
        flag = 0
        call err%set_exit_on_error(.false.)

        ! Process
        pks = find_peaks(x, y, ranges, err)
        if (err%has_error_occurred()) flag = err%get_error_flag()
    end function

! *************************************************************************** !

! *************************************************************************** !
end module
