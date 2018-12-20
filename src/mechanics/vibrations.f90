! vibrations.f90

module vibrations
    use iso_fortran_env
    use ferror
    use mechanics_constants
    use curvefit_interp
    use linalg_core
    use constants
    implicit none
    private
    public :: modal_information
    public :: compute_poincare_section
    public :: compute_modal_response

    ! TO DO:
    ! - Primary Oscillation Frequency Finder (Base upon an FFT, and return the largest magnitude non-DC frequency)
    ! - Frequency Sweep Type FRF's
    ! - Modal Analysis

    !> @brief Contains modal information such as frequency and mode shape.
    type modal_information
        !> The modal frequency, in Hz.
        real(real64) :: frequency
        !> The mode shape.
        real(real64), allocatable, dimension(:) :: mode_shape
    end type

contains
! ------------------------------------------------------------------------------
    !> @brief Computes the values of the dependent variable and its first 
    !! derivative at even intervals for use in generating a Poincare section.
    !!
    !! @param[in] t An n-element array containing the independent variable.
    !! @param[in] x An n-element array containing the dependent variable 
    !!  values corresponding to the values in @p t.
    !! @param[in] v An n-element array containing the first derivative values
    !!  of @p x with respect to @p t.
    !! @param[in] period The sampling period.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - MECH_ARRAY_SIZE_ERROR: Occurs if @p x and @p v are not the same size.
    !!  - MECH_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    !!  - CF_NONMONOTONIC_ARRAY_ERROR: Occurs if @p x is not monotonically
    !!      increasing or decreasing.
    !!  - MECH_INVALID_INPUT_ERROR: Occurs if @p period is less than machine 
    !!      precision.
    !!
    !! @return A matrix with two columns.  Column 1 contains the sampled
    !!  dependent variable values, and column 2 contains the corresponding
    !!  first derivative values.
    function compute_poincare_section(t, x, v, period, err) result(rst)
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
    !> @brief Computes the modal response of a dynamic system.
    !!
    !! @param[in] m An N-by-N mass matrix.
    !! @param[in] k An N-by-N stiffness matrix.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - MECH_ARRAY_SIZE_ERROR: Occurs if either input matrix is not square,
    !!      or if the two matrices are sized differently.
    !!  - LA_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    !!  - LA_CONVERGENCE_ERROR: Occurs if the eigen solver fails to converge.
    !!
    !! @return A list of modal information.
    function compute_modal_response(m, k, err) result(rst)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: m, k
        class(errors), intent(inout), optional, target :: err
        type(modal_information), allocatable, dimension(:) :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: i, j, n, flag, ind
        real(real64), allocatable, dimension(:,:) :: mc, kc
        complex(real64), allocatable, dimension(:) :: vals
        complex(real64), allocatable, dimension(:,:) :: vecs
        real(real64) :: val

        ! Function Prototypes
        interface 
            function IDAMAX(n, x, incx) result(rst)
                use iso_fortran_env
                integer(int32), intent(in) :: n, incx
                real(real64), intent(in) :: x(n)
                integer(int32) :: rst
            end function
        end interface
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        n = size(m, 1)

        ! Input Checking
        if (size(m, 2) /= n) then
            call errmgr%report_error("compute_modal_response", &
                "The mass matrix must be square.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(k, 1) /= n .or. size(k, 2) /= n) then
            call errmgr%report_error("compute_modal_response", &
                "The stiffness matrix must be the same size " // &
                "as the mass matrix.", MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Memory Allocation
        allocate(mc(n,n), stat = flag)
        if (flag == 0) allocate(kc(n,n), stat = flag)
        if (flag == 0) allocate(vals(n), stat = flag)
        if (flag == 0) allocate(vecs(n,n), stat = flag)
        if (flag == 0) allocate(rst(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("compute_modal_response", &
                "There is insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Copy the input matrices to avoid overwriting their contents
        do j = 1, n
            do i = 1, n
                kc(i,j) = k(i,j)
                mc(i,j) = m(i,j)
            end do
        end do

        ! Compute the eigenvalues and eigenvectors
        call eigen(kc, mc, vals, vecs = vecs, err = errmgr)
        if (errmgr%has_error_occurred()) return

        ! Sort the eigenvalues and eigenvectors
        call sort(vals, vecs, .true.)

        ! Report the results
        do i = 1, n
            rst(i)%frequency = sqrt(real(vals(i), real64)) / (2.0d0 * pi)
            rst(i)%mode_shape = real(vecs(:,i), real64)

            ! Normalize the mode shape vector to it's largest magnitude component
            ind = IDAMAX(n, rst(i)%mode_shape, 1)
            val = rst(i)%mode_shape(ind)
            rst(i)%mode_shape = rst(i)%mode_shape / val
        end do
    end function

! ------------------------------------------------------------------------------
end module
