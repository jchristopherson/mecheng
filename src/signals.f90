! signals.f90

!> @brief \b signals
!!
!! @par Purpose
!! This module provides a small collection of basic signal processing routines.
module signals
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use real_transform_routines, only : rfft1i, rfft1b, rfft1f
    use complex_transform_routines, only : cfft1i, cfft1b, cfft1f
    implicit none
    private
    public :: low_pass_filter
    public :: high_pass_filter
    public :: band_pass_filter
    public :: band_stop_filter
    public :: averaging_filter
    public :: fft

contains
! ******************************************************************************
! FILTER ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Applies a low-pass filter to a time-series data set.
    !!
    !! @param[in,out] x On input, the time-series data to filter.  On output,
    !!  the filtered data set.
    !! @param[in] fs The frequency at which @p x was sampled, in Hz.
    !! @param[in] cutoff The filter cutoff frequency, in Hz.  Notice, a discrete
    !!  Fourier transform operation is used in the filtering process.  As such,
    !!  the frequency resolution of the discrete transform is @p fs / n, where
    !!  n is the length of @p x.
    subroutine low_pass_filter(x, fs, cutoff)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, cutoff

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n, indx, lwsave, lwork, flag
        real(real64) :: df, ndp, fmax
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        df = fs / ndp
        fmax = half * ndp * df
        lwsave = n + int(log(ndp), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Determine where to cut the frequency spectrum
        ! Note: f(i) = (i - 1) * df, such that i = f / df + 1
        ! Also, multiply by 2 due to the storage scheme of FFT
        indx = 2 * (int(cutoff / df + 1, int32))

        ! Zero out everything after INDX
        x(indx:n) = zero

        ! Compute the inverse transform to obtain the filtered signal
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Scale by N as the inverse transform does not perform this operation
        x = x / ndp
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Applies a high-pass filter to a time-series data set.
    !!
    !! @param[in,out] x On input, the time-series data to filter.  On output,
    !!  the filtered data set.
    !! @param[in] fs The frequency at which @p x was sampled, in Hz.
    !! @param[in] cutoff The filter cutoff frequency, in Hz.  Notice, a discrete
    !!  Fourier transform operation is used in the filtering process.  As such,
    !!  the frequency resolution of the discrete transform is @p fs / n, where
    !!  n is the length of @p x.
    subroutine high_pass_filter(x, fs, cutoff)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, cutoff

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n, indx, lwsave, lwork, flag
        real(real64) :: df, ndp, fmax
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        df = fs / ndp
        fmax = half * ndp * df
        lwsave = n + int(log(ndp), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Determine where to cut the frequency spectrum
        ! Note: f(i) = (i - 1) * df, such that i = f / df + 1
        ! Also, multiply by 2 due to the storage scheme of FFT
        indx = 2 * (int(cutoff / df + 1, int32))

        ! Zero out everything before INDX
        x(1:indx) = zero

        ! Compute the inverse transform to obtain the filtered signal
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Scale by N as the inverse transform does not perform this operation
        x = x / ndp
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Applies a band-pass filter to a time-series data set.
    !!
    !! @param[in,out] x On input, the time-series data to filter.  On output,
    !!  the filtered data set.
    !! @param[in] fs The frequency at which @p x was sampled, in Hz.
    !! @param[in] f1 The lower cutoff frequency, in Hz.
    !! @param[in] f2 The upper cutoff frequency, in Hz.
    !!
    !! @par Remarks
    !! Notice, a discrete Fourier transform operation is used in the filtering
    !! process.  As such, the frequency resolution of the discrete transform is
    !! @p fs / n, where n is the length of @p x.
    subroutine band_pass_filter(x, fs, f1, f2)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, f1, f2
        
        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n, indx1, indx2, lwsave, lwork, flag
        real(real64) :: df, ndp, fmax
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        df = fs / ndp
        fmax = half * ndp * df
        lwsave = n + int(log(ndp), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Determine where to cut the frequency spectrum
        ! Note: f(i) = (i - 1) * df, such that i = f / df + 1
        ! Also, multiply by 2 due to the storage scheme of FFT
        indx1 = 2 * int(f1 / df + 1, int32)
        indx2 = 2 * int(f2 / df + 1, int32)

        ! Zero out everything outside of the band of interest
        x(1:indx1) = zero
        x(indx2:n) = zero

        ! Compute the inverse transform to obtain the filtered signal
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Scale by N as the inverse transform does not perform this operation
        x = x / ndp
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Applies a band-stop filter to a time-series data set.
    !!
    !! @param[in,out] x On input, the time-series data to filter.  On output,
    !!  the filtered data set.
    !! @param[in] fs The frequency at which @p x was sampled, in Hz.
    !! @param[in] f1 The lower cutoff frequency, in Hz.
    !! @param[in] f2 The upper cutoff frequency, in Hz.
    !!
    !! @par Remarks
    !! Notice, a discrete Fourier transform operation is used in the filtering
    !! process.  As such, the frequency resolution of the discrete transform is
    !! @p fs / n, where n is the length of @p x.
    subroutine band_stop_filter(x, fs, f1, f2)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, f1, f2
        
        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n, indx1, indx2, lwsave, lwork, flag
        real(real64) :: df, ndp, fmax
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        df = fs / ndp
        fmax = half * ndp * df
        lwsave = n + int(log(ndp), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Determine where to cut the frequency spectrum
        ! Note: f(i) = (i - 1) * df, such that i = f / df + 1
        ! Also, multiply by 2 due to the storage scheme of FFT
        indx1 = 2 * int(f1 / df + 1, int32)
        indx2 = 2 * int(f2 / df + 1, int32)

        ! Zero out the band of interest
        x(indx1:indx2) = zero

        ! Compute the inverse transform to obtain the filtered signal
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Scale by N as the inverse transform does not perform this operation
        x = x / ndp
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Applies an averaging filter to a data set.
    !!
    !! @param[in,out] x On input, the signal to filter.  On output, the filtered
    !!  signal.
    !! @param[in] npts The number of points to include in the moving average.
    !!  This value must be at least 2, but not exceed the length of @p x.
    subroutine averaging_filter(x, npts)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        integer(int32), intent(in) :: npts

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        
        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: dnpts
        real(real64), allocatable, dimension(:) :: buffer

        ! Initialization
        n = size(x)
        dnpts = real(npts, real64)
        allocate(buffer(npts))

        ! Process
        buffer = zero
        do i = 1, n
            ! Scroll the buffer - let a value fall off the back end of the array
            buffer(2:npts) = buffer(1:npts - 1)

            ! Add a new sample value to the buffer
            buffer(1) = x(i)

            ! Compute the average
            x(i) = sum(buffer) / dnpts
        end do
    end subroutine

! ******************************************************************************
! FOURIER TRANSFORM ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Computes the Fourier transform of a discrete data set.
    !!
    !! @param[in] x The data set whose transform is to be computed.
    !! @return The complex-valued Fourier transform of @p x.  Notice, this data
    !!  is scaled by the factor N, where N is the length of @p x.
    function fft(x) result(tf)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: tf

        ! Local Variables
        integer(int32) :: i, n, lwsave, lwork, flag
        real(real64) :: ndp
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Parameters
        real(real64), parameter :: zero = 0.0d0

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        lwsave = 2 * n + int(log(ndp), int32) + 4
        lwork = 2 * n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the transform
        call cfft1i(n, wsave, lwsave, flag)

        ! Store data for the transform
        allocate(tf(n))
        do i = 1, n
            tf(i) = cmplx(x(i), zero, real64)
        end do

        ! Compute the transform
        call cfft1f(n, 1, tf, n, wsave, lwsave, work, lwork, flag)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
