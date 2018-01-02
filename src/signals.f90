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
    public :: ifft

! ******************************************************************************
! SIGNALS_FILTER SUBMODULE
! ------------------------------------------------------------------------------
interface
    !> @brief Applies a low-pass filter to a time-series data set.
    !!
    !! @param[in,out] x On input, the time-series data to filter.  On output,
    !!  the filtered data set.
    !! @param[in] fs The frequency at which @p x was sampled, in Hz.
    !! @param[in] cutoff The filter cutoff frequency, in Hz.  Notice, a discrete
    !!  Fourier transform operation is used in the filtering process.  As such,
    !!  the frequency resolution of the discrete transform is @p fs / n, where
    !!  n is the length of @p x.
    module subroutine low_pass_filter(x, fs, cutoff)
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, cutoff
    end subroutine

    !> @brief Applies a high-pass filter to a time-series data set.
    !!
    !! @param[in,out] x On input, the time-series data to filter.  On output,
    !!  the filtered data set.
    !! @param[in] fs The frequency at which @p x was sampled, in Hz.
    !! @param[in] cutoff The filter cutoff frequency, in Hz.  Notice, a discrete
    !!  Fourier transform operation is used in the filtering process.  As such,
    !!  the frequency resolution of the discrete transform is @p fs / n, where
    !!  n is the length of @p x.
    module subroutine high_pass_filter(x, fs, cutoff)
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, cutoff
    end subroutine
    
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
    module subroutine band_pass_filter(x, fs, f1, f2)
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, f1, f2
    end subroutine
    
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
    module subroutine band_stop_filter(x, fs, f1, f2)
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, f1, f2
    end subroutine
    
    !> @brief Applies an averaging filter to a data set.
    !!
    !! @param[in,out] x On input, the signal to filter.  On output, the filtered
    !!  signal.
    !! @param[in] npts The number of points to include in the moving average.
    !!  This value must be at least 2, but not exceed the length of @p x.
    module subroutine averaging_filter(x, npts)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        integer(int32), intent(in) :: npts
    end subroutine
end interface

! ******************************************************************************
! SIGNALS_FOURIER ROUTINES
! ------------------------------------------------------------------------------
interface
    !> @brief Computes the Fourier transform of a data set.
    !!
    !! @param[in] x The data set whose transform is to be computed.
    !! @return The complex-valued Fourier transform of @p x.
    module function fft(x) result(tf)
        real(real64), intent(in), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: tf
    end function

    !> @brief Computes the inverse Fourier transform of a complex-valued data
    !! set.
    !!
    !! @param[in] x The data set whose transform is to be computed.
    !! @return The complex-valued Fourier transform of @p x.
    module function ifft(x) result(tf)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: tf
    end function
end interface

end module
