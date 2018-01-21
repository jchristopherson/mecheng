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
    public :: rfft
    public :: irfft
    public :: upsample
    public :: downsample
    public :: remove_mean
    public :: bartlett_window
    public :: welch_window
    public :: hann_window
    public :: hamming_window
    public :: blackman_window
    public :: window_function
    public :: is_power_of_two
    public :: next_power_of_two
    public :: previous_power_of_two
    public :: buffer
    public :: signal_magnitude

! ******************************************************************************
! GENERAL INTERFACES
! ------------------------------------------------------------------------------
!> @brief Computes the magnitude of the discrete harmonics of a signal.
interface signal_magnitude
    modulce procedure :: signal_magnitude_win
end interface

! ******************************************************************************
! SIGNALS_SPECTRAL_ANALYSIS
! ------------------------------------------------------------------------------
interface
    !> @brief Defines a window function.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    function window_function(bin, winsize) result(x)
        use, intrinsic :: iso_fortran_env, only : int32, real64
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function
    
    !> @brief Computes the magnitude of the discrete harmonics of a signal.
    !!
    !! @param[in] x The N-element array containing the signal data.
    !! @param[in] winfun The window function to apply.
    !! @param[in] winsize The window size.  It is recommended that this value
    !!  be an integer power of two, even if the size of @p x is not.
    !! @return The magnitude of the each harmonic component of @p x.
    module function signal_magnitude_win(x, winfun, winsize) result(y)
        real(real64), intent(in), dimension(:) :: x
        procedure(window_function), pointer, intent(in) :: winfun
        integer(int32), intent(in) :: winsize
        real(real64), allocatable, dimension(:) :: y
    end function
end interface

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

    !> @brief Computes the Fourier transform of a real-valued data set, and
    !! returns only the positive half of the transform.
    !!
    !! @param[in,out] x The data set whose transform is to be computed.  On
    !!  output, the original values are altered as this array is used as
    !!  in-place storage.
    !! @return The complex-valued positive half of the Fourier transform of 
    !!  @p x.
    module function rfft(x) result(tf)
        real(real64), intent(inout), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: tf
    end function

    !> @brief Computes the inverse Fourier transform for a real-valued data set.
    !!
    !! @param[in] x The data set whose transform is to be computed.
    !! @return The real-valued results of the transform of @p x.
    module function irfft(x) result(tf)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        real(real64), allocatable, dimension(:) :: tf
    end function
    
end interface

! ******************************************************************************
! SIGNALS_WINDOWS ROUTINES
! ------------------------------------------------------------------------------
interface
    !> @brief Defines a Bartlett window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure elemental module function bartlett_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function
    
    !> @brief Defines a Welch window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure elemental module function welch_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function
    
    !> @brief Defines a Hann window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure elemental module function hann_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function

    !> @brief Defines a Hamming window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure elemental module function hamming_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function

    !> @brief Defines a Blackman window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure elemental module function blackman_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function
end interface

! ******************************************************************************
! SIGNALS_OPERATIONS ROUTINES
! ------------------------------------------------------------------------------
interface
    !> @brief Resamples a discrete data set by the specified factor.
    !!
    !! @param[in] x The data set to resample.
    !! @param[in] fs The frequency at which the original data was sampled.
    !! @param[in] factor An integer factor defining a factor of how much to
    !!  increase the sample rate @p fs.
    !! @return The resampled data set.
    module function upsample(x, fs, factor) result(y)
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(in) :: fs
        integer(int32), intent(in) :: factor
        real(real64), allocatable, dimension(:) :: y
    end function
    
    !> @brief Resamples a discrete data set by the specified factor.
    !!
    !! @param[in] x The data set to resample.
    !! @param[in] fs The frequency at which the original data was sampled.
    !! @param[in] factor An integer factor defining a factor of how much to
    !!  decrease the sample rate @p fs.
    !! @return The resampled data set.
    module function downsample(x, fs, factor) result(y)
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(in) :: fs
        integer(int32), intent(in) :: factor
        real(real64), allocatable, dimension(:) :: y
    end function

    !> @brief Removes any mean (DC) level offset from a signal.
    !!
    !! @param[in,out] x On input, the signal on which to operate.  On output,
    !!  the modified signal.
    module subroutine remove_mean(x)
        real(real64), intent(inout), dimension(:) :: x
    end subroutine
    
    !> @brief Tests to see if a value is an integer power of two.
    !!
    !! @param[in] n The value to test.
    !! @return Returns true if @p n is a power of two; else, false.
    pure elemental module function is_power_of_two(n) result(x)
        integer(int32), intent(in) :: n
        logical :: x
    end function
    
    !> @brief Computes the next higher integer power of two.
    !!
    !! @param[in] n The value.
    !! @return The next integer power of two higher than @p n.
    pure elemental module function next_power_of_two(n) result(x)
        integer(int32), intent(in) :: n
        integer(int32) :: x
    end function
    
    !> @brief Computes the next lower integer power of two.
    !!
    !! @param[in] n The value.
    !! @return The previous integer power of two lower than @p n.
    pure elemental module function previous_power_of_two(n) result(x)
        integer(int32), intent(in) :: n
        integer(int32) :: x
    end function
    
    !> @brief Splits the array into equally sized buffers.
    !!
    !! @param[in] x The array to split.
    !! @param[in] npts The number of points to place in each buffer.
    !! @return An NPTS-by-NBUFFER matrix containing the buffered data.
    !!
    !! @par Remarks
    !! To accomodate the splitting of the data into the appropriate sized 
    !! buffers, zeros may be added to the first and last buffers to 
    !! appropriately fill each buffer.  Also notice, data may be overlapped if
    !! such an arrangement makes for a better fit within the buffers.
    module function buffer(x, npts) result(y)
        real(real64), intent(in), dimension(:) :: x
        integer(int32), intent(in) :: npts
        real(real64), allocatable, dimension(:,:) :: y
    end function
end interface

end module
