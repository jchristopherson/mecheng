! signals.f90

! TO DO:
! - Numerical Integration:



!> @brief \b signals
!!
!! @par Purpose
!! This module provides a small collection of basic signal processing routines.
!!
!! @par Example
!! The following example illustrates the use of some of the signal processing
!! routines available in this module.  Additionally, the FPLOT library is used
!! to support the plotting operations.
!!
!! @code{.f90}
!! program example
!!     use iso_fortran_env
!!     use signals
!!     use fplot_core
!!     implicit none
!!
!!     ! Variables
!!     integer(int32), parameter :: npts = 4096
!!     real(real64), parameter :: fs = 1024.0d0
!!     integer(int32), parameter :: winSize = 256
!!
!!     real(real64), allocatable, dimension(:,:) :: x
!!     real(real64), allocatable, dimension(:) :: freq, mag, dx, filtMag
!!     procedure(window_function), pointer :: win
!!     type(multiplot) :: mplt
!!     type(plot_2d) :: plt1, plt2, plt3
!!     type(plot_data_2d) :: d1, d2, d3, d1_filt, d4, d4_filt
!!     class(legend), pointer :: lgnd, lgnd3
!!     class(plot_axis), pointer :: xAxis1, xAxis2, xAxis3, yAxis1, yAxis2, yAxis3
!!     class(terminal), pointer :: term
!!
!!     ! Build the signal
!!     x = build_signal(npts, fs)
!!
!!     call d1%set_name("Original Signal")
!!     call d1%define_data(x(:,1), x(:,2))
!!
!!     call d2%set_name("Derivative")
!!     call d2%define_data(x(:,1), x(:,3))
!!
!!     ! Compute the derivative
!!     dx = finite_diff(x(:,1), x(:,2))
!!
!!     call d3%set_name("Computed Derivative")
!!     call d3%define_data(x(:,1), dx)
!!
!!     ! Investigate the magnitude of the signal - use a Hamming window
!!     win => hamming_window
!!     mag = signal_magnitude(x(:,2), win, winSize)
!!     freq = frequencies(fs, winSize)
!!
!!     call d4%set_name("Original Signal")
!!     call d4%define_data(freq, mag)
!!
!!     ! Filter the data set
!!     call low_pass_filter(x(:,2), fs, 1.0d1)
!!
!!     call d1_filt%set_name("Filtered")
!!     call d1_filt%define_data(x(:,1), x(:,2))
!!
!!     ! Look at the magnitude after filtering
!!     filtMag = signal_magnitude(x(:,2), win, winSize)
!!
!!     call d4_filt%set_name("Filtered")
!!     call d4_filt%define_data(freq, filtMag)
!!
!!     ! Create the plots
!!     call mplt%initialize(3, 1)
!!     call plt1%initialize()
!!     call plt2%initialize()
!!     call plt3%initialize()
!!
!!     term => mplt%get_terminal()
!!     call term%set_window_width(1000)
!!     call term%set_window_height(800)
!!
!!     lgnd => plt1%get_legend()
!!     call lgnd%set_is_visible(.true.)
!!
!!     lgnd3 => plt3%get_legend()
!!     call lgnd3%set_is_visible(.true.)
!!
!!     xAxis1 => plt1%get_x_axis()
!!     yAxis1 => plt1%get_y_axis()
!!
!!     xAxis2 => plt2%get_x_axis()
!!     yAxis2 => plt2%get_y_axis()
!!
!!     xAxis3 => plt3%get_x_axis()
!!     yAxis3 => plt3%get_y_axis()
!!
!!     call plt1%push(d1)
!!     call plt1%push(d1_filt)
!!
!!     call xAxis1%set_title("Time [sec]")
!!     call yAxis1%set_title("Signal")
!!
!!     call plt2%push(d4)
!!     call plt2%push(d4_filt)
!!
!!     call xAxis2%set_title("Frequency [Hz]")
!!     call yAxis2%set_title("Amplitude")
!!     call yAxis2%set_is_log_scaled(.true.)
!!
!!     call plt3%push(d2)
!!     call plt3%push(d3)
!!
!!     call xAxis3%set_title("Time [sec]")
!!     call yAxis3%set_title("Derivative")
!!
!!     call mplt%set(1, 1, plt1)
!!     call mplt%set(2, 1, plt2)
!!     call mplt%set(3, 1, plt3)
!!     call mplt%draw()
!! contains
!!     function build_signal(npts, fs) result(x)
!!         integer(int32), intent(in) :: npts
!!         real(real64), intent(in) :: fs
!!         real(real64), dimension(npts, 3) :: x
!!
!!         real(real64) :: dt, nyquist, freq(3), amp(3)
!!         integer(int32) :: i
!!
!!         dt = 1.0d0 / fs
!!         call random_number(freq)
!!         call random_number(amp)
!!         nyquist = 0.5d0 * fs
!!         freq = freq * nyquist
!!         do i = 1, npts
!!             x(i,1) = dt * (i - 1.0d0)
!!             x(i,2) = amp(1) * sin(freq(1) * x(i,1)) + &
!!                 amp(2) * sin(freq(2) * x(i,1)) + &
!!                 amp(3) * sin(freq(3) * x(i,1))
!!             x(i,3) = amp(1) * freq(1) * cos(freq(1) * x(i,1)) + &
!!                 amp(2) * freq(2) * cos(freq(2) * x(i,1)) + &
!!                 amp(3) * freq(3) * cos(freq(3) * x(i,1))
!!         end do
!!     end function
!! end program
!! @endcode
!! @image html signal_example_1.png
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
    public :: frequencies
    public :: fourier_diff
    public :: fourier_diff2
    public :: finite_diff

! ******************************************************************************
! GENERAL INTERFACES
! ------------------------------------------------------------------------------
!> @brief Computes the magnitude of the discrete harmonics of a signal.
interface signal_magnitude
    module procedure :: signal_magnitude_win
    module procedure :: signal_magnitude_no_win
end interface

!> @brief Computes the derivative of a data set by utilizing its Fourier
!! transform.
interface fourier_diff
    module procedure :: fourier_diff_a
    module procedure :: fourier_diff_b
    module procedure :: fourier_diff_c
end interface

!> @brief Computes the second derivative of a data set by utilizing its 
!! Fourier transform.
interface fourier_diff2
    module procedure :: fourier_diff2_a
    module procedure :: fourier_diff2_b
    module procedure :: fourier_diff2_c
end interface

!> @brief Computes the derivative of a signal via finite differences.
interface finite_diff
    module procedure :: finite_diff_a
    module procedure :: finite_diff_b
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
    
    !> @brief Computes an array of frequency values corresponding to a discrete
    !! Fourier transform of an array.
    !!
    !! @param[in] rate The rate at which the data was sampled.
    !! @param[in] npts The number of sampled data points.
    !! @return An M-element array containing the frequency values.  If @p npts
    !!  is even, M = @p npts / 2 + 1; else, M = (@p npts + 1) / 2.
    module function frequencies(rate, npts) result(f)
        ! Arguments
        real(real64), intent(in) :: rate
        integer(int32), intent(in) :: npts
        real(real64), allocatable, dimension(:) :: f
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

    !> @brief Computes the magnitude of the discrete harmonics of a signal.
    !!
    !! @param[in] x The N-element array containing the signal data.
    !! @return The magnitude of the each harmonic component of @p x. 
    module function signal_magnitude_no_win(x) result(y)
        real(real64), intent(in), dimension(:) :: x
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
    pure module function bartlett_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function
    
    !> @brief Defines a Welch window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure module function welch_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function
    
    !> @brief Defines a Hann window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure module function hann_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function

    !> @brief Defines a Hamming window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure module function hamming_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
    end function

    !> @brief Defines a Blackman window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    pure module function blackman_window(bin, winsize) result(x)
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

! ******************************************************************************
! SIGNALS_DIFF.F90
! ------------------------------------------------------------------------------
interface
    !> @brief Computes the derivative of a data set by utilizing its Fourier
    !! transform.  Notice, this routine does expect the signal is periodic at
    !! it's boundaries.  For nonperiodic signals this routine will give 
    !! erroneous results.  Additionally, it is assumed that the data is
    !! sampled over equal intervals.
    !!
    !! @param[in] a The lower limit of the sample region.
    !! @param[in] b The upper limit of the sample region.
    !! @param[in] y An N-element array containing the signal.
    !! @return An N-element array containing the derivative of @p y with respect
    !!  to the independent variable bounded by @p a and @p b.
    !!
    !! @par Example
    !! The following example illustrates how to compute the first and second
    !! derivatives of a periodic signal.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     use signals
    !!     use constants
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 2048
    !!     real(real64), parameter :: a = 0.0d0
    !!     real(real64), parameter :: b = 4.0d0 * pi
    !!
    !!     ! Local Variables
    !!     integer(int32) :: i
    !!     real(real64) :: x(n), y(n), dydx(n), ans(n), d2ydx2(n), dx
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2, d3
    !!     class(legend), pointer :: lgnd
    !!
    !!     ! Initialization
    !!     dx = (b - a) / n
    !!     do i = 1, n
    !!         x(i) = a + dx * (i - 1.0d0)
    !!     end do
    !!     y = sin(x)
    !!     ans = cos(x)
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!
    !!     lgnd => plt%get_legend()
    !!     call lgnd%set_is_visible(.true.)
    !!     call lgnd%set_draw_inside_axes(.false.)
    !!     call lgnd%set_horizontal_position(LEGEND_CENTER)
    !!     call lgnd%set_vertical_position(LEGEND_BOTTOM)
    !!     call lgnd%set_draw_border(.false.)
    !!
    !!     ! Compute the derivatives
    !!     dydx = fourier_diff(a, b, y)
    !!     d2ydx2 = fourier_diff2(a, b, y)
    !!
    !!     ! Plot the results
    !!     call d1%set_name("Fourier")
    !!     call d1%set_line_width(2.0)
    !!     call d1%define_data(x, dydx)
    !!
    !!     call d2%set_name("Analytic")
    !!     call d2%set_line_style(LINE_DASHED)
    !!     call d2%set_line_width(3.0)
    !!     call d2%set_line_color(CLR_RED)
    !!     call d2%define_data(x, ans)
    !!
    !!     call d3%set_name("2nd Derivative")
    !!     call d3%set_line_style(LINE_DASH_DOTTED)
    !!     call d3%set_line_width(2.0)
    !!     call d3%set_line_color(CLR_GREEN)
    !!     call d3%define_data(x, d2ydx2)
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%push(d3)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @image html fourier_diff_example.png
    module function fourier_diff_a(a, b, y) result(dydx)
        real(real64), intent(in) :: a, b
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: dydx
    end function

    !> @brief Computes the derivative of a data set by utilizing its Fourier
    !! transform.  Notice, this routine does expect the signal is periodic at
    !! it's boundaries.  For nonperiodic signals this routine will give 
    !! erroneous results.  Additionally, it is assumed that the data is
    !! sampled over equal intervals.
    !!
    !! @param[in] x An N-element array containing the values of the independent
    !!  variable at which the signal was sampled.  It is assumed that the signal
    !!  is sampled at equal increments.
    !! @param[in] y An N-element array containing the signal.
    !! @return An N-element array containing the derivative of @p y with respect
    !!  to the independent variable bounded by @p a and @p b.
    !!
    !! @par Example
    !! The following example illustrates how to compute the first and second
    !! derivatives of a periodic signal.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     use signals
    !!     use constants
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 2048
    !!     real(real64), parameter :: a = 0.0d0
    !!     real(real64), parameter :: b = 4.0d0 * pi
    !!
    !!     ! Local Variables
    !!     integer(int32) :: i
    !!     real(real64) :: x(n), y(n), dydx(n), ans(n), d2ydx2(n), dx
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2, d3
    !!     class(legend), pointer :: lgnd
    !!
    !!     ! Initialization
    !!     dx = (b - a) / n
    !!     do i = 1, n
    !!         x(i) = a + dx * (i - 1.0d0)
    !!     end do
    !!     y = sin(x)
    !!     ans = cos(x)
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!
    !!     lgnd => plt%get_legend()
    !!     call lgnd%set_is_visible(.true.)
    !!     call lgnd%set_draw_inside_axes(.false.)
    !!     call lgnd%set_horizontal_position(LEGEND_CENTER)
    !!     call lgnd%set_vertical_position(LEGEND_BOTTOM)
    !!     call lgnd%set_draw_border(.false.)
    !!
    !!     ! Compute the derivatives
    !!     dydx = fourier_diff(x, y)
    !!     d2ydx2 = fourier_diff2(x, y)
    !!
    !!     ! Plot the results
    !!     call d1%set_name("Fourier")
    !!     call d1%set_line_width(2.0)
    !!     call d1%define_data(x, dydx)
    !!
    !!     call d2%set_name("Analytic")
    !!     call d2%set_line_style(LINE_DASHED)
    !!     call d2%set_line_width(3.0)
    !!     call d2%set_line_color(CLR_RED)
    !!     call d2%define_data(x, ans)
    !!
    !!     call d3%set_name("2nd Derivative")
    !!     call d3%set_line_style(LINE_DASH_DOTTED)
    !!     call d3%set_line_width(2.0)
    !!     call d3%set_line_color(CLR_GREEN)
    !!     call d3%define_data(x, d2ydx2)
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%push(d3)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @image html fourier_diff_example.png
    module function fourier_diff_b(x, y) result(dydx)
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), dimension(size(y)) :: dydx
    end function

    !> @brief Computes the derivative of a data set by utilizing its Fourier
    !! transform.  Notice, this routine does expect the signal is periodic at
    !! it's boundaries.  For nonperiodic signals this routine will give 
    !! erroneous results.  Additionally, it is assumed that the data is
    !! sampled over equal intervals.
    !!
    !! @param[in] dx The sample interval.
    !! @param[in] y An N-element array containing the signal.
    !! @return An N-element array containing the derivative of @p y with respect
    !!  to the independent variable bounded by @p a and @p b.
    !!
    !! @par Example
    !! The following example illustrates how to compute the first and second
    !! derivatives of a periodic signal.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     use signals
    !!     use constants
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 2048
    !!     real(real64), parameter :: a = 0.0d0
    !!     real(real64), parameter :: b = 4.0d0 * pi
    !!
    !!     ! Local Variables
    !!     integer(int32) :: i
    !!     real(real64) :: x(n), y(n), dydx(n), ans(n), d2ydx2(n), dx
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2, d3
    !!     class(legend), pointer :: lgnd
    !!
    !!     ! Initialization
    !!     dx = (b - a) / n
    !!     do i = 1, n
    !!         x(i) = a + dx * (i - 1.0d0)
    !!     end do
    !!     y = sin(x)
    !!     ans = cos(x)
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!
    !!     lgnd => plt%get_legend()
    !!     call lgnd%set_is_visible(.true.)
    !!     call lgnd%set_draw_inside_axes(.false.)
    !!     call lgnd%set_horizontal_position(LEGEND_CENTER)
    !!     call lgnd%set_vertical_position(LEGEND_BOTTOM)
    !!     call lgnd%set_draw_border(.false.)
    !!
    !!     ! Compute the derivatives
    !!     dydx = fourier_diff(dx, y)
    !!     d2ydx2 = fourier_diff2(dx, y)
    !!
    !!     ! Plot the results
    !!     call d1%set_name("Fourier")
    !!     call d1%set_line_width(2.0)
    !!     call d1%define_data(x, dydx)
    !!
    !!     call d2%set_name("Analytic")
    !!     call d2%set_line_style(LINE_DASHED)
    !!     call d2%set_line_width(3.0)
    !!     call d2%set_line_color(CLR_RED)
    !!     call d2%define_data(x, ans)
    !!
    !!     call d3%set_name("2nd Derivative")
    !!     call d3%set_line_style(LINE_DASH_DOTTED)
    !!     call d3%set_line_width(2.0)
    !!     call d3%set_line_color(CLR_GREEN)
    !!     call d3%define_data(x, d2ydx2)
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%push(d3)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @image html fourier_diff_example.png
    module function fourier_diff_c(dx, y) result(dydx)
        real(real64), intent(in) :: dx
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: dydx
    end function

    !> @brief Computes the second derivative of a data set by utilizing its 
    !! Fourier transform.  Notice, this routine does expect the signal is 
    !! periodic at it's boundaries.  For nonperiodic signals this routine will 
    !! give erroneous results.  Additionally, it is assumed that the data is
    !! sampled over equal intervals.
    !!
    !! @param[in] a The lower limit of the sample region.
    !! @param[in] b The upper limit of the sample region.
    !! @param[in] y An N-element array containing the signal.
    !! @return An N-element array containing the second derivative of @p y with
    !!  respect to the independent variable bounded by @p a and @p b.
    !!
    !! @par Example
    !! The following example illustrates how to compute the first and second
    !! derivatives of a periodic signal.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     use signals
    !!     use constants
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 2048
    !!     real(real64), parameter :: a = 0.0d0
    !!     real(real64), parameter :: b = 4.0d0 * pi
    !!
    !!     ! Local Variables
    !!     integer(int32) :: i
    !!     real(real64) :: x(n), y(n), dydx(n), ans(n), d2ydx2(n), dx
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2, d3
    !!     class(legend), pointer :: lgnd
    !!
    !!     ! Initialization
    !!     dx = (b - a) / n
    !!     do i = 1, n
    !!         x(i) = a + dx * (i - 1.0d0)
    !!     end do
    !!     y = sin(x)
    !!     ans = cos(x)
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!
    !!     lgnd => plt%get_legend()
    !!     call lgnd%set_is_visible(.true.)
    !!     call lgnd%set_draw_inside_axes(.false.)
    !!     call lgnd%set_horizontal_position(LEGEND_CENTER)
    !!     call lgnd%set_vertical_position(LEGEND_BOTTOM)
    !!     call lgnd%set_draw_border(.false.)
    !!
    !!     ! Compute the derivatives
    !!     dydx = fourier_diff(a, b, y)
    !!     d2ydx2 = fourier_diff2(a, b, y)
    !!
    !!     ! Plot the results
    !!     call d1%set_name("Fourier")
    !!     call d1%set_line_width(2.0)
    !!     call d1%define_data(x, dydx)
    !!
    !!     call d2%set_name("Analytic")
    !!     call d2%set_line_style(LINE_DASHED)
    !!     call d2%set_line_width(3.0)
    !!     call d2%set_line_color(CLR_RED)
    !!     call d2%define_data(x, ans)
    !!
    !!     call d3%set_name("2nd Derivative")
    !!     call d3%set_line_style(LINE_DASH_DOTTED)
    !!     call d3%set_line_width(2.0)
    !!     call d3%set_line_color(CLR_GREEN)
    !!     call d3%define_data(x, d2ydx2)
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%push(d3)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @image html fourier_diff_example.png
    module function fourier_diff2_a(a, b, y) result(d2ydx2)
        real(real64), intent(in) :: a, b
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: d2ydx2
    end function

    !> @brief Computes the second derivative of a data set by utilizing its 
    !! Fourier transform.  Notice, this routine does expect the signal is 
    !! periodic at it's boundaries.  For nonperiodic signals this routine will 
    !! give erroneous results.  Additionally, it is assumed that the data is
    !! sampled over equal intervals.
    !!
    !! @param[in] x An N-element array containing the values of the independent
    !!  variable at which the signal was sampled.  It is assumed that the signal
    !!  is sampled at equal increments.
    !! @param[in] y An N-element array containing the signal.
    !! @return An N-element array containing the second derivative of @p y with
    !!  respect to the independent variable bounded by @p a and @p b.
    !!
    !! @par Example
    !! The following example illustrates how to compute the first and second
    !! derivatives of a periodic signal.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     use signals
    !!     use constants
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 2048
    !!     real(real64), parameter :: a = 0.0d0
    !!     real(real64), parameter :: b = 4.0d0 * pi
    !!
    !!     ! Local Variables
    !!     integer(int32) :: i
    !!     real(real64) :: x(n), y(n), dydx(n), ans(n), d2ydx2(n), dx
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2, d3
    !!     class(legend), pointer :: lgnd
    !!
    !!     ! Initialization
    !!     dx = (b - a) / n
    !!     do i = 1, n
    !!         x(i) = a + dx * (i - 1.0d0)
    !!     end do
    !!     y = sin(x)
    !!     ans = cos(x)
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!
    !!     lgnd => plt%get_legend()
    !!     call lgnd%set_is_visible(.true.)
    !!     call lgnd%set_draw_inside_axes(.false.)
    !!     call lgnd%set_horizontal_position(LEGEND_CENTER)
    !!     call lgnd%set_vertical_position(LEGEND_BOTTOM)
    !!     call lgnd%set_draw_border(.false.)
    !!
    !!     ! Compute the derivatives
    !!     dydx = fourier_diff(x, y)
    !!     d2ydx2 = fourier_diff2(x, y)
    !!
    !!     ! Plot the results
    !!     call d1%set_name("Fourier")
    !!     call d1%set_line_width(2.0)
    !!     call d1%define_data(x, dydx)
    !!
    !!     call d2%set_name("Analytic")
    !!     call d2%set_line_style(LINE_DASHED)
    !!     call d2%set_line_width(3.0)
    !!     call d2%set_line_color(CLR_RED)
    !!     call d2%define_data(x, ans)
    !!
    !!     call d3%set_name("2nd Derivative")
    !!     call d3%set_line_style(LINE_DASH_DOTTED)
    !!     call d3%set_line_width(2.0)
    !!     call d3%set_line_color(CLR_GREEN)
    !!     call d3%define_data(x, d2ydx2)
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%push(d3)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @image html fourier_diff_example.png
    module function fourier_diff2_b(x, y) result(d2ydx2)
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), dimension(size(y)) :: d2ydx2
    end function

    !> @brief Computes the second derivative of a data set by utilizing its 
    !! Fourier transform.  Notice, this routine does expect the signal is 
    !! periodic at it's boundaries.  For nonperiodic signals this routine will 
    !! give erroneous results.  Additionally, it is assumed that the data is
    !! sampled over equal intervals.
    !!
    !! @param[in] dx The sample interval.
    !! @param[in] y An N-element array containing the signal.
    !! @return An N-element array containing the second derivative of @p y with
    !!  respect to the independent variable bounded by @p a and @p b.
    !!
    !! @par Example
    !! The following example illustrates how to compute the first and second
    !! derivatives of a periodic signal.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     use signals
    !!     use constants
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 2048
    !!     real(real64), parameter :: a = 0.0d0
    !!     real(real64), parameter :: b = 4.0d0 * pi
    !!
    !!     ! Local Variables
    !!     integer(int32) :: i
    !!     real(real64) :: x(n), y(n), dydx(n), ans(n), d2ydx2(n), dx
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2, d3
    !!     class(legend), pointer :: lgnd
    !!
    !!     ! Initialization
    !!     dx = (b - a) / n
    !!     do i = 1, n
    !!         x(i) = a + dx * (i - 1.0d0)
    !!     end do
    !!     y = sin(x)
    !!     ans = cos(x)
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!
    !!     lgnd => plt%get_legend()
    !!     call lgnd%set_is_visible(.true.)
    !!     call lgnd%set_draw_inside_axes(.false.)
    !!     call lgnd%set_horizontal_position(LEGEND_CENTER)
    !!     call lgnd%set_vertical_position(LEGEND_BOTTOM)
    !!     call lgnd%set_draw_border(.false.)
    !!
    !!     ! Compute the derivatives
    !!     dydx = fourier_diff(dx, y)
    !!     d2ydx2 = fourier_diff2(dx, y)
    !!
    !!     ! Plot the results
    !!     call d1%set_name("Fourier")
    !!     call d1%set_line_width(2.0)
    !!     call d1%define_data(x, dydx)
    !!
    !!     call d2%set_name("Analytic")
    !!     call d2%set_line_style(LINE_DASHED)
    !!     call d2%set_line_width(3.0)
    !!     call d2%set_line_color(CLR_RED)
    !!     call d2%define_data(x, ans)
    !!
    !!     call d3%set_name("2nd Derivative")
    !!     call d3%set_line_style(LINE_DASH_DOTTED)
    !!     call d3%set_line_width(2.0)
    !!     call d3%set_line_color(CLR_GREEN)
    !!     call d3%define_data(x, d2ydx2)
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%push(d3)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @image html fourier_diff_example.png
    module function fourier_diff2_c(dx, y) result(d2ydx2)
        real(real64), intent(in) :: dx
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: d2ydx2
    end function


    !> @brief Computes the derivative of a signal via finite differences.  A
    !! forward difference is used to step into the problem, a central difference
    !! is used through the middle section, and a backward difference is used to
    !! step out of the problem.  Notice, this technique is highly susceptable
    !! to noise in the signal.
    !!
    !! @param[in] x An N-element array containing the values of the independent
    !!  variable at which the signal was sampled.
    !! @param[in] y An N-element array containing the signal.
    !! @return An N-element array containing the derivative.
    module function finite_diff_a(x, y) result(dydx)
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), dimension(size(y)) :: dydx
    end function

    !> @brief Computes the derivative of a signal via finite differences.  A
    !! forward difference is used to step into the problem, a central difference
    !! is used through the middle section, and a backward difference is used to
    !! step out of the problem.  Notice, this technique is highly susceptable
    !! to noise in the signal.
    !!
    !! @param[in] dx The sample interval.
    !! @param[in] y An N-element array containing the signal.
    !! @return An N-element array containing the derivative.
    module function finite_diff_b(dx, y) result(dydx)
        real(real64), intent(in) :: dx
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: dydx
    end function
end interface

end module
