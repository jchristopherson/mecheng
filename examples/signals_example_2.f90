! signals_example_2.f90

program example
    use iso_fortran_env
    use signals
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: npts = 4096
    real(real64), parameter :: fs = 1024.0d0
    integer(int32), parameter :: winSize = 1024

    real(real64), allocatable, dimension(:,:) :: x
    real(real64), allocatable, dimension(:) :: freq, mag, dx, filtMag
    procedure(window_function), pointer :: win
    type(multiplot) :: mplt
    type(plot_2d) :: plt1, plt2, plt3
    type(plot_data_2d) :: d1, d2, d3, d1_filt, d4, d4_filt
    class(legend), pointer :: lgnd, lgnd3
    class(plot_axis), pointer :: xAxis1, xAxis2, xAxis3, yAxis1, yAxis2, yAxis3
    class(terminal), pointer :: term

    ! Build the signal
    x = build_signal(npts, fs)

    call d1%set_name("Original Signal")
    call d1%define_data(x(:,1), x(:,2))

    call d2%set_name("Derivative")
    call d2%define_data(x(:,1), x(:,3))

    ! Compute the derivative
    dx = finite_diff(x(:,1), x(:,2))
    
    call d3%set_name("Computed Derivative")
    call d3%define_data(x(:,1), dx)

    ! Investigate the magnitude of the signal - use a Hamming window
    win => hamming_window
    mag = signal_magnitude(x(:,2), win, winSize)
    freq = frequencies(fs, winSize)

    call d4%set_name("Original Signal")
    call d4%define_data(freq, mag)

    ! Filter the data set
    call low_pass_filter(x(:,2), fs, 3.0d1)

    call d1_filt%set_name("Filtered")
    call d1_filt%define_data(x(:,1), x(:,2))

    ! Look at the magnitude after filtering
    filtMag = signal_magnitude(x(:,2), win, winSize)

    call d4_filt%set_name("Filtered")
    call d4_filt%define_data(freq, filtMag)

    ! Create the plots
    call mplt%initialize(3, 1)
    call plt1%initialize()
    call plt2%initialize()
    call plt3%initialize()

    term => mplt%get_terminal()
    call term%set_window_width(1000)
    call term%set_window_height(800)

    lgnd => plt1%get_legend()
    call lgnd%set_is_visible(.true.)

    lgnd3 => plt3%get_legend()
    call lgnd3%set_is_visible(.true.)

    xAxis1 => plt1%get_x_axis()
    yAxis1 => plt1%get_y_axis()

    xAxis2 => plt2%get_x_axis()
    yAxis2 => plt2%get_y_axis()

    xAxis3 => plt3%get_x_axis()
    yAxis3 => plt3%get_y_axis()

    call plt1%push(d1)
    call plt1%push(d1_filt)

    call xAxis1%set_title("Time [sec]")
    call yAxis1%set_title("Signal")

    call plt2%push(d4)
    call plt2%push(d4_filt)

    call xAxis2%set_title("Frequency [Hz]")
    call yAxis2%set_title("Amplitude")
    call yAxis2%set_is_log_scaled(.true.)

    call plt3%push(d2)
    call plt3%push(d3)

    call xAxis3%set_title("Time [sec]")
    call yAxis3%set_title("Derivative")

    call mplt%set(1, 1, plt1)
    call mplt%set(2, 1, plt2)
    call mplt%set(3, 1, plt3)
    call mplt%draw()
contains
    function build_signal(npts, fs) result(x)
        integer(int32), intent(in) :: npts
        real(real64), intent(in) :: fs
        real(real64), dimension(npts, 3) :: x

        real(real64) :: dt, nyquist, freq(3), amp(3)
        integer(int32) :: i

        dt = 1.0d0 / fs
        call random_number(freq)
        call random_number(amp)
        nyquist = 0.5d0 * fs
        freq = freq * nyquist
        do i = 1, npts
            x(i,1) = dt * (i - 1.0d0)
            x(i,2) = amp(1) * sin(freq(1) * x(i,1)) + &
                amp(2) * sin(freq(2) * x(i,1)) + &
                amp(3) * sin(freq(3) * x(i,1))
            x(i,3) = amp(1) * freq(1) * cos(freq(1) * x(i,1)) + &
                amp(2) * freq(2) * cos(freq(2) * x(i,1)) + &
                amp(3) * freq(3) * cos(freq(3) * x(i,1))
        end do
    end function
end program