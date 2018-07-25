! signals_example_2.f90

program example
    use iso_fortran_env
    use signals
    use fplot_core
    implicit none

    ! Variables
    integer(int32), parameter :: npts = 4096
    real(real64), parameter :: fs = 1024.0d0
    integer(int32), parameter :: winSize = 256

    real(real64), allocatable, dimension(:,:) :: x
    real(real64), allocatable, dimension(:) :: freq, mag
    procedure(window_function), pointer :: win
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2

    ! Build the signal
    x = build_signal(npts, fs)

    call d1%set_name("Original Signal")
    call d1%define_data(x(:,1), x(:,2))

    call d2%set_name("Derivative")
    call d2%define_data(x(:,1), x(:,3))

    ! Compute the derivative

    ! Investigate the magnitude of the signal - use a Hamming window
    win => hamming_window
    mag = signal_magnitude(x, win, winSize)
    freq = frequencies(fs, winSize)

    ! Filter the data set

    ! Look at the magnitude after filtering

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