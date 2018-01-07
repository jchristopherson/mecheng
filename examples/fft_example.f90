! fft_example.f90

program example
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use signals
    use constants
    use fplot_core ! Requries the FPLOT library (https://github.com/jchristopherson/fplot)
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 2000
    real(real64), parameter :: freq = 512.0d0 ! Signal sampled at 512 Hz

    ! Local Variables
    integer(int32) :: i
    real(real64) :: dt, t(npts), s(npts)
    complex(real64), allocatable, dimension(:) :: xfrm
    real(real64), allocatable, dimension(:) :: x, f, p
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis, y2Axis

    ! Build the signal
    t(1) = 0.0d0
    dt = 1.0d0 / freq
    do i = 1, npts
        s(i) = 2.0d0 * sin(2.0d0 * pi * 50.0d0 * t(i)) + &
            0.75 * sin(2.0d0 * pi * 120.0d0 * t(i)) + &
            1.25 * sin(2.0d0 * pi * 200.0d0 * t(i))
        if (i < npts) t(i+1) = t(i) + dt
    end do
    
    ! Plot the signal
    call plt%initialize()
    call plt%set_title("FFT Example - Signal")
    call plt%set_font_size(14)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("Time [sec]")
    
    yAxis => plt%get_y_axis()
    call yAxis%set_title("s(t)")

    call d1%define_data(t, s)
    call d1%set_name("s(t)")
    
    call plt%push(d1)
    call plt%draw()

    ! Now that the signal has been defined, compute it's FFT, and then display
    ! its magnitude and phase as a function of frequency
    xfrm = rfft(s)  ! Notice, s is modified by this function

    ! We can simply take the absolute value of the resulting complex-valued 
    ! array to obtain the magnitude spectrum of the signal
    x = abs(xfrm)

    ! The phase can be computed by computing the arctangent, and convert from
    ! radians to degrees
    p = atan2(aimag(xfrm), real(xfrm, real64)) * (180.0d0 / pi)

    ! The corresponding frequency values can be determined as follows
    allocate(f(size(x)))
    do i = 1, size(x)
        f(i) = freq * (i - 1.0d0) / npts
    end do
    
    ! Plot the results.  Amplitude on the primary axis, and phase on the 
    ! secondary axis
    call plt%clear_all()
    call plt%set_use_y2_axis(.true.)

    call plt%set_title("FFT Example")
    call xAxis%set_title("Frequency [Hz]")
    call yAxis%set_title("Signal Magnitude")

    y2Axis => plt%get_y2_axis()
    call y2Axis%set_title("Phase [deg]")

    call d1%define_data(f, x)
    call d1%set_name("Magnitude")
    call d1%set_use_auto_color(.false.)
    call d1%set_line_color(CLR_BLUE)
    call d1%set_line_width(2.0)
    
    call d2%define_data(f, p)
    call d2%set_name("Phase (Y2)")
    call d2%set_use_auto_color(.false.)
    call d2%set_line_color(CLR_GREEN)
    call d2%set_line_width(2.0)
    call d2%set_line_style(LINE_DASHED)
    call d2%set_draw_against_y2(.true.)

    call plt%push(d1)
    call plt%push(d2)
    call plt%draw()
end program