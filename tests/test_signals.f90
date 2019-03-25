! test_signals.f90

program main
    use iso_fortran_env
    use signals
    use fplot_core
    use constants
    implicit none

    ! Parameters
    integer(int32), parameter :: ntaps = 10
    integer(int32), parameter :: npts = 1000
    real(real64), parameter :: tmax = 2.0d0
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2, d3
    type(legend), pointer :: leg
    type(fir_filter) :: filter
    type(iir_filter) :: ifilter

    ! Local Variables
    integer(int32) :: i
    real(real64) :: dt, t(npts), x(npts), xrand(npts), y(npts), coeffs(ntaps), &
        a(ntaps), b(ntaps + 1), yi(npts)

    ! Initialization
    call random_number(xrand)
    dt = tmax / (npts - 1.0d0)
    t(1) = 0.0d0
    x(1) = 0.0d0
    do i = 2, npts
        x(i) = sin(2.0d0 * pi * 50.0d0 * t(i)) + 0.5d0 * sin(2.0d0 * pi * 20.0d0 * t(i))
        t(i) = t(i-1) + dt
    end do
    x = x + (xrand - 0.5d0) / 4.0d0

    ! Set up an IIR filter using the FIR coefficients - the denominator becomes all zero
    b = 1.0d0 / (ntaps + 1.0d0)
    a = 0.0d0
    call ifilter%initialize(a, b)

    ! Filter the signal - create a basic averaging filter
    coeffs = 1.0d0 / (ntaps + 1.0d0)
    call filter%initialize(coeffs)
    do i = 1, npts
        y(i) = filter%apply(x(i))
        yi(i) = ifilter%apply(x(i))
    end do
    
    
    ! Plot the signal
    call d1%set_name("Original")
    call d1%define_data(t, x)

    call d2%set_name("FIR")
    call d2%define_data(t, y)
    call d2%set_line_width(2.0)

    call d3%set_name("IIR")
    call d3%define_data(t, yi)
    
    call plt%initialize()
    call plt%set_font_size(14)
    leg => plt%get_legend()
    call leg%set_is_visible(.true.)
    call leg%set_draw_inside_axes(.false.)
    call leg%set_draw_border(.false.)
    call plt%push(d1)
    call plt%push(d2)
    call plt%push(d3)
    call plt%draw()
end program
