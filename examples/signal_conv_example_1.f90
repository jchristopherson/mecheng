! signal_conv_example_1.f90

program example
    use iso_fortran_env
    use signals
    use fplot_core
    use constants
    implicit none

    ! Parameters
    integer(int32), parameter :: ns = 1024
    real(real64), parameter :: sampleRate = 1024.0d0
    real(real64), parameter :: max_time = 1.0d0

    ! Local Variables
    real(real64) :: t(ns), xs(ns), xr(ns), c(ns), xsc(ns), numer(ns+1), xf(ns), ir(ns)
    type(iir_filter) :: filter
    type(multiplot) :: plt
    type(plot_2d) :: plt1, plt2
    type(plot_data_2d) :: d1, d2, d3, d4, d5
    class(plot_axis), pointer :: x1, y1, x2, y2
    class(legend), pointer :: lgnd1, lgnd2
    class(terminal), pointer :: term

    ! Create the signal
    t = linspace(0.0d0, max_time, ns)
    xs = 0.5d0 * sin(2.0d0 * pi * 10.0d0 * t) + 1.0d-1 * sin(2.0d0 * pi * 256.0d0 * t)
    xr = exp(-2.5d0 * t)

    ! Create a convolution of the signals
    c = conv(xs, xr)

    ! Now, deconvolve the signals
    xsc = deconv(c, xr)

    ! Utilize a digital filter to perform the deconvolution
    numer(ns+1) = 0.0d0
    numer(1:ns) = c
    ir(1) = 1.0d0
    ir(2:ns) = 0.0d0
    call filter%initialize(xr, numer)
    xf = ns * filter%apply(ir)

    ! Create plots of the signal and its deconvolution
    call plt%initialize(2, 1)
    call plt1%initialize()
    call plt2%initialize()
    call plt%set(1, 1, plt1)
    call plt%set(2, 1, plt2)
    call plt%set_font_name("Arial")
    call plt%set_font_size(11)
    term => plt%get_terminal()
    call term%set_window_width(800)
    x1 => plt1%get_x_axis()
    y1 => plt1%get_y_axis()
    x2 => plt2%get_x_axis()
    y2 => plt2%get_y_axis()
    lgnd1 => plt1%get_legend()
    lgnd2 => plt2%get_legend()

    call x1%set_title("t")
    call x2%set_title("t")
    call y1%set_title("f(t)")
    call y2%set_title("f(t)")

    call y2%set_autoscale(.false.)
    call y2%set_limits(-1.0d0, 1.0d0)

    call lgnd1%set_is_visible(.true.)
    call lgnd1%set_draw_border(.false.)
    call lgnd1%set_draw_inside_axes(.false.)
    call lgnd2%set_is_visible(.true.)
    call lgnd2%set_draw_border(.false.)
    call lgnd2%set_draw_inside_axes(.false.)

    call d1%set_name("Signal 1")
    call d1%define_data(t, xs)

    call d2%set_name("Signal 2")
    call d2%define_data(t, xr)

    call d3%set_name("Convolved")
    call d3%define_data(t, c)

    call d4%set_name("Reconstructed")
    call d4%define_data(t, xsc)

    call d5%set_name("Filtered")
    call d5%define_data(t, xf)

    call plt1%push(d1)
    call plt1%push(d2)
    call plt1%push(d3)

    call plt2%push(d1)
    call plt2%push(d4)
    call plt2%push(d5)

    call plt%draw()
end program