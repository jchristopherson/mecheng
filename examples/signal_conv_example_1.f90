! signal_conv_example_1.f90

program example
    use iso_fortran_env
    use signals
    use fplot_core
    use constants
    implicit none

    ! Parameters
    integer(int32), parameter :: ns = 2000
    real(real64), parameter :: sampleRate = 1024.0d0
    real(real64), parameter :: max_time = 1.0d0

    ! Local Variables
    real(real64) :: t(ns), xs(ns), xr(ns), r(ns)
    real(real64), allocatable, dimension(:) :: c, xsc
    type(multiplot) :: plt
    type(plot_2d) :: plt1, plt2, plt3, plt4
    type(plot_data_2d) :: d1, d2, d3, d4

    ! Create the signal
    t = linspace(0.0d0, max_time, ns)
    call random_number(r)
    xs = 0.0d0
    xs(900:1100) = 1.0d0
    r = 1.0d-2 * r
    xs = xs + r
    xr = exp(-2.5d1 * t)

    ! Create a convolution of the signals
    c = conv(xs, xr)

    ! Now, deconvolve the signals
    xsc = deconv(c, xr)

    ! Create plots of the signal and its deconvolution
    call plt%initialize(2, 2)
    call plt1%initialize()
    call plt2%initialize()
    call plt3%initialize()
    call plt4%initialize()
    call plt%set(1, 1, plt1)
    call plt%set(1, 2, plt2)
    call plt%set(2, 1, plt3)
    call plt%set(2, 2, plt4)

    call d1%define_data(t, xs)
    call plt1%push(d1)
    call plt1%set_title("Signal 1")

    call d2%define_data(t, xr)
    call plt2%push(d2)
    call plt2%set_title("Signal 2")

    call d3%define_data(t, c(1:ns))
    call plt3%push(d3)
    call plt3%set_title("Convolved")

    call d4%define_data(t, xsc)
    call plt4%push(d4)
    call plt4%set_title("Recovered")

    call plt%draw()
end program