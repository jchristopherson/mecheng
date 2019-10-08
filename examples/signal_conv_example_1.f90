! signal_conv_example_1.f90

program example
    use iso_fortran_env
    use signals
    use fplot_core
    use constants
    implicit none

    ! Parameters
    integer(int32), parameter :: ns = 2048
    integer(int32), parameter :: nr = 512
    real(real64), parameter :: sampleRate = 1024.0d0
    real(real64), parameter :: max_time = 1.0d0

    ! Local Variables
    integer(int32) :: i
    real(real64) :: dt, ts(ns), tr(nr), xs(ns), xr(nr), c(ns), xsc(ns)
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    class(legend), pointer :: lgnd

    ! Create the signal
    dt = max_time / (ns - 1.0d0)
    ts(1) = 0.0d0
    xs(1) = 0.0d0
    do i = 2, ns
        ts(i) = ts(i-1) + dt
        xs(i) = 0.5d0 * sin(2.0d0 * pi * 10.0d0 * ts(i)) + &
            1.0d-1 * sin(2.0d0 * pi * 256.0d0 * ts(i))
    end do

    tr(1) = 0.0d0
    xr(1) = 0.0d0
    do i = 2, nr
        tr(i) = tr(i-1) + dt
        xr(i) = sqrt(tr(i))
    end do

    ! Create a convolution of the signals
    c = conv(xs, xr)

    ! Now, deconvolve the signals
    xsc = deconv(c, xr)

    ! Create plots of the signal and its deconvolution
    call plt%initialize()
    call plt%set_font_name("Arial")
    call plt%set_font_size(11)
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()
    lgnd => plt%get_legend()

    call d1%set_name("Original")
    call d1%define_data(ts, xs)

    call d2%set_name("Reconstructed")
    call d2%define_data(ts, xsc)

    call plt%push(d1)
    call plt%push(d2)
    call plt%draw()
end program