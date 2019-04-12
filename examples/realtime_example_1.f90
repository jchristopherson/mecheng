! realtime_example_1.f90

program example
    use iso_fortran_env
    use controls
    use fplot_core
    use constants
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 1024
    real(real64), parameter :: fs = 1024.0d0 ! Signal Sample Rate
    real(real64), parameter :: update = 1024.0d0 ! Real Time Object Update Rate

    ! Local Variables
    integer(int32) :: i
    real(real64) :: dt, t(npts), x(npts), dxdt(npts), ans(npts), omega
    type(realtime_derivative) :: diff
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    class(legend), pointer :: lgnd

    ! Initialization
    x = 0.0d0
    dxdt = 0.0d0
    ans = 0.0d0
    call diff%set_update_rate(update)

    ! Sample the signal, and compute its derivative
    dt = 1.0d0 / fs
    omega = 2.0d0 * pi * 10.0d0
    do i = 2, npts
        t(i) = t(i-1) + dt
        x(i) = cos(omega * t(i)) * sin(t(i))
        ans(i) = cos(omega * t(i)) * cos(t(i)) - &
            omega * sin(omega * t(i)) * sin(t(i))
        dxdt(i) = diff%evaluate(t(i), x(i))
    end do

    ! Plot the results
    call plt%initialize()
    call plt%set_font_size(14)

    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_horizontal_position(LEGEND_LEFT)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("t")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("dx/dt")

    call d1%set_name("Numerical")
    call d1%set_line_width(2.0)
    call d1%define_data(t, dxdt)

    call d2%set_name("Analytical")
    call d2%set_line_width(3.0)
    call d2%set_line_style(LINE_DASHED)
    call d2%define_data(t, ans)

    call plt%push(d1)
    call plt%push(d2)
    call plt%draw()
end program