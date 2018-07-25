! diff_example.f90

program example
    use iso_fortran_env
    use fplot_core
    use signals
    use constants
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 2048
    real(real64), parameter :: a = 0.0d0
    real(real64), parameter :: b = 4.0d0 * pi
    
    ! Local Variables
    integer(int32) :: i
    real(real64) :: x(n), y(n), dydx(n), ans(n), d2ydx2(n), dx, fdydx(n)
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2, d3, d4
    class(legend), pointer :: lgnd

    ! Initialization
    dx = (b - a) / n
    do i = 1, n
        x(i) = a + dx * (i - 1.0d0)
    end do
    y = sin(x)
    ans = cos(x)
    call plt%initialize()
    call plt%set_font_size(14)

    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_draw_inside_axes(.false.)
    call lgnd%set_horizontal_position(LEGEND_CENTER)
    call lgnd%set_vertical_position(LEGEND_BOTTOM)
    call lgnd%set_draw_border(.false.)

    ! Compute the derivatives
    dydx = fourier_diff(dx, y)
    d2ydx2 = fourier_diff2(dx, y)
    fdydx = finite_diff(dx, y)

    ! Plot the results
    call d1%set_name("Fourier")
    call d1%set_line_width(2.0)
    call d1%define_data(x, dydx)

    call d2%set_name("Analytic")
    call d2%set_line_style(LINE_DASHED)
    call d2%set_line_width(3.0)
    call d2%set_line_color(CLR_RED)
    call d2%define_data(x, ans)

    call d3%set_name("2nd Derivative")
    call d3%set_line_style(LINE_DASH_DOTTED)
    call d3%set_line_width(2.0)
    call d3%set_line_color(CLR_GREEN)
    call d3%define_data(x, d2ydx2)
    
    call d4%set_name("Finite Difference")
    call d4%set_line_style(LINE_DOTTED)
    call d4%set_line_width(3.0)
    call d4%set_line_color(CLR_BLACK)
    call d4%define_data(x, fdydx)

    call plt%push(d1)
    call plt%push(d2)
    call plt%push(d3)
    call plt%push(d4)
    call plt%draw()
end program
