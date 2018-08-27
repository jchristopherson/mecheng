! integrate_example.f90

program example
    use iso_fortran_env
    use signals
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000
    real(real64), parameter :: dx = 1.0d-2

    ! Local Variables
    integer(int32) :: i
    real(real64) :: x(n), y(n), f(n), ans(n)
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(legend), pointer :: lgnd

    ! Build the signal
    do i = 1, n
        x(i) = dx * (i - 1.0d0)
        y(i) = sin(x(i))
        ans(i) = -cos(x(i))
    end do

    ! Compute the integral
    f = integrate(dx, y)

    ! Remove the DC offset
    call remove_mean(f)

    ! Plot the results
    call plt%initialize()
    call plt%set_font_size(14)
    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_horizontal_position(LEGEND_CENTER)
    call lgnd%set_vertical_position(LEGEND_BOTTOM)
    call lgnd%set_draw_inside_axes(.false.)
    call lgnd%set_draw_border(.false.)

    call d1%set_name("Numerical")
    call d1%set_line_width(2.0)
    call d1%define_data(x, f)

    call d2%set_name("Analytical")
    call d2%set_line_width(3.0)
    call d2%set_line_color(CLR_RED)
    call d2%set_line_style(LINE_DASHED)
    call d2%define_data(x, ans)

    call plt%push(d1)
    call plt%push(d2)
    call plt%draw()
end program