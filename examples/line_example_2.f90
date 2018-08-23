! line_example_2.f90

program example
    use iso_fortran_env
    use geometry
    use fplot_core
    implicit none

    ! Local Variables
    real(real64), dimension(3, 4) :: pts
    type(line) :: ln1, ln2, sln
    type(plot_3d) :: plt
    type(plot_data_3d) :: pd1, pd2, pd3
    class(plot_axis), pointer :: xAxis, yAxis, zAxis

    ! Create the lines
    call random_number(pts)
    ln1 = line_from_2_points(pts(:,1), pts(:,2))
    ln2 = line_from_2_points(pts(:,3), pts(:,4))

    ! Compute the shortest line segment between ln1 and ln2
    sln = shortest_line_to_line(ln1, ln2)

    ! Set up the plot
    call plt%initialize()
    call plt%set_font_size(14)
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()
    zAxis => plt%get_z_axis()

    call xAxis%set_title("X")
    call yAxis%set_title("Y")
    call zAxis%set_title("Z")

    ! Draw the lines
    call draw_line(ln1, pd1, -2.0d0, 2.0d0)
    call draw_line(ln2, pd2, -2.0d0, 2.0d0)
    call draw_line(sln, pd3, 0.0d0, 1.0d0)

    call pd1%set_line_width(2.0)
    
    call pd2%set_line_width(2.0)
    call pd2%set_line_style(LINE_DASHED)

    call pd3%set_line_width(2.0)
    call pd3%set_line_style(LINE_DASH_DOTTED)
    call pd3%set_line_color(CLR_BLACK)

    call plt%push(pd1)
    call plt%push(pd2)
    call plt%push(pd3)
    call plt%draw()

contains
    ! Populates a plot_data_3d type with the information necessary to draw
    ! a line.
    subroutine draw_line(ln, pd, t0, t1)
        ! Arguments
        type(line), intent(in) :: ln
        real(real64), intent(in) :: t0, t1
        type(plot_data_3d), intent(inout) :: pd

        ! Local Variables
        real(real64), dimension(2) :: x, y, z
        real(real64), dimension(3) :: p0, p1

        ! Compute the locations of the line at t = t0 and t = t1
        p0 = ln%evaluate(t0)
        p1 = ln%evaluate(t1)

        ! Construct the data arrays
        x = [p0(1), p1(1)]
        y = [p0(2), p1(2)]
        z = [p0(3), p1(3)]

        ! Fill the plot_data_3d object
        call pd%define_data(x, y, z)
    end subroutine
end program