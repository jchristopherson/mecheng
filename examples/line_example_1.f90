! line_example_1.f90

program example
    use iso_fortran_env
    use geometry
    use fplot_core
    implicit none

    ! Local Variables
    type(line) :: l1, ls
    real(real64), dimension(3) :: pt1, pt2, pt3
    type(plot_3d) :: plt
    class(plot_axis), pointer :: xAxis, yAxis, zAxis
    type(plot_data_3d) :: d1, d2, d3

    ! Initialization
    call random_number(pt1)
    call random_number(pt2)
    call random_number(pt3)

    call plt%initialize()
    call plt%set_font_size(14)
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()
    zAxis => plt%get_z_axis()

    call xAxis%set_title("X")
    call yAxis%set_title("Y")
    call zAxis%set_title("Z")

    ! Construct the line from PT1 and PT2
    l1 = line_from_2_points(pt1, pt2)

    ! Determine the shortest line between the line L1 and PT3
    ls = shortest_line(l1, pt3)

    ! Plot the results
    call d1%define_data( &
        [l1%a(1), l1%b(1)], &
        [l1%a(2), l1%b(2)], &
        [l1%a(3), l1%b(3)])
    call d1%set_draw_markers(.true.)
    call d1%set_line_width(2.0)
    call d1%set_marker_style(MARKER_X)

    call d2%define_data([pt3(1)], [pt3(2)], [pt3(3)])
    call d2%set_draw_line(.false.)
    call d2%set_draw_markers(.true.)
    call d2%set_line_width(2.0)
    call d2%set_marker_style(MARKER_EMPTY_CIRCLE)

    call d3%define_data( &
        [ls%a(1), ls%b(1)], &
        [ls%a(2), ls%b(2)], &
        [ls%a(3), ls%b(3)])
    call d3%set_line_style(LINE_DASHED)
    call d3%set_line_width(2.0)

    call plt%push(d1)
    call plt%push(d2)
    call plt%push(d3)
    call plt%draw()
end program
