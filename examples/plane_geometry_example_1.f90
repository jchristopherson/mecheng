! plane_geometry_example_1.f90

program example
    use iso_fortran_env
    use geometry
    use fplot_core
    use constants
    use kinematics
    implicit none

    ! Local Variables
    real(real64), dimension(3) :: i, j, k, axis, refpt, origin, &
        cp1, cp2, cp3, cp4, rp1, rp2, rp3, rp4
    type(quaternion) :: q
    real(real64), dimension(2, 2) :: x, y, z
    real(real64) :: angle
    type(plane) :: pln
    type(surface_plot_data) :: sd1
    type(plot_data_3d) :: pd1, pdi, pdj, pdk, ppt
    type(surface_plot) :: plt
    class(plot_axis), pointer :: xAxis, yAxis, zAxis

    ! Initialization
    call plt%initialize()
    call plt%set_show_colorbar(.false.)
    call plt%set_font_size(14)
    call plt%set_show_hidden(.true.)

    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()
    zAxis => plt%get_z_axis()

    call xAxis%set_title("X")
    call yAxis%set_title("Y")
    call zAxis%set_title("Z")

    axis = [1.0d0, 1.0d0, 0.0d0]
    axis = axis / norm2(axis)
    angle = pi / 4.0d0          ! 45 degree angle
    if (abs(axis(1) - axis(2)) < epsilon(angle)) then
        refpt = [axis(3), 0.0d0, axis(1)]   ! Reference point
    else
        refpt = [axis(2), axis(1), 0.0d0]   ! Reference point
    end if

    ! Construct a plane
    pln = plane_from_angle_axis(axis, angle, refpt)

    ! Display the equation of the plane
    print '(A)', "The equation of the plane:"
    print '(AEN12.3AEN12.3AEN12.3AEN12.3A)', "(", pln%a, ", ", pln%b, ", ", &
        pln%c, ", ", pln%d, ")"

    ! Determine a local coordinate system that exists on the plane, and has
    ! a z-axis parallel to the plane normal
    k = pln%normal_vector()
    origin = -pln%d * k ! This is the location of the origin on the plane

    ! Let the rotation axis be the x axis of the local coordinate system
    i = axis

    ! Construct J = K cross I
    j = cross(k, i)

    ! Define a rotation matrix based upon the above unit vectors
    call q%from_angle_axis(angle, axis)

    ! Define 4 points on the plane in order to provide a visual representation
    ! of the plane
    cp1 = [1.0d0, 1.0d0, 0.0d0]
    cp2 = [1.0d0, -1.0d0, 0.0d0]
    cp3 = [-1.0d0, -1.0d0, 0.0d0]
    cp4 = [-1.0d0, 1.0d0, 0.0d0]

    ! Apply the transformation to move the points onto the plane
    rp1 = q%transform(cp1) + origin
    rp2 = q%transform(cp2) + origin
    rp3 = q%transform(cp3) + origin
    rp4 = q%transform(cp4) + origin

    ! Draw the plane
    x = reshape([rp3(1), rp4(1), rp2(1), rp1(1)], [2, 2])
    y = reshape([rp3(2), rp4(2), rp2(2), rp1(2)], [2, 2])
    z = reshape([rp3(3), rp4(3), rp2(3), rp1(3)], [2, 2])
    call sd1%define_data(x, y, z)
    call sd1%set_use_wireframe(.true.)

    ! Draw the rotation axis
    call pd1%define_data( &
        [origin(1), axis(1) + origin(1)], &
        [origin(2), axis(2) + origin(2)], &
        [origin(3), axis(3) + origin(3)])
    call pd1%set_line_color(CLR_BLACK)
    call pd1%set_line_width(3.0)
    call pd1%set_line_style(LINE_DASHED)

    ! Draw the coordinate frame
    call pdi%define_data( &
        [origin(1), i(1) + origin(1)], &
        [origin(2), i(2) + origin(2)], &
        [origin(3), i(3) + origin(3)])
    call pdi%set_line_color(CLR_BLUE)
    call pdi%set_line_width(2.0)
    
    call pdj%define_data( &
        [origin(1), j(1) + origin(1)], &
        [origin(2), j(2) + origin(2)], &
        [origin(3), j(3) + origin(3)])
    call pdj%set_line_color(CLR_RED)
    call pdj%set_line_width(2.0)

    call pdk%define_data( &
        [origin(1), k(1) + origin(1)], &
        [origin(2), k(2) + origin(2)], &
        [origin(3), k(3) + origin(3)])
    call pdk%set_line_color(CLR_GREEN)
    call pdk%set_line_width(2.0)

    call ppt%define_data([refpt(1)], [refpt(2)], [refpt(3)])
    call ppt%set_draw_line(.false.)
    call ppt%set_draw_markers(.true.)
    call ppt%set_marker_style(MARKER_EMPTY_CIRCLE)
    call ppt%set_marker_scaling(2.0)
    call ppt%set_line_width(2.0)
    call ppt%set_line_color(CLR_BLACK)

    call plt%push(sd1)
    call plt%push(pd1)
    call plt%push(pdi)
    call plt%push(pdj)
    call plt%push(pdk)
    call plt%push(ppt)
    call plt%draw()
end program