! plane_line_example_1.f90

program example
    use iso_fortran_env
    use geometry
    use fplot_core
    implicit none

    ! Local Variables
    real(real64), dimension(3,6) :: pts
    type(plane) :: pln1, pln2
    type(line) :: l1
    type(surface_plot_data) :: pd1, pd2
    type(plot_data_3d) :: ld1
    type(surface_plot) :: plt
    class(plot_axis), pointer :: xAxis, yAxis, zAxis

    ! Define two sets of planes
    call random_number(pts)
    pln1 = plane_from_3_points(pts(:,1), pts(:,2), pts(:,3))
    pln2 = plane_from_3_points(pts(:,4), pts(:,5), pts(:,6))

    ! Construct a line from the points
    l1 = line_from_2_planes(pln1, pln2)

    ! Set up the plot
    call plt%initialize()
    call plt%set_show_hidden(.true.)
    call plt%set_show_colorbar(.false.)
    call plt%set_font_size(14)
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()
    zAxis => plt%get_z_axis()

    call xAxis%set_title("X")
    call yAxis%set_title("Y")
    call zAxis%set_title("Z")

    ! Draw the planes
    call draw_plane(pln1, pd1, 1.0d0)
    call draw_plane(pln2, pd2, 1.0d0)

    call pd1%set_use_wireframe(.true.)
    call pd2%set_use_wireframe(.true.)

    ! Draw the lines
    call draw_line(l1, ld1, -2.0d0, 2.0d0)
    call ld1%set_line_width(3.0)
    call ld1%set_line_style(LINE_DASHED)
    call ld1%set_line_color(CLR_BLACK)

    call plt%push(pd1)
    call plt%push(pd2)
    call plt%push(ld1)
    call plt%draw()

contains
    ! Populates a surface_plot_data type with the information necessary to
    ! draw a plane
    subroutine draw_plane(pln, pd, sp)
        ! Arguments
        use kinematics
        type(plane), intent(in) :: pln
        real(real64), intent(in) :: sp
        type(surface_plot_data), intent(inout) :: pd

        ! Parameters
        real(real64), dimension(3) :: ii = [1.0d0, 0.0d0, 0.0d0]
        real(real64), dimension(3) :: jj = [0.0d0, 1.0d0, 0.0d0]
        real(real64), dimension(3) :: kk = [0.0d0, 0.0d0, 1.0d0]

        ! Local Variables
        real(real64), dimension(3) :: n, origin, i, j, p1, p2, p3, p4
        real(real64), dimension(3, 3) :: r
        real(real64), dimension(2, 2) :: x, y, z
        real(real64) :: d, eps

        ! Initialization
        eps = 2.0d0 * epsilon(eps)

        ! Determine the normal to the plane
        n = pln%normal_vector()

        ! Locate an "origin" point on the plane
        origin = proj_point_2_plane(pln, [0.0d0, 0.0d0, 0.0d0])

        ! Determine either an x or y axis of the plane
        if (abs(pln%a - pln%b) > abs(pln%a - pln%c)) then
            ! Project a point onto the plane to construct an x axis
            i = proj_point_2_plane(pln, [pln%b, pln%a, pln%c]) - origin
            i = i / norm2(i)

            ! Compute the y axis direction via cross product
            j = cross(n, i)
        else
            ! Project a point onto the plane to construct an x axis
            i = proj_point_2_plane(pln, [pln%c, pln%b, pln%a]) - origin
            i = i / norm2(i)

            ! Compute the x axis direction via cross product
            j = cross(n, i)
        end if
        
        ! Compute a rotation matrix to define the plane
        r = rotate(ii, jj, kk, i, j, n)

        ! Convert plane coordinates from the local frame to the parent frame
        p1 = [sp, sp, 0.0d0]
        p2 = [-sp, sp, 0.0d0]
        p3 = [-sp, -sp, 0.0d0]
        p4 = [sp, -sp, 0.0d0]

        p1 = matmul(r, p1) + origin
        p2 = matmul(r, p2) + origin
        p3 = matmul(r, p3) + origin
        p4 = matmul(r, p4) + origin

        ! Ensure the rotated points lie on the plane
        d = plane_to_point_distance(pln, p1)
        if (d > eps) then
            print '(AEN12.4)', &
                "Point 1 does not lie on the plane.  The offset is ", d
        end if

        d = plane_to_point_distance(pln, p2)
        if (d > eps) then
            print '(AEN12.4)', &
                "Point 2 does not lie on the plane.  The offset is ", d
        end if

        d = plane_to_point_distance(pln, p3)
        if (d > eps) then
            print '(AEN12.4)', &
                "Point 3 does not lie on the plane.  The offset is ", d
        end if

        d = plane_to_point_distance(pln, p4)
        if (d > eps) then
            print '(AEN12.4)', &
                "Point 4 does not lie on the plane.  The offset is ", d
        end if

        ! Populate the surface_plot_data object
        x = reshape([p3(1), p2(1), p4(1), p1(1)], [2, 2])
        y = reshape([p3(2), p2(2), p4(2), p1(2)], [2, 2])
        z = reshape([p3(3), p2(3), p4(3), p1(3)], [2, 2])

        call pd%define_data(x, y, z)
    end subroutine

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