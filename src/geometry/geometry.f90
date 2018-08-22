! geometry.f90

!> @brief \b geometry
!!
!! @par Purpose
!! This module contains geometric types and relating calculations.
module geometry
    use iso_fortran_env
    implicit none
    private
    public :: cross
    public :: proj
    public :: plane
    public :: line
    public :: plane_from_3_points
    public :: plane_from_line_and_point
    public :: plane_from_angle_axis
    public :: plane_to_point_distance
    public :: proj_point_2_plane
    public :: line_from_2_points
    public :: line_from_2_planes
    public :: shortest_line
    public :: line_to_point_distance
    public :: shortest_line_to_line
    public :: line_to_line_distance

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a plane in 3D Euclidean space that may be described by 
    !! the equation:
    !! \f$ a x + b y + c z + d = 0 \f$.
    type plane
        !> The x-coefficient in the plane equation.
        real(real64) :: a
        !> The y-coefficient in the plane equation.
        real(real64) :: b
        !> The z-coefficient in the plane equation.
        real(real64) :: c
        !> The plane offset along the unit vector defined by (a, b, c).
        real(real64) :: d
    contains
        !> @brief Returns the normal vector of the plane of unit magnitude.
        procedure, public :: normal_vector => pln_normal
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a line in 3D Euclidean space that may be described by the
    !! vector equation: \f$ \mathbf{r} = \mathbf{a} + t \left( \mathbf{b} - 
    !! \mathbf{a} \right) \f$, where \f$ t \f$ is a parametric variable.
    type line
        !> The origin point (t = 0).
        real(real64), dimension(3) :: a
        !> The target point (t = 1).
        real(real64), dimension(3) :: b
    contains
        !> @brief Evaluates the parametric line equation.
        procedure, public :: evaluate => ln_eval
        !> @brief Returns a direction vector along the line whose length is from
        !!  t = 0 to t = 1.
        procedure, public :: direction => ln_dir
    end type

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    !> @brief Constructs a plane from an axis, and an angle of rotation 
    !! as measured from a supplied point.
    !!
    !! @par Example
    !! The following example illustrates how to create a plane by rotation about
    !! an axis by a specified angle.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use geometry
    !!     use fplot_core
    !!     use constants
    !!     use kinematics
    !!     implicit none
    !!
    !!     ! Local Variables
    !!     real(real64), dimension(3) :: i, j, k, axis, refpt, origin, &
    !!         cp1, cp2, cp3, cp4, rp1, rp2, rp3, rp4
    !!     type(quaternion) :: q
    !!     real(real64), dimension(2, 2) :: x, y, z
    !!     real(real64) :: angle
    !!     type(plane) :: pln
    !!     type(surface_plot_data) :: sd1
    !!     type(plot_data_3d) :: pd1, pdi, pdj, pdk, ppt
    !!     type(surface_plot) :: plt
    !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
    !!
    !!     ! Initialization
    !!     call plt%initialize()
    !!     call plt%set_show_colorbar(.false.)
    !!     call plt%set_font_size(14)
    !!     call plt%set_show_hidden(.true.)
    !!
    !!     xAxis => plt%get_x_axis()
    !!     yAxis => plt%get_y_axis()
    !!     zAxis => plt%get_z_axis()
    !!
    !!     call xAxis%set_title("X")
    !!     call yAxis%set_title("Y")
    !!     call zAxis%set_title("Z")
    !!
    !!     axis = [0.0d0, 1.0d0, 0.0d0]
    !!     angle = pi / 4.0d0          ! 45 degree angle
    !!     if (abs(axis(1) - axis(2)) < epsilon(angle)) then
    !!         refpt = [axis(3), 0.0d0, axis(1)]   ! Reference point
    !!     else
    !!         refpt = [axis(2), axis(1), 0.0d0]   ! Reference point
    !!     end if
    !!
    !!     ! Construct a plane
    !!     pln = plane_from_angle_axis(axis, angle, refpt)
    !!
    !!     ! Display the equation of the plane
    !!     print '(A)', "The equation of the plane:"
    !!     print '(AEN12.3AEN12.3AEN12.3AEN12.3A)', "(", pln%a, ", ", pln%b, ", ", &
    !!         pln%c, ", ", pln%d, ")"
    !!
    !!     ! Determine a local coordinate system that exists on the plane, and has
    !!     ! a z-axis parallel to the plane normal
    !!     k = pln%normal_vector()
    !!     origin = -pln%d * k ! This is the location of the origin on the plane
    !!
    !!     ! Let the rotation axis be the x axis of the local coordinate system
    !!     i = axis
    !!
    !!     ! Construct J = K cross I
    !!     j = cross(k, i)
    !!
    !!     ! Define a rotation matrix based upon the above unit vectors
    !!     call q%from_angle_axis(angle, axis)
    !!
    !!     ! Define 4 points on the plane in order to provide a visual representation
    !!     ! of the plane
    !!     cp1 = [1.0d0, 1.0d0, 0.0d0]
    !!     cp2 = [1.0d0, -1.0d0, 0.0d0]
    !!     cp3 = [-1.0d0, -1.0d0, 0.0d0]
    !!     cp4 = [-1.0d0, 1.0d0, 0.0d0]
    !!
    !!     ! Apply the transformation to move the points onto the plane
    !!     rp1 = q%transform(cp1) + origin
    !!     rp2 = q%transform(cp2) + origin
    !!     rp3 = q%transform(cp3) + origin
    !!     rp4 = q%transform(cp4) + origin
    !!
    !!     ! Draw the plane
    !!     x = reshape([rp3(1), rp4(1), rp2(1), rp1(1)], [2, 2])
    !!     y = reshape([rp3(2), rp4(2), rp2(2), rp1(2)], [2, 2])
    !!     z = reshape([rp3(3), rp4(3), rp2(3), rp1(3)], [2, 2])
    !!     call sd1%define_data(x, y, z)
    !!     call sd1%set_use_wireframe(.true.)
    !!
    !!     ! Draw the rotation axis
    !!     call pd1%define_data( &
    !!         [origin(1), axis(1) + origin(1)], &
    !!         [origin(2), axis(2) + origin(2)], &
    !!         [origin(3), axis(3) + origin(3)])
    !!     call pd1%set_line_color(CLR_BLACK)
    !!     call pd1%set_line_width(3.0)
    !!    call pd1%set_line_style(LINE_DASHED)
    !!
    !!     ! Draw the coordinate frame
    !!     call pdi%define_data( &
    !!         [origin(1), i(1) + origin(1)], &
    !!         [origin(2), i(2) + origin(2)], &
    !!         [origin(3), i(3) + origin(3)])
    !!     call pdi%set_line_color(CLR_BLUE)
    !!     call pdi%set_line_width(2.0)
    !!
    !!     call pdj%define_data( &
    !!         [origin(1), j(1) + origin(1)], &
    !!         [origin(2), j(2) + origin(2)], &
    !!         [origin(3), j(3) + origin(3)])
    !!     call pdj%set_line_color(CLR_RED)
    !!     call pdj%set_line_width(2.0)
    !!
    !!     call pdk%define_data( &
    !!         [origin(1), k(1) + origin(1)], &
    !!         [origin(2), k(2) + origin(2)], &
    !!         [origin(3), k(3) + origin(3)])
    !!     call pdk%set_line_color(CLR_GREEN)
    !!     call pdk%set_line_width(2.0)
    !!
    !!     call ppt%define_data([refpt(1)], [refpt(2)], [refpt(3)])
    !!     call ppt%set_draw_line(.false.)
    !!     call ppt%set_draw_markers(.true.)
    !!     call ppt%set_marker_style(MARKER_EMPTY_CIRCLE)
    !!     call ppt%set_marker_scaling(2.0)
    !!     call ppt%set_line_width(2.0)
    !!     call ppt%set_line_color(CLR_BLACK)
    !!
    !!     call plt%push(sd1)
    !!     call plt%push(pd1)
    !!     call plt%push(pdi)
    !!     call plt%push(pdj)
    !!     call plt%push(pdk)
    !!     call plt%push(ppt)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @code{.txt}
    !! The equation of the plane:
    !! (-707.107E-03,    0.000E+00, -707.107E-03,   -0.000E+00)
    !! @endcode
    !! @image html rotated_plane_example.png
    interface plane_from_angle_axis
        module procedure :: plane_from_angle_axis_1
        module procedure :: plane_from_angle_axis_2
    end interface

contains
! ******************************************************************************
! VECTOR RELATED ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Computes the cross-product of a vector.
    !!
    !! @param[in] x The left-hand-side operator.
    !! @param[in] y The right-hand-side operator.
    !! @return The resulting vector.
    pure function cross(x, y) result(z)
        ! Arguments
        real(real64), intent(in), dimension(3) :: x, y
        real(real64), dimension(3) :: z

        ! Process
        z(1) = x(2) * y(3) - x(3) * y(2)
        z(2) = x(3) * y(1) - x(1) * y(3)
        z(3) = x(1) * y(2) - x(2) * y(1)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the projection of a vector onto another vector.
    !!
    !! @param[in] x The vector to project.
    !! @param[in] y The vector onto which @p x is to be projected.
    !! @return The projected vector.
    !!
    !! @par Remarks
    !! The projection of a vector (@p x) onto another vector (@p y) is given by:
    !! \f$ \mathbf{p} = \mathbf{x} \cdot \mathbf{y} 
    !! \frac{ \mathbf{y} }{\| \mathbf{y} \|} \f$.
    pure function proj(x, y) result(z)
        ! Arguments
        real(real64), intent(in), dimension(3) :: x, y
        real(real64), dimension(3) :: z

        ! Local Variables
        real(real64) :: x1

        ! Compute the scalar projection
        x1 = dot_product(x, y) / norm2(y)

        ! Compute the vector projection
        z = x1 * y
    end function

! ******************************************************************************
! PLANE RELATED ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Creates a plane from 3 points.
    !!
    !! @param[in] p1 The first point.
    !! @param[in] p2 The second point.
    !! @param[in] p3 The third point.
    !! @return The resulting plane.  If the three points lie along the same 
    !!  line, the plane is returned with all zero coefficients.
    !!
    !! @par Remarks
    !! The plane is constructed such that it's normal acts along the vector:
    !! \f$ \mathbf{n} = a \mathbf{i} + b \mathbf{j} + c \mathbf{k} = 
    !! \left(\mathbf{p_2} - \mathbf{p_1} \right) \times \left(\mathbf{p_3} - 
    !! \mathbf{p_1} \right)\f$.
    pure function plane_from_3_points(p1, p2, p3) result(p)
        ! Arguments
        real(real64), intent(in), dimension(3) :: p1, p2, p3
        type(plane) :: p

        ! Local Variables
        real(real64), dimension(3) :: n

        ! Compute the normal vector of the plane
        n = cross(p2 - p1, p3 - p1)
        if (norm2(n) < 2.0d0 * epsilon(2.0d0)) then
            p%a = 0.0d0
            p%b = 0.0d0
            p%c = 0.0d0
            p%d = 0.0d0
            return
        end if
        n = n / norm2(n)

        p%a = n(1)
        p%b = n(2)
        p%c = n(3)

        ! Treat point 1 as the origin, and then "d" can be found
        p%d = -(n(1) * p1(1) + n(2) * p1(2) + n(3) * p1(3))
    end function

! ------------------------------------------------------------------------------
    !> @brief Creates a plane from a line and a point.
    !!
    !! @param[in] l1 The line.
    !! @param[in] p1 The point.  The point must not lie on the line.
    !! @return The resulting plane.  If @p p1 lies along @p l1, then the 
    !!  plane is returned with all zero coefficients.
    !!
    !! @par Remarks
    !! The plane is constructed such that it's normal acts along the vector:
    !! \f$ \mathbf{n} = a \mathbf{i} + b \mathbf{j} + c \mathbf{k} = 
    !! \left(\mathbf{p_b} - \mathbf{p_a} \right) \times \left(\mathbf{p_1} - 
    !! \mathbf{p_a} \right)\f$ where \f$ p_{a} \f$ is the point on the line
    !! where \f$ t = 0 \f$, and \f$ p_{b} \f$ is the point on the line where
    !! \f$ t = 1 \f$ (\f$ t \f$ is the parametric variable of the line
    !! equation).
    pure function plane_from_line_and_point(l1, p1) result(p)
        ! Arguments
        class(line), intent(in) :: l1
        real(real64), intent(in), dimension(3) :: p1
        type(plane) :: p

        ! Local Variables
        real(real64), dimension(3) :: pl1, pl2

        ! Process
        pl1 = l1%a
        pl2 = l1%b
        p = plane_from_3_points(pl1, pl2, p1)
    end function

! ------------------------------------------------------------------------------
    !> @brief Constructs a plane from an axis, and an angle of rotation 
    !! as measured from a supplied point.
    !!
    !! @param[in] axis The axis of rotation.
    !! @param[in] angle The angle of rotation, in radians.
    !! @param[in] pt The reference point.  This point must not lie on the axis
    !!  of rotation.
    !! @param[in] opt An optional point defining the a location along the axis. 
    !!  If not given, the axis will pass through the point (0, 0, 0).
    !! @return The resulting plane.
    function plane_from_angle_axis_1(axis, angle, pt, opt) result(p)
        ! Arguments
        use kinematics
        real(real64), intent(in), dimension(3) :: axis, pt
        real(real64), intent(in) :: angle
        real(real64), intent(in), optional, dimension(:) :: opt
        type(plane) :: p

        ! Local Variables
        type(quaternion) :: q
        real(real64), dimension(3) :: rpt

        ! Construct a quaternion using the supplied angle and axis information
        call q%from_angle_axis(angle, axis)

        ! Normalize the quaternion to ensure we're dealing with a unit
        ! quaternion
        call q%normalize()

        ! Apply the rotation to the point.  After this, we'll have a line that
        ! defines 2 points, and a 3rd point to define the plane.
        rpt = q%transform(pt)

        ! Define the plane from the 3 points we now have
        if (present(opt)) then
            p = plane_from_3_points(opt, axis + opt, rpt)
        else
            p = plane_from_3_points([0.0d0, 0.0d0, 0.0d0], axis, rpt)
        end if
    end function

! --------------------
    !> @brief Constructs a plane from an axis, and an angle of rotation 
    !! as measured from a supplied point.
    !!
    !! @param[in] ln The line representing the axis of rotation.
    !! @param[in] angle The angle of rotation, in radians.
    !! @param[in] pt The reference point.  This point must not lie on the axis
    !!  of rotation.
    !! @return The resulting plane.
    function plane_from_angle_axis_2(ln, angle, pt) result(p)
        ! Arguments
        class(line), intent(in) :: ln
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: pt
        type(plane) :: p

        ! Process
        p = plane_from_angle_axis_1(ln%b - ln%a, angle, pt, ln%a)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the distance between the plane and a point.
    !!
    !! @param[in] pln The plane.
    !! @param[in] p The point of interest.
    !! @return The distance between the point and plane.
    !!
    !! @par Details
    !! The distance between the plane and a point can be defined as follows:
    !! @par
    !! \f$ D = \frac{|a x + b y + c z + d|}{\sqrt{a^{2} + b^{2} + c^{2}}} \f$.
    pure function plane_to_point_distance(pln, p) result(d)
        ! Arguments
        class(plane), intent(in) :: pln
        real(real64), intent(in), dimension(3) :: p
        real(real64) :: d

        ! Local Variables
        real(real64), dimension(3) :: n
        real(real64) :: nMag

        ! Extract the normal vector of the plane
        n = [pln%a, pln%b, pln%c]
        nMag = norm2(n)

        ! Compute the distance from the plane
        d = abs(pln%a * p(1) + pln%b * p(2) + pln%c * p(3) + pln%d) / nMag
    end function

! ------------------------------------------------------------------------------
    !> @brief Projects a point onto the plane.
    !!
    !! @param[in] pln The plane.
    !! @param[in] pt The point to project.
    !! @return The projected point.
    !!
    !! @par Remarks
    !! The projection of a point onto a plane is given as follows.
    !! @par
    !! \f$ \mathbf{p} = \mathbf{p_t} - d \mathbf{n} \f$, where
    !! @par
    !! \f$ \mathbf{n} = \frac{a \mathbf{i} + b \mathbf{j} + c \mathbf{k}}
    !! {\sqrt{a^{2} + b^{2} + c^{2}}} \f$, and 
    !! @par
    !! \f$ d = \frac{|a x + b y + c z + d|}{\sqrt{a^{2} + b^{2} + c^{2}}} \f$.
    pure function proj_point_2_plane(pln, pt) result(v)
        ! Arguments
        class(plane), intent(in) :: pln
        real(real64), intent(in), dimension(3) :: pt
        real(real64), dimension(3) :: v

        ! Local Variables
        real(real64), dimension(3) :: n
        real(real64) :: dist

        ! Compute the normal vector of the plane
        n = pln%normal_vector()

        ! Compute the distance the point lies from the plane
        dist = plane_to_point_distance(pln, pt)

        ! Project the point onto the plane
        v = pt - dist * n
    end function

! ******************************************************************************
! PLANE TYPE MEMBER ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Returns the normal vector of the plane of unit magnitude.
    !!
    !! @param[in] this The plane.
    !! @return The normal vector of unit magnitude.
    pure function pln_normal(this) result(n)
        ! Arguments
        class(plane), intent(in) :: this
        real(real64), dimension(3) :: n

        ! Process
        n = [this%a, this%b, this%c]
        n = n / norm2(n)
    end function

! ******************************************************************************
! LINE RELATED ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Constructs a line from two points.
    !!
    !! @param[in] pt1 The first point.
    !! @param[in] pt2 The second point.
    !! @return The resulting line.
    pure function line_from_2_points(pt1, pt2) result(r)
        ! Arguments
        real(real64), intent(in), dimension(3) :: pt1, pt2
        type(line) :: r

        ! Process
        r%a = pt1
        r%b = pt2
    end function

! ------------------------------------------------------------------------------
    !> @brief Constructs a line from the intersection of two planes.
    !!
    !! @param[in] p1 The first plane.
    !! @param[in] p2 The second plane.
    !! @return The resulting line.  If the two planes are parallel, no line is
    !!  formed, and all zeros are returned.
    function line_from_2_planes(p1, p2) result(r)
        ! Arguments
        use linalg_immutable
        class(plane) :: p1, p2
        type(line) :: r

        ! Local Variables
        real(real64) :: tol
        real(real64), dimension(3) :: n1, n2, n1n2, pt
        real(real64), dimension(2) :: d
        real(real64), dimension(2, 3) :: c

        ! Compute the normal vectors of each plane
        n1 = p1%normal_vector()
        n2 = p2%normal_vector()

        ! Ensure the planes are not parallel
        tol = 2.0d0 * epsilon(tol)
        n1n2 = cross(n1, n2)
        if (norm2(n1n2) < tol) then
            ! The two planes are parallel
            r%a = 0.0d0
            r%b = 0.0d0
            return
        end if

        ! Notice, N1 cross N2 is parallel to the line of intersection between
        ! the two planes.  As such, the problem now is to find a point that
        ! lies on both planes.  This may be achieved by solving the two
        ! plane equations simultaneously.  
        c = reshape([p1%a, p2%a, p1%b, p2%b, p1%c, p2%c], [2, 3])
        d = -[p1%d, p2%d]

        ! Solve the system of equations via a least-squares technique
        pt = matmul(mat_pinverse(c), d)

        ! Now that we have a point on the line, and the direction of the line,
        ! we know the line
        r = line_from_2_points(pt, n1n2 + pt)
    end function

! ------------------------------------------------------------------------------
    !> @brief Determines the shortest line segment between a point and another
    !! line.
    !!
    !! @param[in] ln The line.
    !! @param[in] pt The point.
    !! @return The resulting shortest line segment determined such that at 
    !!  t = 0, this line intersects point @p pt, and at t = 1, this line
    !!  intersects the nearest point on the original line.
    !!
    !! @par Remarks
    !! The shortest line segment between a point and a line is computed assuming
    !! the line is given as \f$ \mathbf{r} \left( t \right) = \mathbf{a} + 
    !!  t \mathbf{x} \f$, and that the point is given by \f$ \mathbf{p} \f$.
    !! @par
    !! Then, a new line segment (\f$ \mathbf{q} \left( s \right) \f$) may be
    !! defined as \f$ \mathbf{q} \left( s \right) = \mathbf{p} + s \left( 
    !! \mathbf{a} - \left( \left( \mathbf{a} - \mathbf{p} \right) \cdot 
    !! \mathbf{x} \right) \mathbf{x} - \mathbf{p} \right) \f$.
    !!
    !! @par Example
    !! The following example illustrates how to find the shortest line segment
    !! between another line and a point.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use geometry
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Local Variables
    !!     type(line) :: l1, ls
    !!     real(real64), dimension(3) :: pt1, pt2, pt3
    !!     type(plot_3d) :: plt
    !!     class(plot_axis), pointer :: xAxis, yAxis, zAxis
    !!     type(plot_data_3d) :: d1, d2, d3
    !!
    !!     ! Initialization
    !!     call random_number(pt1)
    !!     call random_number(pt2)
    !!     call random_number(pt3)
    !!
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!     xAxis => plt%get_x_axis()
    !!     yAxis => plt%get_y_axis()
    !!     zAxis => plt%get_z_axis()
    !!
    !!     call xAxis%set_title("X")
    !!     call yAxis%set_title("Y")
    !!     call zAxis%set_title("Z")
    !!
    !!     ! Construct the line from PT1 and PT2
    !!     l1 = line_from_2_points(pt1, pt2)
    !!
    !!     ! Determine the shortest line between the line L1 and PT3
    !!     ls = shortest_line(l1, pt3)
    !!
    !!     ! Plot the results
    !!     call d1%define_data( &
    !!         [l1%a(1), l1%b(1)], &   ! X components of line 1 (L1)
    !!         [l1%a(2), l1%b(2)], &   ! Y components of line 1 (L1)
    !!         [l1%a(3), l1%b(3)])     ! Z components of line 1 (L1)
    !!     call d1%set_draw_markers(.true.)
    !!     call d1%set_line_width(2.0)
    !!     call d1%set_marker_style(MARKER_X)
    !!
    !!     call d2%define_data([pt3(1)], [pt3(2)], [pt3(3)])   ! Point 3
    !!     call d2%set_draw_line(.false.)
    !!     call d2%set_draw_markers(.true.)
    !!     call d2%set_line_width(2.0)
    !!     call d2%set_marker_style(MARKER_EMPTY_CIRCLE)
    !!
    !!     call d3%define_data( &
    !!         [ls%a(1), ls%b(1)], &   ! X components of the shortest line
    !!         [ls%a(2), ls%b(2)], &   ! Y components of the shortest line
    !!         [ls%a(3), ls%b(3)])     ! Z components of the shortest line
    !!     call d3%set_line_style(LINE_DASHED)
    !!     call d3%set_line_width(2.0)
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%push(d3)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output.
    !! @image html shortest_line_example.png
    pure function shortest_line(ln, pt) result(r)
        ! Arguments
        class(line), intent(in) :: ln
        real(real64), intent(in), dimension(3) :: pt
        type(line) :: r

        ! Local Variables
        real(real64), dimension(3) :: n, x
        real(real64) :: t

        ! Process
        n = ln%b - ln%a
        t = dot_product(ln%a - pt, n)
        x = ln%a - pt - t * n

        ! Define the new line
        r = line_from_2_points(pt, x + pt)
    end function

! ------------------------------------------------------------------------------
    !> @brief Determines the shortest distance between a line and a point.
    !!
    !! @param[in] ln The line.
    !! @param[in] pt The point.
    !! @return The distance.
    pure function line_to_point_distance(ln, pt) result(x)
        ! Arguments
        class(line), intent(in) :: ln
        real(real64), intent(in), dimension(3) :: pt
        real(real64) :: x

        ! Local Variables
        type(line) :: r

        ! Process
        r = shortest_line(ln, pt)
        x = norm2(r%direction())
    end function

! ------------------------------------------------------------------------------
    !> @brief Determines the shortest line segment between two lines.  The
    !! line segment is defined such that \f$ t = 0 \f$ at the intersection with
    !! line 1 (@p l1), and \f$ t = 1 \f$ at the intersection with line 2
    !! (@p l2).
    !!
    !! @param[in] l1 The first line.
    !! @param[in] l2 The second line.
    !! @return The resulting line segment.
    !!
    !! @par Remarks
    !! The line segment is determined assuming that line 1 is defined as
    !! \f$ \mathbf{r_{1}} \left( s \right) = \mathbf{a} + s \mathbf{v} \f$, and
    !! line 2 is defined as \f$ \mathbf{r_{2}} \left( t \right) = \mathbf{x} + 
    !! t \mathbf{u} \f$.
    !! @par
    !! Then the location on each line nearest the other is determined as 
    !! follows.
    !! @par
    !! \f$ s = \frac{\det{\left( \mathbf{a} - \mathbf{x}, \mathbf{u}, \mathbf{u}
    !!  \times \mathbf{v} \right)}}{\| \mathbf{u} \times \mathbf{v} \|^2} \f$,
    !! @par
    !! and
    !! @par
    !! \f$ t = \frac{\det{\left( \mathbf{a} - \mathbf{x}, \mathbf{v}, \mathbf{u}
    !!  \times \mathbf{v} \right)}}{\| \mathbf{u} \times \mathbf{v} \|^2} \f$.
    function shortest_line_to_line(ln1, ln2) result(r)
        ! Arguments
        use linalg_core
        class(line), intent(in) :: ln1, ln2
        type(line) :: r

        ! Local Variables
        real(real64), dimension(3, 3) :: a1, a2
        real(real64), dimension(3) :: cp, u, v
        real(real64) :: s, t, denom

        ! Initialization
        u = ln2%direction()
        v = ln1%direction()
        cp = cross(u, v)

        a1(:,1) = ln1%a - ln2%a
        a1(:,2) = u
        a1(:,3) = cp

        a2(:,1) = a1(:,1)
        a2(:,2) = v
        a2(:,3) = cp

        denom = norm2(cp)**2

        ! Compute the parameter on each line
        s = det(a1) / denom
        t = det(a2) / denom

        ! Compute the new line as we now have 2 points
        r = line_from_2_points(ln1%evaluate(s), ln2%evaluate(t))
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the shortest distance between two lines.
    !!
    !! @param[in] l1 The first line.
    !! @param[in] l2 The second line.
    !! @return The shortest distance between @p ln1 and @p ln2.
    function line_to_line_distance(ln1, ln2) result(d)
        ! Arguments
        class(line), intent(in) :: ln1, ln2
        real(real64) :: d

        ! Local Variables
        type(line) :: shortLine

        ! Process
        shortLine = shortest_line_to_line(ln1, ln2)
        d = norm2(shortLine%direction())
    end function

! ******************************************************************************
! LINE TYPE MEMBER ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Evaluates the parametric line equation.
    !!
    !! @param[in] this The line.
    !! @param[in] t The parameter.
    !! @return The position of @p t along the line.
    pure function ln_eval(this, t) result(x)
        ! Arguments
        class(line), intent(in) :: this
        real(real64), intent(in) :: t
        real(real64), dimension(3) :: x

        ! Process
        x = this%a + t * (this%b - this%a)
    end function

! ------------------------------------------------------------------------------
    !> @brief Returns a direction vector along the line whose length is from
    !!  t = 0 to t = 1.
    !!
    !! @param[in] this The line.
    !! @return The direction vector.
    pure function ln_dir(this) result(x)
        ! Arguments
        class(line), intent(in) :: this
        real(real64), dimension(3) :: x

        ! Process
        x = this%b - this%a
    end function


end module