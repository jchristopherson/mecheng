! geometry.f90

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
        !> @brief Computes the distance between the plane and a point.
        !!
        !! @par Details
        !! The distance between the plane and a point can be defined as follows:
        !! @par
        !! \f$ D = \frac{|a x + b y + c z + d|}{\sqrt{a^{2} + b^{2} + c^{2}}} \f$.
        procedure, public :: distance_to_point => pln_point_distance
        !> @brief Returns the normal vector of the plane of unit magnitude.
        procedure, public :: normal_vector => pln_normal
        !> @brief Projects a point onto the plane.
        procedure, public :: project_point => pln_proj_point_2_plane
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
    end type

! ------------------------------------------------------------------------------
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
    !! @return The resulting plane.
    pure function plane_from_3_points(p1, p2, p3) result(p)
        ! Arguments
        real(real64), intent(in), dimension(3) :: p1, p2, p3
        type(plane) :: p

        ! Local Variables
        real(real64), dimension(3) :: n

        ! Compute the normal vector of the plane
        n = cross(p2 - p1, p3 - p1)
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
    !! @return The resulting plane.
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
    !! @param[in] opt An optional point defining the origin location.  If not
    !!  given, the origin will be assumed at (0, 0, 0).
    !! @return The resulting plane.
    function plane_from_angle_axis(axis, angle, pt, opt) result(p)
        ! Local Variables
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

! ******************************************************************************
! PLANE TYPE MEMBER ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Computes the distance between the plane and a point.
    !!
    !! @param[in] this The plane.
    !! @param[in] p The point of interest.
    !! @return The distance between the point and plane.
    !!
    !! @par Details
    !! The distance between the plane and a point can be defined as follows:
    !! @par
    !! \f$ D = \frac{|a x + b y + c z + d|}{\sqrt{a^{2} + b^{2} + c^{2}}} \f$.
    pure function pln_point_distance(this, p) result(d)
        ! Arguments
        class(plane), intent(in) :: this
        real(real64), intent(in), dimension(3) :: p
        real(real64) :: d

        ! Local Variables
        real(real64), dimension(3) :: n
        real(real64) :: nMag

        ! Extract the normal vector of the plane
        n = [this%a, this%b, this%c]
        nMag = norm2(n)

        ! Compute the distance from the plane
        d = abs(this%a * p(1) + this%b * p(2) + this%c * p(3) + this%d) / nMag
    end function

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

! ------------------------------------------------------------------------------
    !> @brief Projects a point onto the plane.
    !!
    !! @param[in] this The plane.
    !! @param[in] pt The point to project.
    !! @return The projected point.
    pure function pln_proj_point_2_plane(this, pt) result(v)
        ! Arguments
        class(plane), intent(in) :: this
        real(real64), intent(in), dimension(3) :: pt
        real(real64), dimension(3) :: v

        ! Local Variables
        real(real64), dimension(3) :: n
        real(real64) :: dist

        ! Compute the normal vector of the plane
        n = this%normal_vector()

        ! Compute the distance the point lies from the plane
        dist = this%distance_to_point(pt)

        ! Project the point onto the plane
        v = pt - dist * n
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module