! geometry.f90

module geometry
    use iso_fortran_env
    implicit none
    private
    public :: cross
    public :: proj
    public :: plane
    public :: plane_from_3_points

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

! ------------------------------------------------------------------------------

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
        n = (this%a, this%b, this%c)
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
        n = (this%a, this%b, this%d)
        n = n / norm2(n)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module