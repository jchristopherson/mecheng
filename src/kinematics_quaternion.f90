! kinematics_quaternion.f90

submodule (kinematics) kinematics_quaternion
contains
! ------------------------------------------------------------------------------
    !> @brief Creates a quaternion representing the specified rotation angle
    !! about the specified rotation axis.
    !!
    !! @param[in,out] this The quaternion object.
    !! @param[in] angle The rotation angle, in radians.
    !! @param[in] axis A 3-element vector containing the x, y, and z components
    !!  of the rotation axis.  The vector is expected to be a unit vector.
    module subroutine quaternion_from_angle_axis(this, angle, axis)
        ! Arguments
        class(quaternion), intent(inout) :: this
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: axis

        ! Local Variables & Parameters
        real(real64), parameter :: half = 0.5d0
        real(real64) :: half_angle, s

        ! Process
        half_angle = half * angle
        s = sin(half_angle)
        this%w = cos(half_angle)
        this%x = axis(1) * s
        this%y = axis(2) * s
        this%z = axis(3) * s
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Converts the quaternion to its equivalent angle-axis
    !! representation.
    !!
    !! @param[in] this The quaternion object.
    !! @param[out] angle The equivalent rotation angle, in radians.
    !! @param[out] axis A 3-element array that on output contains the equivalent
    !!  axis of rotation.
    module subroutine quaternion_to_angle_axis(this, angle, axis)
        ! Arguments
        class(quaternion), intent(in) :: this
        real(real64), intent(out) :: angle
        real(real64), intent(out), dimension(3) :: axis

        ! Parameters
        real(real64), parameter :: two = 2.0d0

        ! Local Variables
        real(real64) :: mag
        real(real64), dimension(3) :: e

        ! Process
        e = [this%x, this%y, this%z]
        mag = norm2(e)
        angle = two * atan2(mag, this%w)
        axis = e / mag
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Creates a quaternion equivalent to the supplied rotation matrix.
    !!
    !! @param[in,out] this The quaternion object.
    !! @param[in] x The 3-by-3 rotation matrix.  It is assumed that this is a
    !!  valid, orthogonal rotation matrix.  Matrix orthogonality is not
    !!  verified.
    module subroutine quaternion_from_matrix(this, x)
        ! Arguments
        class(quaternion), intent(inout) :: this
        real(real64), intent(in), dimension(3,3) :: x

        ! Parameters
        real(real64), parameter :: quarter = 0.25d0
        real(real64), parameter :: half = 0.5d0
        real(real64), parameter :: one = 1.0d0

        ! Local Variables
        integer(int32) :: maxind
        real(real64), dimension(4) :: e

        ! Initialization
        e = [half * sqrt(one + x(1,1) + x(2,2) + x(3,3)), &
            half * sqrt(one + x(1,1) - x(2,2) - x(3,3)), &
            half * sqrt(one - x(1,1) + x(2,2) - x(3,3)), &
            half * sqrt(one - x(1,1) - x(2,2) + x(3,3))]

        ! Process
        maxind = max_abs_index(e)
        select case (maxind)
        case (1)
            this%w = e(1)
            this%x = quarter * (x(3,2) - x(2,3)) / e(1)
            this%y = quarter * (x(1,3) - x(3,1)) / e(1)
            this%z = quarter * (x(2,1) - x(1,2)) / e(1)
        case (2)
            this%w = quarter * (x(3,2) + x(2,3)) / e(2)
            this%x = e(2)
            this%y = quarter * (x(2,1) + x(1,2)) / e(2)
            this%z = quarter * (x(3,1) + x(1,3)) / e(2)
        case (3)
            this%w = quarter * (x(1,3) - x(3,1)) / e(3)
            this%x = quarter * (x(2,1) - x(1,2)) / e(3)
            this%y = e(3)
            this%z = quarter * (x(3,2) - x(2,3)) / e(3)
        case default
            this%w = quarter * (x(2,1) - x(1,2)) / e(4)
            this%x = quarter * (x(3,1) + x(1,3)) / e(4)
            this%y = quarter * (x(3,2) + x(2,3)) / e(4)
            this%z = e(4)
        end select
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Normalizes the quaternion to a unit quaternion.
    !!
    !! @param[in,out] this The quaternion object.
    module subroutine quaternion_normalize(this)
        ! Arguments
        class(quaternion), intent(inout) :: this

        ! Local Variables
        real(real64) :: mag

        ! Process
        mag = this%magnitude()
        this%w = this%w / mag
        this%x = this%x / mag
        this%y = this%y / mag
        this%z = this%z / mag
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Computes the magnitude of the quaternion.
    !!
    !! @param[in] this The quaternion object.
    !!
    !! @return The magnitude of the quaternion.
    pure module function quaternion_magnitude(this) result(x)
        ! Arguments
        class(quaternion), intent(in) :: this
        real(real64) :: x

        ! Process
        x = norm2([this%w, this%x, this%y, this%z])
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts the quaternion to its equivalent rotation matrix.
    !!
    !! @param[in] this The quaternion object.
    !!
    !! @return The resulting 3-by-3 rotation matrix.
    pure module function quaternion_to_matrix(this) result(x)
        ! Arguments
        class(quaternion), intent(in) :: this
        real(real64), dimension(3,3) :: x

        ! Local Variables & Parameters
        real(real64), parameter :: two = 2.0d0
        real(real64) :: mag, e0, e1, e2, e3

        ! Process
        mag = this%magnitude()
        e0 = this%w / mag
        e1 = this%x / mag
        e2 = this%y / mag
        e3 = this%z / mag
        
        x = reshape(&
            [e0**2+e1**2-e2**2-e3**2, two*(e0*e3+e1*e2), two*(e1*e3-e0*e2), &
            two*(e1*e2-e0*e3), e0**2-e1**2+e2**2-e3**2, two*(e0*e1+e2*e3), &
            two*(e0*e2+e1*e3), two*(e2*e3-e0*e1), e0**2-e1**2-e2**2+e3**2], &
            [3, 3])
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts the quaternion to an identity quaternion.
    !!
    !! @param[in,out] this The quaternion object.
    module subroutine quaternion_to_identity(this)
        ! Arguments
        class(quaternion), intent(inout) :: this

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Process
        this%w = zero
        this%x = one
        this%y = zero
        this%z = zero
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Returns the conjugate of the quaternion.
    !!
    !! @param[in] this The quaternion object.
    !! @return The conjugate of the quaternion.
    pure module function quaternion_conjugate(this) result(q)
        ! Arguments
        class(quaternion), intent(in) :: this
        type(quaternion) :: q

        ! Process
        q%w = this%w
        q%x = -this%x
        q%y = -this%y
        q%z = -this%z
    end function

! ------------------------------------------------------------------------------
    !> @brief Returns the inverse of the quaternion.
    !!
    !! @param[in] this The quaternion object.
    !! @return The inverse of the quaternion.
    pure module function quaternion_invert(this) result(q)
        ! Arguments
        class(quaternion), intent(in) :: this
        type(quaternion) :: q

        ! Local Variables
        real(real64) :: mag2

        ! Process
        mag2 = (this%magnitude())**2
        q = this%conjugate()
        q%w = q%w / mag2
        q%x = q%x / mag2
        q%y = q%y / mag2
        q%z = q%z / mag2
    end function

! ------------------------------------------------------------------------------
    !> @brief Extracts just the [X,Y,Z] vector component of the quaternion.
    !!
    !! @param[in] this The quaternion object.
    !! @return The [X,Y,Z] vector component.
    pure module function quaternion_extract_vector(this) result(x)
        class(quaternion), intent(in) :: this
        real(real64), dimension(3) :: x
        x = [this%x, this%y, this%z]
    end function

! ------------------------------------------------------------------------------
    !> @brief Applies the rotation transformation to the supplied vector.
    !!
    !! @param[in] this The quaternion.
    !! @param[in] x The 3-element vector to transform.
    !! @return The resulting 3-element vector.
    pure module function quaternion_xfrm(this, x) result(y)
        ! Arguments
        class(quaternion), intent(in) :: this
        real(real64), intent(in), dimension(3) :: x
        real(real64), dimension(3) :: y

        ! Local Variables
        type(quaternion) :: q

        ! Compute q = this * x * this'
        q = this * x * this%conjugate()

        ! Return the array
        y = [q%x, q%y, q%z]
    end function

! ******************************************************************************
! QUATERNION OPERATORS
! ------------------------------------------------------------------------------
    !> @brief Adds two quaternions.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_add(q, p) result(z)
        ! Arguments
        type(quaternion), intent(in) :: q, p
        type(quaternion) :: z

        ! Process
        z%w = q%w + p%w
        z%x = q%x + p%x
        z%y = q%y + p%y
        z%z = q%z + p%z
    end function

! ------------------------------------------------------------------------------
    !> @brief Subtracts two quaternions.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_subtract(q, p) result(z)
        ! Arguments
        type(quaternion), intent(in) :: q, p
        type(quaternion) :: z

        ! Process
        z%w = q%w - p%w
        z%x = q%x - p%x
        z%y = q%y - p%y
        z%z = q%z - p%z
    end function

! ------------------------------------------------------------------------------
    !> @brief Multiplies two quaternions.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_mult(q, p) result(z)
        ! Arguments
        type(quaternion), intent(in) :: q, p
        type(quaternion) :: z

        ! Process
        z%w = q%w * p%w - q%x * p%x - q%y * p%y - q%z * p%z
        z%x = p%w * q%x + p%x * q%w - p%y * q%z + p%z * q%y
        z%y = p%w * q%y + p%y * q%w + p%x * q%z - p%z * q%x
        z%z = p%w * q%z - p%x * q%y + p%z * q%w + p%y * q%x
    end function

! ------------------------------------------------------------------------------
    !> @brief Multiplies a quaternion and a vector.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaterion_vector_mult(q, p) result(z)
        ! Arguments
        type(quaternion), intent(in) :: q
        real(real64), intent(in), dimension(3) :: p
        type(quaternion) :: z

        ! Process
        z%w = -q%x * p(1) - q%y * p(2) - q%z * p(3)
        z%x = p(1) * q%w - p(2) * q%z + p(3) * q%y
        z%y = p(2) * q%w + p(1) * q%z - p(3) * q%x
        z%z = -p(1) * q%y + p(3) * q%w + p(2) * q%x
    end function

! ------------------------------------------------------------------------------
    !> @brief Multiplies a vector and a quaternion.
    !!
    !! @param[in] x The left-hand-side argument.
    !! @param[in] y The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function vector_quaternion_mult(x, y) result(z)
        ! Arguments
        real(real64), intent(in), dimension(3) :: x
        type(quaternion), intent(in) :: y
        type(quaternion) :: z

        ! Process
        z%w = -x(1) * y%x - x(2) * y%y - x(3) * y%z
        z%x = x(1) * y%w - x(2) * y%z + x(3) * y%y
        z%y = x(2) * y%w + x(1) * y%z - x(3) * y%x
        z%z = -x(1) * y%y + x(3) * y%w + x(2) * y%x
    end function

! ------------------------------------------------------------------------------
    !> @brief Scales a quaternion.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_scalar_mult(q, p) result(z)
        ! Arguments
        type(quaternion), intent(in) :: q
        real(real64), intent(in) :: p
        type(quaternion) :: z

        ! Process
        z%w = q%w * p
        z%x = q%x * p
        z%y = q%y * p
        z%z = q%z * p
    end function

! ------------------------------------------------------------------------------
    !> @brief Scales a quaternion.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_scalar_mult2(q, p) result(z)
        ! Arguments
        real(real64), intent(in) :: q
        type(quaternion), intent(in) :: p
        type(quaternion) :: z

        ! Process
        z%w = p%w * q
        z%x = p%x * q
        z%y = p%y * q
        z%z = p%z * q
    end function

! ------------------------------------------------------------------------------
    !> @brief Divides a quaternion by a scalar.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_scalar_divide(q, p) result(z)
        ! Arguments
        type(quaternion), intent(in) :: q
        real(real64), intent(in) :: p
        type(quaternion) :: z

        ! Process
        z%w = q%w / p
        z%x = q%x / p
        z%y = q%y / p
        z%z = q%z / p
    end function

! ******************************************************************************
! PRIVATE ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Finds the index of the largest magnitude element in an array.
    !!
    !! @param[in] x The array.
    !! @return The index of the largest magnitude array.
    pure function max_abs_index(x) result(ind)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: val

        ! Process
        n = size(x)
        if (n == 0) then
            ind = 0
        else if (n == 1) then
            ind = 1
        else
            ind = 1
            val = abs(x(1))
            do i = 2, n
                if (abs(x(i)) > val) then
                    ind = i
                    val = abs(x(i))
                end if
            end do
        end if
    end function

! ------------------------------------------------------------------------------
end submodule
