! kinematics.f90

module kinematics
    use, intrinsic :: iso_fortran_env, only : real64, int32
    implicit none
    private
    public :: quaternion
    public :: operator(+)
    public :: operator(-)
    public :: operator(*)
    public :: operator(/)


! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a quaternion as a means to represent rotation.
    !!
    !! @par See Also
    !! - [Wikipedia](https://en.wikipedia.org/wiki/Quaternion)
    !! - [Wolfram MathWorld](http://mathworld.wolfram.com/Quaternion.html)
    type quaternion
        !> The w-component of the quaternion (q = x*i + y*j * z*k + w).
        real(real64) :: w = 0.0d0
        !> The x-component of the quaternion (q = x*i + y*j * z*k + w).
        real(real64) :: x = 1.0d0
        !> The y-component of the quaternion (q = x*i + y*j * z*k + w).
        real(real64) :: y = 0.0d0
        !> The z-component of the quaternion (q = x*i + y*j * z*k + w).
        real(real64) :: z = 0.0d0
    contains
        !> @brief Creates a quaternion representing the specified rotation angle
        !! about the specified rotation axis.
        procedure, public :: from_angle_axis => quaternion_from_angle_axis
        !> @brief Converts the quaternion to its equivalent angle-axis
        !! representation.
        procedure, public :: to_angle_axis => quaternion_to_angle_axis
        !> @brief Creates a quaternion equivalent to the supplied rotation 
        !!  matrix.
        procedure, public :: from_matrix => quaternion_from_matrix
        !> @brief Normalizes the quaternion to a unit quaternion.
        procedure, public :: normalize => quaternion_normalize
        !> @brief Computes the magnitude of the quaternion.
        procedure, public :: magnitude => quaternion_magnitude
        !> @brief Converts the quaternion to its equivalent rotation matrix.
        procedure, public :: to_matrix => quaternion_to_matrix
        !> @brief Converts the quaternion to an identity quaternion.
        procedure, public :: to_identity => quaternion_to_identity
        !> @brief Returns the conjugate of the quaternion.
        procedure, public :: conjugate => quaternion_conjugate
        !> @brief Returns the inverse of the quaternion.
        procedure, public :: invert => quaternion_invert
        !> @brief Extracts just the [X,Y,Z] vector component of the quaternion.
        procedure, public :: to_vector => quaternion_extract_vector
        !> @brief Applies the rotation transformation to the supplied vector.
        procedure, public :: transform => quaternion_xfrm
    end type

! ******************************************************************************
! OPERATORS
! ------------------------------------------------------------------------------
    !> @brief Overloads the + operator allowing for quaternion addition.
    interface operator (+)
        module procedure :: quaternion_add
    end interface

    !> @brief Overloads the - operator allowing for quaternion subtraction.
    interface operator (-)
        module procedure :: quaternion_subtract
    end interface

    !> @brief Overloads the * operator allowing for quaternion multiplication.
    interface operator (*)
        module procedure :: quaternion_mult
        module procedure :: quaterion_vector_mult
        module procedure :: vector_quaternion_mult
        module procedure :: quaternion_scalar_mult
        module procedure :: quaternion_scalar_mult2
    end interface

    !> @brief Overloads the / operator allowing for quaternion division.
    interface operator (/)
        module procedure :: quaternion_scalar_divide
    end interface

! ******************************************************************************
! KINEMATICS_QUATERNION
! ------------------------------------------------------------------------------
interface
    !> @brief Creates a quaternion representing the specified rotation angle
    !! about the specified rotation axis.
    !!
    !! @param[in,out] this The quaternion object.
    !! @param[in] angle The rotation angle, in radians.
    !! @param[in] axis A 3-element vector containing the x, y, and z components
    !!  of the rotation axis.  The vector is expected to be a unit vector.
    module subroutine quaternion_from_angle_axis(this, angle, axis)
        class(quaternion), intent(inout) :: this
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: axis
    end subroutine
    
    !> @brief Converts the quaternion to its equivalent angle-axis
    !! representation.
    !!
    !! @param[in] this The quaternion object.
    !! @param[out] angle The equivalent rotation angle, in radians.
    !! @param[out] axis A 3-element array that on output contains the equivalent
    !!  axis of rotation.
    module subroutine quaternion_to_angle_axis(this, angle, axis)
        class(quaternion), intent(in) :: this
        real(real64), intent(out) :: angle
        real(real64), intent(out), dimension(3) :: axis
    end subroutine
    
    !> @brief Creates a quaternion equivalent to the supplied rotation matrix.
    !!
    !! @param[in,out] this The quaternion object.
    !! @param[in] x The 3-by-3 rotation matrix.  It is assumed that this is a
    !!  valid, orthogonal rotation matrix.  Matrix orthogonality is not
    !!  verified.
    module subroutine quaternion_from_matrix(this, x)
        class(quaternion), intent(inout) :: this
        real(real64), intent(in), dimension(3,3) :: x
    end subroutine
    
    !> @brief Normalizes the quaternion to a unit quaternion.
    !!
    !! @param[in,out] this The quaternion object.
    module subroutine quaternion_normalize(this)
        class(quaternion), intent(inout) :: this
    end subroutine
    
    !> @brief Computes the magnitude of the quaternion.
    !!
    !! @param[in] this The quaternion object.
    !!
    !! @return The magnitude of the quaternion.
    pure module function quaternion_magnitude(this) result(x)
        class(quaternion), intent(in) :: this
        real(real64) :: x
    end function
    
    !> @brief Converts the quaternion to its equivalent rotation matrix.
    !!
    !! @param[in] this The quaternion object.
    !!
    !! @return The resulting 3-by-3 rotation matrix.
    pure module function quaternion_to_matrix(this) result(x)
        class(quaternion), intent(in) :: this
        real(real64), dimension(3,3) :: x
    end function
    
    !> @brief Converts the quaternion to an identity quaternion.
    !!
    !! @param[in,out] this The quaternion object.
    module subroutine quaternion_to_identity(this)
        class(quaternion), intent(inout) :: this
    end subroutine
    
    !> @brief Returns the conjugate of the quaternion.
    !!
    !! @param[in] this The quaternion object.
    !! @return The conjugate of the quaternion.
    pure module function quaternion_conjugate(this) result(q)
        class(quaternion), intent(in) :: this
        type(quaternion) :: q
    end function
    
    !> @brief Returns the inverse of the quaternion.
    !!
    !! @param[in] this The quaternion object.
    !! @return The inverse of the quaternion.
    pure module function quaternion_invert(this) result(q)
        class(quaternion), intent(in) :: this
        type(quaternion) :: q
    end function
    
    !> @brief Extracts just the [X,Y,Z] vector component of the quaternion.
    !!
    !! @param[in] this The quaternion object.
    !! @return The [X,Y,Z] vector component.
    pure module function quaternion_extract_vector(this) result(x)
        class(quaternion), intent(in) :: this
        real(real64), dimension(3) :: x
    end function
    
    !> @brief Applies the rotation transformation to the supplied vector.
    !!
    !! @param[in] this The quaternion.
    !! @param[in] x The 3-element vector to transform.
    !! @return The resulting 3-element vector.
    pure module function quaternion_xfrm(this, x) result(y)
        class(quaternion), intent(in) :: this
        real(real64), intent(in), dimension(3) :: x
        real(real64), dimension(3) :: y
    end function
    
    !> @brief Adds two quaternions.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_add(q, p) result(z)
        type(quaternion), intent(in) :: q, p
        type(quaternion) :: z
    end function
    
    !> @brief Subtracts two quaternions.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_subtract(q, p) result(z)
        type(quaternion), intent(in) :: q, p
        type(quaternion) :: z
    end function
    
    !> @brief Multiplies two quaternions.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_mult(q, p) result(z)
        type(quaternion), intent(in) :: q, p
        type(quaternion) :: z
    end function

    !> @brief Multiplies a quaternion and a vector.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaterion_vector_mult(q, p) result(z)
        type(quaternion), intent(in) :: q
        real(real64), intent(in), dimension(3) :: p
        type(quaternion) :: z
    end function
    
    !> @brief Multiplies a vector and a quaternion.
    !!
    !! @param[in] x The left-hand-side argument.
    !! @param[in] y The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function vector_quaternion_mult(x, y) result(z)
        real(real64), intent(in), dimension(3) :: x
        type(quaternion), intent(in) :: y
        type(quaternion) :: z
    end function
    
    !> @brief Scales a quaternion.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_scalar_mult(q, p) result(z)
        type(quaternion), intent(in) :: q
        real(real64), intent(in) :: p
        type(quaternion) :: z
    end function
    
    !> @brief Scales a quaternion.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_scalar_mult2(q, p) result(z)
        real(real64), intent(in) :: q
        type(quaternion), intent(in) :: p
        type(quaternion) :: z
    end function
    
    !> @brief Divides a quaternion by a scalar.
    !!
    !! @param[in] q The left-hand-side argument.
    !! @param[in] p The right-hand-side argument.
    !! @return The resulting quaternion.
    pure module function quaternion_scalar_divide(q, p) result(z)
        type(quaternion), intent(in) :: q
        real(real64), intent(in) :: p
        type(quaternion) :: z
    end function
end interface

end module