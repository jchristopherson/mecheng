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
    public :: rotate_x
    public :: rotate_y
    public :: rotate_z
    public :: translate
    public :: rotate
    public :: dh_mtx
    public :: to_skew_symmetric
    public :: velocity_mtx
    public :: acceleration_mtx


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

! ******************************************************************************
! KINEMATICS_TRANSFORMS
! ------------------------------------------------------------------------------
    !> @brief Defines a rotation matrix describing rotation about the x-axis.
    interface rotate_x
        module procedure :: rotate_x_mtx
        module procedure :: rotate_x_mtx_trans_comp
        module procedure :: rotate_x_mtx_trans
    end interface

! ------------------------------------------------------------------------------
    !> @brief Defines a rotation matrix describing rotation about the y-axis.
    interface rotate_y
        module procedure :: rotate_y_mtx
        module procedure :: rotate_y_mtx_trans_comp
        module procedure :: rotate_y_mtx_trans
    end interface

! ------------------------------------------------------------------------------
    !> @brief Defines a rotation matrix describing rotation about the z-axis.
    interface rotate_z
        module procedure :: rotate_z_mtx
        module procedure :: rotate_z_mtx_trans_comp
        module procedure :: rotate_z_mtx_trans
    end interface

! ------------------------------------------------------------------------------
    !> @brief Defines a transformation matrix describing a rigid body 
    !! translation.
    interface translate
        module procedure :: translate_mtx_comp
        module procedure :: translate_mtx
    end interface

! ------------------------------------------------------------------------------
    !> @brief Computes a rotation matrix.
    interface rotate
        module procedure :: rotate_mtx_gen
    end interface

! ------------------------------------------------------------------------------
    !> @brief Computes the Denavit-Hartenberg transformation matrix.
    interface dh_mtx
        module procedure :: dh_mtx_implement
    end interface

! ------------------------------------------------------------------------------
    !> @brief Converts a three-element vector into its skew-symmetric matrix 
    !! form.
    interface to_skew_symmetric
        module procedure :: to_skew_symm_implement
    end interface

! ------------------------------------------------------------------------------
    !> @brief Provides a 4-by-4 matrix allowing the calculation of the velocity
    !! of any point on a moving rigid body in terms of its parent coordinate
    !! frame.
    interface velocity_mtx
        module procedure :: velocity_mtx_implement
    end interface

! ------------------------------------------------------------------------------
    !> @brief Provides a 4-by-4 matrix allowing the calculation of the
    !! acceleration of any point on a moving rigid body in terms of its parent
    !! coordinate frame.
    interface acceleration_mtx
        module procedure :: accel_mtx_implement
    end interface

! ------------------------------------------------------------------------------
interface
    !> @brief Defines a rotation matrix describing rotation about the x-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @return The 3-by-3 rotation matrix.
    !!
    !! @par Remarks
    !! The x-axis rotation matrix is defined as follows:
    !! @verbatim
    !!      | 1    0      0  |
    !! Rx = | 0   cos   -sin |
    !!      | 0   sin    cos |
    !! @endverbatim
    pure module function rotate_x_mtx(angle) result(x)
        real(real64), intent(in) :: angle
        real(real64), dimension(3, 3) :: x
    end function
    
    !> @brief Defines a homogeneous rotation matrix describing a translation as
    !! well as a rotation about the x-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @param[in] x The x translation.
    !! @param[in] y The y translation.
    !! @param[in] z The z translation.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Remarks
    !! The transformation matrix is defined as follows:
    !! @verbatim
    !!      | 1    0      0     x |
    !! Rx = | 0   cos   -sin    y |
    !!      | 0   sin    cos    z |
    !!      | 0    0      0     1 |
    !! @endverbatim
    pure module function rotate_x_mtx_trans_comp(angle, x, y, z) result(r)
        real(real64), intent(in) :: angle, x, y, z
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Defines a homogeneous rotation matrix describing a translation as
    !! well as a rotation about the x-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @param[in] trans A 3-component array defining the [x, y, z] translation.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Remarks
    !! The transformation matrix is defined as follows:
    !! @verbatim
    !!      | 1    0      0     x |
    !! Rx = | 0   cos   -sin    y |
    !!      | 0   sin    cos    z |
    !!      | 0    0      0     1 |
    !! @endverbatim
    pure module function rotate_x_mtx_trans(angle, trans) result(r)
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: trans
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Defines a rotation matrix describing rotation about the y-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @return The 3-by-3 rotation matrix.
    !!
    !! @par Remarks
    !! The y-axis rotation matrix is defined as follows:
    !! @verbatim
    !!      | cos   0   sin |
    !! Ry = |  0    1    0  |
    !!      |-sin   0   cos |
    !! @endverbatim
    pure module function rotate_y_mtx(angle) result(x)
        real(real64), intent(in) :: angle
        real(real64), dimension(3, 3) :: x
    end function
    
    !> @brief Defines a homogeneous rotation matrix describing a translation as
    !! well as a rotation about the y-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @param[in] x The x translation.
    !! @param[in] y The y translation.
    !! @param[in] z The z translation.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Remarks
    !! The transformation matrix is defined as follows:
    !! @verbatim
    !!      | cos   0   sin   x |
    !! Ry = |  0    1    0    y |
    !!      |-sin   0   cos   z |
    !!      |  0    0    0    1 |
    !! @endverbatim
    pure module function rotate_y_mtx_trans_comp(angle, x, y, z) result(r)
        real(real64), intent(in) :: angle, x, y, z
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Defines a homogeneous rotation matrix describing a translation as
    !! well as a rotation about the y-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @param[in] trans A 3-component array defining the [x, y, z] translation.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Remarks
    !! The transformation matrix is defined as follows:
    !! @verbatim
    !!      | cos   0   sin   x |
    !! Ry = |  0    1    0    y |
    !!      |-sin   0   cos   z |
    !!      |  0    0    0    1 |
    !! @endverbatim
    pure module function rotate_y_mtx_trans(angle, trans) result(r)
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: trans
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Defines a rotation matrix describing rotation about the z-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @return The 3-by-3 rotation matrix.
    !!
    !! @par Remarks
    !! The z-axis rotation matrix is defined as follows:
    !! @verbatim
    !!      | cos  -sin   0 |
    !! Rz = | sin   cos   0 |
    !!      |  0     0    1 |
    !! @endverbatim
    pure module function rotate_z_mtx(angle) result(x)
        real(real64), intent(in) :: angle
        real(real64), dimension(3, 3) :: x
    end function
    
    !> @brief Defines a homogeneous rotation matrix describing a translation as
    !! well as a rotation about the z-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @param[in] x The x translation.
    !! @param[in] y The y translation.
    !! @param[in] z The z translation.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Remarks
    !! The transformation matrix is defined as follows:
    !! @verbatim
    !!      | cos  -sin   0    x |
    !! Rz = | sin   cos   0    y |
    !!      |  0     0    1    z |
    !!      |  0     0    0    1 |
    !! @endverbatim
    pure module function rotate_z_mtx_trans_comp(angle, x, y, z) result(r)
        real(real64), intent(in) :: angle, x, y, z
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Defines a homogeneous rotation matrix describing a translation as
    !! well as a rotation about the z-axis.
    !!
    !! @param[in] angle The rotation angle, in radians.
    !! @param[in] trans A 3-component array defining the [x, y, z] translation.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Remarks
    !! The transformation matrix is defined as follows:
    !! @verbatim
    !!      | cos  -sin   0    x |
    !! Rz = | sin   cos   0    y |
    !!      |  0     0    1    z |
    !!      |  0     0    0    1 |
    !! @endverbatim
    pure module function rotate_z_mtx_trans(angle, trans) result(r)
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: trans
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Defines a transformation matrix describing a rigid body 
    !! translation.
    !!
    !! @param[in] x The x translation.
    !! @param[in] y The y translation.
    !! @param[in] z The z translation.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Remarks
    !! The transformation matrix is defined as follows:
    !! @verbatim
    !!     | 1    0    0    x |
    !! R = | 0    1    0    y |
    !!     | 0    0    1    z |
    !!     | 0    0    0    1 |
    !! @endverbatim
    pure module function translate_mtx_comp(x, y, z) result(r)
        real(real64), intent(in) :: x, y, z
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Defines a transformation matrix describing a rigid body 
    !! translation.
    !!
    !! @param[in] trans A 3-component array defining the [x, y, z] translation.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Remarks
    !! The transformation matrix is defined as follows:
    !! @verbatim
    !!     | 1    0    0    x |
    !! R = | 0    1    0    y |
    !!     | 0    0    1    z |
    !!     | 0    0    0    1 |
    !! @endverbatim
    pure module  function translate_mtx(trans) result(r)
        real(real64), intent(in), dimension(3) :: trans
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Computes a rotation matrix.
    !!
    !! @param[in] i The parent x-axis unit vector.
    !! @param[in] j The parent y-axis unit vector.
    !! @param[in] k The parent z-axis unit vector.
    !! @param[in] ci The child x-axis unit vector in terms of the parent 
    !!  coordinate system.
    !! @param[in] cj The child y-axis unit vector in terms of the parent
    !!  coordinate system.
    !! @param[in] ck The child z-axis unit vector in terms of the parent
    !!  coordinate system.
    !! @return The 3-by-3 rotation matrix.
    !!
    !! @par Remarks
    !! The rotation matrix is constructed as follows, where I, J, and K are
    !! the unit vectors of the parent coordinate frame, and i, j, and k are the
    !! unit vectors of the rotated coordinate frame expressed in terms of the
    !! parent coordinate frame.
    !! @verbatim
    !!     | I * i   I * j   I * k|
    !! x = | J * i   J * j   J * k|
    !!     | K * i   K * j   K * k|
    !! @endverbatim
    pure module function rotate_mtx_gen(i, j, k, ci, cj, ck) result(r)
        real(real64), intent(in), dimension(3) :: i, j, k, ci, cj, ck
        real(real64), dimension(3, 3) :: r
    end function
    
    !> @brief Computes the Denavit-Hartenberg transformation matrix.
    !!
    !! @param[in] theta The rotation angle about the z(i-1) axis, in radians.
    !! @param[in] trans The translation along the z(i-1) axis.
    !! @param[in] twist The rotation angle about the x(i-1) axis, in radians.
    !! @param[in] length The translation along the x(i-1) axis.
    !! @return The 4-by-4 transformation matrix.
    !!
    !! @par Denavit-Hartenberg Coordinate Frame Construction
    !! A Denavit-Hartenberg coordinate frame is constructed by assigning the
    !! z-axis to be coincident with the joint axis, and the x-axis is parallel
    !! to the common normal between the distal and proximal joints such that
    !! x(i) = z(i) * z(i-1), where * represents a vector cross-product.  If
    !! there is no unique common normal (i.e. parallel z axes), then it is
    !! convenient to define x(i) to be collinear with the common normal of the
    !! previous joint.  Another case is if z(i) and z(i-1) intersect.  In this 
    !! event it is best to assign x(i) to be perpendicular to the plane formed
    !! by the two z-axes in the direction of z(i-1) * z(i) (again * represents
    !! a vector cross-product).  Lastly, the case where z(i-1) and z(i) are
    !! collinear, the only nontrivial joint arrangement is prismatic||revolute,
    !! or revolute||prismatic.  Therefore, the x(i) axis should be defined 
    !! such that t(i) (the joint angle about z(i-1)) is set to zero in the
    !! mechanism's rest position.
    !!
    !! @par Remarks
    !! The Denavit-Hartenberg (DH) matrix is constructed as the product of 4
    !! basic transformation matrices describing link (i) as its attached at its
    !! proximal joint (joint i) by link (i-1).
    !!
    !! @verbatim
    !! Twist Rotation Matrix:
    !!           | 1       0            0       0 |
    !! Rx(i-1) = | 0    cos(a(i))   -sin(a(i))  0 |
    !!           | 0    sin(a(i))    cos(a(i))  0 |
    !!           | 0       0            0       1 |
    !! Where a(i) is the link twist angle about the x(i-1) axis.
    !!
    !! Link Length Translation Matrix:
    !!           | 1    0   0   s(i) |
    !! Dx(i-1) = | 0    1   0    0   |
    !!           | 0    0   1    0   |
    !!           | 0    0   0    1   |
    !! Where s(i) is the link translation (length) along the x(i-1) axis.
    !!
    !! Joint Rotation Matrix:
    !!           | cos(t(i))    -sin(t(i))   0      0 |
    !! Rz(i-1) = | sin(t(i))     cos(t(i))   0      0 |
    !!           |    0             0        1      0 |
    !!           |    0             0        0      1 |
    !! Where t(i) is the joint rotation about the z(i-1) axis.
    !!
    !! Joint Translation Matrix:
    !!           | 1    0   0    0   |
    !! Dz(i-1) = | 0    1   0    0   |
    !!           | 0    0   1   d(i) |
    !!           | 0    0   0    1   |
    !! Where d(i) is the joint translation along the z(i-1) axis.
    !! @endverbatim
    !!
    !! The above matrices combine into the complete DH transformation matrix
    !! by observing the following sequence of transformations.  The assumption
    !! will be made that two coordinate frames B(i) and B(i-1) exist at the
    !! current configuration, and that they are initially coincident.
    !! 1. Rotate the B(i) frame through angle a(i) around the x(i-1) axis.
    !! 2. Translate the B(i) frame along the x(i-1) axis by a distance s(i).
    !! 3. Rotate the B(i) frame through angle t(i) around the z(i-1) axis.
    !! 4. Translate the frame B(i) along the z(i-1) axis by distance d(i).
    !!
    !! Applying the above transformation sequence results in the DH 
    !! transformation matrix for link (i) in terms of link (i-1).
    !! @verbatim
    !! T(i,i-1) = Dz(i-1) * Rz(i-1) * Dx(i-1) * Rx(i-1)
    !!
    !! Expanding the above expression yields the following matrix.
    !!            | cos(t(i))   -sin(t(i))*cos(a(i))    sin(t(i))*sin(a(i))     s(i)*cos(t(i)) |
    !! T(i,i-1) = | sin(t(i))    cos(t(i))*cos(a(i))   -cos(t(i))*sin(a(i))     s(i)*sin(t(i)) |
    !!            |    0              sin(a(i))              cos(a(i))                d(i)     |
    !!            |    0                 0                      0                      1       |
    !! @endverbatim
    !! An additional note to remember when using DH notation.  The coordinate
    !! frame of link (i) is B(i), and it is fixed at the distal joint of the
    !! link.  The proximal joint of the link is attached to link (i-1).
    pure module function dh_mtx_implement(theta, trans, twist, length) result(r)
        real(real64), intent(in) :: theta, trans, twist, length
        real(real64), dimension(4, 4) :: r
    end function
    
    !> @brief Converts a three-element vector into its skew-symmetric matrix 
    !! form.
    !!
    !! @param[in] x The 3-element vector.
    !! @return The skew-symemtric 3-by-3 matrix representation of @p x.
    pure module function to_skew_symm_implement(x) result(y)
        ! Arguments
        real(real64), intent(in), dimension(3) :: x
        real(real64), dimension(3, 3) :: y
    end function
    
    !> @brief Provides a 4-by-4 matrix allowing the calculation of the velocity
    !! of any point on a moving rigid body in terms of its parent coordinate
    !! frame.
    !!
    !! @param[in] omega A 3-element vector defining the angular velocity 
    !!  components of the rotating body as described in the parent coordinate
    !!  frame.
    !! @param[in] v A 3-element vector defining the velocity of the body as
    !!  described in the parent coordinate frame.
    !! @param[in] r A 3-element vector defining the location of the body 
    !!  coordinate frame as described in the parent coordinate frame.
    !! @return The 4-by-4 velocity transformation matrix.
    !!
    !! @par Remarks
    !! The velocity transformation matrix allows expression of the velocity of
    !! any point on a rigid body when the position, velocity, and angular 
    !! velocity of said body are known.  As such, the velocity transformation
    !! matrix allows for the following relationship.
    !! @verbatim
    !! v(p) = T * r(p)
    !! where:
    !! r(p) is the location of the point of interest.
    !! T is the velocity transformation matrix.
    !! v(p) is the velocity of point p.
    !!
    !! The velocity transformation matrix is defined as follows.
    !!     | w  (v(b) - w * r(b)) |
    !! T = |                      |
    !!     | 0           0        |
    !! where:
    !!     | 0     -w3     w2 |
    !! w = | w3     0     -w1 |, (w's are angular velocity components)
    !!     |-w2     w1     0  |
    !!
    !!        | x(b) |             | vx(b) |
    !! r(b) = | y(b) |, and v(b) = | vy(b) |
    !!        | z(b) |             | vz(b) |
    !! @endverbatim
    pure module function velocity_mtx_implement(omega, v, r) result(t)
        real(real64), intent(in), dimension(3) :: omega, v, r
        real(real64), dimension(4, 4) :: t
    end function
    
    !> @brief Provides a 4-by-4 matrix allowing the calculation of the
    !! acceleration of any point on a moving rigid body in terms of its parent
    !! coordinate frame.
    !!
    !! @param[in] alpha A 3-element vector defining the angular acceleration 
    !!  components of the rotation body as described in the parent coordinate
    !!  frame.
    !! @param[in] omega A 3-element vector defining the angular velocity 
    !!  components of the rotating body as described in the parent coordinate
    !!  frame.
    !! @param[in] a A 3-element vector defining the acceleration of the body as
    !!  described in the parent coordinate frame.
    !! @param[in] r A 3-element vector defining the location of the body 
    !!  coordinate frame as described in the parent coordinate frame.
    !! @return The 4-by-4 acceleration transformation matrix.
    !!
    !! @par Remarks
    !! The acceleration transformation matrix allows expression of the velocity
    !! of any point on a rigid body when the position, acceleration, angular 
    !! acceleration, and angular velocity of said body are known.  As such, the 
    !! acceleration transformation matrix allows for the following relationship.
    !! @verbatim
    !! a(p) = T * r(p)
    !! where:
    !! r(p) is the location of the point of interest.
    !! T is the acceleration transformation matrix.
    !! a(p) is the acceleration of point p.
    !!
    !! The acceleration transformation matrix is defined as follows.
    !!     | q  (a(b) - q * r(b)) |
    !! T = |                      |
    !!     | 0           0        |
    !! where:
    !! q = b - w * w**T
    !!     | 0     -b3     b2 |
    !! b = | b3     0     -b1 |, (b's are angular acceleration components)
    !!     |-b2     b1     0  |
    !!
    !!     | 0     -w3     w2 |
    !! w = | w3     0     -w1 |, (w's are angular velocity components)
    !!     |-w2     w1     0  |
    !! 
    !!        | x(b) |             | ax(b) |
    !! r(b) = | y(b) |, and a(b) = | ay(b) |
    !!        | z(b) |             | az(b) |
    !! @endverbatim
    pure module function accel_mtx_implement(alpha, omega, a, r) result(t)
        real(real64), intent(in), dimension(3) :: alpha, omega, a, r
        real(real64), dimension(4, 4) :: t
    end function
end interface

end module