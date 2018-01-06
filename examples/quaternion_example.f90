! quaternion_example.f90

program example
    use, intrinsic :: iso_fortran_env, only : real64, int32
    use kinematics
    implicit none

    ! Constants
    real(real64), parameter :: pi = 3.14159265359d0

    ! Local Variables
    type(quaternion) :: qx, qy, qz, qzyx, qs
    real(real64), dimension(3, 3) :: rx, ry, rz, rzyx, qrzyx
    real(real64), dimension(3) :: s1a, s1b, s1q, s1q2
    real(real64) :: ax, ay, az
    integer(int32) :: i

    ! Initialization
    ax = pi / 4.0d0
    ay = -pi / 3.0d0
    az = pi / 6.0d0
    call qx%from_angle_axis(ax, [1.0d0, 0.0d0, 0.0d0]) ! X Rotation
    call qy%from_angle_axis(ay, [0.0d0, 1.0d0, 0.0d0]) ! Y Rotation
    call qz%from_angle_axis(az, [0.0d0, 0.0d0, 1.0d0]) ! Z Rotation
    rx = rotate_x(ax)
    ry = rotate_y(ay)
    rz = rotate_z(az)
    call random_number(s1a)

    ! Compute a Z-Y-X rotation sequance using both matrices and quaternions
    qzyx = qx * qy * qz
    rzyx = matmul(rx, matmul(ry, rz))

    ! Convert the quaternion to a matrix for display purposes
    call qzyx%normalize()
    qrzyx = qzyx%to_matrix()

    ! Display the rotation matrices
    print '(A)', "Rotation Matrix:"
    do i = 1, 3
        print *, rzyx(i,:)
    end do

    print '(A)', "Quaternion Generated Matrix:"
    do i = 1, 3
        print *, qrzyx(i,:)
    end do

    ! Compute: s1b = Rzyx * s1a
    s1b = matmul(rzyx, s1a)

    ! Apply the transformation via quaternion
    qs = qzyx * s1a * qzyx%conjugate()
    s1q = qs%to_vector()

    ! Another example, using the quaternion transform routine
    s1q2 = qzyx%transform(s1a)

    ! Display the results
    print '(A)', new_line('a') // "s1a:"
    print *, s1a

    print '(A)', "Rzyx * s1a:"
    print *, s1b

    print '(A)', "qzyx * s1a * qzyx':"
    print *, s1q

    print '(A)', "qzyx%transform:"
    print *, s1q2
end program
