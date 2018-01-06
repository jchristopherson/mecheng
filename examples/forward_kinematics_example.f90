! forward_kinematics_example.f90

! Reference Figure 5.5 from Jazar's text.
program example
    use, intrinsic :: iso_fortran_env, only : real64, int32
    use kinematics
    use constants
    implicit none

    ! Robot Geometry (Constants)
    real(real64), parameter :: offset1 = 0.0d0
    real(real64), parameter :: offset2 = 3.0d0
    real(real64), parameter :: offset3 = 1.0d0
    real(real64), parameter :: twist1 = 1.5707963267948966d0
    real(real64), parameter :: twist2 = 0.0d0
    real(real64), parameter :: twist3 = 0.0d0

    ! Local Variables
    real(real64) :: theta1, theta2, theta3, trans1, trans2, trans3
    real(real64), dimension(4, 4) :: T0_1, T1_2, T2_3, T
    real(real64), dimension(4) :: p1, k1
    integer(int32) :: i

    ! Define the joint variables
    theta1 = 0.0d0
    theta2 = 0.0d0
    theta3 = pi / 4.0d0

    trans1 = 0.0d0
    trans2 = -1.0d0
    trans3 = 0.0d0

    ! Construct the matrix defining the first link
    T0_1 = dh_mtx(theta1, trans1, twist1, offset1)
    T1_2 = dh_mtx(theta2, trans2, twist2, offset2)
    T2_3 = dh_mtx(theta3, trans3, twist3, offset3)

    ! Construct the overall linkage transformation matrix
    ! T = T0_1 * T1_2 * T2_3
    T = matmul(T0_1, matmul(T1_2, T2_3))

    ! Display the matrices
    print '(A)', "Link 1 Matrix:"
    do i = 1, 4
        print *, T0_1(i,:)
    end do

    print '(A)', "Link 2 Matrix:"
    do i = 1, 4
        print *, T1_2(i,:)
    end do

    print '(A)', "Link 3 Matrix:"
    do i = 1, 4
        print *, T2_3(i,:)
    end do

    print '(A)', "Overall Matrix:"
    do i = 1, 4
        print *, T(i,:)
    end do

    ! The origin of the end-effector is then located at the following point.
    p1 = matmul(T, [0.0d0, 0.0d0, 0.0d0, 1.0d0])
    print '(A)', "End Effector Location:"
    print *, p1(1:3)

    ! The end-effector z-axis lies along the following vector
    k1 = matmul(T, [0.0d0, 0.0d0, 1.0d0, 1.0d0]) - p1
    print '(A)', "End-Effector Z-Axis:"
    print *, k1(1:3)
end program
