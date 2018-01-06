! test_kinematics.f90

program main
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use kinematics
    implicit none

    ! Local Variables
    logical :: rst, overall

    ! Initialization
    overall = .true.

    ! Tests
    rst = qtest_x()
    if (.not.rst) overall = .false.

    rst = qtest_y()
    if (.not.rst) overall = .false.

    rst = qtest_z()
    if (.not.rst) overall = .false.

    rst = qtest_angle_axis()
    if (.not.rst) overall = .false.

    rst = qtest_matrix()
    if (.not.rst) overall = .false.

    ! End
    if (overall) then
        print '(A)', "KINEMATICS TEST STATUS: PASS"
    else
        print '(A)', "KINEMATICS TEST STATUS: FAIL"
    end if

contains
! ******************************************************************************
! HELPER ROUTINES
! ------------------------------------------------------------------------------
    pure function is_mtx_equal(x, y, tol) result(check)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: x, y
        real(real64), intent(in) :: tol
        logical :: check

        ! Local Variables
        integer(int32) :: i, j, m, n
        
        ! Process
        check = .true.
        m = size(x, 1)
        n = size(x, 2)
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            check = .false.
            return
        end if
        do j = 1, n
            do i = 1, m
                if (abs(x(i,j) - y(i,j)) > tol) then
                    check = .false.
                    return
                end if
            end do
        end do
    end function

! ------------------------------------------------------------------------------
    pure function is_vec_equal(x, y, tol) result(check)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), intent(in) :: tol
        logical :: check

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        check = .true.
        n = size(x)
        if (size(y) /= n) then
            check = .false.
            return
        end if
        do i = 1, n
            if (abs(x(i) - y(i)) > tol) then
                check = .false.
                return
            end if
        end do
    end function

! ******************************************************************************
! QUATERNION TESTS
! ------------------------------------------------------------------------------
    function qtest_x() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        real(real64), parameter :: tol = 1.0d-12

        ! Local Variables
        type(quaternion) :: q
        real(real64) :: r(3, 3), angle, axis(3), qr(3, 3), pt(3), ptr(3), ptq(3)

        ! Initialization
        rst = .true.

        ! Define a rotation angle, and compute a rotation matrix
        call random_number(angle)
        r = rotate_x(angle)

        ! Define the rotation axis, and then define the quaternion
        axis = [1.0d0, 0.0d0, 0.0d0]
        call q%from_angle_axis(angle, axis)

        ! Ensure the quaternion is a unit quaternion, and then compute it's
        ! equivalent rotation matrix
        call q%normalize()
        qr = q%to_matrix()

        ! Compare matrices
        if (.not.is_mtx_equal(r, qr, tol)) then
            rst = .false.
            print '(A)', "TEST FAILED: Quaternion x-rotation matrix comparison."
        end if

        ! Apply the rotation to a vector and compare both results
        call random_number(pt)
        ptr = matmul(r, pt)
        ptq = q%transform(pt)
        if (.not.is_vec_equal(ptr, ptq, tol)) then
            rst = .false.
            print '(A)', "TEST FAILED: Quaternion x-rotation application."
        end if
    end function

! ------------------------------------------------------------------------------
    function qtest_y() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        real(real64), parameter :: tol = 1.0d-12

        ! Local Variables
        type(quaternion) :: q
        real(real64) :: r(3, 3), angle, axis(3), qr(3, 3), pt(3), ptr(3), ptq(3)

        ! Initialization
        rst = .true.

        ! Define a rotation angle, and compute a rotation matrix
        call random_number(angle)
        r = rotate_y(angle)

        ! Define the rotation axis, and then define the quaternion
        axis = [0.0d0, 1.0d0, 0.0d0]
        call q%from_angle_axis(angle, axis)

        ! Ensure the quaternion is a unit quaternion, and then compute it's
        ! equivalent rotation matrix
        call q%normalize()
        qr = q%to_matrix()

        ! Compare matrices
        if (.not.is_mtx_equal(r, qr, tol)) then
            rst = .false.
            print '(A)', "TEST FAILED: Quaternion y-rotation matrix comparison."
        end if

        ! Apply the rotation to a vector and compare both results
        call random_number(pt)
        ptr = matmul(r, pt)
        ptq = q%transform(pt)
        if (.not.is_vec_equal(ptr, ptq, tol)) then
            rst = .false.
            print '(A)', "TEST FAILED: Quaternion y-rotation application."
        end if
    end function

! ------------------------------------------------------------------------------
    function qtest_z() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        real(real64), parameter :: tol = 1.0d-12

        ! Local Variables
        type(quaternion) :: q
        real(real64) :: r(3, 3), angle, axis(3), qr(3, 3), pt(3), ptr(3), ptq(3)

        ! Initialization
        rst = .true.

        ! Define a rotation angle, and compute a rotation matrix
        call random_number(angle)
        r = rotate_z(angle)

        ! Define the rotation axis, and then define the quaternion
        axis = [0.0d0, 0.0d0, 1.0d0]
        call q%from_angle_axis(angle, axis)

        ! Ensure the quaternion is a unit quaternion, and then compute it's
        ! equivalent rotation matrix
        call q%normalize()
        qr = q%to_matrix()

        ! Compare matrices
        if (.not.is_mtx_equal(r, qr, tol)) then
            rst = .false.
            print '(A)', "TEST FAILED: Quaternion z-rotation matrix comparison."
        end if

        ! Apply the rotation to a vector and compare both results
        call random_number(pt)
        ptr = matmul(r, pt)
        ptq = q%transform(pt)
        if (.not.is_vec_equal(ptr, ptq, tol)) then
            rst = .false.
            print '(A)', "TEST FAILED: Quaternion z-rotation application."
        end if
    end function

! ------------------------------------------------------------------------------
    function qtest_angle_axis() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        real(real64), parameter :: tol = 1.0d-12

        ! Local Variables
        type(quaternion) :: q
        real(real64) :: angle, axis(3), cAngle, cAxis(3)

        ! Initialization
        rst = .true.
        call random_number(angle)
        call random_number(axis)

        ! Before constructing the quaternion, normalize the rotation axis to
        ! a unit vector
        axis = axis / norm2(axis)

        ! Build the quaternion
        call q%from_angle_axis(angle, axis)

        ! Return the angle and axis as reconstructed from the quaternion
        call q%to_angle_axis(cAngle, cAxis)

        ! Test
        if (abs(angle - cAngle) > tol) then
            rst = .false.
            print '(AF8.6AF8.6A)', "TEST FAILED: Quaternion angle " // &
                "reconstruction failed.  Expected ", angle, ", but found ", &
                cAngle, "."
        end if
        if (.not.is_vec_equal(axis, cAxis, tol)) then
            rst = .false.
            print '(AF8.6AF8.6AF8.6AF8.6AF8.6AF8.6A)', &
                "TEST FAILED: Quaternion axis reconstruction failed.  " // &
                "Expected (", axis(1), ", ", axis(2), ", ", axis(3), &
                "), but found (", cAxis(1), ", ", cAxis(2), ", ", cAxis(3), ")."
        end if
    end function

! ------------------------------------------------------------------------------
    function qtest_matrix() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        real(real64), parameter :: tol = 1.0d-12

        ! Local Variables
        type(quaternion) :: q
        real(real64) :: r(3, 3), rx(3, 3), ry(3, 3), angleX, angleY, p(3), &
            rp(3), qp(3)

        ! Initialization
        rst = .true.
        call random_number(angleX)
        call random_number(angleY)
        call random_number(p)
        rx = rotate_x(angleX)
        ry = rotate_y(angleY)
        r = matmul(ry, rx)

        ! Construct a quaternion from a rotation matrix
        call q%from_matrix(r)

        ! Transform a vector using both the rotation matrix, and the quaternion
        rp = matmul(r, p)
        qp = q%transform(p)

        ! Compare the results
        if (.not.is_vec_equal(rp, qp, tol)) then
            rst = .false.
            print '(A)', "TEST FAILED: Quaternion construction from a matrix."
        end if
    end function

! ------------------------------------------------------------------------------

end program