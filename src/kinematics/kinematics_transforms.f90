! kinematics_transforms.f90

submodule (kinematics) kinematics_transforms
contains
! ------------------------------------------------------------------------------
    pure module function rotate_x_mtx(angle) result(x)
        ! Arguments
        real(real64), intent(in) :: angle
        real(real64), dimension(3, 3) :: x

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Local Variables
        real(real64) :: cx, sx

        ! Process
        cx = cos(angle)
        sx = sin(angle)
        x = reshape([one, zero, zero, zero, cx, sx, zero, -sx, cx], [3, 3])
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_x_mtx_trans_comp(angle, x, y, z) result(r)
        ! Arguments
        real(real64), intent(in) :: angle, x, y, z
        real(real64), dimension(4, 4) :: r

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Process
        r(1:3,1:3) = rotate_x(angle)
        r(1,4) = x
        r(2,4) = y
        r(3,4) = z
        r(4,4) = one
        r(4,1:3) = zero
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_x_mtx_trans(angle, trans) result(r)
        ! Arguments
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: trans
        real(real64), dimension(4, 4) :: r

        ! Process
        r = rotate_x(angle, trans(1), trans(2), trans(3))
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_y_mtx(angle) result(x)
        ! Arguments
        real(real64), intent(in) :: angle
        real(real64), dimension(3, 3) :: x

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Local Variables
        real(real64) :: cx, sx

        ! Process
        cx = cos(angle)
        sx = sin(angle)
        x = reshape([cx, zero, -sx, zero, one, zero, sx, zero, cx], [3, 3])
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_y_mtx_trans_comp(angle, x, y, z) result(r)
        ! Arguments
        real(real64), intent(in) :: angle, x, y, z
        real(real64), dimension(4, 4) :: r

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Process
        r(1:3,1:3) = rotate_y(angle)
        r(1,4) = x
        r(2,4) = y
        r(3,4) = z
        r(4,4) = one
        r(4,1:3) = zero
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_y_mtx_trans(angle, trans) result(r)
        ! Arguments
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: trans
        real(real64), dimension(4, 4) :: r

        ! Process
        r = rotate_y(angle, trans(1), trans(2), trans(3))
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_z_mtx(angle) result(x)
        ! Arguments
        real(real64), intent(in) :: angle
        real(real64), dimension(3, 3) :: x

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Local Variables
        real(real64) :: cx, sx

        ! Process
        cx = cos(angle)
        sx = sin(angle)
        x = reshape([cx, sx, zero, -sx, cx, zero, zero, zero, one], [3, 3])
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_z_mtx_trans_comp(angle, x, y, z) result(r)
        ! Arguments
        real(real64), intent(in) :: angle, x, y, z
        real(real64), dimension(4, 4) :: r

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Process
        r(1:3,1:3) = rotate_z(angle)
        r(1,4) = x
        r(2,4) = y
        r(3,4) = z
        r(4,4) = one
        r(4,1:3) = zero
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_z_mtx_trans(angle, trans) result(r)
        ! Arguments
        real(real64), intent(in) :: angle
        real(real64), intent(in), dimension(3) :: trans
        real(real64), dimension(4, 4) :: r

        ! Process
        r = rotate_y(angle, trans(1), trans(2), trans(3))
    end function

! ------------------------------------------------------------------------------
    pure module function translate_mtx_comp(x, y, z) result(r)
        ! Arguments
        real(real64), intent(in) :: x, y, z
        real(real64), dimension(4, 4) :: r

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Process
        r = reshape([one, zero, zero, zero, zero, one, zero, zero, zero, &
            zero, one, zero, x, y, z, one], [4, 4])
    end function

! ------------------------------------------------------------------------------
    pure module function translate_mtx(trans) result(r)
        ! Arguments
        real(real64), intent(in), dimension(3) :: trans
        real(real64), dimension(4, 4) :: r

        ! Process
        r = translate(trans(1), trans(2), trans(3))
    end function

! ------------------------------------------------------------------------------
    pure module function rotate_mtx_gen(i, j, k, ci, cj, ck) result(r)
        ! Arguments
        real(real64), intent(in), dimension(3) :: i, j, k, ci, cj, ck
        real(real64), dimension(3, 3) :: r

        ! Generate the matrix
        r(1,1) = dot_product(i, ci)
        r(2,1) = dot_product(j, ci)
        r(3,1) = dot_product(k, ci)

        r(1,2) = dot_product(i, cj)
        r(2,2) = dot_product(j, cj)
        r(3,2) = dot_product(k, cj)

        r(1,3) = dot_product(i, ck)
        r(2,3) = dot_product(j, ck)
        r(3,3) = dot_product(k, ck)
    end function

! ------------------------------------------------------------------------------
    pure module function dh_mtx_implement(theta, trans, twist, length) result(r)
        ! Arguments
        real(real64), intent(in) :: theta, trans, twist, length
        real(real64), dimension(4, 4) :: r

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Local Variables
        real(real64) :: ct, st, ca, sa

        ! Process
        ct = cos(theta)
        st = sin(theta)
        ca = cos(twist)
        sa = sin(twist)
        r = reshape([ct, st, zero, zero, &
            -st * ca, ct * ca, sa, zero, &
            st * sa, -ct * sa, ca, zero, &
            length * ct, length * st, trans, one], [4, 4])
    end function

! ------------------------------------------------------------------------------
    pure module function to_skew_symm_implement(x) result(y)
        ! Arguments
        real(real64), intent(in), dimension(3) :: x
        real(real64), dimension(3, 3) :: y

        ! Parameters
        real(real64), parameter :: zero = 0.0d0

        ! Process
        y = reshape([zero, x(3), -x(2), -x(3), zero, x(1), x(2), -x(1), zero], &
            [3, 3])
    end function

! ------------------------------------------------------------------------------
    pure module function velocity_mtx_implement(omega, v, r) result(t)
        ! Arguments
        real(real64), intent(in), dimension(3) :: omega, v, r
        real(real64), dimension(4, 4) :: t

        ! Parameters
        real(real64), parameter :: zero = 0.0d0

        ! Populate the matrix
        t(1:3,1:3) = to_skew_symmetric(omega)
        t(1:3,4) = v - matmul(t(1:3,1:3), r)
        t(4,:) = zero
    end function

! ------------------------------------------------------------------------------
    pure module function accel_mtx_implement(alpha, omega, a, r) result(t)
        ! Arguments
        real(real64), intent(in), dimension(3) :: alpha, omega, a, r
        real(real64), dimension(4, 4) :: t

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: one = 1.0d0

        ! Local Variables
        real(real64), dimension(3, 3) :: wmtx

        ! Compute the skew-symmetric matrices
        t(1:3,1:3) = to_skew_symmetric(alpha)
        wmtx = to_skew_symmetric(omega)

        ! Build the matrix
        t(1:3,1:3) = reshape([&
            -omega(3)**2 - omega(2)**2, &
            omega(1) * omega(2) + alpha(3), &
            omega(1) * omega(3) - alpha(2), &
            omega(1) * omega(2) - alpha(3), &
            -omega(3)**2 - omega(1)**2, &
            omega(2) * omega(3) + alpha(1), &
            omega(1) * omega(3) + alpha(2), &
            omega(2) * omega(3) - alpha(1), &
            -omega(2)**2 - omega(1)**2], [3, 3])
        t(1:3,4) = a - matmul(t(1:3,1:3), r)
        t(4,:) = zero
    end function

! ------------------------------------------------------------------------------
end submodule
