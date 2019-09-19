! vibrations_fit.f90

submodule (vibrations) vibrations_fit
contains
! ------------------------------------------------------------------------------
    pure function partial_fraction_basis_function(s, a, p) result(phi)
        ! Arguments
        complex(real64), intent(in) :: s
        complex(real64), intent(in), dimension(:) :: a
        integer(int32), intent(in) :: p
        complex(real64) :: phi

        ! Process
        phi = 1.0d0 / (s + a(p))
    end function

! ------------------------------------------------------------------------------
    pure function frequency_basis_function(s, a, p) result(phi)
        ! Arguments
        complex(real64), intent(in) :: s
        complex(real64), intent(in), dimension(:) :: a
        integer(int32), intent(in) :: p
        complex(real64) :: phi

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: ten = (1.0d1, 0.0d0)
        complex(real64), parameter :: p1 = (1.0d-1, 0.0d0)

        ! Local Variables
        integer(int32) :: i, ep
        complex(real64) :: temp

        ! Compute the product of s / (s + a(i)) from i = 1 to p - 1
        if (p == 1) then
            phi = zero
        else if (p == 2) then
            phi = s / (s + a(1))
        else
            temp = one
            ep = 0
            do i = 1, p - 1
                temp = temp * (s / (s + a(i)))
                if (temp == zero) then
                    phi = zero
                    return
                end if
                
                do while (abs(temp) < 1.0d0)
                    temp = ten * temp
                    ep = ep - 1
                end do

                do while (abs(temp) > 1.0d1)
                    temp = p1 * temp
                    ep = ep + 1
                end do
            end do
            phi = temp * ten**ep
        end if

        ! Compute the full basis function
        phi = phi * abs(a(p)) / (s + a(p))
    end function

! ------------------------------------------------------------------------------
    pure function evaluate_polynomial(phi, coeff) result(r)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: phi! N element array of 
                                                        ! basis function values
        real(real64), intent(in), dimension(:) :: coeff ! N-element coefficient array
                                                        ! (the highest power coefficient is one)
        complex(real64) :: r                            ! value of the polynomial

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: i, n, nz

        ! Initialization
        n = size(coeff) ! If n < size(z), the order of the polynomial in
                        ! the denominator (source of phi) is greater than
                        ! that of the numerator.  This is OK as this is
                        ! equivalent to stating that the numerator 
                        ! polynomial is of equal order, but contains
                        ! zero-valued coefficients for the higher order
                        ! terms.
        nz = size(phi)  ! For the non-standard case where the order of
                        ! the numerator is greater than that of the
                        ! denominator.
        n = min(n, nz)

        ! Process
        r = zero
        do i = 1, n
            ! Process
            r = r + coeff(i) * phi(i)
        end do
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
    !
    ! - f: An M-element array containing the complex-valued frequency response
    !       function to fit.
    ! - s: An M-element array containing the complex-valued frequency points at
    !       which f is defined.
    ! - poles: An N-element array containing an estimate of the system poles.
    !       On output, the new estimate of pole locations.
    ! - weights: An M-element array containing weighting factors for each 
    !       frequency value.
    ! - a [out]: An N+1 -by- N+1 matrix.
    ! - b [out]: An N+1 -by- 1 matrix.
    ! - c [out]: A 1 -by- N matrix.
    ! - d [out]: A 1 -by- 1 matrix.
    ! - iwork: An X-element workspace array.
    !       X = 2 * N + 1
    !   Breakdown:
    !   - N - cindex
    !   - N + 1 - ipvt
    ! - cwork: An X-element workspace array.
    !       X = 
    !       Breakdown:
    !       - M-by-(N+1) - Dk
    !       - M-by-2*(N+1) - Ac
    !       - N - Cc
    ! - dwork: An X-element workspace array.
    !       X = 
    !       Breakdown:
    !       - 2*M-by-2*(N+1) - Ar
    !       - MIN(2*M, 2*(N + 1)) - tau
    !       - 2*M-by-MIN(2*M, 2*(N + 1)) - Q
    !       - N+1 - escale
    !       - N+1 - x
    subroutine vector_fit(f, s, poles, weights, a, b, c, d, iwork, cwork, dwork)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: f, s
        complex(real64), intent(inout), dimension(:) :: poles
        real(real64), intent(in), dimension(:) :: weights
        real(real64), intent(out), dimension(:,:) :: a, b, c, d
        integer(int32), intent(out), target, dimension(:) :: iwork
        complex(real64), intent(out), target, dimension(:) :: cwork
        real(real64), intent(out), target, dimension(:) :: dwork

        ! Local Variables
        integer(int32) :: ns, n, ndk, nac, nar, ntau, nq, nescale, nx, &
            e1, s2, e2, s3, e3, &
            e1r, s2r, e2r, s3r, e3r, s4r, e4r, s5r, e5r
        integer(int32), pointer, dimension(:) :: cindex, ipvt
        real(real64) :: eps, zerotol, scale
        complex(real64), pointer, dimension(:,:) :: dk, ac
        complex(real64), pointer, dimension(:) :: cc
        real(real64), pointer, dimension(:,:) :: ar, q
        real(real64), pointer, dimension(:) :: tau, escale, x

        ! Initialization
        ns = size(s)
        n = size(poles)
        ndk = ns * (n + 1)
        nac = ns * 2 * (n + 1)
        nar = 4 * ns * (n + 1)
        ntau = min(2 * ns, 2 * (n + 1))
        nq = 2 * ns * ntau
        nescale = n + 1
        nx = n + 1
        eps = epsilon(eps)
        zerotol = 2.0d0 * eps

        ! Assign pointers from the workspace arrays
        e1 = ndk
        s2 = e1 + 1
        e2 = s2 + nac - 1
        s3 = e2 + 1
        e3 = s3 + n - 1

        e1r = nar
        s2r = e1r + 1
        e2r = s2r + ntau - 1
        s3r = e2r + 1
        e3r = s3r + nq - 1
        s4r = e3r + 1
        e4r = s4r + nescale - 1
        s5r = e4r + 1
        e5r = s5r + nx - 1

        cindex => iwork(1:n)
        ipvt => iwork(n+1:2*n+1)

        dk(1:ns,1:n+1) => cwork(1:e1)
        ac(1:ns,1:2*(n+1)) => cwork(s2:e2)
        cc => cwork(s3:e3)

        ar(1:2*ns,1:2*(n+1)) => dwork(1:e1r)
        tau => dwork(s2r:e2r)
        q(1:2*ns, 1:ntau) => dwork(s3r:e3r)
        escale => dwork(s4r:e4r)
        x => dwork(s5r:e5r)

        ! Determine which poles are complex-valued
        call label_complex_values(poles, zerotol, cindex)

        ! Build the denominator for each 's'
        call build_denominator(s, poles, cindex, dk)

        ! Compute scaling for the least-squares problem
        scale = norm2(abs(weights * f)) / real(ns)

        ! Extract the state space matrices
        call denom_to_state_space(weights, f, dk, ac, ar, tau, q, &
            ipvt, escale, x, a, b, c, d)

        ! Construct a complex-valued version of C
        call c_to_cmplx(c, cindex, cc)

        ! Compute the new pole locations (zeros)

        ! Construct the overall state space matrices
    end subroutine

! ------------------------------------------------------------------------------
    ! Identifies the real-valued and complex-conjugate pair values.
    !
    ! - vals: An N-element array of complex values to search.
    ! - tol: A tolerance value to determine closeness to zero.
    ! - labels [out]: An N-element array containing labels identifying each value
    !       type.  The labels are as follows:
    !       * 0: Real
    !       * 1: Complex
    !       * 2: Complex Conjugate
    subroutine label_complex_values(vals, tol, labels)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: vals
        real(real64), intent(in) :: tol
        integer(int32), intent(out), dimension(:) :: labels

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: x, xc

        ! Process
        n = size(vals)
        i = 1
        do while (i <= n)
            x = aimag(vals(i))
            if (abs(x) < tol) then
                ! Real-Values
                labels(i) = 0
                i = i + 1
            else
                ! Complex-Valued
                labels(i) = 1

                ! Check for the conjugate in the next index
                if (i < n) then
                    xc = aimag(vals(i+1))
                    if (abs(x + xc) < tol) then
                        ! Complex-Conjugate
                        labels(i+1) = 2
                        i = i + 2
                    else
                        ! Not a complex-conjugate
                        i = i + 1
                    end if
                else
                    ! Can't be a conjugate value as its the last value in the array
                    i = i + 1
                end if
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Builds the denominator based upon the current pole location estimate.
    !
    ! - s: An M-element array containing the complex-valued frequency points at
    !       which f is defined.
    ! - poles: An N-element array containing an estimate of the system poles.
    !       On output, the new estimate of pole locations.
    ! - cindex: An N-element array determining what pole values are real,
    !       complex, and/or complex-conjugates.  See label_complex_values for
    !       more info.
    ! - dk [out]: An M-by-N+1 matrix.
    subroutine build_denominator(s, poles, cindex, dk)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: s, poles
        integer(int32), intent(in), dimension(:) :: cindex
        complex(real64), intent(out), dimension(:,:) :: dk

        ! Parameters
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: j = (0.0d0, 1.0d0)

        ! Local Variables
        integer(int32) :: i, ns, n

        ! Initialization
        ns = size(s)
        n = size(poles)
        dk(:,n+1) = one

        ! Process
        do i = 1, n
            if (cindex(i) == 0) then
                dk(:,i) = one / (s - poles(i))
            else if (cindex(i) == 1) then
                dk(:,i) = one / (s - poles(i)) + one / (s - poles(i))
            else if (cindex(i) == 2) then
                dk(:,i) = j / (s - poles(i)) - j / (s - poles(i))
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Construct the state-space matrices from the denominator.
    !
    ! - weights: M-element weighting vector
    ! - f: M-element frequency response vector containing data to be fitted.
    ! - dk: An M-by-(N+1) matrix.
    ! - ac: An M-by-2*(N+1) matrix.
    ! - ar: An 2*M-by-2*(N+1) matrix.
    ! - tau: A MIN(2*M,2*(N+1)) array.
    ! - q: An 2*M-by-MIN(2*M,2*(N+1)) matrix.
    ! - pvt: An N+1 array.
    ! - escale: An N+1 array.
    ! - x: An N+1 array.
    ! - a: An N+1-by-N+1 matrix.
    ! - b: An N+1-by-1 matrix.
    ! - c: A 1-by-N matrix.
    ! - d: A 1-by-1 matrix.
    subroutine denom_to_state_space(weights, f, dk, ac, ar, tau, q, pvt, &
            escale, x, a, b, c, d)
        use linalg_core

        ! Arguments
        real(real64), intent(in), dimension(:) :: weights
        complex(real64), intent(in), dimension(:) :: f
        complex(real64), intent(in), dimension(:,:) :: dk
        complex(real64), intent(out), dimension(:,:) :: ac
        real(real64), intent(out), dimension(:,:) :: ar
        real(real64), intent(out), dimension(:) :: tau, escale, x
        real(real64), intent(out), dimension(:,:) :: q
        integer(int32), intent(out), dimension(:) :: pvt

        ! State Space Matrix Outputs
        real(real64), intent(out), dimension(:,:) :: a, b, c, d

        ! Local Variables
        integer(int32) :: i, ns, n, offset, ind1, ind2

        ! Initialization
        ns = size(dk, 1)
        n = size(dk, 2) - 1 ! DK is Ns -by- N + 1
        offset = n + 1;

        ! Process
        do i = 1, offset
            ac(:,i) = weights * dk(:,i)
        end do
        do i = 1, offset
            ac(:,offset+i) = -weights * dk(:,i) * f
        end do
        ar(1:ns,:) = real(ac)
        ar(ns+1:2*ns,:) = aimag(ac)

        ! Integral criteria for sigma
        do i = 1, offset
            ar(2*ns+1,offset+i) = real(scale * sum(dk(:,i)))
        end do

        ! Compute the QR factorization of AR, and then form Q & R fully
        call qr_factor(ar, tau)
        call form_qr(ar, tau, q)    ! R is stored in AR
        
        ! Extract the trailing submatrix from R, and store in the state-space
        ! matrix A
        ind1 = offset + 1
        ind2 = 2 * offset
        a = ar(ind1:ind2,ind1:ind2)

        ! Construct B
        b(:,1) = q(size(q, 1), offset + 1:size(q, 2)) * scale * real(ns)

        ! Finish working on A
        do i = 1, offset
            escale(i) = 1.0d0 / norm2(a(:,i))
            a(:,i) = escale(i) * a(:,i)
        end do

        ! Utilize LU-factorization to solve A * X = B, for X
        ar(ind1:ind2,ind1:ind2) = a
        call lu_factor(ar(ind1:ind2,ind1:ind2), pvt)    ! AR(IND1:IND2,IND1:IND2) is overwritten with L/U
        x = b(:,1)   ! Protect B from being overwritten
        call solve_lu(ar(ind1:ind2,ind1:ind2), pvt, x)

        ! Build C & D
        C(1,:) = x(1:n)
        D(1,1) = x(offset)
    end subroutine

! ------------------------------------------------------------------------------
    ! Convert the C array to complex form.
    !
    ! - c: The 1-by-N matrix to convert.
    ! - cindex: An N-element array determining what pole values are real,
    !       complex, and/or complex-conjugates.  See label_complex_values for
    !       more info.
    ! - cc [out]: The N-element array containing the complex version of C
    subroutine c_to_cmplx(c, cindex, cc)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: c
        integer(int32), intent(in), dimension(:) :: cindex
        complex(real64), intent(out), dimension(:) :: cc

        ! Parameters
        complex(real64), parameter :: j = (0.0d0, 1.0d0)

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: r1, r2

        ! Process
        n = size(c, 2)
        i = 1
        do while (i <= n)
            if (cindex(i) == 1) then
                r1 = c(1,i)
                r2 = c(1,i+1)
                cc(i) = r1 + j * r2
                cc(i+1) = r1 - j * r2
                i = i + 2
            else
                cc(i) = cmplx(c(1,i), real64)
                i = i + 1
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Compute the zeros of the system.
    subroutine compute_zeros(poles, b, c, d, lambda)
        use linalg_core

        ! Arguments
        complex(real64), intent(inout), dimension(:) :: poles
        complex(real64), intent(out), dimension(:,:) :: b  ! N-by-1
        complex(real64), intent(inout), dimension(:) :: c  ! N
        real(real64), intent(in), dimension(:,:) :: d   ! 1 -by- 1
        complex(real64), intent(out), dimension(:,:) :: lambda  ! N-by-N

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: two = (2.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: i, n, m
        complex(real64) :: koko, alpha

        ! Initialization
        n = size(poles)

        lambda = zero
        do i = 1, n
            b(i,1) = 1.0d0
            lambda(i,i) = poles(i)
        end do

        ! Process
        m = 0
        do i = 1, n
            m = m + 1
            if (m < n) then
                if (abs(lambda(m,m)) > abs(real(lambda(m,m)))) then
                    lambda(m+1,m) = -aimag(lambda(m,m))
                    lambda(m,m+1) = aimag(lambda(m,m))
                    lambda(m,m) = real(lambda(m,m))
                    lambda(m+1,m+1) = lambda(m,m)

                    b(m,1) = two
                    b(m+1,1) = zero
                    koko = c(m)
                    c(m) = cmplx(real(koko))
                    c(m+1) = cmplx(aimag(koko))
                    m = m + 1
                end if
            end if
        end do

        ! Construct the eigenvalue problem
        ! ZER = LAMBDA - B * C / D(1,1)  - use ZGEMM to compute
        !
        ! LAMBDA is N-by-N
        ! B is N-by-1
        ! C is 1-by-N
        alpha = cmplx(-1.0d0 / d(1,1), real64)
        call ZGEMM('N', 'N', n, n, 1, alpha, b, n, c, 1, one, lambda, n)
        ! Results of ZGEMM stored in LAMBDA

        ! Compute the eigenvalues
        call eigen(lambda, poles)

        ! Correct any unstable poles - force them to be stable (i.e. ensure the real components are negative)

        ! Sort the poles
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
