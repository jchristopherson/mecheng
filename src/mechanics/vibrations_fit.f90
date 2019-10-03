! vibrations_fit.f90

submodule (vibrations) vibrations_fit
contains
! ------------------------------------------------------------------------------
    module function fit_frf(freq, amp, phase, order, niter, weights, err) result(mdl)
        ! Arguments
        real(real64), intent(in), dimension(:) :: freq, amp, phase
        integer(int32), intent(in) :: order
        integer(int32), intent(in), optional :: niter
        real(real64), intent(in), dimension(:), optional :: weights
        class(errors), intent(inout), target, optional :: err
        type(state_space) :: mdl

        ! Parameters
        complex(real64), parameter :: j = (0.0d0, 1.0d0)

        ! Local Variables
        integer(int32) :: npts, flag, liwork, lcwork, ldwork, minmn, i, ni
        type(errors), target :: deferr
        class(errors), pointer :: errmgr
        character(len = 256) :: errmsg
        integer(int32), allocatable, dimension(:) :: iwork
        complex(real64), allocatable, dimension(:) :: cwork, s, frf
        real(real64), allocatable, dimension(:) :: rwork, wghts

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        npts = size(freq)
        minmn = min(2 * npts, 2 * (order + 1))
        liwork = 2 * order + 1
        lcwork = order**2 + (3 * npts + 2) * order + 4 * npts
        ldwork = (2 * npts + 1) * minmn + (4 * npts + 2) * order + 6 * npts + 2
        allocate(wghts(npts), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fit_Frf", "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if
        
        ! Establish Defaults
        if (present(niter)) then
            ni = niter
        else
            ni = 5
        end if
        if (present(weights)) then
            if (size(weights) /= npts) then
                write (errmsg, '(AI0AI0A)') &
                    "The weighting array is improperly sized.  Expected ", npts, &
                    " elements, but found ", size(weights), "."
                call errmgr%report_error("fit_frf", trim(errmsg), MECH_ARRAY_SIZE_ERROR)
                return
            end if
            wghts = weights
        else
            wghts = 1.0d0
        end if

        ! Input Check
        if (order < 1) then
            call errmgr%report_error("fit_frf", "Requested order is less than 1.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (size(amp) /= npts) then
            write (errmsg, '(AI0AI0A)') &
                "The amplitude array is improperly sized.  Expected ", npts, &
                " elements, but found ", size(amp), "."
            call errmgr%report_error("fit_frf", trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(phase) /= npts) then
            write (errmsg, '(AI0AI0A)') &
                "The phase array is improperly sized.  Expected ", npts, &
                " elements, but found ", size(phase), "."
            call errmgr%report_error("fit_frf", trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Workspace Allocation
        allocate(iwork(liwork), stat = flag)
        if (flag == 0) allocate(cwork(lcwork), stat = flag)
        if (flag == 0) allocate(dwork(ldwork), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fit_frf", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Set up the complex-valued frequency and FRF vectors
        s = j * freq
        frf = amp * (cos(phase) + j * sin(phase))

        ! Construct an initial estimate of pole locations
    end function

! ------------------------------------------------------------------------------
    ! Takes a single step of the fitting algorithm.
    !
    ! References:
    ! 1. B. Gustavsen and A. Semlyen, "Rational approximation of frequency 
    !    domain responses by Vector Fitting", IEEE Trans. Power Delivery, 
    !    vol. 14, no. 3, pp. 1052-1061, July 1999. Link
    ! 2. B. Gustavsen, "Improving the pole relocating properties of vector 
    !    fitting", IEEE Trans. Power Delivery, vol. 21, no. 3, pp. 1587-1592, 
    !    July 2006. Link 
    ! 3. D. Deschrijver, M. Mrozowski, T. Dhaene, and D. De Zutter, 
    !    “Macromodeling of  Multiport Systems Using a Fast Implementation of 
    !    the Vector Fitting Method”, IEEE Microwave and Wireless Components 
    !    Letters, vol. 18, no. 6, pp. 383-385, June 2008.
    !
    ! Websites:
    ! - https://www.sintef.no/vectfit
    !
    ! Arguments:
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
    ! - fit [out]: An M-element array containing the fitted data.
    ! - delta [out]: An M-element array containing the difference between the
    !       fitted and raw data.
    ! - iwork: An X-element workspace array.
    !       X = 2 * N + 1
    !   Breakdown:
    !   - N - cindex
    !   - N + 1 - ipvt
    ! - cwork: An X-element workspace array.
    !       X = N**2 + (3*M + 2)*N + 4*M
    !       Breakdown:
    !       - M-by-(N+1) - Dk
    !       - M-by-2*(N+1) - Ac
    !       - N - Cc
    !       - N - Bc
    !       - N-by-N - LAMBDA
    !       - M - Bb
    ! - dwork: An X-element workspace array.
    !       X = (2*M + 1)*S + (4*M + 2)*N + 6*M + 2
    !           Where: S = MIN(2*M, 2*(N+1))
    !       Breakdown:
    !       - 2*M-by-2*(N+1) - Ar
    !       - MIN(2*M, 2*(N + 1)) - tau
    !       - 2*M-by-MIN(2*M, 2*(N + 1)) - Q
    !       - N+1 - escale
    !       - N+1 - x
    !       - 2*M - Br
    subroutine vector_fit(f, s, poles, weights, a, b, c, d, fit, delta, iwork, cwork, dwork)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: f, s
        complex(real64), intent(inout), dimension(:) :: poles
        real(real64), intent(in), dimension(:) :: weights
        real(real64), intent(out), dimension(:,:) :: a, b, c, d
        complex(real64), intent(out), dimension(:) :: fit, delta
        integer(int32), intent(out), target, dimension(:) :: iwork
        complex(real64), intent(out), target, dimension(:) :: cwork
        real(real64), intent(out), target, dimension(:) :: dwork

        ! Local Variables
        integer(int32) :: ns, n, ndk, nac, nar, ntau, nq, nescale, nx, &
            e1, s2, e2, s3, e3, s4, e4, s5, e5, s6, e6, &
            e1r, s2r, e2r, s3r, e3r, s4r, e4r, s5r, e5r, s6r, e6r
        integer(int32), pointer, dimension(:) :: cindex, ipvt
        real(real64) :: eps, zerotol, scale
        complex(real64), pointer, dimension(:,:) :: dk, ac, lambda
        complex(real64), pointer, dimension(:) :: cc, bc, bb
        real(real64), pointer, dimension(:,:) :: ar, q
        real(real64), pointer, dimension(:) :: tau, escale, x, br

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
        s4 = e3 + 1
        e4 = s4 + n - 1
        s5 = e4 + 1
        e5 = s5 + n * n - 1
        s6 = e5 + 1
        e6 = s6 + ns - 1

        e1r = nar
        s2r = e1r + 1
        e2r = s2r + ntau - 1
        s3r = e2r + 1
        e3r = s3r + nq - 1
        s4r = e3r + 1
        e4r = s4r + nescale - 1
        s5r = e4r + 1
        e5r = s5r + nx - 1
        s6r = e5r + 1
        e6r = s6r + 2 * ns - 1

        cindex => iwork(1:n)
        ipvt => iwork(n+1:2*n+1)

        dk(1:ns,1:n+1) => cwork(1:e1)
        ac(1:ns,1:2*(n+1)) => cwork(s2:e2)
        cc => cwork(s3:e3)
        bc => cwork(s4:e4)
        lambda(1:n,1:n) => cwork(s5:e5)
        bb => cwork(s6:e6)

        ar(1:2*ns,1:2*(n+1)) => dwork(1:e1r)
        tau => dwork(s2r:e2r)
        q(1:2*ns, 1:ntau) => dwork(s3r:e3r)
        escale => dwork(s4r:e4r)
        x => dwork(s5r:e5r)
        br => dwork(s6r:e6r)

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
        ! POLES is overwritten with the new pole location information
        call compute_zeros(poles, bc, cc, d, lambda)

        ! Determine which of the new poles are complex-valued
        call label_complex_values(poles, zerotol, cindex)

        ! Construct the overall state space matrices
        call build_denominator(s, poles, cindex, dk)
        call to_cmplx_state_space(weights, f, dk, ac, ar(:,1:n+1), bb, br, &
            c, d, escale)
        call c_to_cmplx(c, cindex, cc)

        ! Construct the actual fit
        call compute_fit_and_error(f, s, poles, cc, d, dk(:,1:n), fit, delta)

        ! Put the complex-valued state space matrices into real form
        call convert_to_real_form(poles, bc, cc, cindex, a, b, c)
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
    ! - weights: M-element weighting vector.
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
        n = size(dk, 2) - 1 ! DK is Ns -by- (N + 1)
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
    !
    ! - poles: On input, the current estimate of the pole locations.  On output,
    !       the updated estimate.
    ! - b: An N-element complex-valued workspace array.
    ! - c: An N-element complex valued "C" matrix from the state-space definition
    !       of the system - the C matrix is 1-by-n.
    ! - d: A 1-by-1 matrix containing the "D" matrix from the state-space
    !       definition of the system.
    ! - lambda: An N-by-N complex-valued workspace array.
    subroutine compute_zeros(poles, b, c, d, lambda)
        use linalg_core

        ! Arguments
        complex(real64), intent(inout), dimension(:) :: poles
        complex(real64), intent(out), dimension(:) :: b
        complex(real64), intent(out), dimension(:,:) :: lambda
        complex(real64), intent(inout), dimension(:) :: c
        real(real64), intent(in), dimension(:,:) :: d

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: two = (2.0d0, 0.0d0)
        complex(real64), parameter :: j = (0.0d0, 1.0d0)

        ! Local Variables
        integer(int32) :: i, n, m, n1
        complex(real64) :: koko, alpha, temp

        ! Initialization
        n = size(poles)

        lambda = zero
        do i = 1, n
            b(i) = one
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

                    b(m) = two
                    b(m+1) = zero
                    koko = c(m)
                    c(m) = cmplx(real(koko), real64)
                    c(m+1) = cmplx(aimag(koko), real64)
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

        ! Correct any unstable poles - force them to be stable 
        ! (i.e. ensure the real components are negative)
        do i = 1, n
            if (real(poles(i)) > 0.0d0) then
                ! The real component is positive - unstable pole
                poles(i) = poles(i) - 2.0d0 * poles(i)
            end if
        end do

        ! Sort the poles into ascending order
        call sort(poles, .true.)

        ! Now ensure real-valued poles are placed first in the array
        do i = 1, n
            do m = i + 1, n
                if (aimag(poles(m)) == 0.0d0 .and. aimag(poles(i)) /= 0.0d0) then
                    temp = poles(i)
                    poles(i) = poles(m)
                    poles(m) = temp
                end if
            end do
        end do
        n1 = 0
        do i = 1, n
            if (aimag(poles(i)) == 0.0d0) n1 = i
        end do
        if (n1 < n) call sort(poles(n+1:n), .true.)
        poles = poles - 2.0d0 * j * aimag(poles)
    end subroutine

! ------------------------------------------------------------------------------
    ! Constructs the C matrix into its complex-valued form.
    !
    ! - weights: M-element weighting vector.
    ! - f: M-element frequency response vector containing data to be fitted.
    ! - dk: An M-by-(N+1) matrix.
    ! - ac: An M-by-2*(N+1) matrix.
    ! - ar: A 2*M-by-(N+1) matrix.
    ! - bc: An M element array.
    ! - br: A 2*M element array.
    ! - cr: A 1-by-N matrix.
    ! - dr: A 1-by-1 matrix.
    ! - escale: An N+1 element array.
    subroutine to_cmplx_state_space(weights, f, dk, ac, ar, bc, br, cr, dr, escale)
        use linalg_core

        ! Arguments
        real(real64), intent(in), dimension(:) :: weights
        complex(real64), intent(in), dimension(:) :: f
        complex(real64), intent(in), dimension(:,:) :: dk
        complex(real64), intent(out), dimension(:,:) :: ac
        complex(real64), intent(out), dimension(:) :: bc
        real(real64), intent(out), dimension(:,:) :: ar, cr, dr
        real(real64), intent(out), dimension(:) :: escale, br

        ! Local Variables
        integer(int32) :: i, ns, n

        ! Initialization
        ns = size(dk, 1)
        n = size(dk, 2) - 1 ! DK is Ns -by- (N + 1)

        ! Process
        do i = 1, n
            ac(:,i) = weights * dk(:,i)
        end do
        ac(:,n+1) = cmplx(weights, real64)
        ar(ns+1:2*ns,:) = aimag(ac(1:ns,:))
        ar(1:ns,:) = real(ac(1:ns,:))
        bc(1:ns) = weights * f
        br(ns+1:2*ns) = aimag(bc(1:ns))
        br(1:ns) = real(bc(1:ns))
        
        ! Rescale A
        do i = 1, n + 1
            escale(i) = norm(ar(:,i))
            ar(:,i) = ar(:,i) / escale(i)
        end do

        ! Solve A * X = B for X via a least-squares solver
        call solve_least_squares(ar, br) ! AR & BR are overwritten
        br(1:n+1) = br(1:n+1) / escale   ! Rescale the solution
        cr(1,:) = br(1:n) ! The first N elements of BR contain the solution for C

        ! The N+1th element of BR contains D
        dr(1,1) = br(n+1)
    end subroutine

! ------------------------------------------------------------------------------
    !
    ! - f: M-element frequency response vector containing data to be fitted.
    ! - s: An M-element array containing the complex frequency points.
    ! - poles: An N-element array containing the pole locations.
    ! - c: The 1-by-N complex form of the C matrix.
    ! - d: The 1-by-1 D matrix.
    ! - dk: An M-by-N matrix.
    ! - fit [out]: An M-element array containing the fitted data.
    ! - delta [out]: An M-element array containing the difference between the
    !       raw data and the fitted data.
    subroutine compute_fit_and_error(f, s, poles, c, d, dk, fit, delta)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: f, s, poles, c
        complex(real64), intent(out), dimension(:,:) :: dk
        complex(real64), intent(out), dimension(:) :: fit, delta
        real(real64), intent(in), dimension(:,:) :: d

        ! Parameters
        complex(real64), parameter :: one = (1.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: i, n, ns

        ! Initialization
        n = size(poles)
        ns = size(s)

        ! Process
        do i = 1, n
            dk(:,i) = 1.0d0 / (s - poles(i))
        end do

        ! Compute: FIT = DK * C + D
        !
        ! - FIT is M-by-1
        ! - DK is M-by-N
        ! - C is N-by-1
        ! - D is 1-by-1
        fit = cmplx(d(1,1), real64) ! Fill FIT with D(1,1)
        call ZGEMV('N', ns, n, one, dk, ns, c, 1, one, fit, 1)

        ! Compute the difference between the fitted and actual
        delta = fit - f
    end subroutine

! ------------------------------------------------------------------------------
    ! Converts the model to a real-valued state-space model.
    !
    ! - poles: An N-element array containing the pole locations.
    ! - bc: An N-element array containing the complex B matrix.
    ! - cc: An N-element array containing the complex C matrix.
    ! - cindex: An N-element array determining what pole values are real,
    !       complex, and/or complex-conjugates.  See label_complex_values for
    !       more info.
    ! - a: An N-by-N matrix A.
    ! - b: An N-by-1 matrix B.
    ! - c: A 1-by-N matrix C.
    subroutine convert_to_real_form(poles, bc, cc, cindex, a, b, c)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: poles, bc, cc
        real(real64), intent(out), dimension(:,:) :: a, b, c

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: a1, a2, b1, b2, c1, c2

        ! Initialization
        n = size(poles)

        ! Process
        i = 0
        do
            i = i + 1
            if (i == 1) then
                a1 = real(poles(i))
                a2 = aimag(poles(i))
                b1 = 2.0d0 * real(bc(i))
                b2 = -2.0d0 * aimag(bc(i))
                c1 = real(cc(i))
                c2 = aimag(cc(i))

                a(i,i) = a1
                a(i,i+1) = a2
                a(i+1,i) = -a2
                a(i+1,i+1) = a1

                b(i,1) = b1
                b(i+1,1) = b2

                c(1,i) = c1
                c(1,i+1) = c2

                i = i + 1
            else if (i == 0) then
                a(i,i) = real(poles(i))
                b(i,1) = real(bc(i))
                c(1,i) = real(cc(i))
            end if

            ! Check i
            if (i >= n) exit
        end do
    end subroutine

! ------------------------------------------------------------------------------
end submodule
