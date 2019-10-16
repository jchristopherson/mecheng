! vibrations_fit.f90

! TO DO: Continue debugging at line 763

submodule (vibrations) vibrations_fit
contains
! ------------------------------------------------------------------------------
    module subroutine fft_fit_frf(this, freq, amp, phase, order, niter, weights, err)
        ! Arguments
        class(frf_fitting_tool), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: freq, amp, phase
        integer(int32), intent(in) :: order
        integer(int32), intent(in), optional :: niter
        real(real64), intent(in), dimension(:), optional :: weights
        class(errors), intent(inout), target, optional :: err

        ! Parameters
        complex(real64), parameter :: j = (0.0d0, 1.0d0)

        ! Local Variables
        integer(int32) :: npts, flag, liwork, lcwork, lrwork, minmn, i, ni
        type(errors), target :: deferr
        class(errors), pointer :: errmgr
        character(len = 256) :: errmsg
        integer(int32), allocatable, dimension(:) :: iwork
        complex(real64), allocatable, dimension(:) :: cwork, s, frf
        real(real64), allocatable, dimension(:) :: rwork, wghts

        ! Clean up the existing object
        if (allocated(this%poles)) deallocate(this%poles)
        if (allocated(this%residual)) deallocate(this%residual)
        if (allocated(this%rms)) deallocate(this%rms)
        if (allocated(this%model%A)) deallocate(this%model%A)
        if (allocated(this%model%B)) deallocate(this%model%B)
        if (allocated(this%model%C)) deallocate(this%model%C)
        if (allocated(this%model%D)) deallocate(this%model%D)

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        npts = size(freq)
        minmn = min(2 * npts, 2 * (order + 1))
        liwork = order
        lcwork = order**2 + (3 * npts + 2) * order + 4 * npts
        lrwork = (2 * npts + 1) * minmn + order**2 + (4 * npts + 5) * order + 6 * npts + 5
        allocate(wghts(npts), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fft_fit_frf", "Insufficient memory available.", &
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
                call errmgr%report_error("fft_fit_frf", trim(errmsg), MECH_ARRAY_SIZE_ERROR)
                return
            end if
            wghts = weights
        else
            wghts = 1.0d0
        end if

        ! Input Check
        if (ni < 1) then
            call errmgr%report_error("fft_fit_frf", &
                "There must be at least 1 iteration allowed.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (order < 2) then
            call errmgr%report_error("fft_fit_frf", "Requested order is less than 2.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (size(amp) /= npts) then
            write (errmsg, '(AI0AI0A)') &
                "The amplitude array is improperly sized.  Expected ", npts, &
                " elements, but found ", size(amp), "."
            call errmgr%report_error("fft_fit_frf", trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(phase) /= npts) then
            write (errmsg, '(AI0AI0A)') &
                "The phase array is improperly sized.  Expected ", npts, &
                " elements, but found ", size(phase), "."
            call errmgr%report_error("fft_fit_frf", trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Workspace Allocation
        allocate(iwork(liwork), stat = flag)
        if (flag == 0) allocate(cwork(lcwork), stat = flag)
        if (flag == 0) allocate(rwork(lrwork), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fft_fit_frf", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Allocate output information
        allocate(this%frf(npts), stat = flag)
        if (flag == 0) allocate(this%poles(order), stat = flag)
        if (flag == 0) allocate(this%residual(npts), stat = flag)
        if (flag == 0) allocate(this%rms(ni), stat = flag)
        if (flag == 0) allocate(this%model%A(order, order), stat = flag)
        if (flag == 0) allocate(this%model%B(order, 1), stat = flag)
        if (flag == 0) allocate(this%model%C(1, order), stat = flag)
        if (flag == 0) allocate(this%model%D(1, 1), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fft_fit_frf", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Set up the complex-valued frequency and FRF vectors
        s = j * freq
        frf = amp * (cos(phase) + j * sin(phase))

        ! Construct an initial estimate of pole locations
        call create_pole_estimate(freq, order, this%poles, rwork)

        ! Iteration Loop
        do i = 1, ni
            ! Compute the fit and update the pole locations
            call vector_fit(frf, s, this%poles, wghts, &
                this%model%A, this%model%B, this%model%C, this%model%D, &
                this%frf, this%residual, &
                iwork, cwork, rwork)

            ! Compute the RMS error of the current step
            this%rms(i) = norm2(abs(this%residual))
        end do
    end subroutine

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
    ! - a [out]: An N -by- N matrix.
    ! - b [out]: An N -by- 1 matrix.
    ! - c [out]: A 1 -by- N matrix.
    ! - d [out]: A 1 -by- 1 matrix.
    ! - fit [out]: An M-element array containing the fitted data.
    ! - delta [out]: An M-element array containing the difference between the
    !       fitted and raw data.
    ! - iwork: An X-element workspace array.
    !       X = N
    !   Breakdown:
    !   - N - cindex
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
    !       X = (2*M + 1)*S + N**2 + (4*M + 5)*N + 6*M + 5
    !           Where: S = MIN(2*M, 2*(N+1))
    !       Breakdown:
    !       - 2*M-by-2*(N+1) - Ar
    !       - MIN(2*M, 2*(N + 1)) - tau
    !       - 2*M-by-MIN(2*M, 2*(N + 1)) - Q
    !       - N+1 - escale
    !       - N+1 - x
    !       - 2*M - Br
    !       - N+1-by-N+1 - At
    !       - N+1 - Bt
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
            e1r, s2r, e2r, s3r, e3r, s4r, e4r, s5r, e5r, s6r, e6r, &
            s7r, e7r, s8r, e8r
        integer(int32), pointer, dimension(:) :: cindex
        real(real64) :: eps, zerotol, scale
        complex(real64), pointer, dimension(:,:) :: dk, ac, lambda
        complex(real64), pointer, dimension(:) :: cc, bc, bb
        real(real64), pointer, dimension(:,:) :: ar, q, at
        real(real64), pointer, dimension(:) :: tau, escale, x, br, bt

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
        s7r = e6r + 1
        e7r = s7r + (n + 1)**2 - 1
        s8r = e7r + 1
        e8r = s8r + n

        cindex => iwork(1:n)

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
        at(1:n+1,1:n+1) => dwork(s7r:e7r)
        bt => dwork(s8r:e8r)

        ! Determine which poles are complex-valued
        call label_complex_values(poles, zerotol, cindex)

        ! Build the denominator for each 's'
        call build_denominator(s, poles, cindex, dk)

        ! Compute scaling for the least-squares problem
        scale = norm2(abs(weights * f)) / real(ns)

        ! Extract the state space matrices
        call denom_to_state_space(scale, weights, f, dk, &
            ac, ar, bb, br, tau, q, escale, x, at, bt, c, d)

        ! Construct a complex-valued version of C
        call c_to_cmplx(c, cindex, cc)

        ! Compute the new pole locations (zeros)
        ! POLES is overwritten with the new pole location information
        call compute_zeros(poles, bc, cc, d, lambda)

        ! Determine which of the new poles are complex-valued
        call label_complex_values(poles, zerotol, cindex)

        ! Construct the overall state space matrices
        ! call build_denominator(s, poles, cindex, dk)
        call to_cmplx_state_space(weights, s, f, poles, cindex, dk, ac, &
            ar(:,1:n+1), bb, br, c, d, escale)
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
        i = 1
        do while (i <= n)
            if (cindex(i) == 0) then
                dk(:,i) = one / (s - poles(i))
                i = i + 1
            else if (cindex(i) == 1) then
                dk(:,i) = one / (s - poles(i)) + one / (s - conjg(poles(i)))
                dk(:,i+1) = j / (s - poles(i)) - j / (s - conjg(poles(i)))
                i = i + 2
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Construct the state-space matrices from the denominator.
    !
    ! - problem scaling factor.
    ! - weights: M-element weighting vector.
    ! - f: M-element frequency response vector containing data to be fitted.
    ! - dk: An M-by-(N+1) matrix.
    ! - ac: An M-by-2*(N+1) matrix.
    ! - ar: An 2*M-by-2*(N+1) matrix.
    ! - bc: An M-element array.
    ! - br: A 2*M-element array.
    ! - tau: A MIN(2*M,2*(N+1)) array.
    ! - q: An 2*M-by-MIN(2*M,2*(N+1)) matrix.
    ! - pvt: An N+1 array.
    ! - escale: An N+1 array.
    ! - x: An N+1 array.
    ! - a: An N+1-by-N+1 matrix.
    ! - b: An N+1-by-1 matrix.
    ! - c: A 1-by-N matrix.
    ! - d: A 1-by-1 matrix.
    subroutine denom_to_state_space(scale, weights, f, dk, ac, ar, bc, br, &
            tau, q, escale, x, a, b, c, d)
        use linalg_core

        ! Arguments
        real(real64), intent(in) :: scale
        real(real64), intent(in), dimension(:) :: weights
        complex(real64), intent(in), dimension(:) :: f
        complex(real64), intent(in), dimension(:,:) :: dk
        complex(real64), intent(out), dimension(:,:) :: ac
        complex(real64), intent(out), dimension(:) :: bc
        real(real64), intent(out), dimension(:,:) :: ar
        real(real64), intent(out), dimension(:) :: tau, escale, x, br
        real(real64), intent(out), dimension(:,:) :: q

        ! State Space Matrix Outputs
        real(real64), intent(out), dimension(:,:) :: a, c, d
        real(real64), intent(out), dimension(:) :: b

        ! Parameters
        real(real64), parameter :: tol_low = 1.0d-18
        real(real64), parameter :: tol_high = 1.0d18

        ! Local Variables
        integer(int32) :: i, ns, n, offset, ind, ind1, ind2
        real(real64) :: dnew

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
        
        ! Extract the trailing submatrix from R
        ind1 = offset + 1
        ind2 = 2 * offset

        ! Construct B
        b = q(size(q, 1), offset + 1:size(q, 2)) * scale * real(ns)

        ! Finish working on A
        do i = 1, offset
            ind = ind1 + i - 1
            escale(i) = 1.0d0 / norm2(ar(:,ind))
            ar(:,ind) = escale(i) * ar(:,ind)
        end do

        ! Solve A * X = B, for X.  Note: A is upper triangular
        a = ar(ind1:ind2,ind1:ind2)
        x = b   ! Protect B from being overwritten
        call solve_triangular_system(.true., .false., .true., a, x)

        ! Update X
        x = x * escale

        ! Build C & D
        C(1,:) = x(1:n)
        D(1,1) = x(offset)

        ! Ensure D isn't too big or too small
        if (abs(x(offset)) < tol_low .or. abs(x(offset)) > tol_high) then
            if (x(offset) == 0.0d0) then
                dnew = 1.0d0
            else if (abs(x(offset)) < tol_low) then
                dnew = sign(1.0d0, x(offset)) * tol_low
            else if (abs(x(offset)) > tol_high) then
                dnew = sign(1.0d0, x(offset)) * tol_high
            end if

            do i = 1, offset
                ac(:,i) = weights * dk(:,i)
            end do
            do i = 1, offset
                ac(:,offset+i) = -weights * dk(:,i) * f
            end do
            ar(1:ns,:) = real(ac)
            ar(ns+1:2*ns,:) = aimag(ac)
            bc = dnew * weights * f
            br(1:ns) = real(bc)
            br(ns+1:2*ns) = aimag(bc)

            ! Compute the QR factorization of AR, and then form Q & R fully
            call qr_factor(ar, tau)
            call form_qr(ar, tau, q)    ! R is stored in AR
            
            ! Extract the trailing submatrix from R, and store in the state-space
            ! matrix A
            ind1 = offset + 1
            ind2 = 2 * offset
            a = ar(ind1:ind2,ind1:ind2)
            
            ! Compute B = Q(:,IND1:IND2)**T * BR
            ! 
            ! - Q(:,IND1:IND2) is 2*NS-by-N
            ! - BR is 2*NS-by-1
            ! - B is then N-by-1
            call DGEMM('T', 'N', n, 1, 2*ns, 1.0d0, q, 2*ns, br, 2*ns, 0.0d0, b, n)

            ! Finish working on A
            do i = 1, offset
                escale(i) = 1.0d0 / norm2(a(:,i))
                a(:,i) = escale(i) * a(:,i)
            end do

            ! Solve A * X = B, for X.  Note: A is upper triangular
            x = b   ! Protect B from being overwritten
            call solve_triangular_system(.true., .false., .true., a, x)

            ! Update X
            x = x * escale
            
            ! Build C & D
            C(1,:) = x(1:n)
            D(1,1) = dnew
        end if
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
                cc(i) = cmplx(c(1,i), kind = real64)
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
                    c(m) = cmplx(real(koko), kind = real64)
                    c(m+1) = cmplx(aimag(koko), kind = real64)
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
        alpha = cmplx(-1.0d0 / d(1,1), kind = real64)
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
    ! - s: M-element array containing the frequency points.
    ! - f: M-element frequency response vector containing data to be fitted.
    ! - poles: An N-element array containing the current pole locations.
    ! - cindex: An N-element array determining what pole values are real,
    !       complex, and/or complex-conjugates.  See label_complex_values for
    !       more info.
    ! - dk: An M-by-(N+1) matrix.
    ! - ac: An M-by-2*(N+1) matrix.
    ! - ar: A 2*M-by-(N+1) matrix.
    ! - bc: An M element array.
    ! - br: A 2*M element array.
    ! - cr: A 1-by-N matrix.
    ! - dr: A 1-by-1 matrix.
    ! - escale: An N+1 element array.
    subroutine to_cmplx_state_space(weights, s, f, poles, cindex, dk, ac, ar, bc, br, cr, dr, escale)
        use linalg_core

        ! Arguments
        real(real64), intent(in), dimension(:) :: weights
        complex(real64), intent(in), dimension(:) :: s, f, poles
        integer(int32), intent(in), dimension(:) :: cindex
        complex(real64), intent(inout), dimension(:,:) :: dk
        complex(real64), intent(out), dimension(:,:) :: ac
        complex(real64), intent(out), dimension(:) :: bc
        real(real64), intent(out), dimension(:,:) :: ar, cr, dr
        real(real64), intent(out), dimension(:) :: escale, br

        ! Parameters
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: j = (0.0d0, 1.0d0)

        ! Local Variables
        integer(int32) :: i, ns, n

        ! Initialization
        ns = size(dk, 1)
        n = size(dk, 2) - 1 ! DK is Ns -by- (N + 1)

        ! Process
        i = 1
        do while (i <= n)
            if (cindex(i) == 0) then
                dk(:,i) = weights / (s - poles(i))
                i = i + 1
            else if (cindex(i) == 1) then
                dk(:,i) = weights / (s - poles(i)) + weights / (s - conjg(poles(i)))
                dk(:,i+1) = j * weights / (s - poles(i)) - j * weights / (s - conjg(poles(i)))
                i = i + 2
            end if
        end do

        do i = 1, n
            ac(:,i) = weights * dk(:,i)
        end do
        ac(:,n+1) = cmplx(weights, kind = real64)
        ar(ns+1:2*ns,:) = aimag(ac(1:ns,:))
        ar(1:ns,:) = real(ac(1:ns,:))
        bc(1:ns) = weights * f
        br(ns+1:2*ns) = aimag(bc(1:ns))
        br(1:ns) = real(bc(1:ns))
        
        ! Rescale A
        do i = 1, n + 1
            escale(i) = norm2(ar(:,i))
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
        fit = cmplx(d(1,1), kind = real64) ! Fill FIT with D(1,1)
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
        integer(int32), intent(in), dimension(:) :: cindex
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
            if (cindex(i) == 1) then
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
            else if (cindex(i) == 0) then
                a(i,i) = real(poles(i))
                b(i,1) = real(bc(i))
                c(1,i) = real(cc(i))
            end if

            ! Check i
            if (i >= n) exit
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Creates an estimate of pole locations used to initialize the fitting
    ! routine.
    !
    ! - freq: An N-element array containing the frequency points in rad/s.
    ! - order: The order of the system to fit (at least 2).
    ! - poles: An ORDER-element array where the pole estimates will be written.
    ! - work: An ORDER/2-element workspace array.
    subroutine create_pole_estimate(freq, order, poles, work)
        use fplot_core, only : linspace

        ! Arguments
        real(real64), intent(in), dimension(:) :: freq
        integer(int32), intent(in) :: order
        complex(real64), intent(out), dimension(:) :: poles
        real(real64), intent(out), dimension(:) :: work

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)
        complex(real64), parameter :: j = (0.0d0, 1.0d0)

        ! Local Variables
        integer(int32) :: i, k, npts, m
        real(real64) :: alf

        ! Initialization
        npts = size(freq)
        m = max(order / 2, 1)
        work(1:m) = linspace(freq(1), freq(npts), m)
        poles = zero
        k = 1
        do i = 1, m
            alf = -1e-2 * work(i)
            
            poles(k) = alf - j * work(i)
            poles(k+1) = alf + j * work(i)
            k = k + 2
        end do
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
    !
    subroutine residue(u, v, coeffs, poles, k)
        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        complex(real64), intent(out), dimension(:) :: coeffs, poles
        integer(int32), intent(out) :: k

        ! Parameters
        real(real64), parameter :: tol = 1.0d-3

        ! Local Variables
    end subroutine

! ------------------------------------------------------------------------------
    ! Computes the orthogonal polynomial coefficients required for the
    ! rational fraction polynomials method.  This is an implementation of
    ! the original MATLAB code found at:
    ! https://www.mathworks.com/matlabcentral/fileexchange/3805-rational-fraction-polynomial-method
    !
    ! Inputs:
    ! - rec: An N-element array containing the FRF measurements.
    ! - omega: An N-element array containing the frequency points (rad/sec).
    ! - phitheta: Weighting function (must be 1 for phi matrix, or 2 for
    !       the theta matrix).
    ! - kmax: Degree of the polynomial.
    ! - p [Output]: Matrix of the orthogonal polynomials evaluated at each
    !       frequency point.
    !       N-by-(KMAX+1)
    ! - coeff [Output]: Matrix used to transform between the orthogonal
    !       polynomial coefficients and the standard polynomial.
    !       (KMAX+1)-by-(KMAX+1)
    ! - work: A M-element workspae array.
    !       M = (2*KMAX + 8) * N
    ! - cwork: A P-element workspace array.
    !       P = KMAX**2 + 3*KMAX + 2
    subroutine orthogonal(rec, omega, phitheta, kmax, p, coeff, work, cwork)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: rec
        real(real64), intent(in), dimension(:) :: omega
        integer(int32), intent(in) :: phitheta, kmax
        complex(real64), intent(out), dimension(:,:) :: coeff, p
        real(real64), intent(out), dimension(:), target :: work
        complex(real64), intent(out), dimension(:), target :: cwork

        ! Parameters
        complex(real64), parameter :: j = (0.0d0, 1.0d0)
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: i, n, &
            e1, s2, e2, s3, e3, s4, e4, s5, e5, s6, e6, s7, e7, s8, e8, &
            ec1, sc2, ec2
        real(real64) :: vkml, dk
        real(real64), pointer, dimension(:) :: q, r_minus1, r_o, t, t1
        real(real64), pointer, dimension(:,:) :: r, rt, ct
        complex(real64), pointer, dimension(:,:) :: jk, cc

        ! Initialization
        n = size(omega)

        ! Pointer Assignment
        e1 = n
        s2 = e1 + 1
        e2 = s2 + n - 1
        s3 = e2 + 1
        e3 = s3 + n - 1
        s4 = e3 + 1
        e4 = s4 + n * (2 + kmax) - 1
        s5 = e4 + 1
        e5 = s5 + n * (1 + kmax) - 1
        s6 = e5 + 1
        e6 = s6 + n - 1
        s7 = e6 + 1
        e7 = s7 + n - 1
        s8 = e7 + 1
        e8 = s8 + (kmax + 1) * (kmax + 2) - 1

        ec1 = kmax + 1
        sc2 = ec1 + 1
        ec2 = sc2 + (kmax + 1)**2

        q => work(1:e1)                 ! N-element
        r_minus1 => work(s2:e2)         ! N-element
        r_o => work(s3:e3)              ! N-element
        r(1:n,1:2+kmax) => work(s4:e4)  ! N-by-2+KMAX
        rt(1:n,1:1+kmax) => work(s5:e5) ! N-by-1+KMAX
        t => work(s6:e6)                ! N-element
        t1 => work(s7:e7)               ! N-element
        ct(1:kmax+1,1:kmax+2) => work(s8:e8)    ! KMAX+1 -by- KMAX+2
        jk(1:1,1:kmax+1) => cwork(1:ec1)! 1-by-1+KMAX
        cc(1:kmax+1,1:kmax+1) => cwork(sc2:ec2) ! KMAX+1 -by- KMAX+1

        ! Process
        if (phitheta == 1) then
            q = 1.0d0
        else if (phitheta == 2) then
            q = (abs(rec))**2
        end if
        r_minus1 = 0.0d0
        r_o = 1.0d0 / sqrt(2.0d0 * sum(q))
        r(:,1) = r_minus1
        r(:,2) = r_o

        ct = 0.0d0
        ct(1,2) = 1.0d0 / sqrt(2.0d0 * sum(q))
        do i = 1, kmax
            t = omega * r(:,i+1) * r(:,i) * q
            vkml = 2.0d0 * sum(t)
            
            t = omega * r(:,i+1) - vkml * r(:,i)
            t1 = (t**2) * q
            dk = sqrt(2.0d0 * sum(t1))
            r(:,2+i) = t / dk
            ct(:,i+2) = -vkml * ct(:,i)
            ct(2:i+1,i+2) = ct(2:i+1,i+2) + ct(1:i,i+1)
            ct(:,i+2) = ct(:,i+2) / dk
        end do
        rt = r(:,2:2+kmax)  ! Orthogonal Polynomial Matrix
        coeff = cmplx(ct(:,2:2+kmax), kind = real64)
        do i = 0, kmax
            p(:,i+1) = r(:,i+1) * j**i
            jk(1,i+1) = j**i
        end do

        ! Compute coeff = (JK**H * JK) * COEFF
        ! JK is 1-by-(KMAX+1)
        ! COEFF is (KMAX+1)-by-(KMAX+1)
        call ZGEMM('C', 'N', kmax+1, kmax+1, 1, one, jk, 1, jk, 1, zero, cc)
        coeff = matmul(cc, coeff)
    end subroutine

! ------------------------------------------------------------------------------
    ! This is an implementation of MATLAB's MPOLES routine that identifies
    ! repeated poles and their multiplicities.
    !
    ! - p: N-element list of poles.
    ! - mpoles_tol: Tolerance for similarity checking.
    ! - reorder: True to sort the poles.
    ! - mults [Output]: N-element list of pole multiplicities.
    ! - indx [Output]: N-element list of indices used to sort P.
    ! - iwork: 3*N-element workspace array.
    ! - work: N-element workspace array.
    subroutine mpoles(p, mpoles_tol, reorder, mults, indx, iwork, work)
        use linalg_core, only : sort

        ! Arguments
        complex(real64), intent(inout), dimension(:) :: p
        real(real64), intent(in) :: mpoles_tol
        logical, intent(in) :: reorder
        integer(int32), intent(out), dimension(:) :: mults, indx
        integer(int32), intent(out), dimension(:), target :: iwork
        real(real64), intent(out), dimension(:), target :: work

        ! Local Variables
        integer(int32) :: i, ii, j, np, nind, kk, done
        integer(int32), pointer, dimension(:) :: ind, jkl, track
        real(real64), pointer, dimension(:) :: test

        ! Initialization
        np = size(p)

        ! Pointer assignment
        ind => iwork(1:np)
        jkl => iwork(np+1:2*np)
        track => iwork(2*np+1:3*np)
        test => work(1:np)

        ! Initialization
        do i = 1, np
            ind(i) = i
        end do

        ! Reorder - if requested
        if (reorder) then
            test = -abs(p)
            call sort(test, ind, .true.)
            p = p(ind)
        end if

        ! Process
        mults = 0.0d0
        indx = 0
        ii = 1
        do while (np > 1)
            test(1:np) = abs(p(1) - p(1:np))
            if (abs(p(1)) > 0.0d0) then
                call find_less_than(test(1:np), mpoles_tol * abs(p(1)), &
                    jkl, nind)
            else
                call find_less_than(test(1:np), mpoles_tol, jkl, nind)
            end if

            track(1:np) = 1
            do i = 1, nind
                kk = i - 1
                mults(ii+kk) = kk + 1
                done = jkl(i)
                indx(ii+kk) = ind(done)
                track(done) = -1
            end do

            ! Reshape P and IND
            j = 0
            do i = 1, np
                if (track(i) /= -1) then
                    j = j + 1
                    p(j) = p(i)
                    ind(j) = ind(i)
                end if
            end do

            ! Increment the tracking variables
            ii = ii + nind
            np = np - nind
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Finds all items in an array less than the specified value.
    !
    ! - arg: An N-element array to search.
    ! - item: The comparison item.
    ! - ind [Output]: An N-element buffer array containing the output.
    ! - nind [Output]: The number of values written to ind.
    subroutine find_less_than(arg, item, ind, nind)
        ! Arguments
        real(real64), intent(in), dimension(:) :: arg
        real(real64), intent(in) :: item
        integer(int32), intent(out), dimension(:) :: ind
        integer(int32), intent(out) :: nind

        ! Local Variables
        integer(int32) :: i

        ! Process
        nind = 0
        do i = 1, size(arg)
            if (arg(i) < item) then
                nind = nind + 1
                ind(nind) = i
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Implementation of the MATLAB routine RESI2 for computing the residue of
    ! a repeated pole.
    !
    !
    subroutine resi2(u, v, pole, n, k, coeff)
        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        complex(real64), intent(in), dimension(:) :: pole
        integer(int32), intent(in) :: n, k
        complex(real64), intent(out), dimension(:) :: coeff

        ! Local Variables
    end subroutine

! ------------------------------------------------------------------------------
    ! Implementation of the MATLAB routine POLYDER for computing the derivative
    ! of the polynomial ratio U / V as A / B.
    !
    ! - u: An M-element array.
    ! - v: An N-element array.
    ! - a [Output]: An M+N-2 element array.
    ! - b [Output]: An N+N-1 element array.
    ! - work: 3 * (M + N) - 6 element workspace array.
    subroutine polyder(u, v, a, b, na, nb, work)
        use signals, only : conv
        
        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        real(real64), intent(out), dimension(:) :: a, b
        integer(int32), intent(out) :: na, nb
        real(real64), intent(out), dimension(:), target :: work

        ! Local Variables
        integer(int32) :: i, j, nu, nv, e1, s2, e2, s3, e3, &
            s4, e4, ind
        real(real64), pointer, dimension(:) :: up, vp, a1, a2

        ! Initialization
        nu = size(u)
        nv = size(v)
        na = 0
        nb = 0

        ! Assign the pointers
        e1 = nu - 1
        s2 = e1 + 1
        e2 = s2 + nv - 1
        s3 = e2 + 1
        e3 = s3 + nu + nv - 3
        s4 = e3 + 1
        e4 = s4 + nu + nv - 3
        
        up => work(1:e1)        ! NU-1
        vp => work(s2:e2)       ! NV-1
        a1 => work(s3:e3)       ! (NU-1) + NV - 1 = NU + NV - 2
        a2 => work(s4:e4)       ! NU + (NV-1) - 1 = NU + NV - 2

        ! Array Initialization
        j = nu - 1
        do i = 1, nu - 1
            up(i) = u(i) * j
            j = j - 1
        end do

        j = nv - 1
        do i = 1, nv - 1
            vp(i) = v(i) * j
            j = j - 1
        end do

        ! Process
        a1 = conv(up, v)
        a2 = conv(u, vp)

        ! Pad A1 & A2 with zeros to ensure they're the same length.  If all
        ! goes well, they should be
        a = a1 - a2

        ! Compute B
        b = conv(v, v)

        ! Trim zeros from A and B
        ind = 0
        do i = 1, size(a)
            if (a(i) == 0.0d0) then
                ind = i
                exit
            end if
        end do
        if (ind == 0) then
            na = size(a)
        else
            na = size(a) - ind + 1
            a(1:na) = a(ind:size(a))
        end if

        ind = 0
        do i = 1, size(b)
            if (b(i) == 0.0d0) then
                ind = i
                exit
            end if
        end do
        if (ind == 0) then
            nb = size(b)
        else
            nb = size(b) - ind + 1
            b(1:nb) = b(ind:size(b))
        end if
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
