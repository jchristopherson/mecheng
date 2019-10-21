! vibrations_fit.f90

! TO DO: Continue debugging at line 763

submodule (vibrations) vibrations_fit
contains
! ------------------------------------------------------------------------------
    module subroutine fit_init(this, order, nfreq, niter, err)
        ! Arguments
        class(frf_fitting_tool), intent(inout) :: this
        integer(int32), intent(in) :: order, nfreq, niter
        class(errors), intent(inout), optional, target :: err

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (order < 2) then
            call errmgr%report_error("fit_init", &
                "The requested model order must be at least 2.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (order >= nfreq) then
            call errmgr%report_error("fit_init", &
                "The requested model order must not equal or exceed the number " // &
                "of available data points", MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (niter < 1) then
            call errmgr%report_error("fit_init", &
                "There must be at least 1 iteration.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        if (allocated(this%frf)) deallocate(this%frf)
        if (allocated(this%poles)) deallocate(this%poles)
        if (allocated(this%residual)) deallocate(this%residual)
        if (allocated(this%rms)) deallocate(this%rms)
        if (allocated(this%model%A)) deallocate(this%model%A)
        if (allocated(this%model%B)) deallocate(this%model%B)
        if (allocated(this%model%C)) deallocate(this%model%C)
        if (allocated(this%model%D)) deallocate(this%model%D)

        ! Allocate memory
        allocate(this%frf(nfreq), stat = flag)
        if (flag == 0) allocate(this%poles(order), stat = flag)
        if (flag == 0) allocate(this%residual(nfreq), stat = flag)
        if (flag == 0) allocate(this%rms(niter), stat = flag)
        if (flag == 0) allocate(this%model%A(order, order), stat = flag)
        if (flag == 0) allocate(this%model%B(order, 1), stat = flag)
        if (flag == 0) allocate(this%model%C(1, order), stat = flag)
        if (flag == 0) allocate(this%model%D(1, 1), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fit_init", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Initialize each array
        this%frf = zero
        this%poles = zero
        this%residual = zero
        this%rms = zero
    end subroutine

! ------------------------------------------------------------------------------
    pure module function fit_get_iter_count(this) result(n)
        class(frf_fitting_tool), intent(in) :: this
        integer(int32) :: n
        if (allocated(this%rms)) then
            n = size(this%rms)
        else
            n = 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure module function fit_get_relax(this) result(x)
        class(frf_fitting_tool), intent(in) :: this
        logical :: x
        x = this%m_relax
    end function

! --------------------
    module subroutine fit_set_relax(this, x)
        class(frf_fitting_tool), intent(inout) :: this
        logical, intent(in) :: x
        this%m_relax = x
    end subroutine

! ------------------------------------------------------------------------------
    pure module function fit_get_stabalize(this) result(x)
        class(frf_fitting_tool), intent(in) :: this
        logical :: x
        x = this%m_stabalize
    end function

! --------------------
    module subroutine fit_set_stabalize(this, x)
        class(frf_fitting_tool), intent(inout) :: this
        logical, intent(in) :: x
        this%m_stabalize = x
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
    module subroutine fit_frf_1(this, freq, amp, phase, order, weights, niter, err)
        use constants, only : pi
        use curvefit_statistics, only : mean

        ! Arguments
        class(frf_fitting_tool), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: freq, amp, phase, weights
        integer(int32), intent(in), optional :: niter
        class(errors), intent(inout), optional, target :: err

        ! Parameters
        complex(real64), parameter :: i1 = (0.0d0, 1.0d0)

        ! Local Variables
        integer(int32) :: m, n, ni, liwork, lcwork, lwork, flag
        integer(int32), allocatable, dimension(:) :: iwork
        real(real64), allocatable, dimension(:) :: work, omega, theta, r2
        complex(real64), allocatable, dimension(:) :: cwork, frf
        logical :: stabalize, relax
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Initialization
        m = order
        n = size(freq)
        liwork = m
        lcwork = (3 * n + 2) * m + 5 * n
        lwork = 2 * m**2 + (8 * n + 10) * m + 10 * n + 9
        ni = 5
        relax = this%get_allow_relaxation()
        stabalize = this%get_stabalize_poles()
        if (present(niter)) ni = niter
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (order < 2) then
            call errmgr%report_error("fit_frf_1", &
                "The requested model order must be at least 2.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (order >= n) then
            write(errmsg, '(AI0AI0A)') "The requested model order ", &
                order, " must not equal or exceed the number of data points ", &
                n, "."
            call errmgr%report_error("fit_frf_1", trim(errmsg), &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (ni < 1) then
            call errmgr%report_error("fit_frf_1", &
                "The number of iterations must be at least 1.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (size(amp) /= n) then
            write(errmsg, '(AI0AI0A)') &
                "The input amplitude array was expected to have ", n, &
                " elements, but was found to have ", size(amp), " elements."
            call errmgr%report_error("fit_frf_1", trim(errmsg), &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(phase) /= n) then
            write(errmsg, '(AI0AI0A)') &
                "The input phase array was expected to have ", n, &
                " elements, but was found to have ", size(phase), " elements."
            call errmgr%report_error("fit_frf_1", trim(errmsg), &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(weights) /= n) then
            write(errmsg, '(AI0AI0A)') &
                "The input weighting array was expected to have ", n, &
                " elements, but was found to have ", size(weights), " elements."
            call errmgr%report_error("fit_frf_1", trim(errmsg), &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Allocate workspace arrays
        allocate(iwork(liwork), stat = flag)
        if (flag == 0) allocate(work(lwork), stat = flag)
        if (flag == 0) allocate(work(lcwork), stat = flag)
        if (flag == 0) allocate(omega(n), stat = flag)
        if (flag == 0) allocate(frf(n), stat = flag)
        if (flag == 0) allocate(theta(n), stat = flag)
        if (flag == 0) allocate(r2(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fit_frf_1", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Ensure the FRF_FITTING_TOOL is properly initialized
        call this%initialize(order, n, ni, errmgr)

        ! Establish a frequency vector in units of rad/s
        omega = 2.0d0 * pi * freq

        ! Generate an estimate of pole locations
        call estimate_poles(omega, order, this%poles)

        ! Convert the frequency vector to its complex-valued representation
        omega = i1 * omega

        ! Convert the frequency response into its complex-valued form
        theta = pi * phase / 1.8d2
        frf = amp * (cos(theta) + i1 * sin(theta))

        ! Fitting Process
        do i = 1, ni
            ! Compute the fit
            call frf_fit_core(omega, frf, this%poles, weights, relax, stabalize, &
                this%model, this%frf, this%residual, work, cwork, iwork, errmgr)
            
            ! Compute the RMS of the error
            r2 = real(this%residual**2)
            this%rms(i) = sqrt(mean(r2))
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Applies a relaxed vector fitting algorithm to the problem of fitting a
    ! frequency response function of unknown origin.  The code is a slightly 
    ! modified version of the MATLAB code by Bjorn Gustavsen available at
    ! http://www.energy.sintef.no/Produkt/VECTFIT/index.asp.
    !
    ! - s: An N-element array containing the frequency values (rad/s).
    ! - frf: An N-element array containing the frequency response to fit.
    ! - poles: An M-element array containing the estimate of the system poles.
    !       On output, this is updated to a new estimate of pole locations.
    ! - weights: An N-element weighting vector that corresponds to each
    !       input frequency.
    ! - relax: Set to true to allow for relaxation; else, false.
    ! - stable: Set to true to stabilize any unstable poles; else, false to
    !       allow unstable poles.
    ! - mdl [Output]: A state-space model where the matrices are dimensioned
    !       as follows:
    !       - A: M-by-M
    !       - B: M-by-1
    !       - C: 1-by-M
    !       - D: 1-by-1
    ! - fit [Output]: An N-element array containing the fitted data.
    ! - dif [Output]: An N-element array containing the difference between
    !       fitted and actual data.
    ! - work: A K-element workspace array where K is 2*M**2 + (8*N + 10)*M + 10*N + 9.
    ! - cwork: An L-element workspace array where L is (3*N + 2)*M + 5*N.
    ! - iwork: An M-element workspace array.
    ! - err: An error handler used to capture any errors from QR factorization
    !       or eigenvalue extraction routines.
    !
    ! References:
    ! - B. Gustavsen and A. Semlyen, "Rational approximation of frequency       
    !   domain responses by Vector Fitting", IEEE Trans. Power Delivery,        
    !   vol. 14, no. 3, pp. 1052-1061, July 1999.
    ! - B. Gustavsen, "Improving the pole relocating properties of vector
    !   fitting", IEEE Trans. Power Delivery, vol. 21, no. 3, pp. 1587-1592,
    !   July 2006. 
    ! - D. Deschrijver, M. Mrozowski, T. Dhaene, and D. De Zutter,
    !   "Macromodeling of Multiport Systems Using a Fast Implementation of
    !   the Vector Fitting Method", IEEE Microwave and Wireless Components 
    !   Letters, vol. 18, no. 6, pp. 383-385, June 2008.
    subroutine frf_fit_core(s, frf, poles, weights, relax, stable, mdl, &
            fit, dif, work, cwork, iwork, err)
        use linalg_core

        ! Arguments
        complex(real64), intent(in), dimension(:) :: s, frf
        complex(real64), intent(inout), dimension(:) :: poles
        real(real64), intent(in), dimension(:) :: weights
        logical, intent(in) :: relax, stable
        class(state_space), intent(out) :: mdl
        real(real64), intent(out), target, dimension(:) :: work
        complex(real64), intent(out), target, dimension(:) :: fit, dif, cwork
        integer(int32), intent(out), target, dimension(:) :: iwork
        class(errors), intent(inout) :: err

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: i1 = (0.0d0, 1.0d0)
        real(real64), parameter :: tolLow = 1.0d-18
        real(real64), parameter :: tolHi = 1.0d18

        ! Local Variables
        integer(int32) :: i, j, m, n, np, np1, ind1, ind2, &
            e1, s2, e2, s3, e3, s4, e4, s5, e5, s6, e6, s7, e7, s8, e8, &
            e1c, s2c, e2c, s3c, e3c, s4c, e4c, s5c, e5c
        real(real64) :: scale, Dnew, r1, r2
        integer(int32), pointer, dimension(:) :: cindex
        real(real64), pointer, dimension(:) :: tau, x, Escale, b
        real(real64), pointer, dimension(:,:) :: A, Q, R, lambda
        complex(real64) :: pole
        complex(real64), pointer, dimension(:) :: bc, Cc, eigenvals
        complex(real64), pointer, dimension(:,:) :: Dk, Ac

        ! Initialization
        n = size(s)
        np = size(poles)
        np1 = np + 1

        ! Define limits within the workspace arrays
        e1 = (2 * n + 1) * 2 * np1
        s2 = e1 + 1
        e2 = s2 + 2 * np1 - 1
        s3 = e2 + 1
        e3 = s3 + (2 * n + 1) * 2 * np1 - 1
        s4 = e3 + 1
        e4 = s4 + np1 * np1 - 1
        s5 = e4 + 1
        e5 = s5 + np
        s6 = e5 + 1
        e6 = s6 + np
        s7 = e6 + 1
        e7 = s7 + 2 * n - 1
        s8 = e7 + 1
        e8 = s8 + np * np - 1

        e1c = n * np1
        s2c = e1c + 1
        e2c = s2c + n * (2* np1) - 1
        s3c = e2c + 1
        e3c = s3c + 2 * n - 1
        s4c = e3c + 1
        e4c = s4c + np - 1
        s5c = e4c + 1
        e5c = s5c + np - 1

        ! Assign Pointers
        cindex => iwork(1:np)               ! NP-element

        A(1:2*n+1,1:2*np1) => work(1:e1)    ! 2*N+1-by-2*(NP+1)
        tau => work(s2:e2)                  ! 2*(NP+1)-element
        Q(1:2*n+1,1:2*np1) => work(s3:e3)   ! 2*N+1-by-2*(NP+1)
        R(1:np1,1:np1) => work(s4:e4)       ! (NP+1)-by-(NP+1)
        x => work(s5:e5)                    ! (NP+1)-element
        Escale => work(s6:e6)               ! (NP+1)-element
        b => work(s7:e7)                    ! 2*N-element
        lambda(1:np,1:np) => work(s8:e8)    ! NP-by-NP

        Dk(1:n,1:np1) => cwork(1:e1c)       ! N-by-(NP+1)
        Ac(1:n,1:2*np1) => cwork(s2c:e2c)   ! N-by-2*(NP+1)
        bc => cwork(s3c:e3c)                ! 2*N-element
        Cc => cwork(s4c:e4c)                ! NP-element
        eigenvals => cwork(s5c:e5c)         ! NP-element

        ! Locate the complex poles
        call find_complex(poles, cindex)

        ! Build the system matrices
        do i = 1, np
            if (cindex(i) == 0) then
                Dk(:,i) = one / (s - poles(i))
            else
                Dk(:,i) = one / (s - poles(i)) + one / (s - conjg(poles(i)))
                Dk(:,i+1) = i1 / (s - poles(i)) - i1 / (s - conjg(poles(i)))
            end if
        end do
        Dk(:,np1) = one
        scale = norm2(abs(weights * frf)) / real(n)

        ! **********************************************************************
        if (relax) then
            do i = 1, np1
                Ac(:,i) = weights * Dk(:,i)
                Ac(:,i+np1) = -weights * Dk(:,i) * frf
            end do
            A(1:n,:) = real(Ac)
            A(n+1:2*n,:) = aimag(Ac)
            
            ! Establish the criteria for sigma
            A(2*n+1,1:np1) = 0.0d0
            do i = 1, np1
                A(2*n+1,np1+i) = real(scale * sum(Dk(:,i)))
            end do

            ! Compute the QR factorization - economy version of Q.
            ! R overwrites A, and it is upper triangular
            call qr_factor(A, tau, err = err)
            if (err%has_error_occurred()) return

            ! Form the economy version of Q [2*N+1 -by- 2*(NP+1)]
            call form_qr(A, tau, Q, err = err)
            if (err%has_error_occurred()) return

            ind1 = np + 2
            ind2 = 2 * np1
            R = A(ind1:ind2,ind1:ind2)
            x = scale * real(n) * Q(size(Q, 1), ind1:ind2)

            do i = 1, np1
                Escale(i) = 1.0d0 / norm2(R(:,i))
                R(:,i) = Escale(i) * R(:,i)
            end do

            ! Solve the upper triangular system
            call solve_triangular_system(.true., .false., .true., R, x, err)
            if (err%has_error_occurred()) return

            ! Recover the scaling
            x = x * Escale
        end if

        ! Deal with the situation when no relaxation is requested, or D 
        ! is too small
        if (.not.relax .or. abs(x(np1)) < tolLow .or. abs(x(np1)) > tolHi) then
            if (relax) then
                Dnew = 1.0d0
            else
                if (x(np1) == 0.0d0) then
                    Dnew = 1.0d0
                else if (abs(x(np1)) < tolLow) then
                    Dnew = sign(tolLow, x(np1))
                else if (abs(x(np1)) > tolHi) then
                    Dnew = sign(tolHi, x(np1))
                end if
            end if

            do i = 1, np1
                Ac(:,i) = weights * Dk(:,i)
            end do
            do i = 1, np
                Ac(:,i+np1) = -weights * Dk(:,i) * frf
            end do
            bc = Dnew * weights * frf

            A(1:n,1:2*np+1) = real(Ac(:,1:2*np+1))
            A(n+1:2*n,1:2*np+1) = aimag(Ac(:,1:2*np+1))
            b(1:n) = real(bc)
            b(n+1:2*n) = aimag(bc)

            ! Compute the QR factorization of the 2*N-by-2*NP+1 matrix A
            call qr_factor(A(1:2*n,1:2*np+1), tau(1:2*np+1), err = err)
            if (err%has_error_occurred()) return

            ! Form the economy version of Q [2*N -by- 2*(NP+1)]
            call form_qr(A(1:2*n,1:2*np+1), tau(1:2*np+1), Q, err = err)
            if (err%has_error_occurred()) return

            ind1 = np + 2
            ind2 = 2 * np + 1
            R = A(ind1:ind2,ind1:ind2)                  ! R is NP-by-NP

            ! Compute X = Q**T * B where Q**T is NP-by-2*N & b is 2*N such that x is NP
            call DGEMV('T', 2*n, np, 1.0d0, Q(1:2*n,ind1:ind2), 2*n, b, 1, 0.0d0, x(1:np), 1)

            ! Scale the problem
            do i = 1, np
                Escale(i) = 1.0d0 / norm(R(:,i))
                R(:,i) = Escale(i) * R(:,i)
            end do

            ! Solve the upper-triangular problem
            call solve_triangular_system(.true., .false., .true., R(1:np,1:np), x(1:np), err)
            if (err%has_error_occurred()) return

            ! Recover the scaling
            x(1:np) = x(1:np) * Escale(1:np)

            ! Store Dnew
            x(np1) = Dnew
        end if

        ! Now, generate an estimate for C and D
        mdl%C(1,1:np) = x(1:np)
        mdl%D(1,1) = x(np1)

        ! **********************************************************************
        ! Make C complex-valued
        i = 1
        do while (i <= np)
            if (cindex(i) == 1) then
                r1 = mdl%C(1,i)
                r2 = mdl%C(1,i+1)
                Cc(i) = r1 + i1 * r2
                Cc(i+1) = r1 - i1 * r2
                i = i + 2
            else
                Cc(i) = zero
                i = i + 1
            end if
        end do

        ! **********************************************************************
        ! Compute the zeros
        lambda = 0.0d0
        m = 0
        mdl%B = 0.0d0
        mdl%C = 0.0d0
        do i = 1, np
            m = m + 1
            if (m < np) then
                pole = poles(m)
                if (abs(pole) > abs(real(pole))) then
                    ! The pole is complex-valued
                    lambda(m+1,m) = -aimag(pole)
                    lambda(m,m+1) = aimag(pole)
                    lambda(m,m) = real(pole)
                    lambda(m+1,m+1) = real(pole)

                    mdl%B(m,1) = 2.0d0
                    mdl%B(m+1,1) = 0.0d0

                    mdl%C(1,m) = real(Cc(m))
                    mdl%C(1,m+1) = aimag(Cc(m))
                end if
            end if
        end do

        ! Compute ZER = LAMBDA - B * C / D - use LAMBDA to store ZER
        call DGEMM('N', 'N', np, np, 1, -1.0d0, mdl%B, np, mdl%C, 1, 1.0d0, lambda, np)

        ! Compute the eigenvalues - lambda is overwritten by this call
        call eigen(lambda, eigenvals, err = err)
        if (err%has_error_occurred()) return

        ! Force stability?
        if (stable) then
            do i = 1, np
                if (real(eigenvals(i)) > 0.0d0) then
                    eigenvals(i) = eigenvals(i) - 2.0d0 * real(eigenvals(i))
                end if
            end do
        end if
        
        ! Sort the eigenvalus in ascending order
        call sort(eigenvals, .true.)

        ! Move any real poles to the beginning of the eigenvalue list
        do i = 1, np
            do j = i+1, np
                if (aimag(eigenvals(j)) == 0.0d0 .and. aimag(eigenvals(i)) /= 0.0d0) then
                    pole = eigenvals(i);
                    eigenvals(i) = eigenvals(j)
                    eigenvals(j) = pole
                end if
            end do
        end do
        m = 0
        do i = 1, np
            if (aimag(eigenvals(i)) == 0.0d0) then
                m = i
            end if
        end do
        if (m < np) then
            call sort(eigenvals(m+1:np), .true.)
        end if
        eigenvals = eigenvals - 2.0d0 * i1 * aimag(eigenvals)

        ! **********************************************************************
        ! Residual Identification
        poles = eigenvals
        call find_complex(poles, cindex)

        ! Compute the new fitting using the new poles estimate
        do i = 1, np
            if (cindex(i) == 0.0d0) then
                Dk(:,i) = weights / (s - poles(i))
            else if (cindex(i) == 1) then
                Dk(:,i) = weights * (one / (s - poles(i)) + one / (s - conjg(poles(i))))
                Dk(:,i+1) = i1 * weights * (one / (s - poles(i)) - one / (s - conjg(poles(i))))
            end if
        end do

        Ac(:,1:np) = Dk(:,1:np)
        Ac(:,np1) = weights
        bc = weights * frf
        A(1:n,1:np1) = real(Ac(:,1:np1))
        A(n+1:2*n,1:np1) = aimag(Ac(:,1:np1))
        b(1:n) = real(bc)
        b(n+1:2*n) = aimag(bc)

        ! Scale the matrices
        do i = 1, np1
            Escale(i) = norm2(A(:,i))
            A(:,i) = A(:,i) / Escale(i)
        end do

        ! Solve the least-squares problem
        ! A is 2*N-by-NP+1 & b is 2*N, so X is NP+1
        call solve_least_squares(A(1:2*n,1:np1), b, err = err)
        if (err%has_error_occurred()) return

        ! Rescale the solution
        x = b(1:np1) / Escale

        ! Redefine C & D
        mdl%C(1,1:np) = x(1:np)
        mdl%D(1,1) = x(np1)

        ! Make C complex-valued
        i = 1
        do while (i <= np)
            if (cindex(i) == 1) then
                r1 = mdl%C(1,i)
                r2 = mdl%C(1,i+1)
                Cc(i) = r1 + i1 * r2
                Cc(i+1) = r1 - i1 * r2
                i = i + 2
            else
                Cc(i) = zero
                i = i + 1
            end if
        end do

        ! **********************************************************************
        ! Compute the fit, and then determine the error
        do i = 1, np
            Dk(:,i) = one / (s - poles(i))
        end do
        
        ! Compute FIT = Dk(:,1:np) * Cc + D
        fit = mdl%D(1,1)
        call ZGEMV('N', n, np, one, Dk(:,1:np), n, Cc, 1, one, fit, 1)

        ! Compute the error
        dif = fit - frf

        ! **********************************************************************
        ! Convert into a real-valued state-space model
        j = 0
        mdl%A = 0.0d0
        mdl%B = 1.0d0
        do i = 1, np
            j = j + 1
            if (cindex(i) == 1) then
                mdl%A(j,j) = real(poles(j))
                mdl%A(j+1,j) = -aimag(poles(j))
                mdl%A(j,j+1) = aimag(poles(j))
                mdl%A(j+1,j+1) = mdl%A(j,j)

                mdl%B(j,1) = 2.0d0 * real(mdl%B(j,1))
                mdl%B(j+1,1) = 0.0d0
                ! mdl%B(j+1,1) = -2.0d0 * aimag(mdl%B(j,1))

                mdl%C(1,j) = real(Cc(j))
                mdl%C(1,j+1) = aimag(Cc(j))
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Computes an estimate of poles distributed over the frequency range of
    ! interest.
    !
    ! - omega: An N-element array containing the frequency values (rad/s).
    ! - order: The order of the fit being computed.
    ! - poles: An ORDER-element array where the poles will be written.
    subroutine estimate_poles(omega, order, poles)
        use fplot_core, only : linspace

        ! Arguments
        real(real64), intent(in), dimension(:) :: omega
        integer(int32), intent(in) :: order
        complex(real64), intent(out), dimension(:) :: poles

        ! Parameters
        complex(real64), parameter :: j = (0.0d0, 1.0d0)
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: npts, i, k
        real(real64), allocatable, dimension(:) :: beta
        real(real64) :: alf

        ! Initialization
        npts = size(omega)
        beta = linspace(omega(1), omega(npts), order / 2)
        poles = zero
        k = 1
        do i = 1, size(beta)
            alf = -1.0d-2 * beta(i)
            poles(k) = alf - j * beta(i)
            poles(k+1) = conjg(poles(k))
            k = k + 2
        end do
    end subroutine

! ------------------------------------------------------------------------------
    ! Finds and labels real, complex and complex-conjugates in an array.
    !
    ! - x: The N-element array to search.
    ! - ind: The N-element array containing the following codes to denote
    !       the status of a value.
    !       * 0: Real-Value
    !       * 1: Complex-Value
    !       * 2: Complex-Conjugate
    subroutine find_complex(x, ind)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        integer(int32), intent(out), dimension(:) :: ind

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: eps, tol

        ! Initialization
        n = size(x)
        eps = epsilon(eps)
        tol = 2.0d0 * eps

        ! Process
        do i = 1, n
            if (abs(aimag(x(i))) < tol) then
                ind(i) = 0
            else
                if (i == 1) then
                    ind(i) = 1
                else
                    if (abs(aimag(x(i)) + aimag(x(i-1))) <= tol) then
                        ind(i) = 2
                    else
                        ind(i) = 1
                    end if
                end if
            end if
        end do
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
    
end submodule
