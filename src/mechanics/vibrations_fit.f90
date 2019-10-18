! vibrations_fit.f90

! TO DO: Continue debugging at line 763

submodule (vibrations) vibrations_fit
contains
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
    !
    !
    ! - s: An N-element array containing the frequency values (rad/s).
    ! - frf: An N-element array containing the frequency response to fit.
    ! - poles: An M-element array containing the estimate of the system poles.
    !       On output, this is updated to a new estimate of pole locations.
    ! - work: An Z-element workspace array where:
    !       Z:
    !     The breakdown is as follows:
    ! - cwork: A Y-element workspace array where:
    !       Y:
    !     The breakdown is as follows:
    ! - iwork: An M-element workspace array.
    subroutine frf_fit_core(omega, s, frf, poles, weights, relax, work, cwork, iwork, err)
        use linalg_core, only : qr_factor, form_qr, solve_triangular_system

        ! Arguments
        complex(real64), intent(in), dimension(:) :: s, frf
        complex(real64), intent(inout), dimension(:) :: poles
        real(real64), intent(in), dimension(:) :: weights
        logical, intent(in) :: relax
        real(real64), intent(out), target, dimension(:) :: work
        complex(real64), intent(out), target, dimension(:) :: cwork
        integer(int32), intent(out), target, dimension(:) :: iwork
        class(errors), intent(inout) :: err

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: i1 = (0.0d0, 1.0d0)
        real(real64), parameter :: tolLow = 1.0d-18
        real(real64), parameter :: tolHi = 1.0d18

        ! Local Variables
        integer(int32) :: n, np, np1, ind1, ind2, &
            e1, s2, e2, s3, e3, s4, e4, s5, e5, s6, e6, &
            e1c, s2c, e2c
        real(real64) :: scale
        integer(int32), pointer, dimension(:) :: cindex
        real(real64), pointer, dimension(:) :: tau, x, Escale
        real(real64), pointer, dimension(:,:) :: A, Q, R
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

        e1c = n * np1
        s2c = e1c + 1
        e2c = s2c + n * (2* np1) - 1

        ! Assign Pointers
        cindex => iwork(1:np)               ! NP-element

        A(1:2*n+1,1:2*np1) => work(1:e1)    ! 2*N+1-by-2*(NP+1)
        tau => work(s2:e2)                  ! 2*(NP+1)-element
        Q(1:2*n+1,1:2*np1) => work(s3:e3)   ! 2*N+1-by-2*(NP+1)
        R(1:np1,1:np1) => work(s4:e4)       ! (NP+1)-by-(NP+1)
        x => work(s5:e5)                    ! (NP+1)-element
        Escale => work(s6:e6)               ! (NP+!)-element

        Dk(1:n,1:np1) => cwork(1:e1c)       ! N-by-(NP+1)
        Ac(1:n,1:2*np1) => cwork(s2c:e2c)   ! N-by-2*(NP+1)

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

        ! --------------------
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
        if (.not.relax .or. abs(x(np1)) < tolLow .or. abs(xp1) > tolHi) then
        end if
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
