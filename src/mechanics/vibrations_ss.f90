! vibrations_ss.f90

! References:
! - https://en.wikipedia.org/wiki/State-space_representation

submodule (vibrations) vibrations_ss
contains
! ------------------------------------------------------------------------------
    module function ss_eval(this, x, u, err) result(y)
        ! Arguments
        class(state_space), intent(in) :: this
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in), dimension(:) :: u
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: ndof, nout
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        ndof = size(this%A, 1)
        nout = size(this%C, 1)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(x) /= ndof) then
            call errmgr%report_error("ss_eval_npts_inplace", &
                "The state vector is not sized correctly.  " // &
                "The length must match the number of state variables.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(u) /= nout) then
            call errmgr%report_error("ss_eval_npts_inplace", &
                "The forcing function matrix is not sized correctly.  " // &
                "The number of rows must match the number of outputs.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Compute y(k) = C * x(k) + D * u(k)
        y = matmul(this%C, x) + matmul(this%D, u)

        ! Compute x(k+1) = A * x(k) + B * u(k)
        x = matmul(this%A, x) + matmul(this%B, u)
    end function

! ------------------------------------------------------------------------------
    module function ss_tf_eval(this, freq, err) result(h)
        use linalg_core

        ! Arguments
        class(state_space), intent(in) :: this
        real(real64), intent(in) :: freq
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:,:) :: h

        ! Parameters
        complex(real64), parameter :: i1 = (0.0d0, 1.0d0)

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: n, nb, flag, nout, nin
        integer(int32), allocatable, dimension(:) :: pvt
        complex(real64), allocatable, dimension(:,:) :: a, c, x
        complex(real64) :: s

        ! Initialization
        n = size(this%A, 1)
        nb = size(this%B, 2)
        nin = size(this%D, 2)
        nout = size(this%D, 1)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! The transfer function H(s) can be computed as follows:
        ! H(s) = C * (s * I - A)**-1 * B + D.  The matrix inversion
        ! component is more readily computed by letting 
        ! X = (s * I - A)**-1 * B (i.e. solving (s * I - A) * X = B) 
        ! by means of LU or QR factorization.  As s * I - A should be
        ! full rank, LU factorization with partial pivoting is the 
        ! preferred method.

        ! Local Memory Allocation
        allocate(a(n, n), stat = flag)
        if (flag == 0) allocate(x(n, nb), stat = flag)
        if (flag == 0) allocate(h(nout, nin), stat = flag)
        if (flag == 0) allocate(pvt(n), stat = flag)
        if (flag == 0) allocate(c(nout, n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("ss_tf_eval", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Compute H(s) = C * (s * I - A)**-1 * B + D
        s = i1 * freq
        call compute_h(this%A, this%B, this%C, this%D, s, h, a, x, c, pvt)
    end function

! ------------------------------------------------------------------------------
    module function ss_tf_eval_array(this, freq, err) result(h)
        ! Arguments
        class(state_space), intent(in) :: this
        real(real64), intent(in), dimension(:) :: freq
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:,:,:) :: h

        ! Parameters
        complex(real64), parameter :: i1 = (0.0d0, 1.0d0)

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: k, npts, n, nb, flag, nin, nout
        integer(int32), allocatable, dimension(:) :: pvt
        complex(real64), allocatable, dimension(:,:) :: a, c, x
        complex(real64) :: s

        ! Initialization
        n = size(this%A, 1)
        nb = size(this%B, 2)
        nin = size(this%D, 2)
        nout = size(this%D, 1)
        npts = size(freq)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! The transfer function H(s) can be computed as follows:
        ! H(s) = C * (s * I - A)**-1 * B + D.  The matrix inversion
        ! component is more readily computed by letting 
        ! X = (s * I - A)**-1 * B (i.e. solving (s * I - A) * X = B) 
        ! by means of LU or QR factorization.  As s * I - A should be
        ! full rank, LU factorization with partial pivoting is the 
        ! preferred method.

        ! Local Memory Allocation
        allocate(a(n, n), stat = flag)
        if (flag == 0) allocate(x(n, nb), stat = flag)
        if (flag == 0) allocate(h(nout, nin, npts))
        if (flag == 0) allocate(pvt(n), stat = flag)
        if (flag == 0) allocate(c(nout, n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("ss_tf_eval_array", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Loop over each point in S
        do k = 1, npts
            ! Compute H(s) = C * (s * I - A)**-1 * B + D
            s = i1 * freq(k)
            call compute_h(this%A, this%B, this%C, this%D, s, &
                h(:,:,k), a, x, c, pvt)
        end do
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ******************************************************************************
    ! Computes: H(s) = C * (s * I - A) * B + D
    subroutine compute_h(A, B, C, D, s, H, workA, workB, workC, iwork)
        use linalg_core

        ! Arguments
        real(real64), intent(in), dimension(:,:) :: A, B, C, D
        complex(real64), intent(in) :: s
        complex(real64), intent(out), dimension(:,:) :: H, workA, workB, workC
        integer(int32), intent(out), dimension(:) :: iwork

        ! Parameters
        complex(real64), parameter :: one = (1.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: i, j, n, nin, nout

        ! Initialization
        n = size(A, 1)
        nin = size(h, 1)
        nout = size(h, 2)

        ! Process
        do j = 1, n
            do i = 1, n
                if (i == j) then
                    workA(i,j) = s - A(i,j)
                else
                    workA(i,j) = -cmplx(A(i,j), kind = real64)
                end if
            end do
        end do

        ! Solve (s * I - A) * X = B for X
        call lu_factor(workA, iwork)
        workB = cmplx(B, kind = real64)
        call solve_lu(workA, iwork, workB) ! workB holds X upon completion of this call

        ! Compute H = C * X + D as X = (s * I - A)**-1 * B
        h = cmplx(D, kind = real64)
        workC = cmplx(C, kind = real64)
        call zgemm('N', 'N', nin, nout, n, one, workC, nin, workB, n, one, h, nin)
    end subroutine
    
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
