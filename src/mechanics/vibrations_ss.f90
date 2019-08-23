! vibrations_ss.f90

submodule (vibrations) vibrations_ss
contains
! ------------------------------------------------------------------------------
    module subroutine ss_eval_npts_inplace(this, x, u, y, err)
        ! Arguments
        class(state_space), intent(in) :: this
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in), dimension(:,:) :: u
        real(real64), intent(out), dimension(:,:) :: y
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: ndof, npts, nout
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        ndof = size(this%A, 1)
        npts = size(u, 2)
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
        if (size(u, 1) /= nout) then
            call errmgr%report_error("ss_eval_npts_inplace", &
                "The forcing function matrix is not sized correctly.  " // &
                "The number of rows must match the number of outputs.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(y, 1) /= nout) then
            call errmgr%report_error("ss_eval_npts_inplace", &
                "The output matrix is not sized correctly.  " // &
                "The number of rows must match the number of outputs.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(y, 2) /= npts) then
            call errmgr%report_error("ss_eval_npts_inplace", &
                "The output matrix is not sized correctly.  " // &
                "The number of columns must match the number of evaluation points.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        do i = 1, npts
            ! Compute y(k) = C * x(k) + D * u(k)
            y(:,i) = matmul(this%C, x) + matmul(this%D, u(:,i))

            ! Compute x(k+1) = A * x(k) + B * u(k)
            x = matmul(this%A, x) + matmul(this%B, u(:,i))
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine ss_eval_inplace(this, x, u, y, err)
        ! Arguments
        class(state_space), intent(in) :: this
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in), dimension(:) :: u
        real(real64), intent(out), dimension(:) :: y
        class(errors), intent(inout), optional, target :: err

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
            call errmgr%report_error("ss_eval_inplace", &
                "The state vector is not sized correctly.  " // &
                "The length must match the number of state variables.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(u) /= nout) then
            call errmgr%report_error("ss_eval_inplace", &
                "The forcing function array is not sized correctly.  " // &
                "The length must match the number of outputs.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(y) /= nout) then
            call errmgr%report_error("ss_eval_inplace", &
                "The output array is not sized correctly.  " // &
                "The length must match the number of outputs.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Compute y(k) = C * x(k) + D * u(k)
        y = matmul(this%C, x) + matmul(this%d, u)

        ! Compute x(k+1) = A * x(k) + B * u(k)
        x = matmul(this%A, x) + matmul(this%B, u)
    end subroutine

! ------------------------------------------------------------------------------
    module function ss_eval_npts(this, xo, u, err) result(y)
        ! Arguments
        class(state_space), intent(in) :: this
        real(real64), intent(in), dimension(:) :: xo
        real(real64), intent(in), dimension(:,:) :: u
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: y

        ! Local Variables
        integer(int32) :: ndof, nout, npts, flag
        real(real64), allocatable, dimension(:) :: x
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        ndof = size(this%A, 1)
        nout = size(this%C, 1)
        npts = size(u, 2)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Memory Allocation
        allocate(x(ndof), stat = flag)
        if (flag == 0) allocate(y(nout, npts), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("ss_eval_npts", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Process
        x = xo
        call this%evaluate_inplace(x, u, y, errmgr)
    end function

! ------------------------------------------------------------------------------
    module function ss_eval(this, xo, u, err) result(y)
        ! Arguments
        class(state_space), intent(in) :: this
        real(real64), intent(in), dimension(:) :: xo, u
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: ndof, nout, flag
        real(real64), allocatable, dimension(:) :: x
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

        ! Memory Allocation
        allocate(x(ndof), stat = flag)
        if (flag == 0) allocate(y(nout), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("ss_eval", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Process
        x = xo
        call this%evaluate_inplace(x, u, y, errmgr)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
