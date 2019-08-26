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
