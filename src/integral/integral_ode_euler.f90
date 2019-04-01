! integral_ode_euler.f90

submodule (integral_core) integral_ode_euler
contains
! ------------------------------------------------------------------------------
    module function oe_step(this, fcn, x, y, xout, rtol, atol, err) result(brk)
        ! Arguments
        class(ode_euler), intent(inout) :: this
        class(ode_helper), intent(inout) :: fcn
        real(real64), intent(inout) :: x
        real(real64), intent(inout), dimension(:) :: y
        real(real64), intent(in) :: xout
        real(real64), intent(in), dimension(:) :: rtol, atol
        class(errors), intent(inout), optional, target :: err
        logical :: brk

        ! Local Variables
        real(real64) :: h, xerr

        ! Initialization
        brk = .false.
        h = xout - x
        call this%initialize(fcn%get_equation_count(), err)
        
        ! Check to see if root finding is requested

        ! Process
        if (this%m_first) call fcn%evaluate_ode(x, y, this%m_dydx)
        this%m_dydx = h * this%m_dydx + this%m_summationError
        this%m_y1 = y + this%m_dydx

        ! Update the summation error tracking
        this%m_summationError = this%m_dydx - (this%m_y1 - y)

        ! Compute the function value at x + h
        call fcn%evaluate_ode(xout, y, this%m_dydx1)

        ! Estimate the solution error
        this%m_error = 0.5d0 * h * (this%m_dydx - this%m_dydx1)

        ! Update the end time and store the most recent function evaluation
        x = xout
        y = this%m_y1
        this%m_dydx = this%m_dydx1
        this%m_first = .false.
    end function

! ------------------------------------------------------------------------------
    module subroutine oe_reset_integrator(this)
        class(ode_euler), intent(inout) :: this
        this%m_first = .true.
        this%m_summationError = 0.0d0
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine oe_init_workspace(this, neqn, err)
        ! Arguments
        class(ode_euler), intent(inout) :: this
        integer(int32), intent(in) :: neqn
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag
        
        ! Quick Return
        if (allocated(this%m_dydx)) then
            if (size(this%m_dydx) == neqn) return
        end if

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Deallocate as necessary
        if (allocated(this%m_dydx)) deallocate(this%m_dydx)
        if (allocated(this%m_dydx1)) deallocate(this%m_dydx1)
        if (allocated(this%m_error)) deallocate(this%m_error)
        if (allocated(this%m_summationError)) deallocate(this%m_summationError)
        if (allocated(this%m_y1)) deallocate(this%m_y1)

        ! Process
        allocate(this%m_dydx(neqn), stat = flag)
        if (flag == 0) allocate(this%m_dydx1(neqn), stat = flag)
        if (flag == 0) allocate(this%m_error(neqn), stat = flag)
        if (flag == 0) allocate(this%m_summationError(neqn), stat = flag)
        if (flag == 0) allocate(this%m_y1(neqn), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("oe_init_workspace", &
                "Insufficient memory available.", &
                INT_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Initialize appropriate values
        this%m_summationError = 0.0d0
        this%m_first = .true.
    end subroutine

! ------------------------------------------------------------------------------
    module function oe_integrate(this, fcnobj, x, y, err) result(rst)
        ! Arguments
        class(ode_euler), intent(inout) :: this
        class(ode_helper), intent(inout) :: fcnobj
        real(real64), intent(in), dimension(:) :: x, y
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: rst

        ! Ensure the integrator understands this is a fixed-step method
        call this%set_provide_all_output(.false.)

        ! Integrate
        rst = oi_integrate(this, fcnobj, x, y, err)
    end function

! ------------------------------------------------------------------------------
    pure module function oe_get_error_est(this) result(x)
        class(ode_euler), intent(in) :: this
        real(real64), allocatable, dimension(:) :: x
        x = this%m_error
    end function

! ------------------------------------------------------------------------------
end submodule
