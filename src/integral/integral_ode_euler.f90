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

        ! Ensure the direction of step is correct
        
        ! Check to see if root finding is requested

        ! Process
        if (this%m_first) call fcn%evaluate_ode(x, y, this%m_dydx)
        y = y + h * this%m_dydx

        ! Compute the function value at x + h
        call fcn%evaluate_ode(xout, y, this%m_work)

        ! Estimate the solution error
        xerr = 0.5d0 * norm2(this%m_dydx - this%m_work)

        ! Update the end time and store the most recent function evaluation
        x = xout
        this%m_dydx = this%m_work
        this%m_first = .false.
    end function

! ------------------------------------------------------------------------------
    module subroutine oe_reset_integrator(this)
        class(ode_euler), intent(inout) :: this
        this%m_first = .true.
    end subroutine

! ------------------------------------------------------------------------------
end submodule
