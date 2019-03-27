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
        real(real64) :: h

        ! Initialization
        brk = .false.
        h = xout - x

        ! Ensure the direction of step is correct
        
        ! Check to see if root finding is requested

        ! Process
        call fcn%evaluate_ode(x, y, this%m_dydx)
        y = y + h * this%m_dydx

        ! Update the end time
        x = xout
    end function

! ------------------------------------------------------------------------------
    module subroutine oe_reset_integrator(this)
        class(ode_euler), intent(inout) :: this
        ! Do nothing.  This subroutine satisfies the requirements for the
        ! abstract interface of the parent class.
    end subroutine

! ------------------------------------------------------------------------------
end submodule
