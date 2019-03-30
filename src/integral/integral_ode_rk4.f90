! integral_ode_rk4.f90

submodule (integral_core) integral_ode_rk4
contains
! ------------------------------------------------------------------------------
    module function ork4_step(this, fcn, x, y, xout, rtol, atol, err) result(brk)
        class(ode_rk4), intent(inout) :: this
        class(ode_helper), intent(inout) :: fcn
        real(real64), intent(inout) :: x
        real(real64), intent(inout), dimension(:) :: y
        real(real64), intent(in) :: xout
        real(real64), intent(in), dimension(:) :: rtol, atol
        class(errors), intent(inout), optional, target :: err
        logical :: brk
    end function

! ------------------------------------------------------------------------------
    module subroutine ork4_reset_integrator(this)
        class(ode_rk4), intent(inout) :: this
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule