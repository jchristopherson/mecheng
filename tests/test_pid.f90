! test_pid.f90

program main
    use iso_fortran_env
    use fplot_core
    use integral_core
    use controls
    use constants
    implicit none

    ! Local Variables
    type(ode_helper) :: fcn
    type(ode_auto) :: integrator
    procedure(ode_fcn), pointer :: ptr

    ! Initialization
    ptr => eqns
    call fcn%define_equations(2, ptr)

    ! Set up the PID controller

    ! Define the initial conditions


contains
    subroutine eqns(t, x, dxdt)
        real(real64), intent(in) :: t
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(out), dimension(:) :: dxdt

        ! Model Constants
        real(real64), parameter :: zeta = 0.05d0
        real(real64), parameter :: natfreqHz = 100.0d0

        ! Local Variables
        real(real64) :: fn, y, dydt
        
        ! Initialization
        fn = 2.0d0 * pi * natfreqHz

        ! Define the command

        ! Compute the controller output

        ! Equations of motion
        dxdt(1) = x(2)
        dxdt(2) = 2.0d0 * zeta * fn * (dydt - x(2)) + fn**2 * (y - x(1))
    end subroutine
end program

