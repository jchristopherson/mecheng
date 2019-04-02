! realtime_example_3.f90

program example
    use iso_fortran_env
    use controls
    use integral_core
    use fplot_core
    use constants
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 1000
    real(real64), parameter :: update = 1024.0d0
    real(real64), parameter :: stepTime = 0.1d0
    real(real64), parameter :: stepSize = 0.5d0

    ! Local Variables
    procedure(signal_fcn), pointer :: sigfcn
    type(realtime_derivative) :: diff
    type(realtime_signal) :: sig
    type(pid) :: controller
    procedure(ode_fcn), pointer :: ptr
    type(ode_helper) :: fcn
    type(ode_rk4) :: integrator
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    class(legend), pointer :: lgnd
    integer(int32) :: i
    real(real64) :: dt, tn(npts), command(npts), ic(2)
    real(real64), allocatable, dimension(:,:) :: xo

    ! Define the sample times
    dt = 1.0d0 / update
    tn(1) = 0.0d0
    do i = 2, npts
        tn(i) = tn(i-1) + dt
    end do

    ! Set Up the Integrator
    ptr => eqns
    call fcn%define_equations(2, ptr)

    ! Set up the realtime objects
    sigfcn => signal
    call sig%set_function(sigfcn)
    call sig%set_update_rate(update)
    call controller%set_update_rate(update)
    call diff%set_update_rate(update)

    ! Define control parameters
    controller%proportional_gain = 1.0d-2
    controller%integral_gain = 5.6d1
    controller%derivative_gain = 0.01d0

    ! Define the initial conditions
    ic = [0.0d0, 0.0d0]

    ! Compute the solution
    xo = integrator%integrate(fcn, tn, ic)

    ! Construct the command signal for plotting purposes
    do i = 1, npts
        command(i) = sig%evaluate(tn(i))
    end do

    ! Plot the solution
    call plt%initialize()
    call plt%set_font_size(14)

    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_draw_inside_axes(.false.)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("t")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("x(t)")

    call d1%set_name("Desired")
    call d1%set_line_width(1.5)
    call d1%define_data(tn, command)

    call d2%set_name("Response")
    call d2%set_line_width(2.0)
    call d2%define_data(xo(:,1), xo(:,2))

    call plt%push(d1)
    call plt%push(d2)
    call plt%draw()

contains
    ! The equations of motion for the dynamic system under control.  The system
    ! is a single degree-of-freedom mass-spring-damper system undergoing base
    ! excitation such that the equation of motion is: 
    ! x" + 2 * z * wn * x' + wn**2 * x = 2 * z  * wn * y' + wn**2 * y.
    subroutine eqns(t, x, dxdt)
        real(real64), intent(in) :: t
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(out), dimension(:) :: dxdt

        ! Model Constants
        real(real64), parameter :: zeta = 0.05d0
        real(real64), parameter :: natfreqHz = 100.0d0

        ! Local Variables
        real(real64) :: fn, y, dydt, c
        
        ! Initialization
        fn = 2.0d0 * pi * natfreqHz

        ! Retrieve the setpoint to feed the controller
        c = sig%evaluate(t)

        ! Compute the controller output, and its derivative.  We will assume
        ! that the controller is connected to the base with a "perfect" 
        ! actuator such that whatever the controller asks for will be
        ! what motion is generated at the base.
        y = controller%evaluate(c, t, x(1))
        dydt = diff%evaluate(t, y)

        ! Equations of motion
        dxdt(1) = x(2)
        dxdt(2) = 2.0d0 * zeta * fn * (dydt - x(2)) + fn**2 * (y - x(1))
    end subroutine

    ! The signal generator
    function signal(ts) result(ys)
        ! Arguments
        real(real64), intent(in) :: ts
        real(real64) :: ys

        ! Define a step function
        if (ts >= stepTime) then
            ys = stepSize
        else
            ys = 0.0d0
        end if
    end function

end program
