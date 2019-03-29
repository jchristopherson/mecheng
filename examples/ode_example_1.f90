! ode_example_1.f90

program example
    use iso_fortran_env
    use integral_core
    use fplot_core
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 500

    ! Local Variables
    integer(int32) :: i
    type(ode_helper) :: obj
    type(ode_auto) :: intauto
    type(ode_euler) :: inteuler
    procedure(ode_fcn), pointer :: ptr
    real(real64) :: ic(2), t(2), dt, te(npts)
    real(real64), allocatable, dimension(:,:) :: xauto, xeuler
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    class(plot_axis), pointer :: xAxis, yAxis
    class(legend), pointer :: lgnd

    ! Set up the integrators
    ptr => eqn
    call obj%define_equations(2, ptr)

    ! Define the initial conditions
    t = [0.0d0, 5.0d-1]
    ic = [0.0d0, 0.0d0]

    ! Compute the solution using the adaptive integrator
    xauto = intauto%integrate(obj, t, ic)

    ! Construct a vector cotnaining the points at which we desire a solution
    ! for the Euler integrator
    te(1) = 0.0d0
    dt = maxval(t) / (npts - 1.0d0)
    do i = 2, npts
        te(i) = te(i-1) + dt
    end do

    ! Inform the integrator that we're seeking output at every point, not
    ! intermediate points as well
    call inteuler%set_provide_all_output(.false.)

    ! Compute the solution using the Euler integrator
    xeuler = inteuler%integrate(obj, te, ic)

    ! Plot the solution
    call plt%initialize()
    call plt%set_font_size(14)

    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("t")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("x(t)")

    call d1%set_name("Auto")
    call d1%set_line_width(2.0)
    call d1%set_line_style(LINE_DASHED)
    call d1%define_data(xauto(:,1), xauto(:,2))

    call d2%set_name("Euler")
    call d2%set_line_width(2.0)
    call d2%define_data(xeuler(:,1), xeuler(:,2))

    call plt%push(d1)
    call plt%push(d2)
    call plt%draw()

contains
    ! Assume we have the second order system governing a mechanical
    ! vibrating system:
    ! x" + 2 * z * wn * x' + wn**2 * x = 2 * z * wn * y' + wn^2 * y
    !
    ! Where: y = Y * sin(w * t)
    !
    ! Let:
    ! - z = Damping Ratio = 0.2
    ! - wn = Natural Frequency = 10 Hz = 62.8318530 rad/sec
    ! - Y = Excitation Amplitude = 0.125
    ! - w = Excitation Frequency = 20 Hz = 125.663706 rad/sec
    subroutine eqn(t, x, dxdt)
        ! Arguments
        real(real64), intent(in) :: t
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(out), dimension(:) :: dxdt

        ! Constants
        real(real64), parameter :: z = 0.2d0
        real(real64), parameter :: wn = 62.8318530d0
        real(real64), parameter :: Y = 0.125d0
        real(real64), parameter :: w = 125.663706d0

        ! Local Variables
        real(real64) :: yt, dydt

        ! Compute the excitation
        yt = Y * sin(w * t)
        dydt = Y * w * cos(w * t)

        ! Equations
        dxdt(1) = x(2)
        dxdt(2) = 2.0d0 * z * wn * (dydt - x(2)) + wn**2 * (yt - x(1))
    end subroutine
end program
