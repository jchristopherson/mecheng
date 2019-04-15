! lu_gre_friction_example.f90

program example
    use iso_fortran_env
    use fplot_core
    use friction
    use integral_core
    use constants
    implicit none

    ! Model Constants
    real(real64), parameter :: m = 10.0d0
    real(real64), parameter :: g = 9.81d0
    real(real64), parameter :: mu_s = 1.2d0
    real(real64), parameter :: mu_c = 0.5d0
    real(real64), parameter :: k = 250.0d3
    real(real64), parameter :: F = 2.0d3
    real(real64), parameter :: freq = 5.0d0
    real(real64), parameter :: alpha = 1.0d0
    real(real64), parameter :: stribeck = 1.0d-3
    real(real64), parameter :: bristle_stiffness = 1.0d3
    real(real64), parameter :: bristle_damping = 1.5d0
    real(real64), parameter :: viscous_damping = 0.0d0

    ! Plot Variables
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2, d3
    class(plot_axis), pointer :: xAxis, yAxis, y2Axis
    
    ! Integrator Variables
    type(ode_helper) :: obj
    type(ode_auto) :: integrator
    procedure(ode_fcn), pointer :: ptr
    procedure(ode_callback), pointer :: cptr

    ! Local Variables
    real(real64), allocatable, dimension(:,:) :: z
    real(real64), allocatable, dimension(:) :: friction
    real(real64) :: t(2), ic(2)

    ! Friction Model State Variables
    real(real64) :: tf0, zf0, zf

    ! Set up the initial conditions for the friction model
    tf0 = 0.0d0     ! Initial solution time
    zf0 = 0.0d0     ! Initial bristle deflection

    ! Define the solution range and initial conditions
    t = [0.0d0, 2.0d0]
    ic = [0.0d0, 0.0d0]

    ! Set up the integrator
    ptr => eqn
    cptr => callback
    call obj%define_equations(2, ptr)
    call obj%define_callback(cptr)

    ! Compute the solution
    z = integrator%integrate(obj, t, ic)

    ! Plot the solution
    call plt%initialize()
    call plt%set_font_size(14)
    call plt%set_use_y2_axis(.true.)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("t")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("x(t)")

    y2Axis => plt%get_y2_axis()
    call y2Axis%set_title("dx(t)/dt")

    call d1%set_name("x(t)")
    call d1%define_data(z(:,1), z(:,2))

    call d2%set_name("dx(t)/dt")
    call d2%set_draw_against_y2(.true.)
    call d2%define_data(z(:,1), z(:,3))

    call plt%push(d1)
    call plt%push(d2)
    call plt%draw()

    ! ! Compute the friction force
    ! friction = coulomb(mu, m * g, eps, z(:,3))

    ! ! Plot the force-velocity relationship
    ! call plt%set_use_y2_axis(.false.)
    ! call plt%clear_all()

    ! call xAxis%set_title("dx(t)/dt")
    ! call yAxis%set_title("F(t)")

    ! call d3%define_data(z(:,3), friction)
    
    ! call plt%push(d3)
    ! call plt%draw()
contains
    ! The system under study is a single degree-of-freedom mass-spring
    ! system that utilizes a frictional damper.  The normal force
    ! is the static weight of the moving body.
    !
    ! The equation of motion is:
    ! m * x" + f(x',mu) + k * x = F(t)
    !
    ! Let:
    ! m = Mass
    ! g = Acceleration due to gravity
    ! mu = Friction coefficient
    ! k = Stiffness
    ! F = Forcing amplitude
    subroutine eqn(t, z, dzdt)
        ! Arguments
        real(real64), intent(in) :: t
        real(real64), intent(in), dimension(:) :: z
        real(real64), intent(out), dimension(:) :: dzdt

        ! Local Variables
        real(real64) :: Ft, Ff, N, x, dxdt

        ! Initialization
        x = z(1)
        dxdt = z(2)
        Ft = F * sin(2.0d0 * pi * freq * t)
        N = m * g

        ! Compute the friction force
        zf = zf0
        Ff = lu_gre(mu_s, mu_c, N, viscous_damping, bristle_stiffness, &
            bristle_damping, stribeck, alpha, dxdt, tf0, t, zf)

        ! Equations of motion
        dzdt(1) = dxdt
        dzdt(2) = (Ft - Ff - k * x) / m
    end subroutine

    ! Callback routine used to store information regarding the
    ! friction model state at each integration step.
    subroutine callback(tc, zc)
        ! Arguments
        real(real64), intent(in) :: tc
        real(real64), intent(in), dimension(:) :: zc

        ! Store the friction model state variables
        tf0 = tc
        zf0 = zf
    end subroutine
end program