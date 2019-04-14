! friction.f90

!> @brief \b friction
!!
!! @par Purpose
!! To provide various friction models suitable for use in numerical simulations.
module friction
    use iso_fortran_env
    implicit none
    private
    public coulomb
    public lu_gre

contains
    !> @brief Applies the Coulomb friction model.
    !!
    !! @param[in] mu The friction coefficient.
    !! @param[in] normal The normal force.
    !! @param[in] eps A regularization parameter, typically quite small.
    !! @param[in] velocity The relative velocity of the two contacting bodies.
    !! @return The resulting friction force.
    !!
    !! @par Remarks
    !! This model utilizes a regularization parameter \f$ \epsilon \f$ to
    !! assist in dealing with the discontinuity of the pure Coulomb model
    !! when velocity is zero.  The model can be described as follows.
    !! @par
    !! \f$ F = \frac{\mu N}{\epsilon} v \f$ when \f$ |v| \leq \epsilon \f$, and
    !! \f$ F = \mu N sgn(v) \f$ when \f$ |v| > \epsilon \f$.
    !!
    !! @par Example
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use fplot_core
    !!     use friction
    !!     use integral_core
    !!     use constants
    !!     implicit none
    !!
    !!     ! Model Constants
    !!     real(real64), parameter :: m = 10.0d0
    !!     real(real64), parameter :: g = 9.81d0
    !!     real(real64), parameter :: mu = 1.2d0
    !!     real(real64), parameter :: k = 250.0d3
    !!     real(real64), parameter :: F = 2.0d3
    !!     real(real64), parameter :: freq = 5.0d0
    !!     real(real64), parameter :: eps = 1.0d-4
    !!
    !!     ! Plot Variables
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2, d3
    !!     class(plot_axis), pointer :: xAxis, yAxis, y2Axis
    !!
    !!     ! Integrator Variables
    !!     type(ode_helper) :: obj
    !!     type(ode_auto) :: integrator
    !!     procedure(ode_fcn), pointer :: ptr
    !!
    !!     ! Local Variables
    !!     real(real64), allocatable, dimension(:,:) :: z
    !!     real(real64), allocatable, dimension(:) :: friction
    !!     real(real64) :: t(2), ic(2)
    !!
    !!     ! Define the solution range and initial conditions
    !!     t = [0.0d0, 2.0d0]
    !!     ic = [0.0d0, 0.0d0]
    !!
    !!     ! Set up the integrator
    !!     ptr => eqn
    !!     call obj%define_equations(2, ptr)
    !!
    !!     ! Compute the solution
    !!     z = integrator%integrate(obj, t, ic)
    !!
    !!     ! Plot the solution
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!     call plt%set_use_y2_axis(.true.)
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("t")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("x(t)")
    !!
    !!     y2Axis => plt%get_y2_axis()
    !!     call y2Axis%set_title("dx(t)/dt")
    !!
    !!     call d1%set_name("x(t)")
    !!     call d1%define_data(z(:,1), z(:,2))
    !!
    !!     call d2%set_name("dx(t)/dt")
    !!     call d2%set_draw_against_y2(.true.)
    !!     call d2%define_data(z(:,1), z(:,3))
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%draw()
    !!
    !!     ! Compute the friction force
    !!     friction = coulomb(mu, m * g, eps, z(:,3))
    !!
    !!     ! Plot the force-velocity relationship
    !!     call plt%set_use_y2_axis(.false.)
    !!     call plt%clear_all()
    !!
    !!     call xAxis%set_title("dx(t)/dt")
    !!     call yAxis%set_title("F(t)")
    !!
    !!     call d3%define_data(z(:,3), friction)
    !!
    !!     call plt%push(d3)
    !!     call plt%draw()
    !! contains
    !!     ! The system under study is a single degree-of-freedom mass-spring
    !!     ! system that utilizes a frictional damper.  The normal force
    !!     ! is the static weight of the moving body.
    !!     !
    !!     ! The equation of motion is:
    !!     ! m * x" + f(x',mu) + k * x = F(t)
    !!     !
    !!     ! Let:
    !!     ! m = Mass
    !!     ! g = Acceleration due to gravity
    !!     ! mu = Friction coefficient
    !!     ! k = Stiffness
    !!     ! F = Forcing amplitude
    !!     subroutine eqn(t, z, dzdt)
    !!         ! Arguments
    !!         real(real64), intent(in) :: t
    !!         real(real64), intent(in), dimension(:) :: z
    !!         real(real64), intent(out), dimension(:) :: dzdt
    !!
    !!         ! Local Variables
    !!         real(real64) :: Ft, Ff, N, x, dxdt
    !!
    !!         ! Initialization
    !!         x = z(1)
    !!         dxdt = z(2)
    !!         Ft = F * sin(2.0d0 * pi * freq * t)
    !!         N = m * g
    !!
    !!         ! Compute the friction force
    !!         Ff = coulomb(mu, N, eps, dxdt)
    !!
    !!         ! Equations of motion
    !!         dzdt(1) = dxdt
    !!         dzdt(2) = (Ft - Ff - k * x) / m
    !!     end subroutine
    !! end program
    !! @endcode
    !! The above program produces the following plots.
    !! @image html coulomb_friction_model.png
    !! @image html coulomb_friction_model_force_velocity.png
    pure elemental function coulomb(mu, normal, eps, velocity) result(f)
        ! Arguments
        real(real64), intent(in) :: mu, normal, eps, velocity
        real(real64) :: f

        ! Process
        if (abs(velocity) <= eps) then
            f = (mu * normal / eps) * velocity
        else
            f = mu * normal * sign(1.0d0, velocity)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Applies the Lund-Grenoble (Lu-Gre) friction model.
    !!
    !! @param[in] mu_s The static friction coefficient.
    !! @param[in] mu_c The Coulomb (dynamic) friction coefficient.
    !! @param[in] normal The normal force.
    !! @param[in] kz The normalized bristle stiffness (units = length**-1).
    !! @param[in] bz THe normalized bristle damping (units = time * length**-1).
    !! @param[in] stribeck The Stribeck velocity.
    !! @param[in] alpha The velocity ratio exponent.
    !! @param[in] velocity The relative velocity between the contacting bodies.
    !! @param[in] t0 The time at the previously computed frictional state.
    !! @param[in] tf The time at which to evaluate the friction model.
    !! @param[in,out] z0 On input, the bristle deflection at the previously
    !!  computed frictional state.  On output, the bristle deflection at the
    !!  requested time @p tf.
    !! @return The friction force at time @p tf.
    !!
    !! @par Remarks
    !! The Lu-Gre model utilizes a brush-type model to describe frictional
    !! behaviors.  The brush analogy is why the term "bristle" is utilized.
    !! However, another perhaps more relevant analogy, would be to consider the
    !! bristles as asperities and surface irregularities that must deform and
    !! slide across each other when two bodies make contact, and are forced to
    !! slide relative to one another.  To that end, the Lu-Gre model can be
    !! described by the following equations.
    !! @par
    !! \f$ \frac{dz}{dt} = v_{r} - \frac{\sigma_{o} |v_{r}| z}{g(v_{r})} \f$
    !! @par
    !! \f$ g(v_{r}) = \mu_{c} + \left( \mu_{s} - \mu_{c} \right) 
    !!  e^{-|\frac{v_{r}}{v_{s}}|^{\alpha}} \f$
    !! @par
    !! \f$ F = \left( \sigma_{0} z + \sigma_{1} \frac{dz}{dt} + 
    !!  \sigma_{2} v_{r} \right) F_{n} \f$
    !! @par Where:
    !! \f$ \mu_{s} = \f$ Static Friction Coefficient
    !! @par
    !! \f$ \mu_{c} = \f$ Coulomb Friction Coefficient
    !! @par
    !! \f$ F_{n} = \f$ Normal Force
    !! @par
    !! \f$ z = \f$ Bristle Deflection
    !! @par
    !! \f$ v_{r} = \f$ Relative Velocity
    !! @par
    !! \f$ v_{s} = \f$ Stribeck Velocity
    !! @par
    !! \f$ \alpha = \f$ Velocity Ratio Exponent
    !! @par
    !! \f$ \sigma_{0} = \f$ Bristle Stiffness
    !! @par
    !! \f$ \sigma_{1} = \f$ Bristle Damping Coefficient
    !! @par
    !! \f$ \sigma_{2} = \f$ Viscous Damping Coefficient
    function lu_gre(mu_s, mu_c, normal, viscous, kz, bz, stribeck, alpha, &
            velocity, t0, tf, z0) result(f)
        ! Required Modules
        use ode_core

        ! Arguments
        real(real64), intent(in) :: mu_s, mu_c, normal, viscous, kz, bz, &
            stribeck, alpha, velocity, t0, tf
        real(real64), intent(inout) :: z0
        real(real64) :: f

        ! ODE Variables
        procedure(ode_fcn), pointer :: ptr
        type(ode_auto) :: integrator
        type(ode_helper) :: fcn

        ! Local Variables
        integer(int32) :: npts
        real(real64) :: ic(1), time(2), vz(1)
        real(real64), allocatable, dimension(:,:) :: zx

        ! Set up the integrator
        ptr => bristle_eqn
        call fcn%define_equations(1, ptr)

        ! Define the initial conditions
        ic = [z0]
        time = [t0, tf]

        ! Compute the solution
        zx = integrator%integrate(fcn, time, ic)
        npts = size(zx, 1)
        ic(1) = zx(npts, 2)
        z0 = ic(1)

        ! Compute the bristle velocity
        call bristle_eqn(zx(npts, 1), ic, vz)

        ! Compute the friction force
        f = (kz * z0 + bz * vz(1) + viscous * velocity) * normal
    contains
        ! The first-order ODE to solve to determine bristle motion.
        subroutine bristle_eqn(t, z, dzdt)
            ! Arguments
            real(real64), intent(in) :: t
            real(real64), intent(in), dimension(:) :: z
            real(real64), intent(out), dimension(:) :: dzdt

            ! Local Variables
            real(real64) :: g

            ! Compute the friction function
            g = mu_c + (mu_s - mu_c) * exp(-abs(velocity / stribeck)**alpha)

            ! Compute the bristle velocity
            dzdt = velocity - kz * abs(velocity) * z(1) / g
        end subroutine
    end function

end module
