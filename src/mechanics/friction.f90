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
    !! \par
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
    !!     ! Plot Variables
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2
    !!     class(plot_axis), pointer :: xAxis, yAxis, y2Axis
    !!
    !!     ! Integrator Variables
    !!     type(ode_helper) :: obj
    !!     type(ode_auto) :: integrator
    !!     procedure(ode_fcn), pointer :: ptr
    !!
    !!     ! Local Variables
    !!     real(real64), allocatable, dimension(:,:) :: z
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
    !! contains
    !!     ! The system under study is a single degree-of-freedom mass-spring
    !!     ! system that utilizes a frictional damper.  The normal force
    !!     ! is the static weight of the moving body.
    !!     !
    !!     ! The equation of motion is:
    !!     ! m * x" + f(x',mu) + k * x = F(t)
    !!     !
    !!     ! Let:
    !!     ! m = Mass = 5 kg
    !!     ! g = Acceleration due to gravity = 9.81 m/s**2
    !!     ! mu = Friction coefficient = 0.2
    !!     ! k = Stiffness = 250 N/mm
    !!     ! F = Forcing amplitude = 2 kN
    !!     subroutine eqn(t, z, dzdt)
    !!         ! Arguments
    !!         real(real64), intent(in) :: t
    !!         real(real64), intent(in), dimension(:) :: z
    !!         real(real64), intent(out), dimension(:) :: dzdt
    !!
    !!         ! Constants
    !!         real(real64), parameter :: m = 5.0d0
    !!         real(real64), parameter :: g = 9.81d0
    !!         real(real64), parameter :: mu = 0.2d0
    !!         real(real64), parameter :: k = 250.0d3
    !!         real(real64), parameter :: F = 2.0d3
    !!         real(real64), parameter :: freq = 5.0d0
    !!         real(real64), parameter :: eps = 1.0d-6
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
    !! @image html coulomb_friction_model.png
    function coulomb(mu, normal, eps, velocity) result(f)
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

end module
