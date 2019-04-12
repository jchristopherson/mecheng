! friction.f90

!> @brief \b friction
!!
!! @par Purpose
!! To provide various friction models suitable for use in numerical simulations.
module friction
    use iso_fortran_env
    implicit none
    private

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
    !! \f$ F = -\frac{\mu N}{\epsilon} v \f$ when \f$ |v| \leq \epsilon \f$, and
    !! \f$ F = -\mu N sgn(v) \f$ when \f$ |v| > \epsilon \f$.
    function coulomb_model(mu, normal, eps, velocity) result(f)
        ! Arguments
        real(real64), intent(in) :: mu, normal, eps, velocity
        real(real64) :: f

        ! Process
        if (abs(velocity) <= eps) then
            f = -(mu * normal / eps) * velocity
        else
            f = -mu * normal * sign(1.0d0, velocity)
        end if
    end function

end module
