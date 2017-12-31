! sections.f90

!> @brief \b sections
!!
!! @par Purpose
!! This module contains routines used for computing the sectional properties
!! of various shapes.
module sections
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none

contains
! ------------------------------------------------------------------------------
    !> @brief Computes the radius of gyration of a section.
    !!
    !! @param[in] moi The moment of inertia of area for the section.
    !! @param[in] area The cross-sectional area of the section.
    !! @return The radius of gyration for the section.
    pure elemental function radius_of_gyration(moi, area) result(r)
        real(real64), intent(in) :: moi, area
        real(real64) :: r
        r = sqrt(moi / area)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cross-sectional properties of an I-beam section.
    !!
    !! @param[in] webt The web section thickness.
    !! @param[in] webh The web section height (between the flanges).
    !! @param[in] flanget The flange thickness.
    !! @param[in] flangew The flange width.
    !! @return An array containing the following cross-sectional properties.
    !!  - Cross-sectional area.
    !!  - Moment of inertia about the x-axis of the section (Stiff Axis).  
    !!      \f$ I_{x} = \iint_A y^{2} \,dA \f$
    !!  - Moment of inertia about the y-axis of the section (Soft Axis).
    !!      \f$ I_{y} = \iint_A x^{2} \,dA \f$
    !!  - Polar moment of inertia.  \f$ J = \iint_A r^{2} \,dA = 
    !!      \iint_A (x^{2} + y{2}) \,dA = I_{x} + I{y} \f$.
    pure function i_beam_section(webt, webh, flanget, flangew) result(s)
        ! Arguments
        real(real64), intent(in) :: webt, webh, flanget, flangew
        real(real64), dimension(3) :: s

        ! Local Variables
        real(real64) :: flangeArea

        ! Compute the cross-sectional area
        flangeArea = flanget * flangew
        s(1) = 2.0d0 * flangeArea + webt * webh

        ! Compute the moment of inertia about the x-axis (stiff) axis
        s(2) = (1.0d0 / 12.0d0) * (webt * webh**3 + &
            2.0d0 * (flangew * flanget**3 + &
            0.5d0 * flangeArea * (webh + flanget)))
        
        ! Compute the moment of inertia about the y-axis (soft axis)
        s(3) = (1.0d0 / 12.0d0) * (webh * webt**3 + &
            2.0d0 * flanget * flangew**3)
        
        ! Compute the polar moment of inertia
        s(4) = s(2) + s(3)
    end function

! ------------------------------------------------------------------------------
    ! Circular

! ------------------------------------------------------------------------------
    ! Hollow Circular

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
