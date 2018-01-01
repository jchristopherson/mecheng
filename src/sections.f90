! sections.f90

!> @brief \b sections
!!
!! @par Purpose
!! This module contains routines used for computing the sectional properties
!! of various shapes.
module sections
    use, intrinsic :: iso_fortran_env, only : real64
    use constants, only : pi
    implicit none

contains
! ------------------------------------------------------------------------------
    !> @brief Computes the radius of gyration of a section.
    !!
    !! @param[in] moi The moment of inertia of area for the section.
    !! @param[in] area The cross-sectional area of the section.
    !! @return The radius of gyration for the section.
    !!
    !! @par
    !! The radius of gyration is computed as follows.
    !! @par 
    !! \f$ \rho = \sqrt{\frac{I}{A}} \f$
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
    !!  - Moment of inertia about the y-axis of the section (Soft Axis).
    !!  - Polar moment of inertia.
    !!
    !! @par
    !! The cross-sectional properties are computed as follows.
    !! - Area:
    !!  - \f$ A = t_{w}h_{w} + 2w_{f}t_{f} \f$
    !! - Moments of Inertia: 
    !!  - \f$ I_{x} = \int y^{2} \,dA = \frac{1}{12}(t_{w}h_{w}^{3} + 2w_{f}t_{f}^{3}) + \frac{1}{2}w_{f}t_{f}(h_{w} + t_{f})^{2} \f$
    !!  - \f$ I_{y} = \int x^{2} \,dA = \frac{1}{12}(h_{w}t_{w}^{3} + 2t_{f}w_{f}^{3}) \f$
    !! - Polar Moment of Inertia:
    !!  - \f$ J = \int r^{2} \,dA = \int (x^{2} + y^{2}) \,dA = 
    !!      I_{x} + I{y} \f$.
    pure function i_beam_section(webt, webh, flanget, flangew) result(s)
        ! Arguments
        real(real64), intent(in) :: webt, webh, flanget, flangew
        real(real64), dimension(4) :: s

        ! Local Variables
        real(real64) :: flangeArea

        ! Compute the cross-sectional area
        flangeArea = flanget * flangew
        s(1) = 2.0d0 * flangeArea + webt * webh

        ! Compute the moment of inertia about the x-axis (stiff) axis
        s(2) = (1.0d0 / 12.0d0) * (webt * webh**3 + &
            2.0d0 * flangew * flanget**3) + 0.5d0 * flangew * flanget * &
            (webh + flanget)**2
        
        ! Compute the moment of inertia about the y-axis (soft axis)
        s(3) = (1.0d0 / 12.0d0) * (webh * webt**3 + &
            2.0d0 * flanget * flangew**3)
        
        ! Compute the polar moment of inertia
        s(4) = s(2) + s(3)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cross-sectional properties of a circular section.
    !!
    !! @param[in] diameter The diameter of the section.
    !! @return An array containing the following cross-sectional properties.
    !!  - Cross-sectional area.
    !!  - Moment of inertia.
    !!  - Polar moment of inertia.
    !! 
    !! @par
    !! The cross-sectional properties are computed as follows.
    !! - Area:
    !!  - \f$ A = \frac{\pi}{4}d^{2} \f$
    !! - Moment of Inertia:
    !!  - \f$ I = \frac{\pi}{64}d^{4} \f$
    !! - Polar Moment of Inertia:
    !!  - \f$ J = \frac{\pi}{32}d^{4} \f$
    pure function circle_section(diameter) result(s)
        ! Arguments
        real(real64), intent(in) :: diameter
        real(real64), dimension(3) :: s

        ! Compute the cross-sectional area
        s(1) = (pi / 4.0d0) * diameter**2

        ! Compute the moment of inertia
        s(2) = (pi / 64.0d0) * diameter**4

        ! Compute the polar moment of inertia
        s(3) = (pi / 32.0d0) * diameter**4
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cross-sectional properties of a hollow circular 
    !! section.
    !!
    !! @param[in] od The outer diameter of the section.
    !! @param[in] id The inner diameter of the section.
    !! @return An array containing the following cross-sectional properties.
    !!  - Cross-sectional area.
    !!  - Moment of inertia.
    !!  - Polar moment of inertia.
    !! 
    !! @par
    !! The cross-sectional properties are computed as follows.
    !! - Area:
    !!  - \f$ A = \frac{\pi}{4}(d_{o}^{2}-d_{i}^{2}) \f$
    !! - Moment of Inertia:
    !!  - \f$ I = \frac{\pi}{64}(d_{o}^{4}-d_{i}^{4}) \f$
    !! - Polar Moment of Inertia:
    !!  - \f$ J = \frac{\pi}{32}(d_{o}^{4}-d_{i}^{4}) \f$
    pure function hollow_circle_section(od, id) result(s)
        ! Arguments
        real(real64), intent(in) :: od, id
        real(real64), dimension(3) :: s

        ! Compute the cross-sectional area
        s(1) = (pi / 4.0d0) * (od**2 - id**2)

        ! Compute the moment of inertia
        s(2) = (pi / 64.0d0) * (od**4 - id**4)

        ! Compute the polar moment of inertia
        s(3) = (pi / 32.0d0) * (od**4 - id**4)
    end function

! ------------------------------------------------------------------------------
end module
