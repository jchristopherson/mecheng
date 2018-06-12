! strain.f90

!> @brief \b strain
!!
!! @par Purpose
!! Provides routines for performing strain related calculations.
module strain
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none
    private
    public :: wheatstone_bridge
    public :: strain_transform_x
    public :: strain_transform_y
    public :: strain_transfom_xy

! ------------------------------------------------------------------------------
    !> @brief Computes the output of a wheatstone bridge.
    interface wheatstone_bridge
        module procedure :: wheatstone_bridge_1
        module procedure :: wheatstone_bridge_2
    end interface

contains
! ------------------------------------------------------------------------------
    !> @brief Computes the output of a wheatstone bridge.
    !!
    !! @param[in] fg The gage factor of each strain gage.  Each gage is assigned
    !!  the same gage factor.
    !! @param[in] strain1 The strain in leg 1 of the bridge.
    !! @param[in] strain2 The strain in leg 2 of the bridge.
    !! @param[in] strain3 The strain in leg 3 of the bridge.
    !! @param[in] strain4 The strain in leg 4 of the bridge.
    !! @return The output of the bridge, in units of mV/V.
    !!
    !! @par Remarks
    !! The construction of the wheatstone bridge is as follows.
    !! @verbatim
    !!            V(+)
    !!            /\
    !!    Leg 1  /  \ Leg 4
    !! Vout(-) _/    \_ Vout(+)
    !!          \    /
    !!    Leg 2  \  / Leg 3
    !!            \/
    !!            V(-)
    !! @endverbatim
    !!
    !! @par
    !! The output of the bridge assuming each gage has the same resistance and
    !! the same gage factor is as follows.
    !! @par
    !! \f$ \frac{V_{out}}{V} = \frac{f_{g}(f_{g}(\epsilon_{1}\epsilon_{3} -
    !! \epsilon_{2}\epsilon_{4}) - (\epsilon_{4} - \epsilon_{3} + \epsilon_{2} -
    !! \epsilon_{1}))}{4 + f_{g}(f_{g}(\epsilon_{3} (\epsilon_{2} +
    !! \epsilon_{1}) + \epsilon_{4} (\epsilon_{2} + \epsilon_{1})) + 2
    !! (\epsilon_{1} + \epsilon_{2} + \epsilon_{3} + \epsilon_{4}))} \f$
    pure elemental function wheatstone_bridge_1(fg, strain1, strain2, strain3, &
            strain4) result(x)
        real(real64), intent(in) :: fg, strain1, strain2, strain3, strain4
        real(real64) :: x
        ! The 1.0d3 factor converts from V/V to mV/V
        x = 1.0d3 * fg * (fg * (strain1 * strain3 - strain2 * strain4) - &
            (strain4 - strain3 + strain2 - strain1)) / (4.0d0 + &
            fg * (fg * (strain3 * (strain2 + strain1) + &
            strain4 * (strain2 + strain1)) + &
            2.0d0 * (strain1 + strain2 + strain3 + strain4)))
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the output of a wheatstone bridge.
    !!
    !! @param[in] fg A 4-element array containing the gage factor for each
    !!  strain gage (gage 1, gage 2, gage 3, gage 4).
    !! @param[in] r A 4-element array containing the resistance for each strain
    !!  gage (gage 1, gage 2, gage 3, gage 4).
    !! @param[in] strain1 The strain in leg 1 of the bridge.
    !! @param[in] strain2 The strain in leg 2 of the bridge.
    !! @param[in] strain3 The strain in leg 3 of the bridge.
    !! @param[in] strain4 The strain in leg 4 of the bridge.
    !! @return The output of the bridge, in units of mV/V.
    !!
    !! @par Remarks
    !! The construction of the wheatstone bridge is as follows.
    !! @verbatim
    !!            V(+)
    !!            /\
    !!    Leg 1  /  \ Leg 4
    !! Vout(-) _/    \_ Vout(+)
    !!          \    /
    !!    Leg 2  \  / Leg 3
    !!            \/
    !!            V(-)
    !! @endverbatim
    !!
    !! @par
    !! The output of the bridge is defined as follows.
    !! @par
    !! \f$ \frac{V_{out}}{V} = \frac{R_{3} + \Delta R_{3}}{R_{3} + \Delta R_{3}
    !!  + R_{4} + \Delta R_{4}} - \frac{R_{2} + \Delta R_{2}}{R_{2} +
    !!  \Delta R_{2} + R_{1} + \Delta R_{1}}\f$
    !! @par
    !! Where:
    !! \f$ \Delta R_{i} = f_{g,i} R_{i} \epsilon_{i}, i = 1..4 \f$.
    pure function wheatstone_bridge_2(fg, r, strain1, strain2, &
            strain3, strain4) result(x)
        ! Variables
        real(real64), intent(in), dimension(4) :: fg, r
        real(real64), intent(in) :: strain1, strain2, strain3, strain4
        real(real64) :: x, dR1, dR2, dR3, dR4

        ! Compute the change in resistance terms.
        dR1 = fg(1) * r(1) * strain1
        dR2 = fg(2) * r(2) * strain2
        dR3 = fg(3) * r(3) * strain3
        dR4 = fg(4) * r(4) * strain4

        ! Compute the output in mV/V
        x = 1.0d3 * ((r(3) + dR3) / (r(3) + dR3 + r(4) + dR4) - &
            (r(2) + dR2) / (r(2) + dR2 + r(1) + dR1))
    end function

! ------------------------------------------------------------------------------
    !> @brief Applies a rotation transformation to the x-direction strain.
    !!
    !! @param[in] ex The x-direction strain.
    !! @param[in] ey The y-direction strain.
    !! @param[in] gxy The x-y shear strain.
    !! @param[in] theta The rotation angle, in radians.
    !! @return The transformed strain.
    !!
    !! @par Definition
    !! \f$ \epsilon_{x'} = \epsilon_{x} \cos^{2}\theta + \epsilon_{y}
    !! \sin^{2}\theta + \gamma_{xy} \sin\theta \cos\theta \f$
    pure elemental function strain_transform_x(ex, ey, gxy, theta) result(x)
        real(real64), intent(in) :: ex, ey, gxy, theta
        real(real64) :: x, ct, st
        ct = cos(theta)
        st = sin(theta)
        x = ex * ct**2 + ey * st**2 + gxy * st * ct
    end function

! ------------------------------------------------------------------------------
    !> @brief Applies a rotation transformation to the y-direction strain.
    !!
    !! @param[in] ex The x-direction strain.
    !! @param[in] ey The y-direction strain.
    !! @param[in] gxy The x-y shear strain.
    !! @param[in] theta The rotation angle, in radians.
    !! @return The transformed strain.
    !!
    !! @par Definition
    !! \f$ \epsilon_{y'} = \epsilon_{x} \sin^{2}\theta + \epsilon_{y}
    !! \cos^{2}\theta - \gamma_{xy} \sin\theta \cos\theta \f$
    pure elemental function strain_transform_y(ex, ey, gxy, theta) result(x)
        real(real64), intent(in) :: ex, ey, gxy, theta
        real(real64) :: x, ct, st
        ct = cos(theta)
        st = sin(theta)
        x = ex * st**2 + ey * ct**2 - gxy * st * ct
    end function

! ------------------------------------------------------------------------------
    !> @brief Applies a rotation transformation to the x-y shear strain.
    !!
    !! @param[in] ex The x-direction strain.
    !! @param[in] ey The y-direction strain.
    !! @param[in] gxy The x-y shear strain.
    !! @param[in] theta The rotation angle, in radians.
    !! @return The transformed strain.
    !!
    !! @par Definition
    !! \f$ \gamma_{x'y'} = 2 (\epsilon_{x} - \epsilon_{y}) \sin\theta \cos\theta
    !! + \gamma_{xy} (\cos^{2}\theta - \sin^{2}\theta) \f$
    pure elemental function strain_transfom_xy(ex, ey, gxy, theta) result(x)
        real(real64), intent(in) :: ex, ey, gxy, theta
        real(real64) :: x, ct, st
        ct = cos(theta)
        st = sin(theta)
        x = 2.0d0 * (ey - ex) * st * ct + gxy * (ct**2 - st**2)
    end function

! ------------------------------------------------------------------------------
end module
