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
    public :: wheatstone_bridge_series
    public :: strain_transform_x
    public :: strain_transform_y
    public :: strain_transfom_xy

! ------------------------------------------------------------------------------
    !> @brief Computes the output of a wheatstone bridge.
    interface wheatstone_bridge
        module procedure :: wheatstone_bridge_1
        module procedure :: wheatstone_bridge_2
        module procedure :: wheatstone_bridge_3
        module procedure :: wheatstone_bridge_4
    end interface

! ------------------------------------------------------------------------------
    !>
    interface wheatstone_bridge_series
        module procedure :: wheatstone_bridge_series_1
        module procedure :: wheatstone_bridge_series_2
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
    pure function wheatstone_bridge_1(fg, strain1, strain2, strain3, &
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
    pure function wheatstone_bridge_2(fg, strain1, strain2, strain3, &
            strain4) result(x)
        real(real64), intent(in) :: fg
        real(real64), intent(in), dimension(:) :: strain1, strain2, strain3, &
            strain4
        real(real64), allocatable, dimension(:) :: x
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
    pure function wheatstone_bridge_3(fg, r, strain1, strain2, &
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
    pure function wheatstone_bridge_4(fg, r, strain1, strain2, &
            strain3, strain4) result(x)
        ! Variables
        real(real64), intent(in), dimension(4) :: fg, r
        real(real64), intent(in), dimension(:) :: strain1, strain2, strain3, &
            strain4
        real(real64), allocatable, dimension(:) :: x, dR1, dR2, dR3, dR4

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
    !> @brief Computes the output of a wheatstone bridge where two strain gages
    !! are placed in series in each leg.
    !!
    !! @param[in] fg The gage factor of each strain gage.  Each gage is assigned
    !!  the same gage factor.
    !! @param[in] strain1a The strain in leg 1a of the bridge.
    !! @param[in] strain1b The strain in leg 1b of the bridge.
    !! @param[in] strain2a The strain in leg 2a of the bridge.
    !! @param[in] strain2b The strain in leg 2b of the bridge.
    !! @param[in] strain3a The strain in leg 3a of the bridge.
    !! @param[in] strain3b The strain in leg 3b of the bridge.
    !! @param[in] strain4a The strain in leg 4a of the bridge.
    !! @param[in] strain4b The strain in leg 4b of the bridge.
    !! @return The output of the bridge, in units of mV/V.
    !!
    !! @par Remarks
    !! The construction of the wheatstone bridge is as follows.
    !! @verbatim
    !!             V(+)
    !!    Leg 1b   /\  Leg 4b
    !!            /  \
    !!  Leg 1a   /    \  Leg 4a
    !! Vout(-) _/      \_ Vout(+) 
    !!  Leg 2b  \      / Leg 3b
    !!           \    /
    !!    Leg 2a  \  /   Leg 3a
    !!             \/
    !!            V(-)
    !! @endverbatim
    !!
    !! @par
    !! The bridge output is computed similarily to that of a traditional
    !! wheatstone bridge, but the resistance of each leg requires treatment
    !! of two strain gages in series.  The gage resistance in each leg
    !! is as follows.
    !! \f$ R_{eff,i} = f_{a,i} R_{a,i} \epsilon_{a,i} + R_{a,i} + 
    !!  f_{b,i} R_{b,i} \epsilon_{b,i} + R_{b,i} \f$
    !! When the gage factor and nominal resistance is the same for each gage
    !! the relationship simplifies to the following form.
    !! \f$ R_{eff,i} = R_i (f_i (\epsilon_{a,i} + \epsilon_{b,i}) + 2) \f$
    pure function wheatstone_bridge_series_1(fg, strain1a, strain1b, &
            strain2a, strain2b, strain3a, strain3b, strain4a, strain4b) &
            result(x)
        ! Arguments
        real(real64), intent(in) :: fg
        real(real64), intent(in) :: strain1a, strain1b, &
            strain2a, strain2b, strain3a, strain3b, strain4a, strain4b
        real(real64) :: x

        ! Local Variables
        real(real64) :: r1, r2, r3, r4

        ! Compute the individual leg resistance values
        r1 = fg * (strain1a + strain1b) + 2.0d0
        r2 = fg * (strain2a + strain2b) + 2.0d0
        r3 = fg * (strain3a + strain3b) + 2.0d0
        r4 = fg * (strain4a + strain4b) + 2.0d0

        ! Compute the output
        x = 1.0d3 * ((r3 * r1 - r2 * r4) / ((r2 + r1) * r4 + (r2 + r1) * r3))
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the output of a wheatstone bridge where two strain gages
    !! are placed in series in each leg.
    !!
    !! @param[in] fg The gage factor of each strain gage.  Each gage is assigned
    !!  the same gage factor.
    !! @param[in] strain1a The strain in leg 1a of the bridge.
    !! @param[in] strain1b The strain in leg 1b of the bridge.
    !! @param[in] strain2a The strain in leg 2a of the bridge.
    !! @param[in] strain2b The strain in leg 2b of the bridge.
    !! @param[in] strain3a The strain in leg 3a of the bridge.
    !! @param[in] strain3b The strain in leg 3b of the bridge.
    !! @param[in] strain4a The strain in leg 4a of the bridge.
    !! @param[in] strain4b The strain in leg 4b of the bridge.
    !! @return The output of the bridge, in units of mV/V.
    !!
    !! @par Remarks
    !! The construction of the wheatstone bridge is as follows.
    !! @verbatim
    !!             V(+)
    !!    Leg 1b   /\  Leg 4b
    !!            /  \
    !!  Leg 1a   /    \  Leg 4a
    !! Vout(-) _/      \_ Vout(+) 
    !!  Leg 2b  \      / Leg 3b
    !!           \    /
    !!    Leg 2a  \  /   Leg 3a
    !!             \/
    !!            V(-)
    !! @endverbatim
    !!
    !! @par
    !! The bridge output is computed similarily to that of a traditional
    !! wheatstone bridge, but the resistance of each leg requires treatment
    !! of two strain gages in series.  The gage resistance in each leg
    !! is as follows.
    !! \f$ R_{eff,i} = f_{a,i} R_{a,i} \epsilon_{a,i} + R_{a,i} + 
    !!  f_{b,i} R_{b,i} \epsilon_{b,i} + R_{b,i} \f$
    !! When the gage factor and nominal resistance is the same for each gage
    !! the relationship simplifies to the following form.
    !! \f$ R_{eff,i} = f_i R_i ((\epsilon_{a,i} + \epsilon_{b,i}) + 2) \f$
    pure function wheatstone_bridge_series_2(fg, strain1a, strain1b, &
            strain2a, strain2b, strain3a, strain3b, strain4a, strain4b) &
            result(x)
        ! Arguments
        real(real64), intent(in) :: fg
        real(real64), intent(in), dimension(:) :: strain1a, strain1b, &
            strain2a, strain2b, strain3a, strain3b, strain4a, strain4b
        real(real64), allocatable, dimension(:) :: x

        ! Local Variables
        real(real64), allocatable, dimension(:) :: r1, r2, r3, r4

        ! Compute the individual leg resistance values
        r1 = fg * (strain1a + strain1b) + 2.0d0
        r2 = fg * (strain2a + strain2b) + 2.0d0
        r3 = fg * (strain3a + strain3b) + 2.0d0
        r4 = fg * (strain4a + strain4b) + 2.0d0

        ! Compute the output
        x = 1.0d3 * ((r3 * r1 - r2 * r4) / ((r2 + r1) * r4 + (r2 + r1) * r3))
    end function

! ******************************************************************************
! STRAIN TRANSFORMS
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
