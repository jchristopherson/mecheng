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

! ------------------------------------------------------------------------------
    !> @brief Computes the output of a wheatstone bridge.
    interface wheatstone_bridge
        module procedure :: wheatstone_bridge_1
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
    !!         V+
    !!         /\
    !! Leg 1  /  \ Leg 4
    !!       /    \
    !!       \    /
    !! Leg 2  \  / Leg 3
    !!         \/
    !!         V-
    !! @endverbatim
    !!
    !! @par 
    !! The output of the bridge assuming each gage has the same resistance and
    !! the same gage factor is as follows.
    !! \f$ \frac{V_{out}}{V_{in}} = \frac{f_{g}(f_{g}(\epsilon_{1}\epsilon_{3} -
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

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
