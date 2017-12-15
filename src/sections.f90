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
end module
