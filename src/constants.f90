! constants.f90

!> @brief \b constants
!!
!! @par Purpose
!! This module provides constants commonly used in engineering calculations.
module constants
    use, intrinsic :: iso_fortran_env, only : real64
    implicit none

    !> @brief The parameter pi.
    real(real64), parameter :: pi = 3.1415926535897932384626433832795d0
end module
