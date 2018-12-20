! mechanics_constants.f90

!> @brief \b mechanics_constants
!!
!! @par Purpose
!! This module contains constants specific to mechanics calculations.
module mechanics_constants
    use iso_fortran_env
    implicit none

    !> An error code indicating an array size mismatch.
    integer(int32), parameter :: MECH_ARRAY_SIZE_ERROR = 5000

    !> An error code indicating an invalid input parameter.
    integer(int32), parameter :: MECH_INVALID_INPUT_ERROR = 5001

    !> An error code indicating an out-of-memory condition.
    integer(int32), parameter :: MECH_OUT_OF_MEMORY_ERROR = 5002
end module