! geometry.f90

module geometry
    use iso_fortran_env
    implicit none

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a 3-component, Cartesian vector.
    type vector
        !> The x-component of the vector.
        real(real64) :: x
        !> The y-component of the vector.
        real(real64) :: y
        !> The z-component of the vector.
        real(real64) :: z
    end type
contains
end module