! geometry.f90

module geometry
    use iso_fortran_env
    implicit none
    private
    public :: vector
    public :: cross

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

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    !> @brief Computes the cross-product of a vector.
    interface cross
        module procedure :: cross_1
        module procedure :: cross_2
    end interface

! ------------------------------------------------------------------------------
contains
! ******************************************************************************
! VECTOR RELATED ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Computes the cross-product of a vector.
    !!
    !! @param[in] x The left-hand-side operator.
    !! @param[in] y The right-hand-side operator.
    !! @return The resulting vector.
    pure function cross_1(x, y) result(z)
        ! Arguments
        class(vector), intent(in) :: x, y
        type(vector) :: z

        ! Process
        z%x = x%y * y%z - x%z * y%y
        z%y = x%z * y%x - x%x * y%z
        z%z = x%x * y%y - x%y * y%x
    end function

! --------------------
    !> @brief Computes the cross-product of a vector.
    !!
    !! @param[in] x The left-hand-side operator.
    !! @param[in] y The right-hand-side operator.
    !! @return The resulting vector.
    pure function cross_2(x, y) result(z)
        ! Arguments
        class(vector), intent(in) :: x
        real(real64), intent(in), dimension(3) :: y
        type(vector) :: z

        ! Process
        z%x = x%y * y(3) - x%z * y(2)
        z%y = x%z * y(1) - x%x * y(3)
        z%z = x%x * y(2) - x%y * y(1)
    end function

! --------------------

! --------------------

! --------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module