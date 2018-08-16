! geometry.f90

module geometry
    use iso_fortran_env
    implicit none

contains
! ******************************************************************************
! VECTOR RELATED ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Computes the cross-product of a vector.
    !!
    !! @param[in] x The left-hand-side operator.
    !! @param[in] y The right-hand-side operator.
    !! @return The resulting vector.
    pure function cross(x, y) result(z)
        ! Arguments
        real(real64), intent(in), dimension(3) :: x, y
        real(real64), dimension(3) :: z

        ! Process
        z(1) = x(2) * y(3) - x(3) * y(2)
        z(2) = x(3) * y(1) - x(1) * y(3)
        z(3) = x(1) * y(2) - x(2) * y(1)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the projection of a vector onto another vector.
    !!
    !! @param[in] x The vector to project.
    !! @param[in] y The vector onto which @p x is to be projected.
    !! @return The projected vector.
    pure function proj(x, y) result(z)
        ! Arguments
        real(real64), intent(in), dimension(3) :: x, y
        real(real64), dimension(3) :: z

        ! Local Variables
        real(real64) :: x1

        ! Compute the scalar projection
        x1 = dot_product(x, y) / norm2(y)

        ! Compute the projection
        z = x1 * y
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module