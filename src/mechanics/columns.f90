! columns.f90

!> @brief \b columns
!!
!! @par Purpose
!! This module contains calculations relating to buckling and stability of
!! columns in compression.
!!
!! @par References
!! - 
module columns
    use, intrinsic :: iso_fortran_env, only : real64
    use sections, only : radius_of_gyration
    use constants, only : pi
    implicit none
    private
    public :: slenderness_ratio
    public :: euler_buckling
    public :: johnson_buckling
    public :: buckling_transition
    public :: buckling_load

! ------------------------------------------------------------------------------
    !> @brief Computes the slenderness ratio of a column.
    !!
    !! @par Remarks
    !! The slenderness ratio is defined as the ratio between the effective
    !! length of the column (\f$L_{e}\f$), and the radius of gyration of the 
    !! column (\f$\rho\f$):  \f$ R_{s} = \frac{L_{e}}{\rho} \f$.
    interface slenderness_ratio
        module procedure :: slenderness_ratio_1
        module procedure :: slenderness_ratio_2
    end interface

! ------------------------------------------------------------------------------
    !> @brief Computes the critical buckling load of a column by means of
    !! Euler's equation.
    !!
    !! @par Remarks
    !! The Euler buckling equation is as follows:
    !! \f$ P_{cr} = \frac{\pi^2 E A}{R_{s}^2} \f$; where 
    !! \f$ R_{s} = \frac{L_{e}}{\rho} \f$, and 
    !! \f$ \rho = \sqrt{\frac{I}{A}} \f$.
    !! - \f$ P_{cr} = \f$ The critical buckling load.
    !! - \f$ E = \f$ The modulus of elasticity of the material.
    !! - \f$ L_{e} = \f$ The effective length of the column.
    !! - \f$ I = \f$ The moment of inertia (area) of the column (softest axis).
    !! - \f$ A = \f$ The cross-sectional area of the column.
    interface euler_buckling
        module procedure :: euler_buckling_1
        module procedure :: euler_buckling_2
    end interface

! ------------------------------------------------------------------------------
    !> @brief Computes the critical buckling load of a column by means of
    !! J.B. Johnson's equation.
    !!
    !! @par Remarks
    !! The J.B. Johnson buckling equation is as follows:
    !! \f$ P_{cr} = A (S_{y} - (\frac{S_{y} R_{s}}{2 \pi})^2 (\frac{1}{E})) \f$;
    !! where \f$ R_{s} = \frac{L_{e}}{\rho} \f$, and 
    !! \f$ \rho = \sqrt{\frac{I}{A}} \f$.
    !! - \f$ P_{cr} = \f$ The critical buckling load.
    !! - \f$ S_{y} = \f$ The yield strength of the material.
    !! - \f$ E = \f$ The modulus of elasticity of the material.
    !! - \f$ L_{e} = \f$ The effective length of the column.
    !! - \f$ I = \f$ The moment of inertia (area) of the column (softest axis).
    !! - \f$ A = \f$ The cross-sectional area of the column.
    interface johnson_buckling
        module procedure :: johnson_buckling_1
        module procedure :: johnson_buckling_2
    end interface

! ------------------------------------------------------------------------------
    !> @brief Computes the buckling load of a column under compression.  The
    !! routine determines whether to use the J.B. Johnson equation or the
    !! Euler equation based upon the slenderness ratio of the column.
    interface buckling_load
        module procedure :: buckling_load_1
        module procedure :: buckling_load_2
    end interface

contains
! ------------------------------------------------------------------------------
    !> @brief Computes the slenderness ratio of a column.
    !!
    !! @param[in] length The effective length of the column.
    !! @param[in] rad The radius of gyration of the section.
    !! @return The slenderness ratio.
    pure elemental function slenderness_ratio_1(length, rad) result(x)
        real(real64), intent(in) :: length, rad
        real(real64) :: x
        x = length / rad
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the slenderness ratio of a column.
    !!
    !! @param[in] length The effective length of the column.
    !! @param[in] moi The moment of inertia (area) for the section.
    !! @param[in] area The cross-sectional area of the section.
    !! @return The slenderness ratio.
    pure elemental function slenderness_ratio_2(length, moi, area) result(x)
        real(real64), intent(in) :: length, moi, area
        real(real64) :: r, x
        r = radius_of_gyration(moi, area)
        x = slenderness_ratio(length, r)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the critical buckling load of a column by means of
    !! Euler's equation.
    !!
    !! @param[in] modulus The modulus of elasticity of the material from which
    !!  the column was made.
    !! @param[in] ratio The slenderness ratio of the column.
    !! @param[in] area The cross-sectional area of the column.
    !! @return The critical buckling load.
    pure elemental function euler_buckling_1(modulus, ratio, area) result(p)
        real(real64), intent(in) :: modulus, ratio, area
        real(real64) :: p
        p = pi**2 * modulus * area / (ratio**2)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the critical buckling load of a column by means of
    !! Euler's equation.
    !!
    !! @param[in] modulus The modulus of elasticity of the material from which
    !!  the column was made.
    !! @param[in] length The effective length of the column.
    !! @param[in] moi The moment of inertia (area) for the section.
    !! @param[in] area The cross-sectional area of the column.
    !! @return The critical buckling load.
    pure elemental function euler_buckling_2(modulus, length, moi, area) &
            result(p)
        real(real64), intent(in) :: modulus, length, moi, area
        real(real64) :: ratio, p
        ratio = slenderness_ratio(length, moi, area)
        p = euler_buckling(modulus, ratio, area)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the critical buckling load of a column by means of
    !! J.B. Johnson's equation.
    !!
    !! @param[in] modulus The modulus of elasticity of the material from which
    !!  the column is made.
    !! @param[in] yield The yield strength of the material from which the column
    !!  is made.
    !! @param[in] ratio The slenderness ratio of the column.
    !! @param[in] area The cross-sectional area of the column.
    !! @return The critical buckling load.
    pure elemental function johnson_buckling_1(modulus, yield, ratio, area) &
            result(p)
        real(real64), intent(in) :: modulus, ratio, area, yield
        real(real64) :: p
        p = area * (yield - (yield * ratio / (2.0d0 * pi))**2 / modulus)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the critical buckling load of a column by means of
    !! J.B. Johnson's equation.
    !!
    !! @param[in] modulus The modulus of elasticity of the material from which
    !!  the column is made.
    !! @param[in] yield The yield strength of the material from which the column
    !!  is made.
    !! @param[in] length The effective length of the column.
    !! @param[in] moi The moment of inertia (area) for the section.
    !! @param[in] area The cross-sectional area of the column.
    !! @return The critical buckling load.
    pure elemental function johnson_buckling_2(modulus, yield, length, moi, &
            area) result(p)
        real(real64), intent(in) :: modulus, yield, length, moi, area
        real(real64) :: ratio, p
        ratio = slenderness_ratio(length, moi, area)
        p = johnson_buckling(modulus, yield, ratio, area)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the slenderness ratio transition point above which the
    !! Euler equation is more appropriate to estimate the buckling load, but 
    !! below which the Johnson equation is more valid.
    !!
    !! @param[in] modulus The modulus of elasticity of the material from which
    !!  the column is made.
    !! @param[in] yield The yield strength of the material from which the column
    !!  is made.
    pure elemental function buckling_transition(modulus, yield) &
            result(r)
        real(real64), intent(in) :: modulus, yield
        real(real64) :: r
        r = sqrt(2.0d0 * pi**2 * modulus / yield)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the buckling load of a column under compression.  The
    !! routine determines whether to use the J.B. Johnson equation or the
    !! Euler equation based upon the slenderness ratio of the column.  The
    !! routine uses the buckling_transition routine to determine where the
    !! transition between the two equations lies.
    !!
    !! @param[in] modulus The modulus of elasticity of the material from which
    !!  the column is made.
    !! @param[in] yield The yield strength of the material from which the column
    !!  is made.
    !! @param[in] length The effective length of the column.
    !! @param[in] moi The moment of inertia (area) for the section.
    !! @param[in] area The cross-sectional area of the column.
    !! @return The critical buckling load.
    pure elemental function buckling_load_1(modulus, yield, ratio, area) &
            result(p)
        real(real64), intent(in) :: modulus, yield, ratio, area
        real(real64) :: p, critical

        ! Compute the critical slenderness ratio
        critical = buckling_transition(modulus, yield)

        ! Utilize the appropriate relationship
        if (ratio < critical) then
            p = johnson_buckling(modulus, yield, ratio, area)
        else
            p = euler_buckling(modulus, ratio, area)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the buckling load of a column under compression.  The
    !! routine determines whether to use the J.B. Johnson equation or the
    !! Euler equation based upon the slenderness ratio of the column.  The
    !! routine uses the buckling_transition routine to determine where the
    !! transition between the two equations lies.
    !!
    !! @param[in] modulus The modulus of elasticity of the material from which
    !!  the column is made.
    !! @param[in] yield The yield strength of the material from which the column
    !!  is made.
    !! @param[in] length The effective length of the column.
    !! @param[in] moi The moment of inertia (area) for the section.
    !! @param[in] area The cross-sectional area of the section.
    !! @return The critical buckling load.
    pure elemental function buckling_load_2(modulus, yield, length, moi, area) &
            result(p)
        real(real64), intent(in) :: modulus, yield, length, moi, area
        real(real64) :: ratio, p
        ratio = slenderness_ratio(length, moi, area)
        p = buckling_load(modulus, yield, ratio, area)
    end function

! ------------------------------------------------------------------------------
end module