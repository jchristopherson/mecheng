! measurements.f90

!> @brief Provides a collection of routines for operating on 
!! experimental measurement data.
module measurements
    use iso_fortran_env
    use curvefit_statistics
    implicit none
    private
    public :: grr_results

    ! REF:
    ! https://www.muelaner.com/quality-assurance/gage-r-and-r-excel/
    ! https://www.engineering.com/AdvancedManufacturing/ArticleID/16201/Gage-Studies-and-Gage-RR.aspx

    !> @brief Provides a container for GR&R results.
    type grr_results
        !> @brief The part-induced variation.
        real(real64) :: part_variation
        !> @brief The reproducibility variation.
        real(real64) :: reproducibility
        !> @brief The repeatability variation.
        real(real64) :: repeatability
        !> @brief The total process variation.
        real(real64) :: total_variation
        !> @brief The part by operator variation.
        real(real64) :: part_by_operator_variation
    end type

contains
! ------------------------------------------------------------------------------
    !> @brief Computes the sum of the squares of the difference between the
    !! mean for each part, and the overall mean.
    !!
    !! @param[in] x A M-by-N-by-P array containing the data where M is the
    !!  number of parts, N is the number of tests, and P is the number of
    !!  operators.
    !! @param[in] xmean The overall (grand) mean of @p x.
    !!
    !! @return The result of the operation.
    !!
    !! @par Remarks
    !! The sum of the squares of the difference between the mean for each part
    !! (\f$ \overline{x_{i}} \f$) and the overall mean (\f$ \overline{x} \f$)
    !! is computed as follows.
    !! \f$ s_{part} = n_{op} n_{rep} \sum (\overline{x_{i}} - \overline{x})^{2} \f$
    pure function ssq_part(x, xmean) result(ssq)
        ! Arguments
        real(real64), intent(in), dimension(:,:,:) :: x
        real(real64), intent(in) :: xmean
        real(real64) :: ssq

        ! Local Variables
        integer(int32) :: i, j, k, l, nop, npart, nrep
        real(real64) :: xi
        real(real64), allocatable, dimension(:) :: y

        ! Initialization
        npart = size(x, 1)
        nrep = size(x, 2)
        nop = size(x, 3)
        allocate(y(nrep * nop))

        ! Cycle over each part, and compute the sum of the differences
        ssq = 0.0d0
        do i = 1, npart
            ! Collect the data whose mean we are to evaluate, and compute the mean
            l = 0
            do j = 1, nrep
                do k = 1, nop
                    l = l + 1
                    y(l) = x(i,j,k)
                end do
            end do
            xi = mean(y)

            ! Process
            ssq = ssq + (xi - xmean)**2
        end do

        ! Output
        ssq = nop * nrep * ssq
    end function

! ------------------------------------------------------------------------------
    !

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
