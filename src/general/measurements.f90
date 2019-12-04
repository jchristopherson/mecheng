! measurements.f90

!> @brief Provides a collection of routines for operating on 
!! experimental measurement data.
module measurements
    use iso_fortran_env
    use curvefit_statistics
    implicit none
    private
    public :: grr_results
    public :: ssq_part
    public :: ssq_operator
    public :: ssq_part_operator

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
    !! @par
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
            ! Collect the data whose mean we are to evaluate, 
            ! and then compute the mean
            l = 0
            do j = 1, nrep
                do k = 1, nop
                    l = l + 1
                    y(l) = x(i,j,k)
                end do
            end do
            xi = mean(y)

            ! Compute and include the difference
            ssq = ssq + (xi - xmean)**2
        end do

        ! Output
        ssq = nop * nrep * ssq
    end function

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
    !! The sum of the squares of the difference between the mean for each operator
    !! (\f$ \overline{x_{j}} \f$) and the overall mean (\f$ \overline{x} \f$)
    !! is computed as follows.
    !! @par
    !! \f$ s_{op} = n_{part} n_{rep} \sum (\overline{x_{j}} - \overline{x})^{2} \f$
    pure function ssq_operator(x, xmean) result(ssq)
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
        allocate(y(npart * nrep))

        ! Cycle over each operator
        ssq = 0.0d0
        do k = 1, nop
            ! Collect the data whose mean we are to evaluate, 
            ! and then compute the mean
            l = 0
            do j = 1, nrep
                do i = 1, npart
                    l = l + 1
                    y(l) = x(i,j,k)
                end do
            end do
            xi = mean(y)

            ! Compute and include the difference
            ssq = ssq + (xi - xmean)**2
        end do

        ! Output
        ssq = npart * nrep * ssq
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the sum of the squares of the difference between the
    !! each observation and the overall mean.
    !!
    !! @param[in] x A M-by-N-by-P array containing the data where M is the
    !!  number of parts, N is the number of tests, and P is the number of
    !!  operators.
    !! @param[in] xmean The overall (grand) mean of @p x.
    !!
    !! @return The result of the operation.
    !!
    !! @par Remarks
    !! The sum of the squares of the difference between each observation 
    !! (\f$ x_{ijk} \f$) and the overall mean (\f$ \overline{x} \f$)
    !! is computed as follows.
    !! @par
    !! \f$ s_{part*op} = n_{op} n_{rep} \sum (x_{ijk} - \overline{x})^{2} \f$
    pure function ssq_part_operator(x, xmean) result(ssq)
        ! Arguments
        real(real64), intent(in), dimension(:,:,:) :: x
        real(real64), intent(in) :: xmean
        real(real64) :: ssq

        ! Local Variables
        integer(int32) :: i, j, k, nop, npart, nrep
        
        ! Initialization
        npart = size(x, 1)
        nrep = size(x, 2)
        nop = size(x, 3)
        
        ! Process
        ssq = 0.0d0
        do k = 1, nop
            do j = 1, nrep
                do i = 1, npart
                    ssq = ssq + (x(i,j,k) - xmean)**2
                end do
            end do
        end do

        ! Output
        ssq = nop * nrep * ssq
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the sum of the squares of the difference between each
    !! observation and the factor level mean.
    !!
    !! @param[in] x A M-by-N-by-P array containing the data where M is the
    !!  number of parts, N is the number of tests, and P is the number of
    !!  operators.
    !! @param[in] xmean The overall (grand) mean of @p x.
    !!
    !! @return The result of the operation.
    !!
    !! @par Remarks
    !! The sum of the squares of the difference between each observation
    !! (\f$ x_{ijk} \f$) and the factor level mean (\f$ \overline{x_{ij}} \f$)
    !! is computed as follows.
    !! @par
    !! \f$ s_{rep} = \sum (x_{ijk} - \overline{x_{ij}})^{2} \f$
    pure function ssq_repeat(x) result(ssq)
        ! Arguments
        real(real64), intent(in), dimension(:,:,:) :: x
        real(real64) :: ssq

        ! Local Variables
        integer(int32) :: i, j, k, nop, npart, nrep
        real(real64) :: xik
        
        ! Initialization
        npart = size(x, 1)
        nrep = size(x, 2)
        nop = size(x, 3)

        ! Cycle over each observation
        ssq = 0.0d0
        do k = 1, nop
            do i = 1, npart
                xik = mean(x(i,:,k))
                do j = 1, nrep
                    ssq = ssq + (x(i,j,k) - xik)**2
                end do
            end do
        end do
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
