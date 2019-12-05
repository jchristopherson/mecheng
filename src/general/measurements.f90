! measurements.f90

!> @brief Provides a collection of routines for operating on 
!! experimental measurement data.
module measurements
    use iso_fortran_env
    use curvefit_statistics
    use ferror
    implicit none
    private
    public :: MS_INVALID_INPUT_ERROR
    public :: MS_INVALID_DATA_SET_ERROR
    public :: grr_results
    public :: ssq_part
    public :: ssq_operator
    public :: ssq_part_operator
    public :: ssq_repeat
    public :: gage_r_r

    !> @brief Defines an invalid input error condition.
    integer(int32), parameter :: MS_INVALID_INPUT_ERROR = 100001
    !> @brief Defines an invalid data set error condition.
    integer(int32), parameter :: MS_INVALID_DATA_SET_ERROR = 100002

    !> @brief Provides a container for GR&R results.
    type grr_results
        !> @brief The part-induced variation.
        real(real64) :: part_variation
        !> @brief The operator-induced variation.
        real(real64) :: operator_variation
        !> @brief The reproducibility variation.
        real(real64) :: reproducibility
        !> @brief The repeatability variation.
        real(real64) :: repeatability
        !> @brief The part by operator variation.
        real(real64) :: part_by_operator_variation
        !> @brief The total gage variation.
        real(real64) :: gage_variation
        !> @brief The total process variation.
        real(real64) :: total_variation
        !> @brief The value of the F-statistic.
        real(real64) :: f_statistic
        !> @brief The value of the cummulative F-distribution as computed
        !! at the F-statistic.
        real(real64) :: f_test
        !> @brief The tolerance range.
        real(real64) :: tolerance
        !> @brief The sigma multiplier (k).
        real(real64) :: scale
        !> @brief The precision to tolerance ratio (P/T ratio).  This is
        !! computed as: k * sqrt(gage variation) / tolerance.
        real(real64) :: pt_ratio
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
    !> @brief Computes a Gage R&R ANOVA crossed study of an experimental data
    !! set obtained using multipler parts, repeated tests, and multiple
    !! test operators.
    !!
    !! @param[in] x A M-by-N-by-P array containing the data where M is the
    !!  number of parts, N is the number of tests, and P is the number of
    !!  operators.
    !! @param[in] alpha A parameter that lies on the set (0, 1) that is used
    !!  to determine the significance of part-by-operator influence.  The
    !!  default value is 0.05.
    !! @param[in] tol An optional input used to specify tolerance information
    !!  allowing for calculation of the precision-to-tolerance ratio.  If
    !!  not supplied, the precision-to-tolerance ratio is not computed.
    !! @param[in] k An optional input used to specify the number of 
    !!  standard deviations used in computation of the precision-to-tolerance
    !!  ratio.  The default value is 6.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - MS_INVALID_DATA_SET_ERROR: Occurs if there aren't at least 2 parts, 
    !!      2 tests, and 2 operators defined in the data set in @p x.
    !!  - MS_INVALID_INPUT_ERROR: Occurs if alpha is outside the range (0, 1).
    !!
    !! @return A grr_results object containing the calculation results.
    !!
    !! @par References:
    !! - https://www.muelaner.com/quality-assurance/gage-r-and-r-excel/
    !! - https://www.engineering.com/AdvancedManufacturing/ArticleID/16201/Gage-Studies-and-Gage-RR.aspx
    function gage_r_r(x, alpha, tol, k, err) result(rst)
        ! Arguments
        real(real64), intent(in), dimension(:,:,:) :: x
        real(real64), intent(in), optional :: alpha, tol, k
        class(errors), intent(inout), optional, target :: err
        type(grr_results) :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: npart, nrep, nop
        real(real64) :: xmean, ss_part, ss_op, ss_partop, ss_rep, ss_total, &
            ms_part, ms_op, ms_partop, ms_rep, d1, d2, a, kf

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        a = 5.0d-2
        if (present(alpha)) a = alpha
        kf = 6.0d0
        if (present(k)) kf = k
        npart = size(x, 1)
        nrep = size(x, 2)
        nop = size(x, 3)
        rst%scale = kf

        ! Input Check
        if (npart < 2) then
            write (errmsg, '(AI0A)') &
                "Expected to find at least 2 parts, but only found ", &
                npart, "."
            call errmgr%report_error("gage_r_r", trim(errmsg), &
                MS_INVALID_DATA_SET_ERROR)
            return
        end if
        if (nrep < 2) then
            write (errmsg, '(AI0A)') &
                "Expected to find at least 2 tests, but only found ", &
                nrep, "."
            call errmgr%report_error("gage_r_r", trim(errmsg), &
                MS_INVALID_DATA_SET_ERROR)
            return
        end if
        if (nop < 2) then
            write (errmsg, '(AI0A)') &
                "Expected to find at least 2 operators, but only found ", &
                nop, "."
            call errmgr%report_error("gage_r_r", trim(errmsg), &
                MS_INVALID_DATA_SET_ERROR)
            return
        end if
        if (a <= 0.0d0 .or. a >= 1.0d0) then
            write (errmsg, '(AE11.3A)') &
                "The alpha parameter must lie between 0 and 1, " // &
                "but was found to be ", a, "."
            call errmgr%report_error("gage_r_r", trim(errmsg), &
                MS_INVALID_INPUT_ERROR)
            return
        end if

        ! Compute the overall mean
        xmean = mean(pack(x, .true.))

        ! Compute the sum of the square terms
        ss_part = ssq_part(x, xmean)
        ss_op = ssq_operator(x, xmean)
        ss_partop = ssq_part_operator(x, xmean)
        ss_rep = ssq_repeat(x)
        ss_total = ss_part + ss_op + ss_partop + ss_rep

        ! Compute the mean of the squared differences
        d1 = (npart - 1.0d0) * (nop - 1.0d0)
        d2 =  nrep - 1.0d0
        ms_part = ss_part / (npart - 1.0d0)
        ms_op = ss_op / (nop - 1.0d0)
        ms_rep = ss_rep / d2
        ms_partop = ss_partop / d1

        ! Compute the F-Test for signficance
        rst%f_statistic = ms_partop / ms_rep

        ! Compute the F-test for signficance
        rst%f_test = 1.0d0 - f_distribution(rst%f_statistic, d1, d2)

        ! Compare with alpha to determine which method to use in computing
        ! the variance terms
        if (rst%f_test < a) then
            ss_rep = ss_total - ss_part - ss_op
            ms_rep = ss_rep / d2
            rst%part_variation = max(0.0d0, (ms_part - ms_rep) / (nop * nrep))
            rst%operator_variation = max(0.0d0, (ms_op - ms_rep) / (npart * nrep))
            rst%part_by_operator_variation = 0.0d0
        else
            rst%part_variation = max(0.0d0, (ms_part - ms_rep) / (nop * nrep))
            rst%operator_variation = max(0.0d0, (ms_op - ms_rep) / (npart * nrep))
            rst%part_by_operator_variation = max(0.0d0, (ms_partop - ms_rep) / nrep)
        end if
        rst%repeatability = ms_rep
        rst%reproducibility = rst%operator_variation + rst%part_by_operator_variation
        rst%gage_variation = rst%repeatability + rst%reproducibility
        rst%total_variation = rst%gage_variation + rst%part_variation

        ! Compute tolerance related information
        if (present(tol)) then
            rst%tolerance = tol
            rst%pt_ratio = kf * sqrt(rst%gage_variation) / tol
        else
            rst%tolerance = 0.0d0
            rst%pt_ratio = 0.0d0
        end if
    end function
! ------------------------------------------------------------------------------
    !> @brief Computes the cummulative F-distribution.
    !!
    !! @param[in] x The value at which to evaluate the distribution function.
    !! @param[in] d1 
    !! @param[in] d2
    !!
    !! @return The value of the F-distribution at @p x.
    function f_distribution(x, d1, d2) result(f)
        ! Arguments
        real(real64), intent(in) :: x, d1, d2
        real(real64) :: f

        ! Local Variables
        real(real64) :: xf

        ! Compute the incomplete beta function
        xf = d1 * x / (d1 * x + d2)
        f = incomplete_beta(d1, d2, xf)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
