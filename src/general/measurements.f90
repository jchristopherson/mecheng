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
    public :: ems_grr_results
    public :: ssq_part
    public :: ssq_operator
    public :: ssq_repeat
    public :: ems_gauge_r_r
    public :: discrimination_ratio

    !> @brief Defines an invalid input error condition.
    integer(int32), parameter :: MS_INVALID_INPUT_ERROR = 100001
    !> @brief Defines an invalid data set error condition.
    integer(int32), parameter :: MS_INVALID_DATA_SET_ERROR = 100002

    !> @brief Provides a container for expected mean squares (EMS) type
    !! GR&R results.
    type ems_grr_results
        !> @brief The overall mean of the data set.
        real(real64) :: overall_mean
        !> @brief The sum of the squares of the difference between the
        !! mean for each part and the overall mean.
        real(real64) :: sum_squares_part
        !> @brief The sum of the squares of the difference between the
        !! mean from each operator and the overall mean.
        real(real64) :: sum_squares_operator
        !> @brief The sum of the squares of the difference between each
        !! observation and the factor level m ean.
        real(real64) :: sum_squares_within
        !> @brief The sum of the squares of the difference between each
        !! observation and the overall mean.
        real(real64) :: sum_squares_total
        !> @brief The sum of the squares of the part-by-operator results
        !! compared with the overall mean.
        real(real64) :: sum_squares_part_by_operator
        !> @brief The number of operator degrees-of-freedom.
        integer(int32) :: operator_dof
        !> @brief The number of part degrees-of-freedom.
        integer(int32) :: part_dof
        !> @brief The number of part-by-operator degrees-of-freedom.
        integer(int32) :: part_by_operator_dof
        !> @brief The number of repeatability degrees-of-freedom.
        integer(int32) :: repeatability_dof
        !> @brief The total number of degrees-of-freedom.
        integer(int32) :: total_dof
        !> @brief The mean-square part variance.
        real(real64) :: mean_square_part
        !> @brief The mean-square operator variance.
        real(real64) :: mean_square_operator
        !> @brief The mean-square part-by-operator variance.
        real(real64) :: mean_square_part_by_operator
        !> @brief The mean-square repeatability variance.
        real(real64) :: mean_square_repeatability
        !> @brief The mean-square total variance.
        real(real64) :: mean_square_total
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
        !> @brief The total gauge variation.
        real(real64) :: gauge_variation
        !> @brief The total process variation.
        real(real64) :: total_variation
        !> @brief The value of the F-statistic for the part variance term.
        real(real64) :: part_f_ratio
        !> @brief The value of the F-statistic for the operator variance term.
        real(real64) :: operator_f_ratio
        !> @brief The value of the F-statistic for the part-by-operator 
        !! variance term.
        real(real64) :: part_by_operator_f_ratio
        !> @brief The probability of the part variance being a significant contributor.
        real(real64) :: part_probability
        !> @brief The probability of the operator variance being a significant contributor.
        real(real64) :: operator_probability
        !> @brief The probability of the part-by-operator variation
        !! being a signficant contributor.
        real(real64) :: part_by_operator_probability
        !> @brief The tolerance range.
        real(real64) :: tolerance
        !> @brief The sigma multiplier (k).
        real(real64) :: scale
        !> @brief The precision to tolerance ratio (P/T ratio).  This is
        !! computed as: \f$ k * \sigma_{gauge} / tolerance \f$
        real(real64) :: pt_ratio
        !> @brief The precision to process total variation.  This is
        !! computed as: \f$ \frac{\sigma_{meas}}{\sigma_{total}} \f$.
        !! This is basically a measure of what percent of the total 
        !! variation is due to measurement error.
        real(real64) :: ptv_ratio
    end type

    interface
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
        pure module function ssq_part(x, xmean) result(ssq)
            real(real64), intent(in), dimension(:,:,:) :: x
            real(real64), intent(in) :: xmean
            real(real64) :: ssq
        end function

        !> @brief Computes the sum of the squares of the difference between the
        !! mean for each operator, and the overall mean.
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
        pure module function ssq_operator(x, xmean) result(ssq)
            real(real64), intent(in), dimension(:,:,:) :: x
            real(real64), intent(in) :: xmean
            real(real64) :: ssq
        end function

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
        pure module function ssq_repeat(x) result(ssq)
            real(real64), intent(in), dimension(:,:,:) :: x
            real(real64) :: ssq
        end function

        !> @brief Computes the sum of the squares of the difference between each
        !! observation and the overall mean.
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
        !! \f$ s_{part} = n_{op} n_{rep} \sum (x_{ijk} - \overline{x})^{2} \f$
        pure module function ssq_total(x, xmean) result(ssq)
            real(real64), intent(in), dimension(:,:,:) :: x
            real(real64), intent(in) :: xmean
            real(real64) :: ssq
        end function

        !> @brief Computes a Gauge R&R ANOVA crossed study of an experimental data
        !! set obtained using multipler parts, repeated tests, and multiple
        !! test operators.  The expected mean squares (EMS) method is utilized.  If
        !! negative components are found (they are reported as zero values), it
        !! is recommended to utilize a Byesian or restricted maximum likelihood (REML)
        !! approach instead.
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
        !! - https://www.muelaner.com/quality-assurance/gauge-r-and-r-excel/
        !! - https://www.engineering.com/AdvancedManufacturing/ArticleID/16201/Gauge-Studies-and-Gauge-RR.aspx
        !! - https://en.wikipedia.org/wiki/ANOVA_gauge_R%26R
        module function ems_gauge_r_r(x, alpha, tol, k, err) result(rst)
            real(real64), intent(in), dimension(:,:,:) :: x
            real(real64), intent(in), optional :: alpha, tol, k
            class(errors), intent(inout), optional, target :: err
            type(ems_grr_results) :: rst
        end function
    end interface

contains
! ------------------------------------------------------------------------------
    !> @brief Computes the discrimination ratio.
    !!
    !! @param[in] tv The total variance.
    !! @param[in] mv The measurement system variance.
    !!
    !! @return The results of the operation.
    !!
    !! @par 
    !! The discrimination ratio is computed as follows.
    !! @par
    !! /f$ DR = \sqrt{\frac{2 \sigma_{total}^2}{\sigma_{meas}^2} - 1} /f$
    !! @par
    !! An alternate means of computing this parameter (as used in JMP)
    !! is as follows.
    !! @par
    !! /f$ DR = 1.41 \frac{\sigma_{parts}}{\sigma_{meas}} /f$
    pure function discrimination_ratio(tv, mv) result(x)
        ! Arguments
        real(real64), intent(in) :: tv, mv
        real(real64) :: x
        x = sqrt(2.0d0 * (tv / mv) - 1.0d0)
    end function

! ------------------------------------------------------------------------------
end module
