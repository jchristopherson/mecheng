! measurements.f90

!> @brief Provides a collection of routines for operating on 
!! experimental measurement data.
module measurements
    use iso_fortran_env
    use curvefit_statistics
    use ferror
    use fplot_core
    implicit none
    private
    public :: MS_INVALID_INPUT_ERROR
    public :: MS_INVALID_DATA_SET_ERROR
    public :: ems_grr_results
    public :: emp_grr_results
    public :: ssq_part
    public :: ssq_operator
    public :: ssq_repeat
    public :: ems_gauge_r_r
    public :: emp_gauge_r_r
    public :: discrimination_ratio
    public :: plot_grr

    !> @brief Defines an invalid input error condition.
    integer(int32), parameter :: MS_INVALID_INPUT_ERROR = 100001
    !> @brief Defines an invalid data set error condition.
    integer(int32), parameter :: MS_INVALID_DATA_SET_ERROR = 100002
    !> @brief Defines an out-of-memory error condition.
    integer(int32), parameter :: MS_OUT_OF_MEMORY_ERROR = 100003

    !> @brief Provides a container for expected mean squares (EMS) type
    !! GR&R results.
    !!
    !! @par Example
    !! The following example compares results from an EMS and EMP analysis.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use measurements
    !!     implicit none
    !!
    !!     ! Parameters
    !!     real(real64), parameter :: tol = 1.0d3
    !!
    !!     ! Variables
    !!     real(real64), allocatable, dimension(:,:,:) :: x
    !!     type(ems_grr_results) :: ems
    !!     type(emp_grr_results) :: emp
    !!
    !!     ! Populate the data set
    !!     x = format_data()
    !!
    !!     ! Compute the GR&R using the EMS approach
    !!     ems = ems_gauge_r_r(x, tol = tol)
    !!
    !!     ! Compute the GR&R using the EMP approach
    !!     emp = emp_gauge_r_r(x, tol = tol)
    !!
    !!     ! Compare results
    !!     print '(A)', "EMS Method"
    !!     print '(AE10.3AF5.2A)', "Repeatability: ", ems%repeatability, &
    !!         " (", 1.0d2 * ems%repeatability / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Reproducibility: ", ems%reproducibility, &
    !!         " (", 1.0d2 * ems%reproducibility / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Gauge Variance: ", ems%gauge_variation, &
    !!         " (", 1.0d2 * ems%gauge_variation / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Part-Part Variance: ", ems%part_variation, &
    !!         " (", 1.0d2 * ems%part_variation / ems%total_variation, "%)"
    !!     print '(AE10.3)', "Total Variance: ", ems%total_variation
    !!     print '(AF5.3)', "P/T Ratio: ", ems%pt_ratio
    !!     print '(AF5.3)', "P/TV Ratio: ", ems%ptv_ratio
    !!
    !!     print '(A)', new_line('a') // "EMP Method"
    !!     print '(AE10.3AF5.2A)', "Repeatability: ", emp%repeatability, &
    !!         " (", 1.0d2 * emp%repeatability / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Reproducibility: ", emp%reproducibility, &
    !!         " (", 1.0d2 * emp%reproducibility / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Gauge Variance: ", emp%gauge_variation, &
    !!         " (", 1.0d2 * emp%gauge_variation / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Part-Part Variance: ", emp%part_variation, &
    !!         " (", 1.0d2 * emp%part_variation / emp%total_variation, "%)"
    !!     print '(AE10.3)', "Total Variance: ", emp%total_variation
    !!     print '(AF5.3)', "P/T Ratio: ", emp%pt_ratio
    !!     print '(AF5.3)', "P/TV Ratio: ", emp%ptv_ratio
    !!
    !!     contains
    !!     ! The data set is # of parts -by- # of tests -by- # of operators
    !!     function format_data() result(x)
    !!         ! Arguments
    !!         real(real64), allocatable, dimension(:,:,:) :: x
    !!
    !!         ! Parameters
    !!         integer(int32), parameter :: nparts = 9
    !!         integer(int32), parameter :: nops = 2
    !!         integer(int32), parameter :: ntests = 3
    !!
    !!         ! Local Variables
    !!         real(real64), allocatable, dimension(:) :: op
    !!
    !!         ! Allocate space for the output
    !!         allocate(x(nparts, ntests, nops))
    !!         allocate(op(nparts * ntests))
    !!
    !!         ! Fill in the data for operator 1
    !!         op = [39736.0d0, 40088.0d0, 39952.0d0, 39840.0d0, &
    !!             39792.0d0, 39968.0d0, 39384.0d0, 39648.0d0, &
    !!             39704.0d0, 39152.0d0, 39240.0d0, 39312.0d0, &
    !!             38832.0d0, 38688.0d0, 38704.0d0, 38936.0d0, &
    !!             38872.0d0, 38928.0d0, 39040.0d0, 38952.0d0, &
    !!             38968.0d0, 39128.0d0, 38928.0d0, 38928.0d0, &
    !!             38712.0d0, 38688.0d0, 38624.0d0]
    !!         x(:,:,1) = transpose(reshape(op, [ntests, nparts]))
    !!
    !!         ! Fill in the data for operator 2
    !!         op = [40256.0d0, 39864.0d0, 39792.0d0, 39816.0d0, &
    !!             39888.0d0, 39784.0d0, 39760.0d0, 39944.0d0, &
    !!             39832.0d0, 39272.0d0, 39288.0d0, 39296.0d0, &
    !!             38728.0d0, 38768.0d0, 38704.0d0, 38896.0d0, &
    !!             38952.0d0, 38968.0d0, 39032.0d0, 38968.0d0, &
    !!             38992.0d0, 39032.0d0, 39008.0d0, 38968.0d0, &
    !!             38592.0d0, 38640.0d0, 38584.0d0]
    !!         x(:,:,2) = transpose(reshape(op, [ntests, nparts]))
    !!     end function
    !! end program
    !! @endcode
    !! The output of the above program is as follows.
    !! @code{.txt}
    !! EMS Method
    !! Repeatability:  0.999E+04 ( 4.00%)
    !! Reproducibility:  0.127E+04 ( 0.51%)
    !! Gauge Variance:  0.113E+05 ( 4.52%)
    !! Part-Part Variance:  0.238E+06 (95.48%)
    !! Total Variance:  0.250E+06
    !! P/T Ratio: 0.637
    !! P/TV Ratio: 0.045
    !!
    !! EMP Method
    !! Repeatability:  0.778E+04 ( 3.84%)
    !! Reproducibility:  0.835E+03 ( 0.41%)
    !! Gauge Variance:  0.862E+04 ( 4.25%)
    !! Part-Part Variance:  0.194E+06 (95.75%)
    !! Total Variance:  0.203E+06
    !! P/T Ratio: 0.557
    !! P/TV Ratio: 0.043
    !! @endcode
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

    
    !> @brief Provides a container for the "Evaluating the Measurement
    !! Process" (EMP) type GR&R results.
    !!
    !! @par Example
    !! The following example compares results from an EMS and EMP analysis.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use measurements
    !!     implicit none
    !!
    !!     ! Parameters
    !!     real(real64), parameter :: tol = 1.0d3
    !!
    !!     ! Variables
    !!     real(real64), allocatable, dimension(:,:,:) :: x
    !!     type(ems_grr_results) :: ems
    !!     type(emp_grr_results) :: emp
    !!
    !!     ! Populate the data set
    !!     x = format_data()
    !!
    !!     ! Compute the GR&R using the EMS approach
    !!     ems = ems_gauge_r_r(x, tol = tol)
    !!
    !!     ! Compute the GR&R using the EMP approach
    !!     emp = emp_gauge_r_r(x, tol = tol)
    !!
    !!     ! Compare results
    !!     print '(A)', "EMS Method"
    !!     print '(AE10.3AF5.2A)', "Repeatability: ", ems%repeatability, &
    !!         " (", 1.0d2 * ems%repeatability / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Reproducibility: ", ems%reproducibility, &
    !!         " (", 1.0d2 * ems%reproducibility / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Gauge Variance: ", ems%gauge_variation, &
    !!         " (", 1.0d2 * ems%gauge_variation / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Part-Part Variance: ", ems%part_variation, &
    !!         " (", 1.0d2 * ems%part_variation / ems%total_variation, "%)"
    !!     print '(AE10.3)', "Total Variance: ", ems%total_variation
    !!     print '(AF5.3)', "P/T Ratio: ", ems%pt_ratio
    !!     print '(AF5.3)', "P/TV Ratio: ", ems%ptv_ratio
    !!
    !!     print '(A)', new_line('a') // "EMP Method"
    !!     print '(AE10.3AF5.2A)', "Repeatability: ", emp%repeatability, &
    !!         " (", 1.0d2 * emp%repeatability / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Reproducibility: ", emp%reproducibility, &
    !!         " (", 1.0d2 * emp%reproducibility / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Gauge Variance: ", emp%gauge_variation, &
    !!         " (", 1.0d2 * emp%gauge_variation / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Part-Part Variance: ", emp%part_variation, &
    !!         " (", 1.0d2 * emp%part_variation / emp%total_variation, "%)"
    !!     print '(AE10.3)', "Total Variance: ", emp%total_variation
    !!     print '(AF5.3)', "P/T Ratio: ", emp%pt_ratio
    !!     print '(AF5.3)', "P/TV Ratio: ", emp%ptv_ratio
    !!
    !!     contains
    !!     ! The data set is # of parts -by- # of tests -by- # of operators
    !!     function format_data() result(x)
    !!         ! Arguments
    !!         real(real64), allocatable, dimension(:,:,:) :: x
    !!
    !!         ! Parameters
    !!         integer(int32), parameter :: nparts = 9
    !!         integer(int32), parameter :: nops = 2
    !!         integer(int32), parameter :: ntests = 3
    !!
    !!         ! Local Variables
    !!         real(real64), allocatable, dimension(:) :: op
    !!
    !!         ! Allocate space for the output
    !!         allocate(x(nparts, ntests, nops))
    !!         allocate(op(nparts * ntests))
    !!
    !!         ! Fill in the data for operator 1
    !!         op = [39736.0d0, 40088.0d0, 39952.0d0, 39840.0d0, &
    !!             39792.0d0, 39968.0d0, 39384.0d0, 39648.0d0, &
    !!             39704.0d0, 39152.0d0, 39240.0d0, 39312.0d0, &
    !!             38832.0d0, 38688.0d0, 38704.0d0, 38936.0d0, &
    !!             38872.0d0, 38928.0d0, 39040.0d0, 38952.0d0, &
    !!             38968.0d0, 39128.0d0, 38928.0d0, 38928.0d0, &
    !!             38712.0d0, 38688.0d0, 38624.0d0]
    !!         x(:,:,1) = transpose(reshape(op, [ntests, nparts]))
    !!
    !!         ! Fill in the data for operator 2
    !!         op = [40256.0d0, 39864.0d0, 39792.0d0, 39816.0d0, &
    !!             39888.0d0, 39784.0d0, 39760.0d0, 39944.0d0, &
    !!             39832.0d0, 39272.0d0, 39288.0d0, 39296.0d0, &
    !!             38728.0d0, 38768.0d0, 38704.0d0, 38896.0d0, &
    !!             38952.0d0, 38968.0d0, 39032.0d0, 38968.0d0, &
    !!             38992.0d0, 39032.0d0, 39008.0d0, 38968.0d0, &
    !!             38592.0d0, 38640.0d0, 38584.0d0]
    !!         x(:,:,2) = transpose(reshape(op, [ntests, nparts]))
    !!     end function
    !! end program
    !! @endcode
    !! The output of the above program is as follows.
    !! @code{.txt}
    !! EMS Method
    !! Repeatability:  0.999E+04 ( 4.00%)
    !! Reproducibility:  0.127E+04 ( 0.51%)
    !! Gauge Variance:  0.113E+05 ( 4.52%)
    !! Part-Part Variance:  0.238E+06 (95.48%)
    !! Total Variance:  0.250E+06
    !! P/T Ratio: 0.637
    !! P/TV Ratio: 0.045
    !!
    !! EMP Method
    !! Repeatability:  0.778E+04 ( 3.84%)
    !! Reproducibility:  0.835E+03 ( 0.41%)
    !! Gauge Variance:  0.862E+04 ( 4.25%)
    !! Part-Part Variance:  0.194E+06 (95.75%)
    !! Total Variance:  0.203E+06
    !! P/T Ratio: 0.557
    !! P/TV Ratio: 0.043
    !! @endcode
    type emp_grr_results
        !> @brief The mean of each operator.
        real(real64), allocatable, dimension(:) :: operator_means
        !> @brief The mean of each part.
        real(real64), allocatable, dimension(:) :: part_means
        !> @brief The mean of each test sequence.
        real(real64), allocatable, dimension(:) :: repeat_means
        !> @brief The range of the average of the each operator-part combination.
        real(real64) :: average_range
        !> @brief The range of operator averages.
        real(real64) :: operator_range
        !> @brief The range of part averages.
        real(real64) :: part_range
        !> @brief The repeatability variation.
        real(real64) :: repeatability
        !> @brief The reproducibility variation.
        real(real64) :: reproducibility
        !> @brief The part-part variation.
        real(real64) :: part_variation
        !> @brief The gauge variation.
        real(real64) :: gauge_variation
        !> @brief The total variation.
        real(real64) :: total_variation
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
        !! is recommended to utilize an alternative approach.
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
        !! @return An ems_grr_results object containing the calculation results.
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

    interface
        !> @brief Computes a Gauge R&R crossed study of an experimental data
        !! set obtained using multipler parts, repeated tests, and multiple
        !! test operators.  The "Evaluating the Measurement Process" (EMP) 
        !! method is utilized.
        !!
        !! @param[in] x A M-by-N-by-P array containing the data where M is the
        !!  number of parts, N is the number of tests, and P is the number of
        !!  operators.
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
        !!  - MS_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory available.
        !!
        !! @return An emp_grr_results object containing the calculation results.
        !!
        !! @par References:
        !! - https://www.spcforexcel.com/knowledge/measurement-systems-analysis/three-methods-analyze-gage-rr-studies
        !! - https://www.qualitydigest.com/inside/twitter-ed/problems-gauge-rr-studies.html
        module function emp_gauge_r_r(x, tol, kf, err) result(rst)
            real(real64), intent(in), dimension(:,:,:) :: x
            real(real64), intent(in), optional :: tol, kf
            class(errors), intent(inout), optional, target :: err
            type(emp_grr_results) :: rst
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
    !> @brief Creates a GR&R plot.
    !!
    !! @param[in] x The M-by-N-by-P GR&R data matrix where M is the number of
    !!  parts, N is the number of tests, and P is the number of operators.
    !! @param[in,out] plt The plot object to which the data will be added.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - MS_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory available.
    !!
    !! @par Example
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use measurements
    !!     use fplot_core
    !!     implicit none
    !!
    !!     ! Parameters
    !!     real(real64), parameter :: tol = 1.0d3
    !!
    !!     ! Variables
    !!     real(real64), allocatable, dimension(:,:,:) :: x
    !!     type(ems_grr_results) :: ems
    !!     type(emp_grr_results) :: emp
    !!     type(plot_2d) :: plt
    !!     class(plot_axis), pointer :: xAxis, yAxis
    !!     class(legend), pointer :: lgnd
    !!
    !!     ! Populate the data set
    !!     x = format_data()
    !!
    !!     ! Compute the GR&R using the EMS approach
    !!     ems = ems_gauge_r_r(x, tol = tol)
    !!
    !!     ! Compute the GR&R using the EMP approach
    !!     emp = emp_gauge_r_r(x, tol = tol)
    !!
    !!     ! Compare results
    !!     print '(A)', "EMS Method"
    !!     print '(AE10.3AF5.2A)', "Repeatability: ", ems%repeatability, &
    !!         " (", 1.0d2 * ems%repeatability / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Reproducibility: ", ems%reproducibility, &
    !!         " (", 1.0d2 * ems%reproducibility / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Gauge Variance: ", ems%gauge_variation, &
    !!         " (", 1.0d2 * ems%gauge_variation / ems%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Part-Part Variance: ", ems%part_variation, &
    !!         " (", 1.0d2 * ems%part_variation / ems%total_variation, "%)"
    !!     print '(AE10.3)', "Total Variance: ", ems%total_variation
    !!     print '(AF5.3)', "P/T Ratio: ", ems%pt_ratio
    !!     print '(AF5.3)', "P/TV Ratio: ", ems%ptv_ratio
    !!
    !!     print '(A)', new_line('a') // "EMP Method"
    !!     print '(AE10.3AF5.2A)', "Repeatability: ", emp%repeatability, &
    !!         " (", 1.0d2 * emp%repeatability / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Reproducibility: ", emp%reproducibility, &
    !!         " (", 1.0d2 * emp%reproducibility / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Gauge Variance: ", emp%gauge_variation, &
    !!         " (", 1.0d2 * emp%gauge_variation / emp%total_variation, "%)"
    !!     print '(AE10.3AF5.2A)', "Part-Part Variance: ", emp%part_variation, &
    !!         " (", 1.0d2 * emp%part_variation / emp%total_variation, "%)"
    !!     print '(AE10.3)', "Total Variance: ", emp%total_variation
    !!     print '(AF5.3)', "P/T Ratio: ", emp%pt_ratio
    !!     print '(AF5.3)', "P/TV Ratio: ", emp%ptv_ratio
    !!
    !!     ! Create a plot of the data
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!     call plt%set_show_gridlines(.false.)
    !!
    !!     xAxis => plt%get_x_axis()
    !!     yAxis => plt%get_y_axis()
    !!
    !!     call xAxis%set_title("Part No.")
    !!     call yAxis%set_title("Measured")
    !!
    !!     lgnd => plt%get_legend()
    !!     call lgnd%set_is_visible(.true.)
    !!
    !!     call plot_grr(x, plt)
    !!
    !!     ! Show the plot
    !!     call plt%draw()
    !!
    !! contains
    !!     ! The data set is # of parts -by- # of tests -by- # of operators
    !!     function format_data() result(x)
    !!         ! Arguments
    !!         real(real64), allocatable, dimension(:,:,:) :: x
    !!
    !!         ! Parameters
    !!         integer(int32), parameter :: nparts = 9
    !!         integer(int32), parameter :: nops = 2
    !!         integer(int32), parameter :: ntests = 3
    !!
    !!         ! Local Variables
    !!         real(real64), allocatable, dimension(:) :: op
    !!
    !!         ! Allocate space for the output
    !!         allocate(x(nparts, ntests, nops))
    !!         allocate(op(nparts * ntests))
    !!
    !!         ! Fill in the data for operator 1
    !!         op = [39736.0d0, 40088.0d0, 39952.0d0, 39840.0d0, &
    !!             39792.0d0, 39968.0d0, 39384.0d0, 39648.0d0, &
    !!             39704.0d0, 39152.0d0, 39240.0d0, 39312.0d0, &
    !!             38832.0d0, 38688.0d0, 38704.0d0, 38936.0d0, &
    !!             38872.0d0, 38928.0d0, 39040.0d0, 38952.0d0, &
    !!             38968.0d0, 39128.0d0, 38928.0d0, 38928.0d0, &
    !!             38712.0d0, 38688.0d0, 38624.0d0]
    !!         x(:,:,1) = transpose(reshape(op, [ntests, nparts]))
    !!
    !!         ! Fill in the data for operator 2
    !!         op = [40256.0d0, 39864.0d0, 39792.0d0, 39816.0d0, &
    !!             39888.0d0, 39784.0d0, 39760.0d0, 39944.0d0, &
    !!             39832.0d0, 39272.0d0, 39288.0d0, 39296.0d0, &
    !!             38728.0d0, 38768.0d0, 38704.0d0, 38896.0d0, &
    !!             38952.0d0, 38968.0d0, 39032.0d0, 38968.0d0, &
    !!             38992.0d0, 39032.0d0, 39008.0d0, 38968.0d0, &
    !!             38592.0d0, 38640.0d0, 38584.0d0]
    !!         x(:,:,2) = transpose(reshape(op, [ntests, nparts]))
    !!     end function
    !! end program
    !! @endcode
    !! The above code produces the following output.
    !! @code{.txt}
    !! EMS Method
    !! Repeatability:  0.999E+04 ( 4.00%)
    !! Reproducibility:  0.127E+04 ( 0.51%)
    !! Gauge Variance:  0.113E+05 ( 4.52%)
    !! Part-Part Variance:  0.238E+06 (95.48%)
    !! Total Variance:  0.250E+06
    !! P/T Ratio: 0.637
    !! P/TV Ratio: 0.045
    !!
    !! EMP Method
    !! Repeatability:  0.778E+04 ( 3.84%)
    !! Reproducibility:  0.835E+03 ( 0.41%)
    !! Gauge Variance:  0.862E+04 ( 4.25%)
    !! Part-Part Variance:  0.194E+06 (95.75%)
    !! Total Variance:  0.203E+06
    !! P/T Ratio: 0.557
    !! P/TV Ratio: 0.043
    !! @endcode
    !! @image html grr_plot_example.png
    subroutine plot_grr(x, plt, err)
        use curvefit_statistics
        use strings

        ! Arguments
        real(real64), intent(in), dimension(:,:,:) :: x
        class(plot), intent(inout) :: plt
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(plot_data_2d) :: pdata
        type(plot_data_error_bars) :: edata
        integer(int32) :: i, k, nparts, nops, flag
        real(real64), allocatable, dimension(:) :: xd, y, ymin, ymax
        type(color) :: clr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        nparts = size(x, 1)
        nops = size(x, 3)
        allocate(xd(nparts), stat = flag)
        if (flag == 0) allocate(y(nparts), stat = flag)
        if (flag == 0) allocate(ymin(nparts), stat = flag)
        if (flag == 0) allocate(ymax(nparts), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("plot_grr", &
                "Insufficient memory available.", MS_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Establish an x-axis
        do i = 1, nparts
            xd(i) = real(i)
        end do

        ! Cycle over each operator, and generate the plot data
        do k = 1, nops
            ! Compute the mean of each parts set, and determine the range
            do i = 1, nparts
                y(i) = mean(x(i,:,k))
                ! yerr(i) = 0.5d0 * (maxval(x(i,:,k)) - minval(x(i,:,k)))
                ymin(i) = minval(x(i,:,k))
                ymax(i) = maxval(x(i,:,k))
            end do

            ! Define the plot data
            call pdata%define_data(xd, y)
            call pdata%set_line_style(LINE_DOTTED)
            call plt%push(pdata)
            clr = pdata%get_line_color()

            call edata%define_y_error_data(xd, y, ymin, ymax)
            call edata%set_name("Operator " // to_string(k))
            call edata%set_line_color(clr)
            call plt%push(edata)

            !  Notice, the above code works as a copy of each data object is
            ! stored and managed by the plot
        end do
    end subroutine

! ------------------------------------------------------------------------------
end module
