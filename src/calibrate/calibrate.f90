! calibrate.f90

!> @brief A collection of types and routines supporting calibration operations.
module calibrate
    use iso_fortran_env
    use ferror
    use nonlin_polynomials
    implicit none

    !> @brief Defines an out-of-memory error condition.
    integer(int32), parameter :: CAL_OUT_OF_MEMORY_ERROR = 20000
    !> @brief Defines an invalid input argument error condition.
    integer(int32), parameter :: CAL_INVALID_INPUT_ERROR = 20001

    !> @brief A type representing the environmental conditions during the 
    !! calibration.
    type environment
        !> @brief The temperature.
        real(real64) :: temperature
        !> @brief The relative humidity.
        real(real64) :: humidity
    end type

    !> @brief Represents a calibration operation.
    type calibration
        ! -------------------- !
        !> The calibration data - 2 column matrix where the first column
        !! contains the output from the reference standard, and the second 
        !! column contains the UUT output.
        real(real64), private, allocatable, dimension(:,:) :: m_data
        !> @brief The calibration polynomial.
        type(polynomial), private :: m_poly

        ! -------------------- !
        !> @brief The environmental conditions.
        type(environment), public :: conditions
        !> @brief The operator.
        character(len = :), public, allocatable :: operator
        !> @brief Any operator comments or notes.
        character(len = :), public, allocatable :: notes
        !> @brief The one-based index defining the index of the calibration
        !! point considered to be the zero point.
        integer(int32), public :: zero_index

    contains
        ! evaluate polynomial
        ! compute calibration errors
        ! get polynomial coefficients
        procedure, public :: initialize => cal_init
        procedure, public :: get_capacity => cal_get_capacity
        procedure, public :: set_capacity => cal_set_capacity
        procedure, public :: get_count => cal_get_count
        procedure, public :: get => cal_get_data_point
        generic, public :: set => cal_set_data_point, cal_set_data_point_args
        generic, public :: append => cal_add_data_point, cal_add_data_point_args
        procedure, public :: remove_last => cal_remove_last_point
        procedure, public :: fit_polynomial => cal_fit_poly
        
        procedure ::cal_set_data_point
        procedure :: cal_set_data_point_args
        procedure :: cal_add_data_point
        procedure :: cal_add_data_point_args
    end type

    interface
        !> @brief Initializes a new instance of the calibration class.
        !!
        !! @param[in] this The calibration instance.
        module subroutine cal_init(this)
            class(calibration), intent(inout) :: this
        end subroutine

        !> @brief Gets the capacity of the calibration instance to accept
        !! additional data points.
        !!
        !! @param[in] this The calibration instance.
        !! @return The capacity.
        pure module function cal_get_capacity(this) result(n)
            class(calibration), intent(in) :: this
            integer(int32) :: n
        end function

        !> @brief Sets the capacity of the calibration instance to accept
        !! additional data points.
        !!
        !! @param[in,out] this The calibration instance.
        !! @param[in] n The desired capacity.  This number must be a positive,
        !!  non-zero integer.
        !! @param[in,out] err An optional parameter that is used to track the
        !!  error status of the routine.  The following error codes are
        !!  possible.
        !!  - CAL_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!  - CAL_INVALID_INPUT_ERROR: Occurs if @p n is not positive and 
        !!      non-zero.
        module subroutine cal_set_capacity(this, n, err)
            class(calibration), intent(inout) :: this
            integer(int32), intent(in) :: n
            class(errors), intent(inout), optional, target :: err
        end subroutine

        !> @brief Gets the current number of data points stored in this object.
        !!
        !! @param[in] this The calibration instance.
        !! @return The number of stored data points.
        pure module function cal_get_count(this) result(n)
            class(calibration), intent(in) :: this
            integer(int32) :: n
        end function

        !> @brief Gets the requested data point.
        !!
        !! @param[in] this The calibration instance.
        !! @param[in] ind The one-based index of the point to retrieve.
        !! @return A two-element array containing the requested data point.
        !!  The first entry in the array contains the reference standard
        !!  data point, and the second entry in the array contains the UUT
        !!  data point.
        pure module function cal_get_data_point(this, ind) result(x)
            class(calibration), intent(in) :: this
            integer(int32), intent(in) :: ind
            real(real64) :: x(2)
        end function

        !> @brief Replaces the specified data point.
        !!
        !! @param[in,out] this The calibration instance.
        !! @param[in] ind The one-based index of the point to replace.
        !! @param[in] x A two-element array containing the data point.  The
        !!  first entry in the array contains the reference standard
        !!  data point, and the second entry in the array contains the UUT
        !!  data point.
        module subroutine cal_set_data_point(this, ind, x)
            class(calibration), intent(inout) :: this
            integer(int32), intent(in) :: ind
            real(real64), intent(in) :: x(2)
        end subroutine

        !> @brief Replaces the specified data point.
        !!
        !! @param[in,out] this The calibration instance.
        !! @param[in] ind The one-based index of the point to replace.
        !! @param[in] std The reference standard value.
        !! @param[in] uut The unit-under-test value.
        module subroutine cal_set_data_point_args(this, ind, std, uut)
            class(calibration), intent(inout) :: this
            integer(int32), intent(in) :: ind
            real(real64), intent(in) :: std, uut
        end subroutine

        !> @brief Appends a new data point onto the end of the calibration
        !! data collection.
        !!
        !! @param[in,out] this The calibration instance.
        !! @param[in] x A two-element array containing the data point.  The
        !!  first entry in the array contains the reference standard
        !!  data point, and the second entry in the array contains the UUT
        !!  data point.
        !! @param[in,out] err An optional parameter that is used to track the
        !!  error status of the routine.  The following error codes are
        !!  possible.
        !!  - CAL_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        module subroutine cal_add_data_point(this, x, err)
            class(calibration), intent(inout) :: this
            real(real64), intent(in) :: x(2)
            class(errors), intent(inout), optional, target :: err
        end subroutine

        !> @brief Appends a new data point onto the end of the calibration
        !! data collection.
        !!
        !! @param[in,out] this The calibration instance.
        !! @param[in] std The reference standard value.
        !! @param[in] uut The unit-under-test value.
        !! @param[in,out] err An optional parameter that is used to track the
        !!  error status of the routine.  The following error codes are
        !!  possible.
        !!  - CAL_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        module subroutine cal_add_data_point_args(this, std, uut, err)
            class(calibration), intent(inout) :: this
            real(real64), intent(in) :: std, uut
            class(errors), intent(inout), optional, target :: err
        end subroutine

        !> @brief Removes the last data point from the calibration data
        !! collection.
        !!
        !! @param[in,out] this The calibration instance.
        module subroutine cal_remove_last_point(this)
            class(calibration), intent(inout) :: this
        end subroutine

        !> @brief Fits a polynomial to the current data set.
        !!
        !! @param[in,out] this The calibration instance.
        !! @param[in] order The order of polynomial to fit.  This value must
        !!  at least one less than the number of stored data points, and be at
        !!  least one.
        !! @param[in,out] err An optional parameter that is used to track the
        !!  error status of the routine.  The following error codes are
        !!  possible.
        !!  - CAL_INVALID_INPUT_ERROR: Occurs if @p order is too large, or less
        !!      than one.
        module subroutine cal_fit_poly(this, order, err)
            class(calibration), intent(inout) :: this
            integer(int32), intent(in)  :: order
            class(errors), intent(inout), optional, target :: err
        end subroutine
    end interface
end module
