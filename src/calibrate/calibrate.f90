! calibrate.f90

!> @brief A collection of types and routines supporting calibration operations.
module calibrate
    use iso_fortran_env
    use nonlin_polynomials
    implicit none

    !> @brief A type representing a measurement instrument.
    type instrument
        !> @brief A description of the instrument
        character(len = :), allocatable :: description
        !> @brief the unit of measure for the instrument.
        character(len = :), allocatable :: unit_type
        !> @brief The output measurement of the instrument.
        real(real64) :: measurement
    end type

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
        !! contains the output from the UUT, and the second column contains
        !! the reference standard information.
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
        ! add_point
        ! remove_last_point
        ! get/set a calibration data point
        ! get the number of calibration points
        ! fit polynomial
        ! evaluate polynomial
        ! compute calibration errors
        ! get polynomial coefficients
    end type

    interface
        !> @brief Initializes a new instance of the calibration class.
        !!
        !! @param[in] this The calibration instance.
        module subroutine cal_init(this)
            class(calibration), intent(inout) :: this
        end subroutine
    end interface
end module
