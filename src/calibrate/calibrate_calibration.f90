! calibrate_calibration.f90

submodule (calibrate) calibrate_calibration
contains
! ------------------------------------------------------------------------------
!> @brief Initializes a new instance of the calibration class.
!!
!! @param[in] this The calibration instance.
module subroutine cal_init(this)
    class(calibration), intent(inout) :: this
end subroutine

! ------------------------------------------------------------------------------
!> @brief Gets the capacity of the calibration instance to accept
!! additional data points.
!!
!! @param[in] this The calibration instance.
!! @return The capacity.
pure module function cal_get_capacity(this) result(n)
    class(calibration), intent(in) :: this
    integer(int32) :: n
end function

! ------------------------------------------------------------------------------
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

! ------------------------------------------------------------------------------
!> @brief Gets the current number of data points stored in this object.
!!
!! @param[in] this The calibration instance.
!! @return The number of stored data points.
pure module function cal_get_count(this) result(n)
    class(calibration), intent(in) :: this
    integer(int32) :: n
end function

! ------------------------------------------------------------------------------
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

! ------------------------------------------------------------------------------
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

! ------------------------------------------------------------------------------
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

! ------------------------------------------------------------------------------
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

! ------------------------------------------------------------------------------
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

! ------------------------------------------------------------------------------
!> @brief Removes the last data point from the calibration data
!! collection.
!!
!! @param[in,out] this The calibration instance.
module subroutine cal_remove_last_point(this)
    class(calibration), intent(inout) :: this
end subroutine

! ------------------------------------------------------------------------------
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

! ------------------------------------------------------------------------------
!> @brief Evaluates the calibration polynomial at the points specified.
!!
!! @param[in] this The calibration instance.
!! @param[in] x The point(s) at which to evaluate the calibration 
!!  polynomial.
!! @return The value(s) of the calibration polynomial as evaluated
!!  at @p x.
pure elemental module function cal_eval_poly(this, x) result(y)
    class(calibration), intent(in) :: this
    real(real64), intent(in) :: x
    real(real64) :: y
end function

! ------------------------------------------------------------------------------
!> @brief Evaluates the calibration polynomial at the stored 
!! calibraiton points.
!!
!! @param[in] this The calibration instance.
!! @return An array containing the value of the calibration polynomial
!!  at each of the stored calibration points.
pure module function cal_eval_poly_at_cal_points(this) result(y)
    class(calibration), intent(in) :: this
    real(real64), allocatable, dimension(:) :: y
end function

! ------------------------------------------------------------------------------
!> @brief Computes the errors in the calibration at each calibration
!! point.
!!
!! @param[in] this The calibration instance.
!! @return An array containing the difference between each calibration
!!  point, and the corresponding reference standard value.
pure module function cal_compute_err(this) result(y)
    class(calibration), intent(in) :: this
    real(real64), allocatable, dimension(:) :: y
end function

! ------------------------------------------------------------------------------
!> @brief Returns the calibration polynomial coefficients in ascending
!! order.
!!
!! @param[in] this The calibration instance.
!! @return An array containing the calibration polynomial coefficients
!!  in ascending order.
pure module function cal_get_coeff(this) result(c)
    class(calibration), intent(in) :: this
    real(real64), allocatable, dimension(:) :: c
end function

! ------------------------------------------------------------------------------
end submodule
