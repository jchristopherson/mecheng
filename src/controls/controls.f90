! controls.f90

! References:
! http://bestune.50megs.com/typeABC.htm
! https://github.com/Hendryputra/PID/blob/master/main.c

module controls
    use iso_fortran_env
    implicit none
    private

    !> @breif Defines a basic PID controller.
    type pid
    private
        !> @brief The proportional gain.
        real(real64), public :: proportional_gain
        !> @brief The integral gain.
        real(real64), public :: integral_gain
        !> @brief The derivative gain.
        real(real64), public :: derivative_gain

        ! PRIVATE VARIABLES
        logical, private :: m_useFilter = .true.
        real(real64), private :: m_upperLimit = 1.0d3
        real(real64), private :: m_lowerLimit = -1.0d3
        real(real64), private :: m_setpoint = 0.0d0
        real(real64), private :: m_updateTime = 1.0d0
        real(real64), private :: m_previousTime = 0.0d0
        real(real64), private :: m_previousSignal = 0.0d0
        real(real64), private :: m_previousDerivative = 0.0d0
        real(real64), private :: m_previousOutput = 0.0d0
        real(real64), private :: m_filterTimeConstant = 1.0d-3
        logical :: m_reset = .true.

    contains
        procedure, public :: get_use_filter => pid_get_use_filter
        procedure, public :: set_use_filter => pid_set_use_filter
        procedure, public :: get_lower_limit => pid_get_lower_limit
        procedure, public :: set_lower_limit => pid_set_lower_limit
        procedure, public :: get_upper_limit => pid_get_upper_limit
        procedure, public :: set_upper_limit => pid_set_upper_limit
        procedure, public :: get_update_time => pid_get_update_time
        procedure, public :: set_update_time => pid_set_update_time
        procedure, public :: reset => pid_reset
        procedure, public :: get_filter_time_constant => pid_get_filter_time_constant
        procedure, public :: set_filter_time_constant => pid_set_filter_time_constant
        procedure, public :: filter => pid_filter
        procedure, public :: evaluate => pid_eval_1
    end type

contains
! ------------------------------------------------------------------------------
    !> @brief Gets a flag determining if the filter should be applied
    !! when computing the controller output.  The default is true.
    !!
    !! @param[in] this The pid object.
    !! @return Returns true if the filter should be used; else, false.
    pure function pid_get_use_filter(this) result(x)
        class(pid), intent(in) :: this
        logical :: x
        x = this%m_useFilter
    end function

    !> @brief Sets a flag determining if the filter should be applied
    !! when computing the controller output.
    !!
    !! @param[in,out] this The pid object.
    !! @param[in] x True if the filter should be used; else, false.
    subroutine pid_set_use_filter(this, x)
        class(pid), intent(inout) :: this
        logical, intent(in) :: x
        this%m_useFilter = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the lower limit on the controller output.
    !!
    !! @param[in] x The pid object.
    !! @return The limit value.
    pure function pid_get_lower_limit(this) result(x)
        class(pid), intent(in) :: this
        real(real64) :: x
        x = this%m_lowerLimit
    end function

    !> @brief Sets the lower limit on the controller output.
    !!
    !! @param[in,out] this The pid object.
    !! @param[in] x The limit value.
    subroutine pid_set_lower_limit(this, x)
        class(pid), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_lowerLimit = x
    end subroutine


    !> @brief Gets the upper limit on the controller output.
    !!
    !! @param[in] x The pid object.
    !! @return The limit value.
    pure function pid_get_upper_limit(this) result(x)
        class(pid), intent(in) :: this
        real(real64) :: x
        x = this%m_upperLimit
    end function

    !> @brief Sets the upper limit on the controller output.
    !!
    !! @param[in,out] this The pid object.
    !! @param[in] x The limit value.
    subroutine pid_set_upper_limit(this, x)
        class(pid), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_upperLimit = x
    end subroutine

! ------------------------------------------------------------------------------
    !
    pure function pid_get_update_time(this) result(x)
        class(pid), intent(in) :: this
        real(real64) :: x
        x = this%m_updateTime
    end function

    subroutine pid_set_update_time(this, x)
        class(pid), intent(inout) :: this
        real(real64), intent(in) :: x
    end subroutine

! ------------------------------------------------------------------------------
    !
    pure function pid_get_filter_time_constant(this) result(x)
        class(pid), intent(in) :: this
        real(real64) :: x
        x = this%m_filterTimeConstant
    end function

    subroutine pid_set_filter_time_constant(this, x)
        class(pid), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_filterTimeConstant = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Resets the controller.
    !!
    !! @param[in,out] this The pid object.
    subroutine pid_reset(this)
        class(pid), intent(inout) :: this
        this%m_reset = .true.
        this%m_previousDerivative = 0.0d0
        this%m_previousOutput = 0.0d0
        this%m_previousSignal = 0.0d0
        this%m_previousTime = 0.0d0
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Applies a digital filter to the supplied signal.
    !!
    !! @param[in] this The pid object.
    !! @param[in] t The time at which the signal was sampled.
    !! @param[in] y The signal to filter.
    !! @return The filtered signal.
    pure function pid_filter(this, t, y) result(yfilt)
        ! Arguments
        class(pid), intent(in) :: this
        real(real64), intent(in) :: t, y
        real(real64) :: yfilt

        ! Local Variables
        real(real64) :: Tf, T0

        ! Initialization
        Tf = this%get_filter_time_constant()
        T0 = t - this%m_previousTime

        ! Process
        yfilt = this%m_previousSignal * Tf / (Tf * T0) + y * T0 / (Tf + T0)
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the output of the controller given the input state.
    !!
    !! @param[in,out] this The pid object.
    !! @param[in] setpoint The target setpoint.
    !! @param[in] t The time at which the signal was sampled.
    !! @param[in] y The signal to filter.
    !! @return The controller output.
    function pid_eval_1(this, setpoint, t, y) result(co)
        ! Arguments
        class(pid), intent(inout) :: this
        real(real64), intent(in) :: setpoint, y, t
        real(real64) :: co

        ! Local Variables
        real(real64) :: yk, ek, dt, vk, ak, du

        ! Initialization
        co = 0.0d0

        ! Filter
        if (this%get_use_filter()) then
            yk = this%filter(t, y)
        else
            yk = y
        end if

        ! Compute the controller output - if sufficient time has passed or the 
        ! the setpoint has been modified
        if ((t - this%m_previousTime >= this%get_update_time()) .or. &
                (setpoint /= this%m_setpoint) .or. this%m_reset) then
            ! Update the setpoint
            this%m_setpoint = setpoint

            ! Compute the controller output
            dt = t - this%m_previousTime ! TO DO: Ensure dt /= 0
            ek = setpoint - yk
            vk = (yk - this%m_previousSignal) / dt
            ak = (vk - this%m_previousDerivative) / dt
            du = (this%integral_gain * ek - &
                this%proportional_gain * vk - &
                this%derivative_gain * ak) * dt
            co = this%m_previousOutput + du

            ! Check the output against the limit values
            if (co > this%get_upper_limit()) co = this%get_upper_limit()
            if (co < this%get_lower_limit()) co = this%get_lower_limit()

            ! Update parameters
            this%m_previousDerivative = vk
            this%m_previousOutput = co
            this%m_previousSignal = yk
            this%m_previousTime = t
            this%m_reset = .false.
        else
            ! No controller output update is necessary, just return
            ! the previous value
            co = this%m_previousOutput
        end if
        
    end function

end module
