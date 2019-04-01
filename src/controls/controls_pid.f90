! controls_pid.f90

submodule (controls) controls_pid
contains
! ------------------------------------------------------------------------------
    pure module function pid_get_lower_limit(this) result(x)
        class(pid), intent(in) :: this
        real(real64) :: x
        x = this%m_lowerLimit
    end function

    
    module subroutine pid_set_lower_limit(this, x)
        class(pid), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_lowerLimit = x
    end subroutine

! ------------------------------------------------------------------------------    
    pure module function pid_get_upper_limit(this) result(x)
        class(pid), intent(in) :: this
        real(real64) :: x
        x = this%m_upperLimit
    end function

    module subroutine pid_set_upper_limit(this, x)
        class(pid), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_upperLimit = x
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine pid_reset(this)
        class(pid), intent(inout) :: this
        this%m_reset = .true.
        this%m_previousDerivative = 0.0d0
        this%m_previousOutput = 0.0d0
        this%m_previousSignal = 0.0d0
        this%m_previousTime = 0.0d0
    end subroutine

! ------------------------------------------------------------------------------
    module function pid_eval_1(this, setpoint, t, y) result(co)
        ! Arguments
        class(pid), intent(inout) :: this
        real(real64), intent(in) :: setpoint, y, t
        real(real64) :: co

        ! Local Variables
        real(real64) :: ek, dt, vk, ak, du, dt

        ! Initialization
        co = 0.0d0
        dt = 1.0d0 / this%get_update_rate()

        ! Compute the controller output - if sufficient time has passed or the 
        ! the setpoint has been modified
        if ((t - this%m_previousTime >= dt) .or. &
                (setpoint /= this%m_setpoint) .or. this%m_reset) then
            ! Update the setpoint
            this%m_setpoint = setpoint

            ! Compute the controller output
            dt = t - this%m_previousTime
            ek = setpoint - y
            if (dt == 0.0d0) then
                vk = 0.0d0
                ak = 0.0d0
            else
                vk = (y - this%m_previousSignal) / dt
                ak = (vk - this%m_previousDerivative) / dt
            end if
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
            this%m_previousSignal = y
            this%m_previousTime = t
            this%m_reset = .false.
        else
            ! No controller output update is necessary, just return
            ! the previous value
            co = this%m_previousOutput
        end if
        
    end function
end submodule
