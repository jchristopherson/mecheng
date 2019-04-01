! controls_realtime.f90

submodule (controls) controls_realtime
contains
! ------------------------------------------------------------------------------
    pure module function rto_get_update_rate(this) result(x)
        class(realtime_object), intent(in) :: this
        real(real64) :: x
        x = this%m_updateRate
    end function

! ------------------------------------------------------------------------------
    module subroutine rto_set_update_rate(this, x)
        class(realtime_object), intent(inout) :: this
        real(real64), intent(in) :: x
        this%m_updateRate = x
    end subroutine

! ******************************************************************************
    module function rd_evaluate(this, t, y) result(dydt)
        ! Arguments
        class(realtime_derivative), intent(inout) :: this
        real(real64), intent(in) :: t, y
        real(real64) :: dydt

        ! Local Variables
        real(real64) :: update, dt

        ! Initialization
        update = 1.0d0 / this%get_update_rate()
        dt = t - this%m_previousTime

        ! Compute the derivative
        dydt = (y - this%m_previousSignal) / dt

        ! Update the stored values, if necessary
        if (abs(dt) >= update) then
            this%m_previousTime = t
            this%m_previousSignal = y
        end if
    end function

! ------------------------------------------------------------------------------
    module subroutine rd_reset(this)
        class(realtime_derivative), intent(inout) :: this
        this%m_previousSignal = 0.0d0
        this%m_previousTime = 0.0d0
    end subroutine

! ------------------------------------------------------------------------------
end submodule
