! integral_ode_rk4.f90

submodule (integral_core) integral_ode_rk4
contains
! ------------------------------------------------------------------------------
    module function ork4_step(this, fcn, x, y, xout, rtol, atol, err) result(brk)
        ! Arguments
        class(ode_rk4), intent(inout) :: this
        class(ode_helper), intent(inout) :: fcn
        real(real64), intent(inout) :: x
        real(real64), intent(inout), dimension(:) :: y
        real(real64), intent(in) :: xout
        real(real64), intent(in), dimension(:) :: rtol, atol
        class(errors), intent(inout), optional, target :: err
        logical :: brk

        ! Local Variables
        real(real64) :: h

        ! Initialization
        brk = .false.
        h = xout - x
        call this%initialize(fcn%get_equation_count(), err)

        ! Check to see if root finding is requested

        ! Process
        call fcn%evaluate_ode(x, y, this%m_k1)

        this%m_work = y + 0.5d0 * h * this%m_k1
        call fcn%evaluate_ode(x + 0.5d0 *h, this%m_work, this%m_k2)
        
        this%m_work = y + 0.5d0 * h * this%m_k2
        call fcn%evaluate_ode(x + 0.5d0 * h, this%m_work, this%m_k3)

        this%m_work = y + h * this%m_k3
        call fcn%evaluate_ode(xout, this%m_work, this%m_k4)

        this%m_work = (h / 6.0d0) * (this%m_k1 + &
            2.0d0 * (this%m_k2 + this%m_k3) + this%m_k4) + &
            this%m_summationError
        this%m_y1 = y + this%m_work

        ! Update the summation error tracking
        this%m_summationError = this%m_work - (this%m_y1 - y)

        ! Estimate the solution error

        ! Update
        x = xout
        y = this%m_y1
    end function

! ------------------------------------------------------------------------------
    module subroutine ork4_reset_integrator(this)
        class(ode_rk4), intent(inout) :: this
        this%m_summationError = 0.0d0
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine ork4_init_workspace(this, neqn, err)
        ! Arguments
        class(ode_rk4), intent(inout) :: this
        integer(int32), intent(in) :: neqn
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag

        ! Quick Return
        if (allocated(this%m_k1)) then
            if (size(this%m_k1) == neqn) return
        end if
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Deallocate existing
        if (allocated(this%m_k1)) deallocate(this%m_k1)
        if (allocated(this%m_k2)) deallocate(this%m_k2)
        if (allocated(this%m_k3)) deallocate(this%m_k3)
        if (allocated(this%m_k4)) deallocate(this%m_k4)
        if (allocated(this%m_work)) deallocate(this%m_work)
        if (allocated(this%m_summationError)) deallocate(this%m_summationError)
        if (allocated(this%m_y1)) deallocate(this%m_y1)

        ! Process
        allocate(this%m_k1(neqn), stat = flag)
        if (flag == 0) allocate(this%m_k2(neqn), stat = flag)
        if (flag == 0) allocate(this%m_k3(neqn), stat = flag)
        if (flag == 0) allocate(this%m_k4(neqn), stat = flag)
        if (flag == 0) allocate(this%m_work(neqn), stat = flag)
        if (flag == 0) allocate(this%m_summationError(neqn), stat = flag)
        if (flag == 0) allocate(this%m_y1(neqn), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("ork4_init_workspace", &
                "Insufficient memory available.", &
                INT_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Initialize
        this%m_summationError = 0.0d0
    end subroutine

! ------------------------------------------------------------------------------
    module function ork4_integrate(this, fcnobj, x, y, err) result(rst)
        ! Arguments
        class(ode_rk4), intent(inout) :: this
        class(ode_helper), intent(inout) :: fcnobj
        real(real64), intent(in), dimension(:) :: x, y
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: rst

        ! Ensure the integrator understands this is a fixed-step method
        call this%set_provide_all_output(.false.)

        ! Integrate
        rst = oi_integrate(this, fcnobj, x, y, err)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule