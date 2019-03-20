! signals_realtime.f90

! http://www.eas.uccs.edu/~mwickert/ece5655/lecture_notes/ece5655_chap7.pdf
! https://github.com/MattPennock/Biquad
! https://github.com/jhgorse/C-filters/blob/master/fir_filter.c


submodule (signals) signals_realtime
contains
! ------------------------------------------------------------------------------
    module subroutine fir_init_1(this, taps, err)
        ! Arguments
        class(fir_filter), intent(inout) :: this
        integer(int32), intent(in) :: taps
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (taps < 1) then
            call errmgr%report_error("fir_init_1", &
                "Invalid number of filter taps.", &
                SIG_INVALID_INPUT_ERROR)
            return
        end if

        ! Memory Allocation
        if (allocated(this%m_buffer)) deallocate(this%m_buffer)
        if (allocated(this%m_coefficients)) deallocate(this%m_coefficients)
        allocate(this%m_buffer(taps), stat = flag)
        if (flag == 0) allocate(this%m_coefficients(taps), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("fir_init_2", &
                "Insufficient memory available.", &
                SIG_OUT_OF_MEMORY_ERROR)
            return
        end if
        this%m_buffer = 0.0d0
        this%m_coefficients = 1.0d0
        this%m_iter = 1
    end subroutine
    
! ------------------------------------------------------------------------------
    module subroutine fir_init_2(this, coeffs, err)
        ! Arguments
        class(fir_filter), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: coeffs
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: taps

        ! Process
        taps = size(coeffs)
        call fir_init_1(this, taps, err)
    end subroutine

! ------------------------------------------------------------------------------
    pure module function fir_get_tap_count(this) result(x)
        class(fir_filter), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_buffer) .and. allocated(this%m_coefficients)) then
            x = size(this%m_buffer)
        else
            x = 0
        end if
    end function

! ------------------------------------------------------------------------------
    module function fir_apply_filter(this, x) result(y)
        ! Arguments
        class(fir_filter), intent(inout) :: this
        real(real64), intent(in) :: x
        real(real64) :: y

        ! Local Variables
        integer(int32) :: i, j, n

        ! Initialization
        y = 0.0d0
        n = this%get_tap_count()
        if (n == 0) return

        ! Place the input into the correct buffer location
        this%m_buffer(this%m_iter) = x
        this%m_iter = mod(this%m_iter + 1, n) + 1

        ! Apply the filter
        do i = 1, n
            j = mod(this%m_iter + i, n) + 1
            y = y + this%m_buffer(j) * this%m_coefficients(i)
        end do
    end function
    
! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
