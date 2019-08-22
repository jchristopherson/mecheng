! vibrations_lti.f90

submodule (vibrations) vibrations_lti
contains
! ------------------------------------------------------------------------------
    module function lti_get_zeros(this, err) result(z)
        ! Arguments
        class(LTI), intent(in) :: this
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:) :: z

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the transfer function is properly defined
        if (.not.this%validate()) then
            call errmgr%report_error("lti_get_zeros", &
                "The transfer function is not valid.", &
                MECH_INVALID_TRANSFER_FUNCTION_ERROR)
            return
        end if

        ! Compute the zeros - roots of the numerator
        z = this%numerator%roots(errmgr)
    end function

! ------------------------------------------------------------------------------
    module function lti_get_poles(this, err) result(p)
        ! Arguments
        class(LTI), intent(in) :: this
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:) :: p

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the transfer function is properly defined
        if (.not.this%validate()) then
            call errmgr%report_error("lti_get_poles", &
                "The transfer function is not valid.", &
                MECH_INVALID_TRANSFER_FUNCTION_ERROR)
            return
        end if

        ! Compute the poles - roots of the denominator
        p = this%denominator%roots(errmgr)
    end function

! ------------------------------------------------------------------------------
    pure module function lti_validate(this) result(x)
        ! Arguments
        class(LTI), intent(in) :: this
        logical :: x

        ! Process
        integer(int32) :: nOrder, dOrder

        nOrder = this%numerator%order()
        dOrder = this%denominator%order()

        x = .true.
        if (nOrder < 0 .or. dOrder < 0) x = .false.
        if (nOrder >= dOrder) x = .false.
    end function

! ------------------------------------------------------------------------------
    elemental module function lti_evaluate(this, omega) result(x)
        ! Arguments
        class(LTI), intent(in) :: this
        real(real64), intent(in) :: omega
        complex(real64) :: x

        ! Define the complex frequency
        complex(real64), parameter :: i = (0.0d0, 1.0d0)
        complex(real64) :: s, n, d
        s = i * omega

        ! Evaluate each polynomial
        n = this%numerator%evaluate(s)
        d = this%denominator%evaluate(s)
        x = n / d
    end function

! ------------------------------------------------------------------------------
    module subroutine lti_bode(this, freq, settings)
        ! Required Module Support
        use constants, only : pi
        use arrays, only : unwrap
        use fplot_core

        ! Arguments
        class(LTI), intent(in) :: this
        real(real64), intent(in), dimension(:) :: freq
        type(bode_settings), intent(in), optional :: settings

        ! Parameters
        real(real64), parameter :: zero = 0.0d0

        ! Local Variables
        class(terminal), pointer :: term
        type(multiplot) :: plt
        type(plot_2d) :: plt1, plt2
        type(plot_data_2d) :: d1, d2
        class(plot_axis), pointer :: x1, x2, y1, y2
        real(real64), allocatable, dimension(:) :: gain, phase, omega
        complex(real64), allocatable, dimension(:) :: tf
        complex(real64) :: dc
        logical :: unwrapPhase
        character(len = :), allocatable :: fontName
        integer(int32) :: fontSize, width, height
        real(real32) :: lineWidth

        ! Initialization
        unwrapPhase = .true.
        fontName = "Calibri"
        fontSize = 12
        width = 800
        height = 600
        lineWidth = 1.0
        if (present(settings)) then
            unwrapPhase = settings%unwrap_phase
            fontName = settings%font_name
            fontSize = settings%font_size
            height = settings%window_height
            width = settings%window_width
            lineWidth = settings%line_width
        end if

        ! Construct the transfer function, and convert the gain to dB
        ! Also construct the phase, unwrap if necessary, and convert
        ! to degrees
        dc = this%evaluate(zero)
        omega = 2.0d0 * pi * freq
        tf = this%evaluate(omega)
        gain = 2.0d1 * log10(abs(tf / dc))
        phase = atan2(aimag(tf), real(tf))
        if (unwrapPhase) then
            call unwrap(phase)
        end if
        phase = 1.8d2 * phase / pi

        ! Initialize the plot objects
        call plt%initialize(2, 1)
        call plt1%initialize()
        call plt2%initialize()

        x1 => plt1%get_x_axis()
        y1 => plt1%get_y_axis()
        x2 => plt2%get_x_axis()
        y2 => plt2%get_y_axis()

        term => plt%get_terminal()

        call plt%set_font_name(fontName)
        call plt%set_font_size(fontSize)

        call term%set_window_height(height)
        call term%set_window_width(width)

        ! Establish axis labels
        call x1%set_title("Frequency (Hz)")
        call y1%set_title("Gain (dB)")
        call x2%set_title("Frequency (Hz)")
        call y2%set_title("Phase (deg)")

        ! Set the data
        call d1%set_name("Gain")
        call d1%define_data(freq, gain)
        call d1%set_line_width(lineWidth)
        call plt1%push(d1)

        call d2%set_name("Phase")
        call d2%define_data(freq, phase)
        call d2%set_line_width(lineWidth)
        call plt2%push(d2)

        call plt%set(1, 1, plt1)
        call plt%set(2, 1, plt2)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
