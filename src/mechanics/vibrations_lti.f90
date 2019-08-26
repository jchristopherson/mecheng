! vibrations_lti.f90

! References:
! - http://web.mit.edu/2.14/www/Handouts/PoleZero.pdf
! - https://lpsa.swarthmore.edu/Representations/SysRepTransformations/TF2SS.html

submodule (vibrations) vibrations_lti
    use fplot_core
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
        fontName = "Arial"
        fontSize = 11
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
    module subroutine lti_pole_zero_plot(this, settings, err)
        ! Arguments
        class(LTI), intent(in) :: this
        type(pole_zero_settings), intent(in), optional :: settings
        class(errors), intent(inout), optional :: err

        ! Local Variables
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2
        class(plot_axis), pointer :: xAxis, yAxis
        class(legend), pointer :: lgnd
        complex(real64), allocatable, dimension(:) :: poles, zeros
        character(len = :), allocatable :: fontName
        integer(int32) :: fontSize
        real(real32) :: lineWidth, markerSize
        logical :: showLegend

        ! Initialization
        fontName = "Arial"
        fontSize = 11
        lineWidth = 1.5
        markerSize = 3.0
        showLegend = .true.
        if (present(settings)) then
            fontName = settings%font_name
            fontSize = settings%font_size
            lineWidth = settings%line_width
            markerSize = settings%marker_size
            showLegend = settings%show_legend
        end if

        ! Compute the poles and zeros of the system
        zeros = this%compute_zeros(err)
        poles = this%compute_poles(err)

        ! If there are no zero values, simply plot a single zero at the origin
        if (.not.allocated(zeros)) then
            zeros = [(0.0d0, 0.0d0)]
        end if
        if (size(zeros) < 1) then
            zeros = [(0.0d0, 0.0d0)]
        end if

        ! Initialize the plot
        call plt%initialize()
        xAxis => plt%get_x_axis()
        yAxis => plt%get_y_axis()

        call plt%set_font_name(fontName)
        call plt%set_font_size(fontSize)

        lgnd => plt%get_legend()
        call lgnd%set_is_visible(showLegend)
        call lgnd%set_vertical_position(LEGEND_BOTTOM)
        call lgnd%set_horizontal_position(LEGEND_CENTER)
        call lgnd%set_draw_inside_axes(.false.)
        call lgnd%set_draw_border(.false.)

        ! Define the data
        call d1%set_name("Pole")
        call d1%set_draw_line(.false.)
        call d1%set_draw_markers(.true.)
        call d1%set_marker_style(MARKER_X)
        call d1%define_data(real(poles), aimag(poles))
        call d1%set_marker_scaling(markerSize)
        call d1%set_line_width(lineWidth)
        call plt%push(d1)

        call d2%set_name("Zero")
        call d2%set_draw_line(.false.)
        call d2%set_draw_markers(.true.)
        call d2%set_marker_style(MARKER_EMPTY_CIRCLE)
        call d2%define_data(real(zeros), aimag(zeros))
        call d2%set_marker_scaling(markerSize)
        call d2%set_line_width(lineWidth)
        call plt%push(d2)
        
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine lti_to_ss(this, ss, err)
        ! Arguments
        class(LTI), intent(in) :: this
        class(state_space), intent(out) :: ss
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, n, nb
        real(real64) :: a0, b0
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the transfer function is valid
        if (.not.this%validate()) then
            call errmgr%report_error("lti_to_ss", &
                "The transfer function is not valid.", &
                MECH_INVALID_TRANSFER_FUNCTION_ERROR)
            return
        end if

        ! Allocate space for the matrices
        n = this%denominator%order()
        allocate(ss%A(n, n))
        allocate(ss%B(n, 1))
        allocate(ss%C(1, n))
        allocate(ss%D(1, 1))

        ! Construct A
        do j = 2, n
            do i = 1, n
                if (i == j - 1) then
                    ss%A(i,j) = 1.0d0
                else
                    ss%A(i,j) = 0.0d0
                end if
            end do
        end do
        a0 = this%denominator%get(n+1)  ! Coefficient of the highest power
        do i = 1, n
            ss%A(i,1) = -(this%denominator%get(n-i+1)) / a0
        end do

        ! Construct B
        ss%B = 0.0d0
        nb = this%numerator%order()
        do i = 1, nb + 1
            ss%B(n-i+1,1) = this%numerator%get(i)
        end do

        if (nb == n) then
            b0 = this%numerator%get(nb+1)
            ss%B(:,1) = (ss%B(:,1) / b0) + ss%A(:,1) * b0
        else
            b0 = 0.0d0
        end if

        ! Construct C
        ss%C(1,1) = 1.0d0
        ss%C(1,2:n) = 0.0d0

        ! Construct D
        ss%D(1,1) = b0
    end subroutine

! ******************************************************************************
    pure module function modal_info_from_poles(poles) result(x)
        use constants

        ! Arguments
        complex(real64), intent(in), dimension(:) :: poles
        real(real64), allocatable, dimension(:,:) :: x

        ! Local Variables
        integer(int32) :: i, j, k, n
        real(real64) :: tol, arg1, arg2
        logical :: flag
        complex(real64), allocatable, dimension(:) :: buffer

        ! Initialization
        n = size(poles)
        tol = sqrt(epsilon(tol))

        ! Process
        allocate(buffer(n))
        i = 1
        j = 0
        do while (i <= n)
            k = i + 1
            flag = .false.
            if (k <= n) then
                arg1 = abs(aimag(poles(i)))
                arg2 = abs(aimag(poles(k)))
                if (abs(arg1 - arg2) < tol) then
                    j = j + 1
                    buffer(j) = poles(i)
                    flag = .true.
                end if
            end if

            if (flag) then
                ! A complex-conjugate pair was found
                i = i + 2
            else
                ! No complex-conjugate pair was found
                i = i + 1
            end if
        end do

        ! Quick return
        if (j == 0) return

        ! Allocate space for the output
        allocate(x(j,2))

        ! Compute the magnitude of each value to determine the natural
        ! frequency values, in Hz
        x(:,1) = abs(buffer(1:j)) / (2.0d0 * pi)

        ! Compute the angle of each value from the negative real axis to
        ! determine the damping ratio
        x(:,2) = cos( atan2( abs(aimag(buffer(1:j))), abs(real(buffer(1:j))) ) )
    end function

! ------------------------------------------------------------------------------
end submodule
