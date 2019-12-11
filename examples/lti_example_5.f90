! lti_example_5.f90

program example
    use iso_fortran_env
    use vibrations
    use constants
    use fplot_core
    implicit none

    ! Construct the model to represent a mechanical system with
    ! the following properties:
    ! - Natural Frequency: 50 Hz
    ! - Damping Ratio: 0.1
    ! - Sprung Mass: 20 kg
    ! - Force Amplitude: 1 kN
    ! - Sampling Interval: 1 ms

    ! Model Parameters
    real(real64), parameter :: fn = 5.0d1
    real(real64), parameter :: zeta = 1.0d-1
    real(real64), parameter :: mass = 2.0d1
    real(real64), parameter :: force = 1.0d3
    integer(int32), parameter :: npts = 1000

    ! Local Variables
    type(LTI) :: sys
    type(state_space) :: mdl
    real(real64) :: wn
    real(real64), allocatable, dimension(:) :: freq, omega, &
        magLTI, phaseLTI, magSS, phaseSS
    complex(real64) :: offset
    complex(real64), allocatable, dimension(:) :: tfLTI
    complex(real64), allocatable, dimension(:,:,:) :: tfSS
    type(multiplot) :: plt
    type(plot_2d) :: plt1, plt2
    type(plot_data_2d) :: d1LTI, d2LTI, d1SS, d2SS
    class(plot_axis), pointer :: x1, x2, y1, y2
    class(legend), pointer :: lgnd

    ! Build the model noting the equation of motion is:
    ! x" + 2 zeta * wn x' + wn**2 = F / m
    wn = 2.0d0 * pi * fn
    call sys%numerator%initialize([force / mass])
    call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])
    
    ! Convert to a state-space model
    call sys%to_state_space(mdl)

    ! Construct the frequency vector
    freq = linspace(1.0d0, 1.0d2, npts)
    omega = 2.0d0 * pi * freq

    ! Evaluate the transfer function via the LTI object
    tfLTI = sys%evaluate(omega)

    ! Evaluate the transfer function via the state-space model
    tfSS = mdl%evaluate_transfer_function(omega)

    ! Compute the magnitude and phase for each
    offset = sys%evaluate(0.0d0)
    magLTI = 2.0d1 * log10(abs(tfLTI / offset))
    phaseLTI = (1.8d2 / pi) * atan2(aimag(tfLTI), real(tfLTI))

    magSS = 2.0d1 * log10(abs(tfSS(1,1,:) / offset))
    phaseSS = (1.8d2 / pi) * atan2(aimag(tfSS(1,1,:)), real(tfSS(1,1,:)))

    ! Set up the plots
    call plt%initialize(2, 1)
    call plt1%initialize()
    call plt2%initialize()

    call plt%set_font_name("Arial")
    call plt%set_font_size(11)

    x1 => plt1%get_x_axis()
    y1 => plt1%get_y_axis()
    x2 => plt2%get_x_axis()
    y2 => plt2%get_y_axis()

    call x1%set_title("Frequency (Hz)")
    call y1%set_title("Gain (dB)")
    call x2%set_title("Frequency (Hz)")
    call y2%set_title("Phase (deg)")

    lgnd => plt1%get_legend()
    call lgnd%set_is_visible(.true.)

    ! Plot the data
    call d1LTI%set_name("LTI")
    call d1LTI%define_data(freq, magLTI)
    call plt1%push(d1LTI)

    call d1SS%set_name("SS")
    call d1SS%define_data(freq, magSS)
    call d1SS%set_line_width(2.0)
    call d1SS%set_line_style(LINE_DASHED)
    call d1SS%set_line_color(CLR_RED)
    call plt1%push(d1SS)

    call d2LTI%set_name("LTI")
    call d2LTI%define_data(freq, phaseLTI)
    call plt2%push(d2LTI)

    call d2SS%set_name("SS")
    call d2SS%define_data(freq, phaseSS)
    call d2SS%set_line_width(2.0)
    call d2SS%set_line_style(LINE_DASHED)
    call d2SS%set_line_color(CLR_RED)
    call plt2%push(d2SS)

    call plt%set(1, 1, plt1)
    call plt%set(2, 1, plt2)
    call plt%draw()
end program
