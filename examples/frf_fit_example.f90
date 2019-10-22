! frf_fit_example.f90

program example
    use iso_fortran_env
    use vibrations
    use fplot_core
    use constants
    use arrays
    implicit none

    ! Parameters
    complex(real64), parameter :: zero = (0.0d0, 0.0d0)
    complex(real64), parameter :: j = (0.0d0, 1.0d0)
    integer(int32), parameter :: nfreq = 1000
    real(real64), parameter :: m1 = 0.75d0
    real(real64), parameter :: m2 = 1.5d0
    real(real64), parameter :: b1 = 1.25d2
    real(real64), parameter :: b2 = 2.75d1
    real(real64), parameter :: k1 = 3.5d6
    real(real64), parameter :: k2 = 7.5d6
    real(real64), parameter :: y = 1.5d-3
    real(real64), parameter :: min_freq = 1.0d0
    real(real64), parameter :: max_freq = 1.0d3

    ! Local Variables
    real(real64) :: k(2,2), b(2,2), m(2,2), freq(nfreq), omega(nfreq), &
        frf_mag(nfreq), frf_phase(nfreq), fit_mag(nfreq), fit_phase(nfreq)
    real(real64), allocatable, dimension(:,:) :: fitted_info
    complex(real64) :: f(nfreq,2)
    complex(real64), allocatable, dimension(:,:) :: frf
    type(modal_information), allocatable, dimension(:) :: info
    type(frf_fitting_tool) :: fit
    type(multiplot) :: plt
    type(plot_2d) :: plt1, plt2
    type(plot_data_2d) :: d1mag, d1phase, d2mag, d2phase
    class(plot_axis), pointer :: x1, x2, y1, y2
    class(legend), pointer :: lgnd

    ! Construct the frequency values
    freq = linspace(min_freq, max_freq, nfreq)
    omega = 2.0d0 * pi * freq

    ! Initialize the system matrices
    k = reshape([k1 + k2, -k2, -k2, k2], [2, 2])
    b = reshape([b1 + b2, -b2, -b2, b2], [2, 2])
    m = reshape([m1, 0.0d0, 0.0d0, m2], [2, 2])

    ! Construct the forcing function array
    f(:,1) = y * (j * omega * b1 + k1)
    f(:,2) = zero

    ! Compute the FRF
    frf = compute_frequency_response(m, k, b, f, omega)
    frf_mag = abs(frf(:,1))
    frf_phase = atan2(aimag(frf(:,1)), real(frf(:,1)))

    ! Unwrap the phase, and then convert to degrees
    call unwrap(frf_phase)
    frf_phase = (1.8d2 / pi) * frf_phase

    ! Compute the modal response
    info = compute_modal_response(m, k)

    ! Display the resonant frequencies
    print '(AF5.1A)', "Mode 1: ", info(1)%frequency, " Hz"
    print '(AF5.1A)', "Mode 2: ", info(2)%frequency, " Hz"

    ! Attempt to fit the FRF
    call fit%fit(freq, frf_mag, frf_phase, 4)

    ! Extract the magnitude and phase components
    fit_mag = abs(fit%frf)
    fit_phase = atan2(aimag(fit%frf), real(fit%frf))

    ! Unwrap the phase, and then convert to degrees
    call unwrap(fit_phase)
    fit_phase = (1.8d2 / pi) * fit_phase

    ! Compute the resonant frequency and damping information from the fitted model
    fitted_info = modal_info_from_poles(fit%poles)
    print '(AF5.1A)', "Fitted Mode 1: ", fitted_info(2,1), " Hz"
    print '(AF5.1A)', "Fitted Mode 2: ", fitted_info(1,1), " Hz"
    print '(AF6.4)', "Damping Ratio 1: ", fitted_info(2,2)
    print '(AF6.4)', "Damping Ratio 2: ", fitted_info(1,2)

    ! Plot the FRFs
    call plt%initialize(2, 1)
    call plt1%initialize()
    call plt2%initialize()
    call plt%set_font_name("Arial")
    call plt%set_font_size(11)

    x1 => plt1%get_x_axis()
    y1 => plt1%get_y_axis()
    x2 => plt2%get_x_axis()
    y2 => plt2%get_y_axis()
    lgnd => plt1%get_legend()

    call x1%set_title("Frequency (Hz)")
    call y1%set_title("Amplitude (mm)")
    call y1%set_is_log_scaled(.true.)

    call x2%set_title("Frequency (Hz)")
    call y2%set_title("Phase (deg)")

    call lgnd%set_is_visible(.true.)

    ! Add the data to the plots
    call d1mag%set_name("Actual")
    call d1mag%define_data(freq, 1e3 * frf_mag)
    call d1mag%set_line_width(2.0)
    call plt1%push(d1mag)

    call d2mag%set_name("Fitted")
    call d2mag%define_data(freq, 1e3 * fit_mag)
    call d2mag%set_line_width(2.0)
    call d2mag%set_line_style(LINE_DASHED)
    call d2mag%set_line_color(CLR_RED)
    call plt1%push(d2mag)

    call d1phase%define_data(freq, frf_phase)
    call d1phase%set_line_width(2.0)
    call plt2%push(d1phase)

    call d2phase%define_data(freq, fit_phase)
    call d2phase%set_line_width(2.0)
    call d2phase%set_line_style(LINE_DASHED)
    call d2phase%set_line_color(CLR_RED)
    call plt2%push(d2phase)

    call plt%set(1, 1, plt1)
    call plt%set(2, 1, plt2)
    call plt%draw()
end program
