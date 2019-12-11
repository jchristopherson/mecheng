! frequency_sweep_example_1.f90

program example
    use iso_fortran_env
    use vibrations
    use fplot_core
    use constants
    implicit none

    ! Model Parameters
    real(real64), parameter :: delta = 1.0d-1
    real(real64), parameter :: alpha = 1.0d0
    real(real64), parameter :: gamma = 1.0d0
    real(real64), parameter :: beta = 2.0d-1

    ! Additional Parameters
    integer(int32), parameter :: nfreq = 200

    ! Local Variables
    procedure(harmonic_ode_fcn), pointer :: fcn
    real(real64) :: fup(nfreq), fdown(nfreq), xo(2)
    real(real64), allocatable, dimension(:,:) :: rup, rdown, ans
    type(plot_2d) :: plt
    type(plot_data_2d) :: dup, ddown, dans
    class(plot_axis), pointer :: xAxis, yAxis
    class(legend), pointer :: lgnd

    ! Initialization
    fcn => duffing
    fup = linspace(1.0d-1, 1.0d0, nfreq)
    fdown = linspace(1.0d0, 1.0d-1, nfreq)
    xo = [0.0d0, 0.0d0]

    ! Sweep through frequency to compute the solution - both up and down
    rup = compute_frequency_sweep(fcn, 2.0d0 * pi * fup, xo)
    rdown = compute_frequency_sweep(fcn, 2.0d0 * pi * fdown, xo)

    ! Compute the analytical estimate of the FRF
    ans = duffing_frf()

    ! Set up the plot
    call plt%initialize()
    call plt%set_font_size(11)
    call plt%set_font_name("Arial")
    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()
    lgnd => plt%get_legend()

    call xAxis%set_title("Frequency")
    call xAxis%set_is_log_scaled(.true.)
    call yAxis%set_title("Amplitude")
    call lgnd%set_is_visible(.true.)
    call lgnd%set_horizontal_position(LEGEND_LEFT)

    call dans%set_name("Analytical")
    call dans%define_data(ans(:,1), ans(:,2))

    call dup%set_name("Upward")
    call dup%define_data(fup, rup(:,1))
    call dup%set_draw_line(.false.)
    call dup%set_draw_markers(.true.)
    call dup%set_marker_style(MARKER_EMPTY_CIRCLE)

    call ddown%set_name("Downward")
    call ddown%define_data(fdown, rdown(:,1))
    call ddown%set_draw_line(.false.)
    call ddown%set_draw_markers(.true.)
    call ddown%set_marker_style(MARKER_X)

    call plt%push(dans)
    call plt%push(dup)
    call plt%push(ddown)
    call plt%draw()

contains
    ! The Duffing equation.
    !
    ! x" + delta * x' + alpha * x + beta * x**3 = gamma * sin(omega * t)
    subroutine duffing(t, x, omega, dxdt)
        real(real64), intent(in) :: t, omega
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(out), dimension(:) :: dxdt

        ! Equations of motion
        dxdt(1) = x(2)
        dxdt(2) = gamma * sin(omega * t) - delta * x(2) - alpha * x(1) - beta * x(1)**3
    end subroutine

    ! An analytical representation of the frequency response of Duffing's equation.
    function duffing_frf() result(rst)
        ! Arguments
        real(real64), allocatable, dimension(:,:) :: rst

        ! Parameters
        integer(int32), parameter :: npts = 100
        real(real64), parameter :: maxfreq = 1.0d0
        real(real64), parameter :: minfreq = 1.0d-2

        ! Local Variables
        integer(int32) :: i, j
        real(real64) :: w2, f, x
        real(real64), allocatable, dimension(:) :: z, arg1, arg2
        real(real64), allocatable, dimension(:,:) :: buffer

        ! Initialization
        z = linspace(1.0d-2, 1.0d1, npts)
        allocate(buffer(2 * npts, 2))

        ! Process
        arg1 = 4.0d0 * gamma**2 - 3.0d0 * beta * delta**2 * z**4 + &
            (delta**4 - 4.0d0 * alpha * delta**2) * z**2
        arg2 = 3.0d0 * beta * z**3 + (4.0d0 * alpha - 2.0d0 * delta**2) * z

        j = 0
        do i = 1, npts
            if (arg1(i) < 0.0d0) cycle
            w2 = (2.0d0 * sqrt(arg1(i)) + arg2(i)) / (4.0d0 * z(i))
            f = sqrt(w2) / (2.0d0 * pi)
            if (f < minfreq .or. f > maxfreq) cycle
            j = j + 1
            buffer(j,1) = f
            buffer(j,2) = z(i)
        end do
        do i = 1, npts
            if (arg1(i) < 0.0d0) cycle
            w2 = -(2.0d0 * sqrt(arg1(i)) - arg2(i)) / (4.0d0 * z(i))
            f = sqrt(w2) / (2.0d0 * pi)
            if (f < minfreq .or. f > maxfreq) cycle
            j = j + 1
            buffer(j,1) = f
            buffer(j,2) = z(i)
        end do
        allocate(rst(j,2))
        rst = buffer(1:j,:)
    end function
end program
