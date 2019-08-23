! lti_example_4.f90

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
    ! - Forcing Frequency: 20 Hz
    ! - Sampling Interval: 1 ms

    ! Model Parameters
    real(real64), parameter :: fn = 5.0d1
    real(real64), parameter :: zeta = 1.0d-1
    real(real64), parameter :: mass = 2.0d1
    real(real64), parameter :: force = 1.0d3
    real(real64), parameter :: freq = 2.0d1
    real(real64), parameter :: dt = 1.0d-3
    integer(int32), parameter :: npts = 1000

    ! Local Variables
    type(LTI) :: sys
    type(state_space) :: mdl
    real(real64) :: wn, xo(2)
    integer(int32) :: i
    real(real64) :: t(npts), u(1, npts), y(1, npts)
    type(plot_2d) :: plt
    type(plot_data_2d) :: ds1
    class(plot_axis), pointer :: xAxis, yAxis

    ! Build the model noting the equation of motion is:
    ! x" + 2 zeta * wn x' + wn**2 = F / m
    wn = 2.0d0 * pi * fn
    call sys%numerator%initialize([force / mass])
    call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])
    
    ! Convert to a state-space model
    call sys%to_state_space(mdl)

    ! Display the state space matrices
    print '(AI0AI0A)', "A (", size(mdl%A, 1), "x", size(mdl%A, 2), "): "
    do i = 1, size(mdl%A, 1)
        print *, mdl%A(i,:)
    end do

    print *, ""
    print '(AI0AI0A)', "B (", size(mdl%B, 1), "x", size(mdl%B, 2), "): "
    do i = 1, size(mdl%B, 1)
        print *, mdl%B(i,:)
    end do

    print *, ""
    print '(AI0AI0A)', "C (", size(mdl%C, 1), "x", size(mdl%C, 2), "): "
    do i = 1, size(mdl%C, 1)
        print *, mdl%C(i,:)
    end do

    print *, ""
    print '(AI0AI0A)', "D (", size(mdl%D, 1), "x", size(mdl%D, 2), "): "
    do i = 1, size(mdl%D, 1)
        print *, mdl%D(i,:)
    end do

    ! Build a sample time vector, and a forcing function
    do i = 1, npts
        if (i == 1) then
            t(i) = 0.0d0
        else
            t(i) = t(i-1) + dt
        end if
        u(1,i) = sin(2.0d0 * pi * freq * t(i))
    end do

    ! Evaluate the state-space model assuming an initial zero-state
    xo = 0.0d0
    call mdl%evaluate_inplace(xo, u, y)

    ! Plot the solution
    call plt%initialize()
    call plt%set_font_size(11)
    call plt%set_font_name("Arial")

    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()

    call xAxis%set_title("Time (s)")
    call yAxis%set_title("Position (mm)")

    call ds1%define_data(t, 1.0d3 * y(1,:))
    call plt%push(ds1)
    call plt%draw()
end program
