! lti_example_4.f90

program example
    use iso_fortran_env
    use vibrations
    use constants
    use fplot_core
    use integral_core
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
    integer(int32) :: i
    real(real64) :: wn, xo(2), t(2)
    real(real64), allocatable, dimension(:,:) :: mdlOut, dirOut
    type(ode_helper) :: ssObj, dirObj
    type(ode_auto) :: integrator
    procedure(ode_fcn), pointer :: ssFcn, dirFcn
    type(plot_2d) :: plt
    type(plot_data_2d) :: ds1, ds2
    class(plot_axis), pointer :: xAxis, yAxis
    class(legend), pointer :: lgnd

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
    
    ! Set up the integrator
    ssFcn => state_space_model
    call ssObj%define_equations(2, ssFcn)

    dirFcn => direct_equations
    call dirObj%define_equations(2, dirFcn)

    ! Define the initial conditions
    t = [0.0d0, 1.0d0]
    xo = [0.0d0, 0.0d0]

    ! Perform the integration
    mdlOut = integrator%integrate(ssObj, t, xo)

    call integrator%reset()
    dirOut = integrator%integrate(dirObj, t, xo)

    ! Plot the solution
    call plt%initialize()
    call plt%set_font_size(11)
    call plt%set_font_name("Arial")

    xAxis => plt%get_x_axis()
    yAxis => plt%get_y_axis()

    call xAxis%set_title("Time (s)")
    call yAxis%set_title("Position (mm)")

    lgnd => plt%get_legend()
    call lgnd%set_is_visible(.true.)
    call lgnd%set_draw_border(.false.)
    call lgnd%set_draw_inside_axes(.false.)
    call lgnd%set_horizontal_position(LEGEND_CENTER)

    call ds1%set_name("State-Space Model")
    call ds1%define_data(mdlOut(:,1), 1.0d3 * mdlOut(:,2))
    call plt%push(ds1)

    call ds2%set_name("Direct")
    call ds2%define_data(dirOut(:,1), 1.0d3 * dirOut(:,2))
    call ds2%set_draw_line(.false.)
    call ds2%set_draw_markers(.true.)
    call plt%push(ds2)

    call plt%draw()

contains
    ! The routine utilizing the state-space model
    subroutine state_space_model(t, x, dxdt)
        ! Arguments
        real(real64), intent(in) :: t
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(out), dimension(:) :: dxdt

        ! Local Variables
        real(real64) :: u(1)
        real(real64), allocatable, dimension(:) :: y

        ! Define the forcing function
        u(1) = sin(2.0d0 * pi * freq * t)

        ! Evaluate the state space model
        dxdt = x    ! Use to store the current state, the model will update
        y = mdl%evaluate(dxdt, u)
    end subroutine

    ! Direct application of the equations
    subroutine direct_equations(t, x, dxdt)
        ! Arguments
        real(real64), intent(in) :: t
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(out), dimension(:) :: dxdt

        ! Define the output
        dxdt(1) = x(2)
        dxdt(2) = (force / mass) * sin(2.0d0 * pi * freq * t) - &
            (2.0d0 * zeta * wn * x(2) + wn**2 * x(1))
    end subroutine
end program
