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
    ! - Forcing Frequency: 20 Hz
    ! - Sampling Interval: 1 ms

    ! Model Parameters
    real(real64), parameter :: fn = 5.0d1
    real(real64), parameter :: zeta = 1.0d-1
    real(real64), parameter :: mass = 2.0d1
    real(real64), parameter :: force = 1.0d3
    real(real64), parameter :: freq = 2.0d1
    integer(int32), parameter :: npts = 1000
    complex(real64), parameter :: j = (0.0d0, 1.0d0)

    ! Local Variables
    type(LTI) :: sys
    type(state_space) :: mdl
    real(real64) :: wn
    real(real64), allocatable, dimension(:) :: freq, omega
    complex(real64), allocatable, dimension(:) :: s, tfLTI
    complex(real64), allocatable, dimension(:,:,:) :: tfSS

    ! Build the model noting the equation of motion is:
    ! x" + 2 zeta * wn x' + wn**2 = F / m
    wn = 2.0d0 * pi * fn
    call sys%numerator%initialize([force / mass])
    call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])
    
    ! Convert to a state-space model
    call sys%to_state_space(mdl)

    ! Construct the frequency vector
    freq = linspace(1, 100, npts)
    omega = 2.0d0 * pi * freq
    s = j * omega

    ! Evaluate the transfer function via the LTI object
    tfLTI = sys%evaluate(omega)

    ! Evaluate the transfer function via the state-space model
    tfSS = mdl%evaluate_transfer_function(s)
end program
