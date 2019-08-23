! lti_example_3.f90

program example
    use iso_fortran_env
    use vibrations
    use constants
    implicit none

    ! Construct the model to represent a mechanical system with
    ! the following properties:
    ! - Natural Frequency: 50 Hz
    ! - Damping Ratio: 0.1
    ! - Sprung Mass: 20 kg
    ! - Force Amplitude: 1 kN

    ! Model Parameters
    real(real64), parameter :: fn = 5.0d1
    real(real64), parameter :: zeta = 1.0d-1
    real(real64), parameter :: mass = 2.0d1
    real(real64), parameter :: force = 1.0d3
    integer(int32), parameter :: npts = 1000

    ! Local Variables
    type(LTI) :: sys
    real(real64) :: wn
    complex(real64), allocatable, dimension(:) :: poles
    real(real64), allocatable, dimension(:,:) :: info

    ! Build the model noting the equation of motion is:
    ! x" + 2 zeta * wn x' + wn**2 = F / m
    wn = 2.0d0 * pi * fn
    call sys%numerator%initialize([force / mass])
    call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])

    ! Compute the system poles
    poles = sys%compute_poles()

    ! Compute the modal information for the system
    info = modal_info_from_poles(poles)

    ! Display the resonant frequency and damping ratio
    print '(AI0)', "# of modes found: ", size(info, 1)
    print '(AF5.2A)', "Natural Frequency: ", info(1,1), " Hz"
    print '(AF4.2)', "Damping Ratio: ", info(1, 2)
end program
