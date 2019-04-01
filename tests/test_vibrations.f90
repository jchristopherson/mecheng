! test_vibrations.f90

program test
    use iso_fortran_env
    use vibrations
    use fplot_core
    use constants
    implicit none

    ! Local Variables
    logical :: overall, rst

    ! Initialization
    overall = .true.

    ! Tests
    rst = test_modal_response()
    if (.not.rst) overall = .false.

    call test_frequency_response()

    ! Output
    if (overall) then
        print '(A)', "VIBRATIONS TESTS PASSED"
    else
        print '(A)', "VIBRATIONS TESTS FAILED"
    end if

contains
! ******************************************************************************
    function test_modal_response() result(rst)
        ! Arguments/Outputs
        logical :: rst

        ! Define the model parameters
        real(real64), parameter :: m1 = 0.5d0
        real(real64), parameter :: m2 = 2.5d0
        real(real64), parameter :: m3 = 0.75d0
        real(real64), parameter :: k1 = 5.0d6
        real(real64), parameter :: k2 = 10.0d6
        real(real64), parameter :: k3 = 10.0d6
        real(real64), parameter :: k4 = 5.0d6
        real(real64), parameter :: tol = 1.0d-3

        ! Local Variables
        integer(int32) :: i
        real(real64) :: m(3,3), k(3,3), modes(3)
        type(modal_information), allocatable, dimension(:) :: info

        ! Initialization
        rst = .true.

        ! Solution
        modes = [232.9225d0, 749.6189d0, 923.5669d0]

        ! Define the mass matrix
        m = reshape([m1, 0.0d0, 0.0d0, 0.0d0, m2, 0.0d0, 0.0d0, 0.0d0, m3], [3, 3])

        ! Define the stiffness matrix
        k = reshape([k1 + k2, -k2, 0.0d0, -k2, k2 + k3, -k3, 0.0d0, -k3, k3 + k4], &
            [3, 3])

        ! Compute the modal response
        info = compute_modal_response(m, k)

        ! Cycle through and test each resonant frequency
        do i = 1, 3
            if (abs(info(i)%frequency - modes(i)) > tol) then
                rst = .false.
                print '(AEN12.4AEN12.3A)', "TEST_MODAL_RESPONSE FAILED: Expected ", &
                    modes(i), " Hz, but found ", info(i)%frequency, " Hz."
            end if

            print '(AI0AF8.4A)', "Mode ", i, ": ", info(i)%frequency, " Hz"
            print *, info(i)%mode_shape
        end do
    end function

! ******************************************************************************
    subroutine test_frequency_response()
        ! Define the model parameters
        real(real64), parameter :: m1 = 0.5d0
        real(real64), parameter :: m2 = 2.5d0
        real(real64), parameter :: m3 = 0.75d0
        real(real64), parameter :: k1 = 5.0d6
        real(real64), parameter :: k2 = 10.0d6
        real(real64), parameter :: k3 = 10.0d6
        real(real64), parameter :: k4 = 5.0d6
        real(real64), parameter :: zeta = 1.0d-2
        integer(int32), parameter :: npts = 500
        real(real64), parameter :: y1 = 1.0d-3

        ! Local Variables
        integer(int32) :: i
        real(real64) :: m(3,3), k(3,3), freq(npts), x1(npts), x2(npts), x3(npts), &
            p1(npts), p2(npts), p3(npts)
        complex(real64) :: f(npts, 3)
        complex(real64), allocatable, dimension(:,:) :: rsp
        type(multiplot) :: plt
        type(plot_2d) :: plt1, plt2
        type(plot_data_2d) :: d1, d2, d3, d1a, d2a, d3a
        class(plot_axis), pointer :: xAxis1, xAxis2, yAxis1, yAxis2

        ! Define the mass matrix
        m = reshape([m1, 0.0d0, 0.0d0, 0.0d0, m2, 0.0d0, 0.0d0, 0.0d0, m3], [3, 3])

        ! Define the stiffness matrix
        k = reshape([k1 + k2, -k2, 0.0d0, -k2, k2 + k3, -k3, 0.0d0, -k3, k3 + k4], &
            [3, 3])

        ! Define the frequency vector, and the forcing function
        freq = 2.0d0 * pi * logspace(2.0d0, 3.0d0, npts)
        f = (0.0d0, 0.0d0)
        f(:,1) = k1 * y1

        ! Compute the FRF
        rsp = compute_frequency_response(m, k, zeta, f, freq)

        ! Compute the amplitude values
        x1 = abs(rsp(:,1))
        x2 = abs(rsp(:,2))
        x3 = abs(rsp(:,3))

        ! Compute the phase angles, and convert to degrees
        p1 = 180.0d0 * atan2(aimag(rsp(:,1)), real(rsp(:,1))) / pi
        p2 = 180.0d0 * atan2(aimag(rsp(:,2)), real(rsp(:,2))) / pi
        p3 = 180.0d0 * atan2(aimag(rsp(:,3)), real(rsp(:,3))) / pi

        ! Plot the solution
        call plt%initialize(2, 1)
        call plt%set_font_size(14)
        call plt1%initialize()
        call plt2%initialize()

        xAxis1 => plt1%get_x_axis()
        yAxis1 => plt1%get_y_axis()
        xAxis2 => plt2%get_x_axis()
        yAxis2 => plt2%get_y_axis()

        call xAxis1%set_is_log_scaled(.true.)
        call yAxis1%set_is_log_scaled(.true.)
        call xAxis2%set_is_log_scaled(.true.)

        call xAxis1%set_title("Frequency [Hz]")
        call yAxis1%set_title("Amplitude (X / Y)")
        call xAxis2%set_title("Frequency [Hz]")
        call yAxis2%set_title("Phase [deg]")

        call d1%define_data(freq / (2.0d0 * pi), x1 / y1)
        call d2%define_data(freq / (2.0d0 * pi), x2 / y1)
        call d3%define_data(freq / (2.0d0 * pi), x3 / y1)

        call d1a%define_data(freq / (2.0d0 * pi), p1)
        call d2a%define_data(freq / (2.0d0 * pi), p2)
        call d3a%define_data(freq / (2.0d0 * pi), p3)

        call plt1%push(d1)
        call plt1%push(d2)
        call plt1%push(d3)
        call plt2%push(d1a)
        call plt2%push(d2a)
        call plt2%push(d3a)

        call plt%set(1, 1, plt1)
        call plt%set(2, 1, plt2)
        call plt%draw()
    end subroutine

! ******************************************************************************
end program