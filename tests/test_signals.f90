! test_signals.f90

program main
    ! Routines
    call test_fft()
    call test_lowpass_filter()

contains
! ------------------------------------------------------------------------------
    ! Example Reference:
    ! https://www.mathworks.com/help/matlab/ref/fft.html?s_tid=srchtitle
    subroutine test_fft()
        use, intrinsic :: iso_fortran_env
        use constants
        use signals
        use fplot_core
        implicit none

        ! Parameters
        real(real64), parameter :: fs = 1.0d3
        integer(int32), parameter :: npts = 1500

        ! Local Variables
        integer(int32) :: i
        real(real64) :: s(npts), dt, t, noise(npts)
        complex(real64), allocatable, dimension(:) :: xfrm
        real(real64), allocatable, dimension(:) :: p1, f1
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1
        class(plot_axis), pointer :: xAxis, yAxis

        ! Build the signal
        t = 0.0d0
        dt = 1.0d0 / fs
        do i = 1, npts
            s(i) = 0.7d0 * sin(2.0d0 * pi * 50.0d0 * t) + & 
                sin(2.0d0 * pi * 120.0d0 * t)
            t = t + dt
        end do

        ! Generate some noise to corrupt the signal
        call random_number(noise)
        s = s + 2.0d0 * (noise - 0.5d0)

        ! Compute the FFT
        xfrm = fft(s)

        ! Extract the meaningful portions of the transform 
        ! NOTE: factor of 2 due to symmetry of the transform
        p1 = 2.0d0 * abs(xfrm(1:npts/2 + 1))

        ! Compute a meaningful frequency axis
        allocate(f1(size(p1)))
        do i = 1, size(p1)
            f1(i) = fs * (i - 1.0d0) / npts
        end do

        ! Plot the results
        call plt%initialize()
        call plt%set_title("FFT Test 1")
        
        xAxis => plt%get_x_axis()
        call xAxis%set_title("Frequency [Hz]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("Amplitude")

        call d1%define_data(f1, p1)
        call d1%set_name("Data Set 1")

        call plt%push(d1)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    ! Low Pass Filter Test
    subroutine test_lowpass_filter()
        use, intrinsic :: iso_fortran_env
        use constants
        use signals
        use fplot_core
        implicit none

        ! Parameters
        real(real64), parameter :: fs = 1.0d3
        integer(int32), parameter :: npts = 1500
        real(real64), parameter :: cutoff = 50.0d0

        ! Local Variables
        integer(int32) :: i
        real(real64) :: s(npts), sfilt(npts), t(npts), dt
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2
        class(plot_axis), pointer :: xAxis, yAxis

        ! Signal Initialization
        dt = 1.0d0 / fs
        t(1) = 0.0d0
        do i = 1, npts
            s(i) = sin(2.0d0 * pi * 25.0d0 * t(i)) + &
                cos(2.0d0 * pi * 26.0d0 * t(i)) + &
                0.25 * sin(2.0d0 * pi * 120.0d0 * t(i))
            sfilt(i) = s(i)
            if (i < npts) t(i+1) = t(i) + dt
        end do

        ! Filter the signal
        call low_pass_filter(sfilt, fs, cutoff)

        ! Plot the signal, along with the original
        call plt%initialize()
        call plt%set_title("Low Pass Filter Test 1")
        
        xAxis => plt%get_x_axis()
        call xAxis%set_title("Time [s]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("s(t)")

        call d1%define_data(t, s)
        call d1%set_name("Unfiltered")
        call d1%set_use_auto_color(.false.)
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(t, sfilt)
        call d2%set_name("Filtered")
        call d2%set_use_auto_color(.false.)
        call d2%set_line_color(CLR_GREEN)

        call plt%push(d1)
        call plt%push(d2)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end program
