! test_signals.f90

! Example Reference:
! https://www.mathworks.com/help/matlab/ref/fft.html?s_tid=srchtitle
program main
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
    real(real64) :: s(npts), dt, t, noise
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
    s = s + noise

    ! Compute the FFT
    xfrm = fft(s)

    ! Extract the meaningful portions of the transform
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

    call plt%push(d1)
    call plt%draw()
end program
