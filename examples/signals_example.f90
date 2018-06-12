! signals_example.f90

program example
    use, intrinsic :: iso_fortran_env
    use constants
    use signals
    use fplot_core
    implicit none

    ! Routines
    call test_fft()
    call test_lowpass_filter()
    call test_highpass_filter()
    call test_bandpass_filter()
    call test_bandstop_filter()
    call test_averaging_filter()
    call test_resampling()
    call test_remove_mean()

    if (.not.test_buffer()) then
        print '(A)', "SIGNALS TESTS FAILED."
    else
        print '(A)', "SIGNALS TESTS PASSED."
    end if

contains
! ------------------------------------------------------------------------------
    ! Example Reference:
    ! https://www.mathworks.com/help/matlab/ref/fft.html?s_tid=srchtitle
    subroutine test_fft()
        ! Parameters
        real(real64), parameter :: fs = 1.0d3
        integer(int32), parameter :: npts = 1500

        ! Local Variables
        integer(int32) :: i
        real(real64) :: s(npts), dt, t, noise(npts)
        complex(real64), allocatable, dimension(:) :: xfrm, rxfrm
        real(real64), allocatable, dimension(:) :: p1, f1, p2
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2
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

        ! Use a real-valued transform
        rxfrm = rfft(s) ! s is modified by this call

        ! Extract the meaningful portions of the transform
        ! NOTE: factor of 2 due to symmetry of the transform
        p1 = 2.0d0 * abs(xfrm(1:npts/2 + 1))

        ! Notice, we do not need to multiply the real-valued transform by 2 as
        ! the transform applies the correct scaling.
        p2 = abs(rxfrm)

        ! Compute a meaningful frequency axis
        allocate(f1(size(p1)))
        do i = 1, size(p1)
            f1(i) = fs * (i - 1.0d0) / npts
        end do

        ! Plot the results
        call plt%initialize()
        call plt%set_title("FFT Example")

        xAxis => plt%get_x_axis()
        call xAxis%set_title("Frequency [Hz]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("Amplitude")

        call d1%define_data(f1, p1)
        call d1%set_name("Complex-Valued FFT")
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(f1, p2)
        call d2%set_name("Real-Valued FFT")
        call d2%set_line_color(CLR_GREEN)
        call d2%set_line_style(LINE_DASHED)
        call d2%set_line_width(2.0)

        call plt%push(d1)
        call plt%push(d2)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    ! Low Pass Filter Test
    subroutine test_lowpass_filter()
        ! Parameters
        real(real64), parameter :: fs = 1024.0d0
        integer(int32), parameter :: npts = 4096
        real(real64), parameter :: cutoff = 25.0d0

        ! Local Variables
        integer(int32) :: i
        real(real64) :: s(npts), sfilt(npts), t(npts), dt, expect(npts)
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2, d3
        class(plot_axis), pointer :: xAxis, yAxis

        ! Signal Initialization
        dt = 1.0d0 / fs
        t(1) = 0.0d0
        do i = 1, npts
            s(i) = sin(2.0d0 * pi * 25.0d0 * t(i)) + &
                cos(2.0d0 * pi * 26.0d0 * t(i)) + &
                0.25 * sin(2.0d0 * pi * 120.0d0 * t(i))
            sfilt(i) = s(i)
            expect(i) = sin(2.0d0 * pi * 25.0d0 * t(i))
            if (i < npts) t(i+1) = t(i) + dt
        end do

        ! Filter the signal
        call low_pass_filter(sfilt, fs, cutoff)

        ! Plot the signal, along with the original
        call plt%initialize()
        call plt%set_title("Low Pass Filter Example")

        xAxis => plt%get_x_axis()
        call xAxis%set_title("Time [s]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("s(t)")

        call d1%define_data(t, s)
        call d1%set_name("Unfiltered")
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(t, sfilt)
        call d2%set_name("Filtered")
        call d2%set_line_color(CLR_GREEN)
        call d2%set_line_width(2.0)

        call d3%define_data(t, expect)
        call d3%set_name("Expected")
        call d3%set_line_color(CLR_RED)
        call d3%set_line_style(LINE_DASHED)
        call d3%set_line_width(2.0)

        call plt%push(d1)
        call plt%push(d2)
        call plt%push(d3)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    ! High Pass Filter Test
    subroutine test_highpass_filter()
        ! Parameters
        real(real64), parameter :: fs = 1024.0d0
        integer(int32), parameter :: npts = 4096
        real(real64), parameter :: cutoff = 25.0d0

        ! Local Variables
        integer(int32) :: i
        real(real64) :: s(npts), sfilt(npts), t(npts), dt, expect(npts)
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2, d3
        class(plot_axis), pointer :: xAxis, yAxis

        ! Signal Initialization
        dt = 1.0d0 / fs
        t(1) = 0.0d0
        do i = 1, npts
            s(i) = sin(2.0d0 * pi * 25.0d0 * t(i)) + &
                cos(2.0d0 * pi * 26.0d0 * t(i)) + &
                0.25 * sin(2.0d0 * pi * 120.0d0 * t(i))
            sfilt(i) = s(i)
            expect(i) = cos(2.0d0 * pi * 26.0d0 * t(i)) + &
                0.25 * sin(2.0d0 * pi * 120.0d0 * t(i))
            if (i < npts) t(i+1) = t(i) + dt
        end do

        ! Filter the signal
        call high_pass_filter(sfilt, fs, cutoff)

        ! Plot the signal, along with the original
        call plt%initialize()
        call plt%set_title("High Pass Filter Example")

        xAxis => plt%get_x_axis()
        call xAxis%set_title("Time [s]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("s(t)")

        call d1%define_data(t, s)
        call d1%set_name("Unfiltered")
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(t, sfilt)
        call d2%set_name("Filtered")
        call d2%set_line_color(CLR_GREEN)
        call d2%set_line_width(2.0)

        call d3%define_data(t, expect)
        call d3%set_name("Expected")
        call d3%set_line_color(CLR_RED)
        call d3%set_line_style(LINE_DASHED)
        call d3%set_line_width(2.0)

        call plt%push(d1)
        call plt%push(d2)
        call plt%push(d3)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    ! Band Pass Filter Test
    subroutine test_bandpass_filter()
        ! Parameters
        real(real64), parameter :: fs = 1024.0d0
        integer(int32), parameter :: npts = 4096
        real(real64), parameter :: f1 = 24.5d0
        real(real64), parameter :: f2 = 25.5d0

        ! Local Variables
        integer(int32) :: i
        real(real64) :: s(npts), sfilt(npts), t(npts), dt, expect(npts)
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2, d3
        class(plot_axis), pointer :: xAxis, yAxis

        ! Signal Initialization
        dt = 1.0d0 / fs
        t(1) = 0.0d0
        do i = 1, npts
            s(i) = sin(2.0d0 * pi * 25.0d0 * t(i)) + &
                cos(2.0d0 * pi * 26.0d0 * t(i)) + &
                0.25 * sin(2.0d0 * pi * 120.0d0 * t(i))
            sfilt(i) = s(i)
            expect(i) = sin(2.0d0 * pi * 25.0d0 * t(i))
            if (i < npts) t(i+1) = t(i) + dt
        end do

        ! Filter the signal
        call band_pass_filter(sfilt, fs, f1, f2)

        ! Plot the signal, along with the original
        call plt%initialize()
        call plt%set_title("Band Pass Filter Example")

        xAxis => plt%get_x_axis()
        call xAxis%set_title("Time [s]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("s(t)")

        call d1%define_data(t, s)
        call d1%set_name("Unfiltered")
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(t, sfilt)
        call d2%set_name("Filtered")
        call d2%set_line_color(CLR_GREEN)
        call d2%set_line_width(2.0)

        call d3%define_data(t, expect)
        call d3%set_name("Expected")
        call d3%set_line_color(CLR_RED)
        call d3%set_line_style(LINE_DASHED)
        call d3%set_line_width(2.0)

        call plt%push(d1)
        call plt%push(d2)
        call plt%push(d3)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    ! Band Stop Filter Test
    subroutine test_bandstop_filter()
        ! Parameters
        real(real64), parameter :: fs = 1024.0d0
        integer(int32), parameter :: npts = 4096
        real(real64), parameter :: f1 = 24.5d0
        real(real64), parameter :: f2 = 25.5d0

        ! Local Variables
        integer(int32) :: i
        real(real64) :: s(npts), sfilt(npts), t(npts), dt, expect(npts)
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2, d3
        class(plot_axis), pointer :: xAxis, yAxis

        ! Signal Initialization
        dt = 1.0d0 / fs
        t(1) = 0.0d0
        do i = 1, npts
            s(i) = sin(2.0d0 * pi * 25.0d0 * t(i)) + &
                cos(2.0d0 * pi * 26.0d0 * t(i)) + &
                0.25d0 * sin(2.0d0 * pi * 120.0d0 * t(i))
            sfilt(i) = s(i)
            expect(i) = cos(2.0d0 * pi * 26.0d0 * t(i)) + &
                0.25d0 * sin(2.0d0 * pi * 120.0d0 * t(i))
            if (i < npts) t(i+1) = t(i) + dt
        end do

        ! Filter the signal
        call band_stop_filter(sfilt, fs, f1, f2)

        ! Plot the signal, along with the original
        call plt%initialize()
        call plt%set_title("Band Stop Filter Example")

        xAxis => plt%get_x_axis()
        call xAxis%set_title("Time [s]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("s(t)")

        call d1%define_data(t, s)
        call d1%set_name("Unfiltered")
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(t, sfilt)
        call d2%set_name("Filtered")
        call d2%set_line_color(CLR_GREEN)
        call d2%set_line_width(2.0)

        call d3%define_data(t, expect)
        call d3%set_name("Expected")
        call d3%set_line_color(CLR_RED)
        call d3%set_line_style(LINE_DASHED)
        call d3%set_line_width(2.0)

        call plt%push(d1)
        call plt%push(d2)
        call plt%push(d3)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    ! Averaging Filter Test
    subroutine test_averaging_filter()
        ! Parameters
        real(real64), parameter :: fs = 1024.0d0
        integer(int32), parameter :: npts = 4096
        integer(int32), parameter :: filtsize = 50

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
        call averaging_filter(sfilt, filtsize)

        ! Plot the signal, along with the original
        call plt%initialize()
        call plt%set_title("Averaging Filter Example")

        xAxis => plt%get_x_axis()
        call xAxis%set_title("Time [s]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("s(t)")

        call d1%define_data(t, s)
        call d1%set_name("Unfiltered")
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(t, sfilt)
        call d2%set_name("Filtered")
        call d2%set_line_color(CLR_GREEN)

        call plt%push(d1)
        call plt%push(d2)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    ! Resampling Test
    subroutine test_resampling()
        ! Parameters
        real(real64), parameter :: fs = 1024.0d0
        integer(int32), parameter :: n = 1000
        integer(int32), parameter :: factor = 4

        ! Local Variables
        integer(int32) :: i
        real(real64) :: dt, t(n), tu(factor * n), td(n / factor), s(n)
        real(real64), allocatable, dimension(:) :: su, sd
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2, d3
        class(plot_axis), pointer :: xAxis, yAxis

        ! Generate the signal
        t(1) = 0.0d0
        dt = 1.0d0 / fs
        do i = 1, n
            s(i) = sin(2.0d0 * pi * 5.0d0 * t(i)) + &
                sin(2.0d0 * pi * 50.0d0 * t(i))
            if (i < n) t(i+1) = t(i) + dt
        end do

        ! Generate the upsampled & downsampled time vectors
        tu(1) = 0.0d0
        dt = 1.0d0 / (fs * factor)
        do i = 2, size(tu)
            tu(i) = tu(i-1) + dt
        end do

        td(1) = 0.0d0
        dt = factor / fs
        do i = 2, size(td)
            td(i) = td(i-1) + dt
        end do

        ! Upsample the signal
        su = upsample(s, fs, factor)

        ! Downsample the original signal
        sd = downsample(s, fs, factor)

        ! Plot the signal, along with the original
        call plt%initialize()
        call plt%set_title("Resampling Example")

        xAxis => plt%get_x_axis()
        call xAxis%set_title("Time [s]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("s(t)")

        call d1%define_data(t, s)
        call d1%set_name("Original")
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(tu, su)
        call d2%set_name("Upsampled")
        call d2%set_line_color(CLR_GREEN)

        call d3%define_data(td, sd)
        call d3%set_name("Downsampled")
        call d3%set_line_color(CLR_RED)

        call plt%push(d1)
        call plt%push(d2)
        call plt%push(d3)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    subroutine test_remove_mean()
        ! Parameters
        real(real64), parameter :: fs = 1024.0d0
        integer(int32), parameter :: n = 1000

        ! Local Variables
        integer(int32) :: i
        real(real64) :: dt, t(n), s(n), s1(n), so(n)
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1, d2, d3
        class(plot_axis), pointer :: xAxis, yAxis

        ! Create the signal
        t(1) = 0.0d0
        dt = 1.0d0 / fs
        do i = 1, n
            so(i) = sin(2.0d0 * pi * 5.0d0 * t(i)) + &
                sin(2.0d0 * pi * 50.0d0 * t(i))
            s(i) = so(i) + 1.5d0
            s1(i) = s(i)
            if (i < n) t(i+1) = t(i) + dt
        end do

        ! Remove the mean on s1
        call remove_mean(s1)

        ! Plot the signal, along with the original
        call plt%initialize()
        call plt%set_title("Remove Mean Example")

        xAxis => plt%get_x_axis()
        call xAxis%set_title("Time [s]")

        yAxis => plt%get_y_axis()
        call yAxis%set_title("s(t)")

        call d1%define_data(t, s)
        call d1%set_name("Original")
        call d1%set_line_color(CLR_BLUE)

        call d2%define_data(t, s1)
        call d2%set_name("Mean Removed")
        call d2%set_line_color(CLR_GREEN)

        call d3%define_data(t, so)
        call d3%set_name("Expected")
        call d3%set_line_color(CLR_RED)
        call d3%set_line_style(LINE_DASHED)
        call d3%set_line_width(2.0)

        call plt%push(d1)
        call plt%push(d2)
        call plt%push(d3)
        call plt%draw()
    end subroutine

! ------------------------------------------------------------------------------
    function test_buffer() result(rst)
        ! Local Variables
        logical :: rst
        integer(int32) :: i
        real(real64) :: x1(21), x2(19), ans1(4, 5)
        real(real64), allocatable, dimension(:,:) :: s1

        ! Initialization
        rst = .true.
        do i = 1, size(x1)
            x1(i) = real(i, real64)
        end do
        do i = 1, size(x2)
            x2(i) = real(i, real64)
        end do

        ! Define the solution matrix for test 1 - no overlap
        ans1 = reshape(x1, [4, 5])

        ! Test 1
        s1 = buffer(x1, 9)
        print '(AI0AI0)', "S1 ROWS: ", size(s1, 1), ", COLUMNS: ", size(s1, 2)
        do i = 1, size(s1, 1)
            print *, s1(i,:)
        end do
    end function

! ------------------------------------------------------------------------------
end program
