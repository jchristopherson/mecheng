! signals_operations.f90

submodule (signals) signals_operations
contains
! ------------------------------------------------------------------------------
    module function upsample(x, fs, factor) result(y)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(in) :: fs
        integer(int32), intent(in) :: factor
        real(real64), allocatable, dimension(:) :: y
        
        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: nyquist

        ! Define the workspace array by padding with zeros
        n = size(x)
        allocate(y(factor * n))
        y = zero
        do i = 1, n
            y(factor * i) = factor * x(i)
        end do
        
        ! Apply a low-pass filter to the data to smooth the zero values.  The
        ! Nyquist frequency of the original signal makes a good cutoff frequency
        ! for the filter
        nyquist = half * fs
        call low_pass_filter(y, factor * fs, nyquist)
    end function

! ------------------------------------------------------------------------------
    module function downsample(x, fs, factor) result(y)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(in) :: fs
        integer(int32), intent(in) :: factor
        real(real64), allocatable, dimension(:) :: y

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n
        real(real64) :: nyquist
        real(real64), allocatable, dimension(:) :: work

        ! Define the workspace array
        n = size(x)
        work = x

        ! Apply a low-pass filter to the data to act as an antialiasing filter
        nyquist = half * fs / factor
        call low_pass_filter(work, fs, nyquist)

        ! Collect the output
        y = work(1:n:factor)
    end function

! ------------------------------------------------------------------------------
    module subroutine remove_mean(x)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: two = 2.0d0

        ! Local Variables
        integer(int32) :: lwsave, lwork, n, flag
        real(real64) :: ndp
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        lwsave = n + int(log(ndp) / log(two), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Zero out the DC term
        x(1) = zero

        ! Compute the inverse FFT
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)
    end subroutine

! ------------------------------------------------------------------------------
    pure elemental module function is_power_of_two(n) result(x)
        integer(int32), intent(in) :: n
        logical :: x
        if (n == 0) then
            x = .true.
        else
            x = iand(n, n - 1) == 0
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental module function next_power_of_two(n) result(x)
        integer(int32), intent(in) :: n
        integer(int32) :: x
        if (is_power_of_two(n)) then
            x = n + 1
        else
            x = int(ceiling(log(real(n, real64)) / log(2.0d0)), int32)
        end if
    end function

! ------------------------------------------------------------------------------
    pure elemental module function previous_power_of_two(n) result(x)
        integer(int32), intent(in) :: n
        integer(int32) :: x
        if (is_power_of_two(n)) then
            x = n - 1
        else
            x = int(floor(log(real(n, real64)) / log(2.0d0)), int32)
        end if
    end function

! ------------------------------------------------------------------------------
end submodule