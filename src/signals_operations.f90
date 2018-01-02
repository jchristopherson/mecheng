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
    module function downsample(x, fs, factor)
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
        real(real64), allocatable, dimension(:) :: work

        ! Define the workspace array
        n = size(x)
        work = x

        ! Apply a low-pass filter to the data to act as an antialiasing filter
        nyquist = half * fs / factor
        call low_pass_filter(work, fs, nyquist)

        ! Collect the output
        y = work(1:factor:n)
    end function

! ------------------------------------------------------------------------------
end submodule