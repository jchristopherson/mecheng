! signals_diff.f90

submodule (signals) signals_diff
contains
! ------------------------------------------------------------------------------
! References: 
!   - https://en.wikibooks.org/wiki/Parallel_Spectral_Numerical_Methods/Finding_Derivatives_using_Fourier_Spectral_Methods
!   - https://math.stackexchange.com/questions/740840/derivative-of-function-using-discrete-fourier-transform-matlab
!   - https://math.mit.edu/~stevenj/fft-deriv.pdf
    module function fourier_diff_a(a, b, y) result(dydx)
        use constants

        ! Arguments
        real(real64), intent(in) :: a, b
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: dydx

        ! Parameters
        complex(real64), parameter :: j = (0.0d0, 1.0d0)
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: n, n2, i, k, m
        complex(real64), allocatable, dimension(:) :: ffty

        ! Initialization
        n = size(y)

        ! Compute the FFT of Y - only use the positive half as the data is
        ! real valued
        ffty = rfft(y)

        ! Compute the derivative
        n2 = size(ffty)
        if (mod(n, 2) == 0) then
            m = n2 - 1
            ffty(n2) = zero
        else
            m = n2
        end if
        do i = 1, m
            k = -2.0d0 * pi * (i - 1.0d0) / (b - a)
            ffty(i) = j * k * ffty(i)
        end do

        ! Compute the inverse transform
        dydx = irfft(ffty)
    end function

! ------------------------------------------------------------------------------
    module function fourier_diff_b(x, y) result(dydx)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), dimension(size(y)) :: dydx

        ! Local Variables
        real(real64) :: dx, a, b

        ! Process
        dx = x(2) - x(1)
        a = x(1)
        b = x(size(x)) + dx
        dydx = fourier_diff_a(a, b, y)
    end function

! ------------------------------------------------------------------------------
    module function fourier_diff_c(dx, y) result(dydx)
        ! Arguments
        real(real64), intent(in) :: dx
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: dydx

        ! Local Variables
        real(real64) :: xMax, a, b
        integer(int32) :: n

        ! Process
        n = size(y)
        a = 0.0d0
        xMax = dx * (n - 1.0d0)
        b = xMax + dx
        dydx = fourier_diff_a(a, b, y)
    end function

! ------------------------------------------------------------------------------
    module function fourier_diff2_a(a, b, y) result(d2ydx2)
        use constants

        ! Arguments
        real(real64), intent(in) :: a, b
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: d2ydx2

        ! Parameters
        complex(real64), parameter :: j = (0.0d0, 1.0d0)
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: n, n2, i, k, m
        complex(real64), allocatable, dimension(:) :: ffty

        ! Initialization
        n = size(y)

        ! Compute the FFT of Y - only use the positive half as the data is
        ! real valued
        ffty = rfft(y)

        ! Compute the derivative
        n2 = size(ffty)
        if (mod(n, 2) == 0) then
            m = n2 - 1
            ffty(n2) = zero
        else
            m = n2
        end if
        do i = 1, m
            k = 2.0d0 * pi * (i - 1.0d0) / (b - a)
            ffty(i) = -k**2 * ffty(i)
        end do

        ! Compute the inverse transform
        d2ydx2 = irfft(ffty)
    end function

! ------------------------------------------------------------------------------
    module function fourier_diff2_b(x, y) result(dydx)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), dimension(size(y)) :: dydx

        ! Local Variables
        real(real64) :: dx, a, b

        ! Process
        dx = x(2) - x(1)
        a = x(1)
        b = x(size(x)) + dx
        dydx = fourier_diff2_a(a, b, y)
    end function

! ------------------------------------------------------------------------------
    module function fourier_diff2_c(dx, y) result(dydx)
        ! Arguments
        real(real64), intent(in) :: dx
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: dydx

        ! Local Variables
        real(real64) :: xMax, a, b
        integer(int32) :: n

        ! Process
        n = size(y)
        a = 0.0d0
        xMax = dx * (n - 1.0d0)
        b = xMax + dx
        dydx = fourier_diff2_a(a, b, y)
    end function

! ------------------------------------------------------------------------------
    module function finite_diff_a(x, y) result(dydx)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), dimension(size(y)) :: dydx

        ! Local Variables
        integer(int32) :: n

        ! Initialization
        n = min(size(x), size(y))

        ! Process
        dydx(1) = (y(2) - y(1)) / (x(2) - x(1))
        do i = 2, n - 1
            dydx(i) = (y(i+1) - y(i-1)) / (x(i+1) - x(i-1))
        end do
        dydx(n) = (y(n) - y(n-1)) / (x(n) - x(n-1))
    end function

! ------------------------------------------------------------------------------
    module function finite_diff_b(dx, y) result(dydx)
        ! Arguments
        real(real64), intent(in) :: dx
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: dydx

        ! Local Variables
        integer(int32) :: n
        real(real64) :: dx2

        ! Initialization
        n = size(y)
        dx2 = 2.0d0 * dx

        ! Process
        dydx(1) = (y(2) - y(1)) / dx
        do i = 2, n - 1
            dydx(i) = (y(i+1) - y(i-1)) / dx2
        end do
        dydx(n) = (y(n) - y(n-1)) / dx
    end function

end submodule
