! signals_diff.f90

submodule (signals) signals_diff
contains
! ------------------------------------------------------------------------------
! References: 
!   - https://en.wikibooks.org/wiki/Parallel_Spectral_Numerical_Methods/Finding_Derivatives_using_Fourier_Spectral_Methods
!   - https://math.stackexchange.com/questions/740840/derivative-of-function-using-discrete-fourier-transform-matlab
!   - https://math.mit.edu/~stevenj/fft-deriv.pdf
    module function fourier_diff(a, b, y) result(dydx)
        use constants

        ! Arguments
        real(real64), intent(in) :: a, b
        real(real64), intent(in), dimension(:) :: y
        real(real64), dimension(size(y)) :: dydx

        ! Parameters
        complex(real64), parameter :: j = (0.0d0, -1.0d0)
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: n, n2, i, k, m
        real(real64), allocatable, dimension(:) :: yc
        complex(real64), allocatable, dimension(:) :: ffty

        ! Initialization
        n = size(y)

        ! Compute the FFT of Y - only use the positive half as the data is
        ! real valued
        yc = y
        ffty = rfft(yc)

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
            ffty(i) = j * k * ffty(i)
        end do

        ! Compute the inverse transform
        dydx = irfft(ffty)
    end function

! ------------------------------------------------------------------------------
end submodule
