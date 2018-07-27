! test_integration.f90

program main
    use iso_fortran_env
    use constants
    use signals
    implicit none

    ! Parameters
    integer(int32), parameter :: n = 1000
    real(real64), parameter :: a = 0.0d0
    real(real64), parameter :: b = 5.0 * pi

    ! Local Variables
    integer(int32) :: i
    real(real64) :: tol, dx, ans, f1, f2, x(n), y(n)

    ! Build the signal
    dx = (b - a) / (n - 1.0d0)
    do i = 1, n
        x(i) = a + dx * (i - 1.0d0)
        y(i) = sin(x(i)) * cos(2.0d0 * x(i))
    end do

    ! Define a suitable tolerance
    tol = 1.0d1**(-log10(real(n)) - 1)

    ! The integral from a -> b of sin(x) * cos(2*x) is:
    ! cos(3a) / 6 - cos(a) / 2 - cos(3b) / 6 + cos(b) / 2
    ans = cos(3.0d0 * a) / 6.0d0 - cos(a) / 2.0d0 - cos(3.0d0 * b) / 6.0d0 + cos(b) / 2.0d0

    ! Compute the integral via trapezoidal integration
    f1 = trapz_integrate(dx, y)
    f2 = trapz_integrate(x, y)

    ! Test
    if (abs(f1 - ans) > tol) then
        print '(A)', "INTEGRATION TEST 1 FAILED"
        print '(AF8.5AF8.5A)', "Expected ", ans, ", but found ", f1, "."
    else if (abs(f2 - ans) > tol) then
    else
        print '(A)', "INTEGRATION TESTS PASSED"
    end if
end program