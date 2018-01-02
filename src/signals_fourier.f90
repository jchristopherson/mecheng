! signals_fourier.f90

submodule (signals) signals_fourier
contains
! ------------------------------------------------------------------------------
    module function fft(x) result(tf)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: tf

        ! Local Variables
        integer(int32) :: i, n, lwsave, lwork, flag
        real(real64) :: ndp
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: two = 2.0d0

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        lwsave = 2 * n + int(log(ndp) / log(two), int32) + 4
        lwork = 2 * n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the transform
        call cfft1i(n, wsave, lwsave, flag)

        ! Store data for the transform
        allocate(tf(n))
        do i = 1, n
            tf(i) = cmplx(x(i), zero, real64)
        end do

        ! Compute the transform
        call cfft1f(n, 1, tf, n, wsave, lwsave, work, lwork, flag)
    end function
        
! ------------------------------------------------------------------------------
    module function ifft(x) result(tf)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: tf

        ! Local Variables
        integer(int32) :: n, lwsave, lwork, flag
        real(real64) :: ndp
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: two = 2.0d0

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        lwsave = 2 * n + int(log(ndp) / log(two), int32) + 4
        lwork = 2 * n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the transform
        call cfft1i(n, wsave, lwsave, flag)

        ! Store data for the transform
        allocate(tf(n))
        tf = x

        ! Compute the transform
        call cfft1b(n, 1, tf, n, wsave, lwsave, work, lwork, flag)
    end function

! ------------------------------------------------------------------------------
end submodule