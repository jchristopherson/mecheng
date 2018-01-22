! signals_spectral_analysis.f90

submodule (signals) signals_spectral_analysis
contains
! ------------------------------------------------------------------------------
    module function frequencies(rate, npts) result(f)
        ! Arguments
        real(real64), intent(in) :: rate
        integer(int32), intent(in) :: npts
        real(real64), allocatable, dimension(:) :: f

        ! Parameters
        real(real64), parameter :: one = 1.0d0

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        if (mod(npts, 2) == 0) then
            n = npts / 2 + 1
        else
            n = (npts + 1) / 2
        end if
        allocate(f(n))

        ! Process
        do i = 1, n
            f(i) = rate * (i - one) / npts
        end do
    end function

! ------------------------------------------------------------------------------
    module function signal_magnitude_win(x, winfun, winsize) result(y)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        procedure(window_function), pointer, intent(in) :: winfun
        integer(int32), intent(in) :: winsize
        real(real64), allocatable, dimension(:) :: y

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: two = 2.0d0

        ! Local Variables
        integer(int32) :: i, j, lwork, lwsave, flag, nxfrm
        real(real64), allocatable, dimension(:,:) :: bf
        real(real64), dimension(winsize) :: w
        real(real64), allocatable, dimension(:) :: wsave, work
        real(real64) :: winsum
        complex(real64) :: num

        ! Initialization
        lwork = winsize
        lwsave = winsize + int(log(real(winsize, real64)) / log(two), int32) + 4
        allocate(wsave(lwsave))
        allocate(work(lwork))
        call rfft1i(winsize, wsave, lwsave, flag)
        if (mod(winsize, 2) == 0) then
            nxfrm = winsize / 2 + 1
        else
            nxfrm = (winsize + 1) / 2
        end if
        allocate(y(nxfrm))
        y = zero

        ! Buffer the data, and compute the window function
        bf = buffer(x, winsize)
        winsum = zero
        do i = 1, winsize
            w(i) = winfun(i - 1, winsize)
            winsum = winsum + w(i)
        end do
        winsum = winsum / winsize

        ! Apply the window function, and compute the FFT of each buffer
        do i = 1, size(bf, 2)
            ! Apply the window function
            bf(:,i) = w * bf(:,i)

            ! Compute the FFT - only the positive half
            call rfft1f(winsize, 1, bf(:,i), winsize, wsave, lwsave, work, &
                lwork, flag)
            
            ! Compute the magnitude
            if (mod(winsize, 2) == 0) then
                y(1) = y(1) + abs(bf(1, i) / winsum)
                do j = 2, nxfrm - 1
                    num = cmplx(bf(2*j-2,i), bf(2*j-1,i), real64)
                    y(j) = y(j) + abs(num / winsum)
                end do
                y(nxfrm) = y(i) + abs(bf(winsize, i) / winsum)
            else
                y(1) = y(1) + abs(bf(1, i)) / winsum
                do j = 2, nxfrm
                    num = cmplx(bf(2*j-2,i), bf(2*j-1,i), real64)
                    y(j) = y(j) + abs(num / winsum)
                end do
            end if
        end do
        
        ! Compute the average
        y = y / size(bf, 2)
    end function    

! ------------------------------------------------------------------------------
    module function signal_magnitude_no_win(x) result(y)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        real(real64), allocatable, dimension(:) :: y

        ! Parameters
        real(real64), parameter :: two = 2.0d0

        ! Local Variables
        integer(int32) :: i, n, lwsave, lwork, flag, nxfrm
        real(real64), allocatable, dimension(:) :: s, wsave, work
        complex(real64) :: num

        ! Initialization
        n = size(x)
        lwork = n
        lwsave = n + int(log(real(n, real64)) / log(two), int32) + 4
        allocate(work(lwork))
        allocate(wsave(lwsave))
        allocate(s(n))
        s = x
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, s, n, wsave, lwsave, work, lwork, flag)

        ! Compute the magnitude
        if (mod(n, 2) == 0) then
            nxfrm = n / 2 + 1
            allocate(y(nxfrm))
            y(1) = abs(s(1))
            do i = 2, nxfrm - 1
                num = cmplx(s(2 * i - 2), s(2 * i - 1), real64)
                y(i) = abs(num)
            end do
            y(nxfrm) = abs(s(n))
        else
            nxfrm = (n + 1) / 2
            allocate(y(nxfrm))
            y(1) = abs(s(1))
            do i = 2, nxfrm
                num = cmplx(s(2 * i - 2), s(2 * i - 1), real64)
                y(i) = abs(num)
            end do
        end if
    end function

! ------------------------------------------------------------------------------
end submodule
