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
    module function rfft(x) result(tf)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: tf

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: two = 2.0d0

        ! Local Variables
        integer(int32) :: i, n, lwsave, lwork, flag, nxfrm
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

        ! Determine the appropriate length for the transformed data set
        if (mod(n, 2) == 0) then
            nxfrm = n / 2 + 1
        else
            nxfrm = (n + 1) / 2
        end if
        allocate(tf(nxfrm))

        ! Reposition the data
        if (mod(n, 2) == 0) then
            tf(1) = cmplx(x(1), zero, real64)
            do i = 2, nxfrm - 1
                tf(i) = cmplx(x(2*i-2), x(2*i-1), real64)
            end do
            tf(nxfrm) = cmplx(x(n), zero, real64)
        else
            tf(1) = cmplx(x(1), zero, real64)
            do i = 2, nxfrm
                tf(i) = cmplx(x(2*i-2), x(2*i-1), real64)
            end do
        end if
    end function

! ------------------------------------------------------------------------------
    module function irfft(x) result(tf)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        real(real64), allocatable, dimension(:) :: tf

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: two = 2.0d0

        ! Local Variables
        integer(int32) :: i, n, lwsave, lwork, flag, nxfrm
        real(real64) :: ndp
        real(real64), allocatable, dimension(:) :: wsave, work
        
        ! Initialization
        nxfrm = size(x)

        ! Determine the appropriate length for the transformed data set
        if (mod(nxfrm, 2) == 0) then
            n = 2 * nxfrm - 1
        else
            n = 2 * (nxfrm - 1)
        end if
        allocate(tf(n))

        ! Define workspace variables
        ndp = real(n, real64)
        lwsave = n + int(log(ndp) / log(two), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Reposition the data
        tf(1) = real(x(1), real64)
        if (mod(nxfrm, 2) == 0) then
            do i = 2, nxfrm - 1
                tf(2*i-2) = real(x(i), real64)
                tf(2*i-1) = aimag(x(i))
            end do
        else
            do i = 2, nxfrm - 1
                tf(2*i-2) = real(x(i), real64)
                tf(2*i-1) = aimag(x(i))
            end do
            tf(n) = real(x(n), real64)
        end if

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the inverse FFT
        call rfft1b(n, 1, tf, n, wsave, lwsave, work, lwork, flag)
    end function

! ------------------------------------------------------------------------------
    !> @brief Defines a Bartlett window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    module pure elemental function bartlett_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 1.0d0 - abs(2.0d0 * bin / (winsize - 1.0d0) - 1.0d0)
    end function
    
! ------------------------------------------------------------------------------
    !> @brief Defines a Welch window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    module pure elemental function welch_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 1.0d0 - (2.0d0 * bin / (winsize - 1.0d0) - 1.0d0)**2
    end function
    
! ------------------------------------------------------------------------------
    !> @brief Defines a Hann window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    moudle pure elemental function hann_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 0.5d0 * (1.0d0 - cos(2.0d0 * pi * bin / winsize))
    end function

! ------------------------------------------------------------------------------
    !> @brief Defines a Hamming window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    module pure elemental function hamming_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 0.54d0 - 0.46d0 * cos(2.0d0 * pi * bin / winsize)
    end function

! ------------------------------------------------------------------------------
    !> @brief Defines a Blackman window.
    !!
    !! @param[in] bin The index or bin number (0 <= @p bin <= @p winsize)
    !! @param[in] winsize The window size.
    !! @return The window function value.
    module pure elemental function blackman_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 0.42d0 - 0.5d0 * cos(2.0d0 * pi * bin / (winsize - 1.0d0) + &
            8.0d-2 * cos(4.0d0 * pi * bin / (winsize - 1.0d0)))
    end function

end submodule