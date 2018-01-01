! signals_filter.f90

submodule (signals) signals_filter
contains
! ------------------------------------------------------------------------------
    module subroutine low_pass_filter(x, fs, cutoff)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, cutoff

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n, indx, lwsave, lwork, flag
        real(real64) :: df, ndp, fmax
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        df = fs / ndp
        fmax = half * ndp * df
        lwsave = n + int(log(ndp), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Determine where to cut the frequency spectrum
        ! Note: f(i) = (i - 1) * df, such that i = f / df + 1
        ! Also, multiply by 2 due to the storage scheme of FFT
        indx = 2 * (int(cutoff / df + 1, int32))

        ! Zero out everything after INDX
        x(indx:n) = zero

        ! Compute the inverse transform to obtain the filtered signal
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Scale by N as the inverse transform does not perform this operation
        x = x / ndp
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine high_pass_filter(x, fs, cutoff)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, cutoff

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n, indx, lwsave, lwork, flag
        real(real64) :: df, ndp, fmax
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        df = fs / ndp
        fmax = half * ndp * df
        lwsave = n + int(log(ndp), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Determine where to cut the frequency spectrum
        ! Note: f(i) = (i - 1) * df, such that i = f / df + 1
        ! Also, multiply by 2 due to the storage scheme of FFT
        indx = 2 * (int(cutoff / df + 1, int32))

        ! Zero out everything before INDX
        x(1:indx) = zero

        ! Compute the inverse transform to obtain the filtered signal
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Scale by N as the inverse transform does not perform this operation
        x = x / ndp
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine band_pass_filter(x, fs, f1, f2)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, f1, f2
        
        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n, indx1, indx2, lwsave, lwork, flag
        real(real64) :: df, ndp, fmax
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        df = fs / ndp
        fmax = half * ndp * df
        lwsave = n + int(log(ndp), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Determine where to cut the frequency spectrum
        ! Note: f(i) = (i - 1) * df, such that i = f / df + 1
        ! Also, multiply by 2 due to the storage scheme of FFT
        indx1 = 2 * int(f1 / df + 1, int32)
        indx2 = 2 * int(f2 / df + 1, int32)

        ! Zero out everything outside of the band of interest
        x(1:indx1) = zero
        x(indx2:n) = zero

        ! Compute the inverse transform to obtain the filtered signal
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Scale by N as the inverse transform does not perform this operation
        x = x / ndp
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine band_stop_filter(x, fs, f1, f2)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        real(real64), intent(in) :: fs, f1, f2
        
        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: half = 0.5d0

        ! Local Variables
        integer(int32) :: n, indx1, indx2, lwsave, lwork, flag
        real(real64) :: df, ndp, fmax
        real(real64), allocatable, dimension(:) :: wsave, work

        ! Initialization
        n = size(x)
        ndp = real(n, real64)
        df = fs / ndp
        fmax = half * ndp * df
        lwsave = n + int(log(ndp), int32) + 4
        lwork = n
        allocate(wsave(lwsave))
        allocate(work(lwork))

        ! Initialize the FFT
        call rfft1i(n, wsave, lwsave, flag)

        ! Compute the FFT
        call rfft1f(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Determine where to cut the frequency spectrum
        ! Note: f(i) = (i - 1) * df, such that i = f / df + 1
        ! Also, multiply by 2 due to the storage scheme of FFT
        indx1 = 2 * int(f1 / df + 1, int32)
        indx2 = 2 * int(f2 / df + 1, int32)

        ! Zero out the band of interest
        x(indx1:indx2) = zero

        ! Compute the inverse transform to obtain the filtered signal
        call rfft1b(n, 1, x, n, wsave, lwsave, work, lwork, flag)

        ! Scale by N as the inverse transform does not perform this operation
        x = x / ndp
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine averaging_filter(x, npts)
        ! Arguments
        real(real64), intent(inout), dimension(:) :: x
        integer(int32), intent(in) :: npts

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        
        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: dnpts
        real(real64), allocatable, dimension(:) :: buffer

        ! Initialization
        n = size(x)
        dnpts = real(npts, real64)
        allocate(buffer(npts))

        ! Process
        buffer = zero
        do i = 1, n
            ! Scroll the buffer - let a value fall off the back end of the array
            buffer(2:npts) = buffer(1:npts - 1)

            ! Add a new sample value to the buffer
            buffer(1) = x(i)

            ! Compute the average
            x(i) = sum(buffer) / dnpts
        end do
    end subroutine

! ------------------------------------------------------------------------------
end submodule
