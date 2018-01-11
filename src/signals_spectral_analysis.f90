! signals_spectral_analysis.f90

submodule (signals) signals_spectral_analysis
contains
! ------------------------------------------------------------------------------
    module subroutine sa_init(this, nseg)
        ! Arguments
        class(spectral_analyzer), intent(inout) :: this
        integer(int32), intent(in) :: nseg

        ! Parameters
        real(real64), parameter :: zero = 0.0d0
        real(real64), parameter :: two = 2.0d0

        ! Local Variables
        integer(int32) :: lwsave, flag
        real(real64) :: ndp

        ! Initialization
        ndp = real(nseg, real64)
        lwsave = nseg + int(log(ndp) / log(two), int32) + 4
        this%m_initialized = .false.
        this%m_segments = 0
        this%m_segmentLength = nseg
        if (mod(nseg, 2) == 0) then
            this%m_spectrumLength = nseg / 2 + 1
        else
            this%m_spectrumLength = (nseg + 1) / 2
        end if
        if (allocated(this%m_sum)) deallocate(this%m_sum)
        if (allocated(this%m_params)) deallocate(this%m_params)
        if (allocated(this%m_work)) deallocate(this%m_work)
        if (allocated(this%m_temp)) deallocate(this%m_temp)
        allocate(this%m_sum(this%m_spectrumLength))
        allocate(this%m_params(lwsave))
        allocate(this%m_work(nseg))
        allocate(this%m_temp(nseg))
        this%m_sum = zero
        this%m_initialized = .true.
        call rfft1i(nseg, this%m_params, lwsave, flag)
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine sa_add(this, x, window)
        ! Arguments
        class(spectral_analyzer), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x
        procedure(window_function), intent(in), pointer :: window

        ! Parameters
        real(real64), parameter :: zero = 0.0d0

        ! Local Variables
        integer(int32) :: i, n, m, lwsave, lwork, flag
        real(real64) :: w, sumw
        complex(real64) :: num

        ! Initialization
        n = size(x)
        if (.not.this%m_initialized) call this%initialize(n)

        ! Load the data, and apply the window function
        sumw = zero
        do i = 1, n
            w = window(i - 1, n)
            this%m_temp(i) = w * x(i)
            sumw = sumw + w**2
        end do
        
        ! Compute the FFT
        lwsave = size(this%m_params)
        lwork = size(this%m_work)
        call rfft1f(n, 1, this%m_temp, n, this%m_params, lwsave, this%m_work, &
            lwork, flag)
        
        ! Store the FFT results
        if (mod(n, 2) == 0) then
            m = n / 2 + 1
            this%m_sum(1) = this%m_sum(1) + abs(this%m_temp(1))**2 / sumw
            do i = 2, m - 1
                num = cmplx(x(2*i-2), x(2*i-1), real64)
                this%m_sum(i) = this%m_sum(i) + abs(num)**2 / sumw
            end do
            this%m_sum(m) = this%m_sum(m) + abs(x(m))**2 / sumw
        else
            m = (n + 1) / 2
            this%m_sum(1) = this%m_sum(1) + abs(this%m_temp(1))**2 / sumw
            do i = 2, m
                num = cmplx(x(2*i-2), x(2*i-1), real64)
                this%m_sum(i) = this%m_sum(i) + abs(num)**2 / sumw
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
