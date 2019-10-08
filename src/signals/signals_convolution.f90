! signals_convolution.f90

submodule (signals) signals_convolution
contains
! ------------------------------------------------------------------------------
    module function conv(u, v) result(r)
        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        real(real64), allocatable, dimension(:) :: r

        ! Local Variables
        integer(int32) :: nu, nv
        real(real64), allocatable, dimension(:) :: vz
        complex(real64), allocatable, dimension(:) :: ufft, vfft, q

        ! Initialization
        nu = size(u)
        nv = size(v)

        ! Ensure nv <= nu

        ! Compute the FFT's of each data set
        ufft = rfft(u)
        if (nu /= nv) then
            ! Pad with zeros
            allocate(vz(nu))
            vz(1:nv) = v
            vz(nv+1:nu) = 0.0d0
            vfft = rfft(vz)
        else
            vfft = rfft(v)
        end if

        ! Multiply, and then perform the inverse Fourier transform
        q = ufft * vfft
        r = irfft(q)
    end function

! ------------------------------------------------------------------------------
    module function deconv(u, v) result(r)
        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        real(real64), allocatable, dimension(:) :: r

        ! Local Variables
        integer(int32) :: nu, nv
        real(real64), allocatable, dimension(:) :: vz
        complex(real64), allocatable, dimension(:) :: ufft, vfft, q

        ! Initialization
        nu = size(u)
        nv = size(v)

        ! Ensure nv <= nu

        ! Compute the FFT's of each data set
        ufft = rfft(u)
        if (nu /= nv) then
            ! Pad with zeros
            allocate(vz(nu))
            vz(1:nv) = v
            vz(nv+1:nu) = 0.0d0
            vfft = rfft(vz)
        else
            vfft = rfft(v)
        end if

        ! Divide, and then perform the inverse Fourier transform
        q = ufft / vfft
        r = irfft(q)
    end function

! ------------------------------------------------------------------------------
end submodule
