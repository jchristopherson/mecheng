! signals_convolution.f90

submodule (signals) signals_convolution
contains
! ------------------------------------------------------------------------------
    module function conv(u, v, err) result(r)
        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        class(errors), intent(inout), optional, target :: err
        real(real64), dimension(size(u)) :: r

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        nu = size(u)
        nv = size(v)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Compute the convolution
        call conv_engine(.true., u, v, r, errmgr)
    end function

! ------------------------------------------------------------------------------
    module function deconv(u, v, err) result(r)
        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        class(errors), intent(inout), optional, target :: err
        real(real64), dimension(size(u)) :: r

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        nu = size(u)
        nv = size(v)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Compute the deconvolution
        call conv_engine(.false., u, v, r, errmgr)
    end function

! ------------------------------------------------------------------------------
    subroutine conv_engine(convolve, u, v, r, err)
        ! Arguments
        logical, intent(in) :: convolve
        real(real64), intent(in), dimension(:) :: u, v
        real(real64), intent(out), dimension(:) :: r
        class(errors), intent(inout) :: err

        ! Local Variables
        integer(int32) :: nu, nv, nv2, n, npts, nz, nzend, flag
        real(real64), allocatable, dimension(:) :: uz, vz, temp
        complex(real64), allocatable, dimension(:) :: cu, cv, q

        ! Initialization
        nu = size(u)
        nv = size(v)
        nv2 = max(nv / 2, 1)
        n = nu + nv2

        ! Local memory allocation
        allocate(uz(n), stat = flag)
        if (flag == 0) allocate(vz(n), stat = flag)
        if (flag /= 0) then
            call err%report_error("conv_engine", &
                "Insufficient memory available.", &
                SIG_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Fill in u and pad the end with zeros
        uz(1:nu) = u
        uz(nu+1:n) = 0.0d0

        ! Populate and wrap around the response vector
        npts = nv - nv2 ! # of values in the upper half of V
        nz = n - nv ! # of zero values in the padded version of V
        nzend = npts + nz + 1
        vz(1:npts) = v(nv2+1:nv)
        vz(npts+1:nzend) = 0.0d0
        vz(nzend+1:n) = v(1:nv2)

        ! Compute the FFTs, and then perform the proper operations
        cu = rfft(uz)
        cv = rfft(vz)
        if (convolve) then
            ! Multiply as this is a convolution operation
            q = cu * cv
        else
            ! Divide as this is a deconvolution operation
            q = cu / cv
        end if

        ! Compute the inverse transform
        temp = irfft(q)

        ! Only return the relevant portion of the transform
        r = temp(1:nu)
    end subroutine

! ------------------------------------------------------------------------------
end submodule
