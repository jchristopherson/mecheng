! signals_convolution.f90

submodule (signals) signals_convolution
contains
! ------------------------------------------------------------------------------
    module function conv(u, v, sol, err) result(r)
        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        integer(int32), intent(in), optional :: sol
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:) :: r

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: nu, nv, n, n2, flag, s
        real(real64), allocatable, dimension(:) :: uz, vz
        complex(real64), allocatable, dimension(:) :: temp, tu, tv, q
        
        ! Initialization
        s = SIG_FULL_CONVOLUTION
        if (present(sol)) then
            s = sol
        end if
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        nu = size(u)
        nv = size(v)
        n = nu + nv - 1
        n2 = 2**next_power_of_two(n)

        ! Input Validation
        if (s /= SIG_FULL_CONVOLUTION .and. &
            s /= SIG_VALID_CONVOLUTION .and. &
            s /= SIG_SAME_CONVOLUTION) &
        then
            ! Reset to a default, and warn the user
            call errmgr%report_warning("conv", &
                "Unknown convolution results flag requested.  " // &
                "Defaulting to a full solution.", &
                SIG_INVALID_INPUT_ERROR)
            s = SIG_FULL_CONVOLUTION
        end if

        ! Local Memory Allocation
        allocate(uz(n2), stat = flag)
        if (flag == 0) allocate(vz(n2), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("conv", &
                "Insufficient memory available.", &
                SIG_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Pad with zeros, as necessary
        uz(1:nu) = u
        uz(nu+1:n2) = 0.0d0
        vz(1:nv) = v
        vz(nv+1:n2) = 0.0d0

        ! Compute the FFT's, and then perform the convolution
        tu = fft(uz)
        tv = fft(vz)
        q = tu * tv

        ! Compute the inverse transform to obtain the full solution
        temp = ifft(q)

        ! Determine which solution the user is after
        if (s == SIG_VALID_CONVOLUTION) then
            ! Only the nu + nv - 1 element solution is returned
            n = nv / 2 + 1
            r = real(temp(n:n+nu-1))
        else if (s == SIG_SAME_CONVOLUTION) then
            ! An NU element solution is returned
            r = real(temp(1:nu))
        else
            ! The full solution is returned
            r = real(temp(1:n))
        end if
    end function

! ------------------------------------------------------------------------------
    module function deconv(u, v, err) result(r)
        use linalg_core, only : solve_least_squares

        ! Arguments
        real(real64), intent(in), dimension(:) :: u, v
        class(errors), intent(inout), optional, target :: err
        real(real64), dimension(size(u)) :: r

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
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
        integer(int32) :: nu, nv, nv2, n, flag
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

        ! Fill in u and v, and pad the end with zeros
        uz(1:nu) = u
        uz(nu+1:n) = 0.0d0
        vz(1:nv) = v
        vz(nv+1:n) = 0.0d0

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
