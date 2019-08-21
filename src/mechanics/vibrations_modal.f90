! vibrations_modal.f90

submodule (vibrations) vibrations_modal
contains
! ------------------------------------------------------------------------------
    module function compute_modal_response(m, k, err) result(rst)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: m, k
        class(errors), intent(inout), optional, target :: err
        type(modal_information), allocatable, dimension(:) :: rst

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: i, j, n, flag, ind
        real(real64), allocatable, dimension(:,:) :: mc, kc
        complex(real64), allocatable, dimension(:) :: vals
        complex(real64), allocatable, dimension(:,:) :: vecs
        real(real64) :: val

        ! Function Prototypes
        interface 
            function IDAMAX(n, x, incx) result(rst)
                use iso_fortran_env
                integer(int32), intent(in) :: n, incx
                real(real64), intent(in) :: x(n)
                integer(int32) :: rst
            end function
        end interface
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        n = size(m, 1)

        ! Input Checking
        if (size(m, 2) /= n) then
            call errmgr%report_error("compute_modal_response", &
                "The mass matrix must be square.", &
                MECH_ARRAY_SIZE_ERROR)
            return
        end if
        if (size(k, 1) /= n .or. size(k, 2) /= n) then
            call errmgr%report_error("compute_modal_response", &
                "The stiffness matrix must be the same size " // &
                "as the mass matrix.", MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Memory Allocation
        allocate(mc(n,n), stat = flag)
        if (flag == 0) allocate(kc(n,n), stat = flag)
        if (flag == 0) allocate(vals(n), stat = flag)
        if (flag == 0) allocate(vecs(n,n), stat = flag)
        if (flag == 0) allocate(rst(n), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("compute_modal_response", &
                "There is insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Copy the input matrices to avoid overwriting their contents
        do j = 1, n
            do i = 1, n
                kc(i,j) = k(i,j)
                mc(i,j) = m(i,j)
            end do
        end do

        ! Compute the eigenvalues and eigenvectors
        call eigen(kc, mc, vals, vecs = vecs, err = errmgr)
        if (errmgr%has_error_occurred()) return

        ! Sort the eigenvalues and eigenvectors
        call sort(vals, vecs, .true.)

        ! Report the results
        do i = 1, n
            rst(i)%frequency = sqrt(real(vals(i), real64)) / (2.0d0 * pi)
            rst(i)%mode_shape = real(vecs(:,i), real64)

            ! Normalize the mode shape vector to it's largest magnitude component
            ind = IDAMAX(n, rst(i)%mode_shape, 1)
            val = rst(i)%mode_shape(ind)
            rst(i)%mode_shape = rst(i)%mode_shape / val
        end do
    end function

! ------------------------------------------------------------------------------
    module function compute_frequency_response_1(m, k, b, f, freq, err) result(rsp)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: m, k, b
        complex(real64), intent(in), dimension(:,:) :: f
        real(real64), intent(in), dimension(:) :: freq
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:,:) :: rsp

        ! Parameters
        complex(real64), parameter :: ci = (0.0d0, 1.0d0)

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, j, nfreq, neqn, flag
        real(real64), allocatable, dimension(:,:) :: mc, kc
        complex(real64), allocatable, dimension(:) :: vals, s
        complex(real64), allocatable, dimension(:,:) :: vecs, &
            vectrans, mm, km, bm, fm, q
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        neqn = size(m, 1)
        nfreq = size(freq)

        ! Array Size Check
        if (size(m, 2) /= neqn) then
            call errmgr%report_error("compute_frequency_response_1", &
                "The mass matrix is not square.", MECH_ARRAY_SIZE_ERROR)
            return
        end if

        if (size(k, 1) /= neqn .or. size(k, 2) /= neqn) then
            write(errmsg, '(AI0AI0AI0AI0A)') &
                "The stiffness matrix is not sized properly.  Expected a ", &
                neqn, "-by-", neqn, " matrix, but found a ", size(k, 1), &
                "-by-", size(k, 2), " matrix."
            call errmgr%report_error("compute_frequency_response_1", &
                trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if

        if (size(b, 1) /= neqn .or. size(b, 2) /= neqn) then
            write(errmsg, '(AI0AI0AI0AI0A)') &
                "The damping matrix is not sized properly.  Expected a ", &
                neqn, "-by-", neqn, " matrix, but found a ", size(b, 1), &
                "-by-", size(b, 2), " matrix."
            call errmgr%report_error("compute_frequency_response_1", &
                trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Local Memory Allocation
        allocate(mc(neqn, neqn), stat = flag)
        if (flag == 0) allocate(kc(neqn, neqn), stat = flag)
        if (flag == 0) allocate(vals(neqn), stat = flag)
        if (flag == 0) allocate(vecs(neqn, neqn), stat = flag)
        if (flag == 0) allocate(q(nfreq, neqn), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("compute_frequency_response_1", &
                "Insufficient memory available.", MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Create a copy of the mass and stiffness matrices such that the
        ! eigen solver does not overwrite the originals
        do j = 1, neqn
            do i = 1, neqn
                mc(i,j) = m(i,j)
                kc(i,j) = k(i,j)
            end do
        end do

        ! Compute the eigenvalues and eigenvectors
        call eigen(kc, mc, vals, vecs = vecs, err = errmgr)
        if (errmgr%has_error_occurred()) return

        ! Compute the modal matrices
        vectrans = transpose(vecs)
        mm = matmul(vectrans, matmul(m, vecs))
        km = matmul(vectrans, matmul(k, vecs))
        bm = matmul(vectrans, matmul(b, vecs))
        fm = matmul(f, vecs)

        ! Compute the complex frequency term
        s = ci * freq

        ! Compute the FRF of each decoupled equation
        do j = 1, neqn
            q(:,j) = fm(:,j) / (mm(j,j) * s**2 + bm(j,j) * s + km(j,j))
        end do

        ! Convert back to the original, coupled configuration
        rsp = matmul(q, vectrans)
    end function

! ------------------------------------------------------------------------------
    module function compute_frequency_response_2(m, k, zeta, f, freq, err) result(rsp)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: m, k
        complex(real64), intent(in), dimension(:,:) :: f
        real(real64), intent(in), dimension(:) :: zeta, freq
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:,:) :: rsp

        ! Parameters
        complex(real64), parameter :: ci = (0.0d0, 1.0d0)

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, j, nfreq, neqn, flag
        real(real64), allocatable, dimension(:,:) :: mc, kc
        complex(real64), allocatable, dimension(:) :: vals, s
        complex(real64), allocatable, dimension(:,:) :: vecs, &
            vectrans, mm, km, fm, q
        complex(real64) :: b
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        neqn = size(m, 1)
        nfreq = size(freq)

        ! Array Size Check
        if (size(m, 2) /= neqn) then
            call errmgr%report_error("compute_frequency_response_2", &
                "The mass matrix is not square.", MECH_ARRAY_SIZE_ERROR)
            return
        end if

        if (size(k, 1) /= neqn .or. size(k, 2) /= neqn) then
            write(errmsg, '(AI0AI0AI0AI0A)') &
                "The stiffness matrix is not sized properly.  Expected a ", &
                neqn, "-by-", neqn, " matrix, but found a ", size(k, 1), &
                "-by-", size(k, 2), " matrix."
            call errmgr%report_error("compute_frequency_response_2", &
                trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if

        if (size(zeta) /= neqn) then
            write(errmsg, '(AI0AI0A)') &
                "The damping ratio vector is not sized properly.  " // &
                "Expected an array of ", neqn, &
                " elements, but found an array of ", size(zeta), &
                " elements."
            call errmgr%report_error("compute_frequency_response_2", &
                trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Local Memory Allocation
        allocate(mc(neqn, neqn), stat = flag)
        if (flag == 0) allocate(kc(neqn, neqn), stat = flag)
        if (flag == 0) allocate(vals(neqn), stat = flag)
        if (flag == 0) allocate(vecs(neqn, neqn), stat = flag)
        if (flag == 0) allocate(q(nfreq, neqn), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("compute_frequency_response_2", &
                "Insufficient memory available.", MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Create a copy of the mass and stiffness matrices such that the
        ! eigen solver does not overwrite the originals
        do j = 1, neqn
            do i = 1, neqn
                mc(i,j) = m(i,j)
                kc(i,j) = k(i,j)
            end do
        end do

        ! Compute the eigenvalues and eigenvectors
        call eigen(kc, mc, vals, vecs = vecs, err = errmgr)
        if (errmgr%has_error_occurred()) return

        ! Compute the modal matrices
        vectrans = transpose(vecs)
        mm = matmul(vectrans, matmul(m, vecs))
        km = matmul(vectrans, matmul(k, vecs))
        fm = matmul(f, vecs)

        ! Compute the complex frequency term
        s = ci * freq

        ! Compute the FRF of each decoupled equation
        do j = 1, neqn
            b = 2.0d0 * zeta(j) * sqrt(km(j,j) * mm(j,j))
            q(:,j) = fm(:,j) / (mm(j,j) * s**2 + b * s + km(j,j))
        end do

        ! Convert back to the original, coupled configuration
        rsp = matmul(q, vectrans) 
    end function

! ------------------------------------------------------------------------------
    module function compute_frequency_response_3(m, k, zeta, f, freq, err) result(rsp)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: m, k
        real(real64), intent(in) :: zeta
        complex(real64), intent(in), dimension(:,:) :: f
        real(real64), intent(in), dimension(:) :: freq
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:,:) :: rsp

        ! Parameters
        complex(real64), parameter :: ci = (0.0d0, 1.0d0)

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, j, nfreq, neqn, flag
        real(real64), allocatable, dimension(:,:) :: mc, kc
        complex(real64), allocatable, dimension(:) :: vals, s
        complex(real64), allocatable, dimension(:,:) :: vecs, &
            vectrans, mm, km, fm, q
        complex(real64) :: b
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        neqn = size(m, 1)
        nfreq = size(freq)

        ! Array Size Check
        if (size(m, 2) /= neqn) then
            call errmgr%report_error("compute_frequency_response_3", &
                "The mass matrix is not square.", MECH_ARRAY_SIZE_ERROR)
            return
        end if

        if (size(k, 1) /= neqn .or. size(k, 2) /= neqn) then
            write(errmsg, '(AI0AI0AI0AI0A)') &
                "The stiffness matrix is not sized properly.  Expected a ", &
                neqn, "-by-", neqn, " matrix, but found a ", size(k, 1), &
                "-by-", size(k, 2), " matrix."
            call errmgr%report_error("compute_frequency_response_3", &
                trim(errmsg), MECH_ARRAY_SIZE_ERROR)
            return
        end if

        ! Local Memory Allocation
        allocate(mc(neqn, neqn), stat = flag)
        if (flag == 0) allocate(kc(neqn, neqn), stat = flag)
        if (flag == 0) allocate(vals(neqn), stat = flag)
        if (flag == 0) allocate(vecs(neqn, neqn), stat = flag)
        if (flag == 0) allocate(q(nfreq, neqn), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("compute_frequency_response_3", &
                "Insufficient memory available.", MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Create a copy of the mass and stiffness matrices such that the
        ! eigen solver does not overwrite the originals
        do j = 1, neqn
            do i = 1, neqn
                mc(i,j) = m(i,j)
                kc(i,j) = k(i,j)
            end do
        end do

        ! Compute the eigenvalues and eigenvectors
        call eigen(kc, mc, vals, vecs = vecs, err = errmgr)
        if (errmgr%has_error_occurred()) return

        ! Compute the modal matrices
        vectrans = transpose(vecs)
        mm = matmul(vectrans, matmul(m, vecs))
        km = matmul(vectrans, matmul(k, vecs))
        fm = matmul(f, vecs)

        ! Compute the complex frequency term
        s = ci * freq

        ! Compute the FRF of each decoupled equation
        do j = 1, neqn
            b = 2.0d0 * zeta * sqrt(km(j,j) * mm(j,j))
            q(:,j) = fm(:,j) / (mm(j,j) * s**2 + b * s + km(j,j))
        end do

        ! Convert back to the original, coupled configuration
        rsp = matmul(q, vectrans) 
    end function

! ------------------------------------------------------------------------------
end submodule
