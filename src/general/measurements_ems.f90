! measurements_ems.f90

submodule (measurements) measurements_ems
contains
! ------------------------------------------------------------------------------
pure module function ssq_part(x, xmean) result(ssq)
    ! Arguments
    real(real64), intent(in), dimension(:,:,:) :: x
    real(real64), intent(in) :: xmean
    real(real64) :: ssq

    ! Local Variables
    integer(int32) :: i, j, k, l, nop, npart, nrep
    real(real64) :: xi
    real(real64), allocatable, dimension(:) :: y

    ! Initialization
    npart = size(x, 1)
    nrep = size(x, 2)
    nop = size(x, 3)
    allocate(y(nrep * nop))

    ! Cycle over each part, and compute the sum of the differences
    ssq = 0.0d0
    do i = 1, npart
        ! Collect the data whose mean we are to evaluate, 
        ! and then compute the mean
        l = 0
        do j = 1, nrep
            do k = 1, nop
                l = l + 1
                y(l) = x(i,j,k)
            end do
        end do
        xi = mean(y)

        ! Compute and include the difference
        ssq = ssq + (xi - xmean)**2
    end do

    ! Output
    ssq = nop * nrep * ssq
end function

! ------------------------------------------------------------------------------
pure module function ssq_operator(x, xmean) result(ssq)
    ! Arguments
    real(real64), intent(in), dimension(:,:,:) :: x
    real(real64), intent(in) :: xmean
    real(real64) :: ssq

    ! Local Variables
    integer(int32) :: i, j, k, l, nop, npart, nrep
    real(real64) :: xi
    real(real64), allocatable, dimension(:) :: y

    ! Initialization
    npart = size(x, 1)
    nrep = size(x, 2)
    nop = size(x, 3)
    allocate(y(npart * nrep))

    ! Cycle over each operator
    ssq = 0.0d0
    do k = 1, nop
        ! Collect the data whose mean we are to evaluate, 
        ! and then compute the mean
        l = 0
        do j = 1, nrep
            do i = 1, npart
                l = l + 1
                y(l) = x(i,j,k)
            end do
        end do
        xi = mean(y)

        ! Compute and include the difference
        ssq = ssq + (xi - xmean)**2
    end do

    ! Output
    ssq = npart * nrep * ssq
end function

! ------------------------------------------------------------------------------
pure module function ssq_repeat(x) result(ssq)
    ! Arguments
    real(real64), intent(in), dimension(:,:,:) :: x
    real(real64) :: ssq

    ! Local Variables
    integer(int32) :: i, j, k, nop, npart, nrep
    real(real64) :: xik
    
    ! Initialization
    npart = size(x, 1)
    nrep = size(x, 2)
    nop = size(x, 3)

    ! Cycle over each observation
    ssq = 0.0d0
    do k = 1, nop
        do i = 1, npart
            xik = mean(x(i,:,k))
            do j = 1, nrep
                ssq = ssq + (x(i,j,k) - xik)**2
            end do
        end do
    end do
end function

! ------------------------------------------------------------------------------
pure module function ssq_total(x, xmean) result(ssq)
    ! Arguments
    real(real64), intent(in), dimension(:,:,:) :: x
    real(real64), intent(in) :: xmean
    real(real64) :: ssq

    ! Local Variables
    integer(int32) :: i, j, k, nop, npart, nrep
    
    ! Initialization
    npart = size(x, 1)
    nrep = size(x, 2)
    nop = size(x, 3)

    ! Cycle over each part, and compute the results
    ssq = 0.0d0
    do k = 1, nop
        do j = 1, nrep
            do i = 1, npart
                ssq = ssq + (x(i,j,k) - xmean)**2
            end do
        end do
    end do
end function

! ------------------------------------------------------------------------------
module function ems_gauge_r_r(x, alpha, tol, k, err) result(rst)
    ! Arguments
    real(real64), intent(in), dimension(:,:,:) :: x
    real(real64), intent(in), optional :: alpha, tol, k
    class(errors), intent(inout), optional, target :: err
    type(ems_grr_results) :: rst

    ! Local Variables
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    character(len = 256) :: errmsg
    integer(int32) :: npart, nrep, nop
    real(real64) :: a

    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    a = 5.0d-2
    if (present(alpha)) a = alpha
    rst%scale = 6.0d0
    if (present(k)) rst%scale = k
    npart = size(x, 1)
    nrep = size(x, 2)
    nop = size(x, 3)

    ! Input Check
    if (npart < 2) then
        write (errmsg, '(AI0A)') &
            "Expected to find at least 2 parts, but only found ", &
            npart, "."
        call errmgr%report_error("ems_gauge_r_r", trim(errmsg), &
            MS_INVALID_DATA_SET_ERROR)
        return
    end if
    if (nrep < 2) then
        write (errmsg, '(AI0A)') &
            "Expected to find at least 2 tests, but only found ", &
            nrep, "."
        call errmgr%report_error("ems_gauge_r_r", trim(errmsg), &
            MS_INVALID_DATA_SET_ERROR)
        return
    end if
    if (nop < 2) then
        write (errmsg, '(AI0A)') &
            "Expected to find at least 2 operators, but only found ", &
            nop, "."
        call errmgr%report_error("ems_gauge_r_r", trim(errmsg), &
            MS_INVALID_DATA_SET_ERROR)
        return
    end if
    if (a <= 0.0d0 .or. a >= 1.0d0) then
        write (errmsg, '(AE11.3A)') &
            "The alpha parameter must lie between 0 and 1, " // &
            "but was found to be ", a, "."
        call errmgr%report_error("ems_gauge_r_r", trim(errmsg), &
            MS_INVALID_INPUT_ERROR)
        return
    end if

    ! Compute the overall mean
    rst%overall_mean = mean(pack(x, .true.))

    ! Compute the sum of the square terms
    rst%sum_squares_part = ssq_part(x, rst%overall_mean)
    rst%sum_squares_operator = ssq_operator(x, rst%overall_mean)
    rst%sum_squares_within = ssq_repeat(x)
    rst%sum_squares_total = ssq_total(x, rst%overall_mean)
    rst%sum_squares_part_by_operator = rst%sum_squares_total - &
        rst%sum_squares_part - rst%sum_squares_operator - rst%sum_squares_within

    ! Compute the DOF terms
    rst%part_dof = npart - 1
    rst%operator_dof = nop - 1
    rst%part_by_operator_dof = rst%part_dof * rst%operator_dof
    rst%repeatability_dof = npart * nop * (nrep - 1)
    rst%total_dof = npart * nop * nrep - 1

    ! Compute the mean of the squared differences
    rst%mean_square_part = rst%sum_squares_part / rst%part_dof
    rst%mean_square_operator = rst%sum_squares_operator / rst%operator_dof
    rst%mean_square_part_by_operator = rst%sum_squares_part_by_operator / &
        rst%part_by_operator_dof
    rst%mean_square_repeatability = rst%sum_squares_within / rst%repeatability_dof
    rst%mean_square_total = rst%sum_squares_total / rst%total_dof

    ! Compute the F-Test for signficance
    rst%part_f_ratio = rst%mean_square_part / rst%mean_square_part_by_operator
    rst%operator_f_ratio = rst%mean_square_operator / rst%mean_square_part_by_operator
    rst%part_by_operator_f_ratio = rst%mean_square_part_by_operator / &
        rst%mean_square_repeatability

    ! Compute the probability terms
    rst%part_probability = 1.0d0 - &
        f_distribution(rst%part_f_ratio, rst%part_dof, &
        rst%repeatability_dof)
    rst%operator_probability = 1.0d0 - &
        f_distribution(rst%operator_f_ratio, rst%operator_dof, &
        rst%repeatability_dof)
    rst%part_by_operator_probability = 1.0d0 - &
        f_distribution(rst%part_by_operator_f_ratio, &
        rst%part_by_operator_dof, rst%repeatability_dof)

    ! Compute the variance terms
    if (rst%part_by_operator_probability > a) then
        rst%part_variation = max(0.0d0, &
            (rst%mean_square_part - &
            rst%mean_square_part_by_operator) / (nop * nrep))
        rst%operator_variation = max(0.0d0, &
            (rst%mean_square_operator - &
            rst%mean_square_part_by_operator) / (npart * nrep))
    else
        rst%part_variation = max(0.0d0, &
            (rst%mean_square_part - &
            rst%mean_square_repeatability) / (nop * nrep))
        rst%operator_variation = max(0.0d0, &
            (rst%mean_square_operator - &
            rst%mean_square_repeatability) / (npart * nrep))
    end if
    rst%part_by_operator_variation = max(0.0d0, &
        (rst%mean_square_part_by_operator - rst%mean_square_repeatability) / nrep)
    rst%repeatability = rst%mean_square_repeatability
    rst%reproducibility = rst%operator_variation + rst%part_by_operator_variation
    rst%gauge_variation = rst%repeatability + rst%reproducibility
    rst%total_variation = rst%gauge_variation + rst%part_variation

    ! Compute the P/T information
    if (present(tol)) then
        rst%tolerance = tol
        rst%pt_ratio = rst%scale * sqrt(rst%gauge_variation) / tol
    else
        rst%tolerance = 0.0d0
        rst%pt_ratio = 0.0d0
    end if

    ! Compute the P/TV information
    rst%ptv_ratio = rst%gauge_variation / rst%total_variation
end function
! ------------------------------------------------------------------------------
!> @brief Computes the cummulative F-distribution.
!!
!! @param[in] x The value at which to evaluate the distribution function.
!! @param[in] d1 
!! @param[in] d2
!!
!! @return The value of the F-distribution at @p x.
function f_distribution(x, dof1, dof2) result(f)
    ! Arguments
    real(real64), intent(in) :: x
    integer(int32), intent(in) :: dof1, dof2
    real(real64) :: f

    ! Local Variables
    real(real64) :: xf, d1, d2

    ! Compute the incomplete beta function
    d1 = real(dof1, real64)
    d2 = real(dof2, real64)
    xf = d1 * x / (d1 * x + d2)
    f = incomplete_beta(d1, d2, xf)
end function

! ------------------------------------------------------------------------------
end submodule
