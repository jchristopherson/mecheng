! measurements_emp.f90

submodule (measurements) measurements_emp

contains
! -------------------------------------------------------------------
! Returns the appropriate d2 bias correction factor.
!
! REF: http://www.bessegato.com.br/UFJF/resources/table_of_control_chart_constants_old.pdf
pure function bias_correction_factor(n) result(d)
    ! Arguments
    integer(int32), intent(in) :: n
    real(real64) :: d

    ! Define the table and associated index
    integer(int32), parameter, dimension(24) :: index = [ &
        2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, &
        17, 18, 19, 20, 21, 22, 23, 24, 25]
    real(real64), parameter, dimension(24) :: values = [ &
        1.128d0, 1.693d0, 2.059d0, 2.326d0, 2.534d0, 2.704d0, &
        2.847d0, 2.970d0, 3.078d0, 3.173d0, 3.258d0, 3.336d0, &
        3.407d0, 3.472d0, 3.532d0, 3.588d0, 3.640d0, 3.689d0, &
        3.735d0, 3.778d0, 3.819d0, 3.858d0, 3.895d0, 3.931d0]

    ! Local Variables
    integer(int32) :: i

    ! Ensure the index is within bounds
    if (n < 2) then
        d = 0.0d0
        return
    else if (n > size(index)) then
        d = values(size(index))
        return
    end if

    ! Find the matching array index, and return the correct value
    do i = 1, size(index)
        if (n == index(i)) exit
    end do
    d = values(i)
end function

! -------------------------------------------------------------------
! Computes the range of an array of values.
pure function array_range(x) result(r)
    ! Arguments
    real(real64), intent(in), dimension(:) :: x
    real(real64) :: r

    ! Local Variables
    integer(int32) :: i, n
    real(real64) :: maxx, minx

    ! Quick Return
    n = size(x)
    r = 0.0d0
    if (n == 0) return

    ! Determine the difference between the max and min values
    maxx = x(1)
    minx = x(1)
    do i = 2, n
        if (x(i) > maxx) maxx = x(i)
        if (x(i) < minx) minx = x(i)
    end do
    r = maxx - minx
end function

! -------------------------------------------------------------------
module function emp_gauge_r_r(x, tol, kf, err) result(rst)
    ! Arguments
    real(real64), intent(in), dimension(:,:,:) :: x
    real(real64), intent(in), optional :: tol, kf
    class(errors), intent(inout), optional, target :: err
    type(emp_grr_results) :: rst

    ! Local Variables
    class(errors), pointer :: errmgr
    type(errors), target :: deferr
    character(len = 256) :: errmsg
    integer(int32) :: i, j, k, npart, nrep, nop, flag
    real(real64) :: d2rep, d2rpro, d2part
    real(real64), allocatable, dimension(:) :: reprng

    ! Initialization
    if (present(err)) then
        errmgr => err
    else
        errmgr => deferr
    end if
    rst%scale = 6.0d0
    if (present(kf)) rst%scale = kf

    npart = size(x, 1)
    nrep = size(x, 2)
    nop = size(x, 3)

    ! Input Check
    if (npart < 2) then
        write (errmsg, '(AI0A)') &
            "Expected to find at least 2 parts, but only found ", &
            npart, "."
        call errmgr%report_error("emp_gauge_r_r", trim(errmsg), &
            MS_INVALID_DATA_SET_ERROR)
        return
    end if
    if (nrep < 2) then
        write (errmsg, '(AI0A)') &
            "Expected to find at least 2 tests, but only found ", &
            nrep, "."
        call errmgr%report_error("emp_gauge_r_r", trim(errmsg), &
            MS_INVALID_DATA_SET_ERROR)
        return
    end if
    if (nop < 2) then
        write (errmsg, '(AI0A)') &
            "Expected to find at least 2 operators, but only found ", &
            nop, "."
        call errmgr%report_error("emp_gauge_r_r", trim(errmsg), &
            MS_INVALID_DATA_SET_ERROR)
        return
    end if

    ! Compute the approriate bias correction factors
    d2rep = bias_correction_factor(nrep)
    d2rpro = bias_correction_factor(nop)
    d2part = bias_correction_factor(npart)

    ! Output Memory Allocations
    allocate(rst%operator_means(nop), stat = flag)
    if (flag == 0) allocate(rst%part_means(npart), stat = flag)
    if (flag == 0) allocate(rst%repeat_means(nrep), stat = flag)
    if (flag == 0) allocate(reprng(npart * nop), stat = flag)
    if (flag /= 0) then
        call errmgr%report_error("emp_gauge_r_r", &
            "Insufficient memory available.", &
            MS_OUT_OF_MEMORY_ERROR)
        return
    end if

    ! Compute the mean (average) terms
    do k = 1, nop
        rst%operator_means(k) = mean(reshape(x(:,:,k), [npart * nrep]))
    end do

    do j = 1, nrep
        rst%repeat_means(j) = mean(reshape(x(:,j,:), [npart * nop]))
    end do

    do i = 1, npart
        rst%part_means(i) = mean(reshape(x(i,:,:), [nrep * nop]))
    end do

    ! Compute the range terms
    rst%operator_range = array_range(rst%operator_means)
    rst%part_range = array_range(rst%part_means)
    j = 0
    do k = 1, nop
        do i = 1, npart
            j = j + 1
            reprng(j) = array_range(x(i,:,k))
        end do
    end do
    rst%average_range = mean(reprng)

    ! Compute the variance terms
    rst%repeatability = (rst%average_range / d2rep)**2
    rst%reproducibility = (rst%operator_range / d2rpro)**2 - &
        (nop / (npart * nop * nrep)) * rst%repeatability
    rst%gauge_variation = rst%repeatability + rst%reproducibility
    rst%part_variation = (rst%part_range / d2part)**2 - &
        (npart / (npart * nop * nrep)) * rst%repeatability
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

! -------------------------------------------------------------------

! -------------------------------------------------------------------

! -------------------------------------------------------------------

! -------------------------------------------------------------------

! -------------------------------------------------------------------

! -------------------------------------------------------------------

! -------------------------------------------------------------------

! -------------------------------------------------------------------

! -------------------------------------------------------------------

! -------------------------------------------------------------------
end submodule
