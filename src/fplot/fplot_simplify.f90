! fplot_simplify.f90

! References:
! - https://www.codeproject.com/Articles/114797/Polyline-Simplification
! - https://en.wikipedia.org/wiki/Ramer%E2%80%93Douglas%E2%80%93Peucker_algorithm

module fplot_simplify
    use iso_fortran_env
    use ferror
    use geometry
    use fplot_errors
    implicit none
    private

contains
    ! Employs the Ramer-Douglas-Peucker algorithm to simplify a 2D data set.
    recursive function douglas_peucker_driver_2d(x, y, tol, err) result(pts)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), intent(in) :: tol
        class(errors), intent(inout) :: err
        real(real64), allocatable, dimension(:,:) :: pts

        ! Local Variables
        integer(int32) :: i, n, index, n1, n2, nt, flag
        real(real64) :: d, dmax, pt(3)
        type(line) :: ln
        real(real64), allocatable, dimension(:,:) :: rst1, rst2

        ! Find the point with the maximum distance from the line drawn between the first and last points
        index = 0
        dmax = 0.0d0
        pt = 0.0d0
        n = size(x)
        ln%a = [x(1), y(1), 0.0d0]
        ln%b = [x(n), y(n), 0.0d0]
        do i = 2, n - 1
            ! Locate the point
            pt(1) = x(i)
            pt(2) = y(i)

            ! Compute the distance between the line and point
            d = line_to_point_distance(ln, [x(i), y(i), 0.0d0])

            ! Only keep the largest distance
            if (d > dmax) then
                index = i
                dmax = d
            end if
        end do

        ! If the max distance is greater than the tolerance, recursively simplify
        if (dmax > tol) then
            ! Recursive calls to perform further simplification
            rst1 = douglas_peucker_driver_2d(x(1:index), y(1:index), tol, err)
            if (err%has_error_occurred()) return

            rst2 = douglas_peucker_driver_2d(x(index:n), y(index:n), tol, err)
            if (err%has_error_occurred()) return

            ! Build the results list
            n1 = size(rst1, 1)
            n2 = size(rst2, 1)
            nt = (n1 - 1) + n2
            allocate(pts(nt, 2), stat = flag)
            if (flag /= 0) then
                call err%report_error("douglas_peucker_driver_2d", &
                    "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
                return
            end if
            pts(1:n1-1,:) = rst1(1:n1-1,:)
            pts(n1:nt,:) = rst2
        else
            ! Keep only the first and last points
            pts = reshape([x(1), x(n), y(1), y(n)], [2, 2])
        end if
    end function

end module
