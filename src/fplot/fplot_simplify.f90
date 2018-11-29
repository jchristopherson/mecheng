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
    public :: simplify_polyline

    !> @brief Simplifies a polyline using the Ramer-Doublas-Peucker algorithm.
    interface simplify_polyline
        module procedure :: simplify_polyline_2d1
        module procedure :: simplify_polyline_3d1
        module procedure :: simplify_polyline_mtx
    end interface

contains
    !> @brief Simplifies a 2D polyline using the Ramer-Doublas-Peucker algorithm.
    !!
    !! @param[in] x An N-element array containing the x-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] y An N-element array containing the y-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] tol The distance tolerance to use when simplifying the polyline.
    !!  This value must be positive, and larger than machine epsilon.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input array sizes are not
    !!      compatible.
    !!  - PLOT_INVALID_INPUT_ERROR: Occurs if @p tol is not positive and greater
    !!      than machine epsilon.
    !!
    !! @return A matrix containing the simplified polyline vertices.  The first
    !! column of the matrix contains the x-coordinates, and the second column
    !! contains the y-coordinates.
    function simplify_polyline_2d1(x, y, tol, err) result(ln)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), intent(in) :: tol
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: ln

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: n
        real(real64) :: eps
        
        ! Initialization
        n = size(x)
        eps = epsilon(eps)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(y) /= n) then
            write(errmsg, '(AI0AI0A)') "The array sizes did not match.  " // &
                "The x array contained ", size(x), &
                " items, but the y array contained ", size(y), "."
            call errmgr%report_error("simplify_polyline_2d1", trim(errmsg), &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        if (tol < eps) then
            call errmgr%report_error("simplify_polyline_2d1", &
                "The tolerance value is either negative or less " // &
                "than machine precision.", PLOT_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        ln = douglas_peucker_driver_2d(x, y, tol, errmgr)
    end function



    !> @brief Simplifies a 3D polyline using the Ramer-Doublas-Peucker algorithm.
    !!
    !! @param[in] x An N-element array containing the x-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] y An N-element array containing the y-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] z An N-element array containing the z-coordinates of the vertices
    !!  making up the polyline.
    !! @param[in] tol The distance tolerance to use when simplifying the polyline.
    !!  This value must be positive, and larger than machine epsilon.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input array sizes are not
    !!      compatible.
    !!  - PLOT_INVALID_INPUT_ERROR: Occurs if @p tol is not positive and greater
    !!      than machine epsilon.
    !!
    !! @return A matrix containing the simplified polyline vertices.  The first
    !! column of the matrix contains the x-coordinates, the second column
    !! contains the y-coordinates, and the third column contains the z-coordinates.
    function simplify_polyline_3d1(x, y, z, tol, err) result(ln)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y, z
        real(real64), intent(in) :: tol
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: ln

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: n
        real(real64) :: eps
        
        ! Initialization
        n = size(x)
        eps = epsilon(eps)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(y) /= n .or. size(z) /= n) then
            write(errmsg, '(AI0AI0AI0A)') "The array sizes did not match.  " // &
                "The x array contained ", size(x), &
                " items, the y array contained ", size(y), &
                ", and the z array contained ", size(z), "."
            call errmgr%report_error("simplify_polyline_3d1", trim(errmsg), &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        if (tol < eps) then
            call errmgr%report_error("simplify_polyline_3d1", &
                "The tolerance value is either negative or less " // &
                "than machine precision.", PLOT_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        ln = douglas_peucker_driver_3d(x, y, z, tol, errmgr)
    end function


    !> @brief Simplifies a 2D or 3D polyline using the Ramer-Doublas-Peucker algorithm.
    !!
    !! @param[in] xy An N-by-2 or N-by-3 matrix containing the polyline vertex data.
    !! @param[in] tol The distance tolerance to use when simplifying the polyline.
    !!  This value must be positive, and larger than machine epsilon.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - PLOT_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is available.
    !!  - PLOT_ARRAY_SIZE_MISMATCH_ERROR: Occurs if the input array sizes are not
    !!      compatible.
    !!  - PLOT_INVALID_INPUT_ERROR: Occurs if @p tol is not positive and greater
    !!      than machine epsilon.
    !!
    !! @return A matrix containing the simplified polyline vertices.  The first
    !! column of the matrix contains the x-coordinates, the second column
    !! contains the y-coordinates, and if necessary, the third column contains
    !! the z-coordinates.
    function simplify_polyline_mtx(xy, tol, err) result(ln)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: xy
        real(real64), intent(in) :: tol
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: ln

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there are at least 2 columns of data in XY
        if (size(xy, 2) < 2) then
            write(errmsg, '(AI0A)') "The input matrix must have at " // &
                "least 2 columns; however, only ", size(xy, 2), " was found."
            call errmgr%report_error("simplify_polyline_mtx", trim(errmsg), &
                PLOT_ARRAY_SIZE_MISMATCH_ERROR)
            return
        end if

        ! Process
        if (size(xy, 2) == 2) then
            ln = simplify_polyline_2d1(xy(:,1), xy(:,2), tol, errmgr)
        else
            ln = simplify_polyline_3d1(xy(:,1), xy(:,2), xy(:,3), tol, errmgr)
        end if
    end function

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


    ! Employs the Ramer-Douglas-Peucker algorithm to simplify a 3D data set.
    recursive function douglas_peucker_driver_3d(x, y, z, tol, err) result(pts)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y, z
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
        n = size(x)
        ln%a = [x(1), y(1), z(1)]
        ln%b = [x(n), y(n), z(n)]
        do i = 2, n - 1
            ! Locate the point
            pt(1) = x(i)
            pt(2) = y(i)
            pt(3) = z(i)

            ! Compute the distance between the line and point
            d = line_to_point_distance(ln, [x(i), y(i), z(i)])

            ! Only keep the largest distance
            if (d > dmax) then
                index = i
                dmax = d
            end if
        end do

        ! If the max distance is greater than the tolerance, recursively simplify
        if (dmax > tol) then
            ! Recursive calls to perform further simplification
            rst1 = douglas_peucker_driver_3d(x(1:index), y(1:index), z(1:index), tol, err)
            if (err%has_error_occurred()) return

            rst2 = douglas_peucker_driver_3d(x(index:n), y(index:n), z(index:n), tol, err)
            if (err%has_error_occurred()) return

            ! Build the results list
            n1 = size(rst1, 1)
            n2 = size(rst2, 1)
            nt = (n1 - 1) + n2
            allocate(pts(nt, 2), stat = flag)
            if (flag /= 0) then
                call err%report_error("douglas_peucker_driver_3d", &
                    "Insufficient memory available.", PLOT_OUT_OF_MEMORY_ERROR)
                return
            end if
            pts(1:n1-1,:) = rst1(1:n1-1,:)
            pts(n1:nt,:) = rst2
        else
            ! Keep only the first and last points
            pts = reshape([x(1), x(n), y(1), y(n), z(1), z(n)], [2, 3])
        end if
    end function

end module
