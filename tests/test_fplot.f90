! test_fplot.f90

program test_fplot
    use iso_fortran_env
    use fplot_core
    use fplot_simplify
    implicit none

    ! Local Variables
    integer(int32), parameter :: npts = 10000
    real(real64), parameter :: tol = 1.0d-2
    real(real64), parameter :: xMax = 2.0d0
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2
    real(real64) :: dx, x(npts), y(npts)
    real(real64), allocatable, dimension(:,:) :: xy1
    integer(int32) :: i
    character(len = 256) :: msg

    ! Define the data
    x(1) = 0.0d0
    y(1) = 0.0d0
    dx = xMax / (npts - 1.0d0)
    do i = 2, npts
        x(i) = x(i-1) + dx
        y(i) = exp(-0.7 * x(i)) * sin(15.0d0 * x(i))
    end do

    ! Create a simplified data set
    xy1 = simplify_polyline(x, y, tol)

    ! Plot the data
    call d1%set_name("Original")
    call d1%define_data(x, y)
    call d1%set_simplify_data(.false.)

    call d2%set_name("Simplified")
    call d2%define_data(xy1(:,1), xy1(:,2))

    call plt%initialize()
    write(msg, '(AI0AI0)') "Original: ", size(x), ", Simplified: ", size(xy1, 1)
    call plt%set_title(trim(msg))
    call plt%push(d1)
    call plt%push(d2)
    call plt%draw()
end program
