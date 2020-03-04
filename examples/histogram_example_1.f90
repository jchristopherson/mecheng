! histogram_example_1.f90

program example
    use iso_fortran_env
    use fplot_core
    implicit none

    ! Local Variables
    integer(int32), parameter :: n = 500
    integer(int32), parameter :: nbins = 10
    real(real64) :: x(n)
    type(plot_2d) :: plt
    type(plot_data_histogram) :: pd1

    ! Initialization
    call plt%initialize()
    call plt%set_font_size(14)

    ! Plot the data
    call random_number(x)
    call pd1%set_bin_count(nbins)   ! must be called before define_data
    call pd1%define_data(x)
    call pd1%set_transparency(0.2)
    call plt%push(pd1)
    call plt%draw()
end program