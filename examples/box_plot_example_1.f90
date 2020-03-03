! box_plot_example_1.f90

program example
    use iso_fortran_env
    use fplot_core
    use strings
    implicit none

    ! Local Variables
    real(real64) :: x(5)
    type(string) :: labels(5)
    type(plot_2d) :: plt
    type(plot_data_bar) :: pd1

    ! Initialization
    call plt%initialize()
    call plt%set_font_size(14)

    labels(1)%str = '"Label 1"'
    labels(2)%str = '"Label 2"'
    labels(3)%str = '"Label 3"'
    labels(4)%str = '"Label 4"'
    labels(5)%str = '"Label 5"'

    ! Plot the data
    call random_number(x)
    call pd1%define_data(labels, x)
    call pd1%set_is_filled(.false.)
    call plt%push(pd1)
    call plt%draw()
    ! call plt%save_file("box_plot.plt")
end program