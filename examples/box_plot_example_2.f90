! box_plot_example_2.f90

program example
    use iso_fortran_env
    use fplot_core
    use strings
    implicit none

    ! Local Variables
    real(real64) :: x(5)
    type(string) :: labels(5)
    type(plot_bar) :: plt
    type(plot_data_bar) :: pd1

    ! Initialization
    call plt%initialize()
    call plt%set_font_size(14)
    call plt%set_bar_width(0.5)

    labels(1)%str = '"Label 1"'
    labels(2)%str = '"Label 2"'
    labels(3)%str = '"Label 3"'
    labels(4)%str = '"Label 4"'
    labels(5)%str = '"Label 5"'

    ! Plot the data
    call random_number(x)
    call pd1%define_data(labels, x)
    call plt%push(pd1)
    call plt%draw()
end program