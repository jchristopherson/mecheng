! fplot_histogram.f90

submodule (fplot_core) fplot_histogram
contains
! ------------------------------------------------------------------------------
module function phist_get_cmd(this) result(x)
    ! Arguments
    class(plot_histogram), intent(in) :: this
    character(len = :), allocatable :: x

    ! Local Variables
    type(string_builder) :: str

    ! Initialization
    call str%initialize()

    ! Histogram-Specific Formatting
    ! call str%append(new_line('a'))
    ! call str%append("set style data histograms")
    
    ! call str%append(new_line('a'))
    ! call str%append("set boxwidth 0.9 absolute")

    ! call str%append(new_line('a'))
    ! call str%append("set style fill solid 1.00 border lt -1")

    ! call str%append(new_line('a'))
    ! call str%append("set style histogram clustered gap 1")

    ! Call the base routine to finish the plotting commands
    call str%append(new_line('a'))
    call str%append(this%plot_2d%get_command_string())

    ! REF: http://www.gnuplotting.org/calculating-histograms/
    ! http://gnuplot.sourceforge.net/docs_4.2/node249.html
    ! http://gnuplot.sourceforge.net/demo_5.2/histograms2.html
    ! http://gnuplot.sourceforge.net/demo_5.2/histograms.html
    ! http://www.gnuplotting.org/manpage-gnuplot-4-6/#Q1-1-147


    ! End
    x = str%to_string()
end function

! ------------------------------------------------------------------------------
end submodule
