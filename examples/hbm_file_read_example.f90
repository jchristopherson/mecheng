! hbm_file_read_example.f90

program example
    use iso_fortran_env
    use fortio_hbm
    use fplot_core
    use strings
    implicit none

    ! Local Variables
    type(hbm_data_file) :: file
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1
    integer(int16) :: i
    character(len = :), allocatable :: name

    ! Read the file - change the path to suit your needs
    file = read_catman_file("C:\\Users\\christoj\\Documents\\Code\\mecheng\\examples\\Files\\Example_HBM_Data_File.BIN")

    ! Display outputs from the file
    print '(A)', "File: " // file%filename // " read completed."
    print '(AI0)', "Channel Count: ", file%channel_count

    ! Initialize a plot
    call plt%initialize()
    call plt%set_font_size(14)

    ! Plot each data channel
    do i = 1, file%channel_count
        name = "Channel " // to_string(file%channels(i)%channel_location) // " " // file%channels(i)%name
        print '(AI0A)', char(9), i, ". " // name
        call plt%set_title(name)
        call d1%define_data(file%channels(1)%values, file%channels(i)%values)
        if (i /= 1) call plt%clear_all()
        call plt%push(d1)
        call plt%draw()
    end do
end program
