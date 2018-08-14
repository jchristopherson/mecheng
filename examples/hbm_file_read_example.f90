! hbm_file_read_example.f90

program example
    use iso_fortran_env
    use fortio_hbm
    implicit none

    ! Local Variables
    type(hbm_data_file) :: file

    ! Read the file - change the path to suit your needs
    file = read_catman_file("C:\\Users\\christoj\\Documents\\Code\\mecheng\\examples\\Files\\Example_HBM_Data_File.BIN")

    ! Display outputs from the file
    print '(A)', "File: " // file%filename // " read completed."
    print '(AI0)', "Channel Count: ", file%channel_count
end program
