! strings_example.f90

program example
    use iso_fortran_env
    use strings
    implicit none

    ! Create a string to split
    character(len = *), parameter :: str = "2/5/2019 1:55:19 PM,500000,12375," &
        // "Pine_PZT_100K_1_2_1_3_0_435mm,DN-014,7200,0.435,,,nm,0,1,-8.58e\r\n"
    
    ! Local Variables
    type(string) :: buffer(50)  ! Arbitrarily chosen buffer size
    integer(int32) :: i, n

    ! Split the string
    n = split_string(str, ",", buffer)

    ! Print the results
    print '(A)', "Split: " // str
    print '(AI0A)', "  ", n, " strings created:"
    do i = 1, n
        print '(AI0A)', achar(9), i, ". " // trim(buffer(i)%str)
    end do
end program
