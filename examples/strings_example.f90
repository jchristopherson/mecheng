! strings_example.f90

program example
    use iso_fortran_env
    use strings
    implicit none

    ! Create a string to split
    character(len = *), parameter :: str = "This,is,a,test,,string,with,repeating,,,delimiters"
    
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
