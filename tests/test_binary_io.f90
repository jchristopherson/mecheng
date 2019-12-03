! test_binary_io.f90

program main
    use :: iso_fortran_env
    use :: fortio_binary
    implicit none

    logical :: rst, overall
    overall = .true.

    rst = test_formatter()
    if (.not.rst) overall = .false.

    print *, ""
    if (overall) then
        print '(A)', "BINARY I/O TESTS PASSED"
    else
        print '(A)', "BINARY I/O TESTS FAILED"
    end if

contains
    ! Compares matrices
    function test_matrices(fcn, x, y) result(rst)
        ! Arguments
        character(len = *), intent(in) :: fcn
        real(real64), intent(in), dimension(:,:) :: x, y
        logical :: rst

        ! Local Variables
        integer(int32) :: i, j, m, n

        ! Initialization
        m = size(x, 1)
        n = size(x, 2)
        rst = .true.
        
        ! Check size
        if (size(y, 1) /= m .or. size(y, 2) /= n) then
            rst = .false.
            print '(AI0AI0AI0AI0A)', "TEST FAILED (" // fcn // &
                ").  Expected a matrix of ", m, " x ", n, &
                ", but found a matrix of ", size(y, 1), " x ", &
                size(y, 2), "."
            return
        end if

        ! Check each value
        do j = 1, n
            do i = 1, m
                if (x(i,j) /= y(i,j)) then
                    rst = .false.
                    print '(AI0AI0AE11.3AE11.3A)', "TEST FAILED (" // fcn // &
                        ").  At ", i, ", ", j, " expected: ", x(i,j), &
                        ", but found: ", y(i,j), "."
                    return
                end if
            end do
        end do
    end function


    ! Tests the binary_formatter object
    function test_formatter() result(rst)
        ! Arguments
        logical :: rst

        ! Local Variables
        integer(int32), parameter :: m = 200
        integer(int32), parameter :: n = 200
        integer(int32), parameter :: int32_size = 4
        integer(int32), parameter :: real64_size = 8
        real(real64), allocatable, dimension(:,:) :: x
        real(real64) :: ans(m, n)
        type(binary_formatter) :: formatter
        integer(int32) :: expectedSize, actualSize

        ! Initialization
        rst = .true.
        call random_number(ans)
        call formatter%initialize()
        expectedSize = 2 * int32_size + m * n * real64_size

        ! Serialize the matrix ANS using the formatter
        call formatter%add(ans)

        ! Check the size of the formatter.  It should account for the entire
        ! matrix + 2 integers defining the matrix size
        actualSize = formatter%get_count()
        if (actualSize /= expectedSize) then
            print '(AI0AI0A)', "TEST FAILED (TEST_FORMATTER): Expected a size of ", &
                expectedSize, " bytes, but found a size of ", actualSize, " bytes."
            rst = .false.
            return
        end if

        ! Deserialize, and check
        x = formatter%get_real64_matrix()
        rst = test_matrices("TEST_FORMATTER", ans, x)
    end function
end program
