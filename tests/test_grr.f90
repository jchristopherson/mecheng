! test_grr.f90

program main
    use iso_fortran_env
    use measurements
    implicit none

    ! Local Variables
    logical :: local, check

    ! Initialization
    check = .true.

    ! Tests
    local = test()
    if (.not.local) check = .false.

    ! local = test_2()
    ! if (.not.local) check = .false.

    ! End
    print *, ""
    if (check) then
        print '(A)', "GAGE R&R TESTS PASSED"
    else
        print '(A)', "GAGE R&R TESTS FAILED"
    end if
    print *, ""

contains
    ! The data set is # of parts -by- # of tests -by- # of operators
    function format_data() result(x)
        ! Arguments
        real(real64), allocatable, dimension(:,:,:) :: x

        ! Parameters
        integer(int32), parameter :: nparts = 9
        integer(int32), parameter :: nops = 2
        integer(int32), parameter :: ntests = 3

        ! Local Variables
        real(real64), allocatable, dimension(:) :: op

        ! Allocate space for the output
        allocate(x(nparts, ntests, nops))
        allocate(op(nparts * ntests))

        ! Fill in the data for operator 1
        op = [39736.0d0, 40088.0d0, 39952.0d0, 39840.0d0, &
            39792.0d0, 39968.0d0, 39384.0d0, 39648.0d0, &
            39704.0d0, 39152.0d0, 39240.0d0, 39312.0d0, &
            38832.0d0, 38688.0d0, 38704.0d0, 38936.0d0, &
            38872.0d0, 38928.0d0, 39040.0d0, 38952.0d0, &
            38968.0d0, 39128.0d0, 38928.0d0, 38928.0d0, &
            38712.0d0, 38688.0d0, 38624.0d0]
        x(:,:,1) = transpose(reshape(op, [ntests, nparts]))

        ! Fill in the data for operator 2
        op = [40256.0d0, 39864.0d0, 39792.0d0, 39816.0d0, &
            39888.0d0, 39784.0d0, 39760.0d0, 39944.0d0, &
            39832.0d0, 39272.0d0, 39288.0d0, 39296.0d0, &
            38728.0d0, 38768.0d0, 38704.0d0, 38896.0d0, &
            38952.0d0, 38968.0d0, 39032.0d0, 38968.0d0, &
            38992.0d0, 39032.0d0, 39008.0d0, 38968.0d0, &
            38592.0d0, 38640.0d0, 38584.0d0]
        x(:,:,2) = transpose(reshape(op, [ntests, nparts]))
    end function

    ! The solution to this problem has been computed using JMP v15
    function test() result(check)
        ! Parameters
        real(real64), parameter :: tol = 1.0d-1
        real(real64), parameter :: part2part = 238266.67d0
        real(real64), parameter :: repeatability = 9993.48d0
        real(real64), parameter :: reproducibility = 1273.68d0
        real(real64), parameter :: gauge = 11267.16d0
        real(real64), parameter :: pt = 0.63688d0

        ! Local Variables
        real(real64), allocatable, dimension(:,:,:) :: x
        type(ems_grr_results) :: rst
        logical :: check

        ! General Initialization
        check = .true.

        ! Initialize the data set
        x = format_data()

        ! Compute the GR&R
        rst = ems_gauge_r_r(x, tol = 1.0d3, alpha = 0.05d0)

        ! Compare some critical results
        if (abs(part2part - rst%part_variation) > tol) then
            check = .false.
            print '(AE10.3AE10.3)', "TEST_GRR - 1: Part-Part Variation - Expected: ", &
                part2part, ", but found: ", rst%part_variation
        end if

        if (abs(repeatability - rst%repeatability) > tol) then
            check = .false.
            print '(AE10.3AE10.3)', "TEST_GRR - 1: Repeatability - Expected: ", &
                repeatability, ", but found: ", rst%repeatability
        end if

        if (abs(reproducibility - rst%reproducibility) > tol) then
            check = .false.
            print '(AE10.3AE10.3)', "TEST_GRR - 1: Reproducibility - Expected: ", &
                reproducibility, ", but found: ", rst%reproducibility
        end if

        if (abs(gauge - rst%gauge_variation) > tol) then
            check = .false.
            print '(AE10.3AE10.3)', "TEST_GRR - 1: Gauge Variation - Expected: ", &
                gauge, ", but found: ", rst%gauge_variation
        end if

        if (abs(pt - rst%pt_ratio) > tol) then
            check = .false.
            print '(AE10.3AE10.3)', "TEST_GRR - 1: P/T RATIO - Exepected: ", &
                pt, ", but found: ", rst%pt_ratio
        end if
    end function

! ---------------------------------------------------------------------------------------
    ! function format_data_2() result(x)
    !     ! Arguments
    !     real(real64), allocatable, dimension(:,:,:) :: x

    !     ! Parameters
    !     integer(int32), parameter :: nparts = 9
    !     integer(int32), parameter :: nops = 2
    !     integer(int32), parameter :: ntests = 3

    !     ! Local Variables
    !     real(real64), allocatable, dimension(:) :: op

    !     ! Allocate space for the output
    !     allocate(x(nparts, ntests, nops))
    !     allocate(op(nparts * ntests))

    !     ! Fill in the data for operator 1
    !     op = [22008.0d0, 22104.0d0, 22104.0d0, 21960.0d0, 22104.0d0, &
    !         22120.0d0, 21912.0d0, 22048.0d0, 21880.0d0, 21752.0d0, &
    !         22056.0d0, 22008.0d0, 21776.0d0, 21832.0d0, 21776.0d0, &
    !         21832.0d0, 21960.0d0, 21912.0d0, 21936.0d0, 22008.0d0, &
    !         22104.0d0, 21416.0d0, 21984.0d0, 21888.0d0, 21808.0d0, &
    !         22128.0d0, 22144.0d0]
    !     x(:,:,1) = transpose(reshape(op, [ntests, nparts]))

    !     ! Fill in the data for operator 2
    !     op = [21984.0d0, 22192.0d0, 22120.0d0, 22032.0d0, 22232.0d0, &
    !         22232.0d0, 21960.0d0, 21952.0d0, 21960.0d0, 21984.0d0, &
    !         22008.0d0, 22152.0d0, 21832.0d0, 21840.0d0, 21912.0d0, &
    !         22008.0d0, 22032.0d0, 22032.0d0, 22032.0d0, 22144.0d0, &
    !         22128.0d0, 21912.0d0, 22056.0d0, 22168.0d0, 22032.0d0, &
    !         22192.0d0, 22232.0d0]
    !     x(:,:,2) = transpose(reshape(op, [ntests, nparts]))
    ! end function

    ! ! The solution to this problem has been computed using JMP v15
    ! function test_2() result(check)
    !     ! Parameters
    !     real(real64), parameter :: tol = 1.0d-1
    !     real(real64), parameter :: part2part = 5.839465d0
    !     real(real64), parameter :: repeatability = 54.853534d0
    !     real(real64), parameter :: reproducibility = 12.520450d0
    !     real(real64), parameter :: gauge = 49.248994d0
    !     real(real64), parameter :: pt = 2.46245d0

    !     ! Local Variables
    !     real(real64), allocatable, dimension(:,:,:) :: x
    !     type(grr_results) :: rst
    !     logical :: check

    !     ! General Initialization
    !     check = .true.

    !     ! Initialize the data set
    !     x = format_data_2()

    !     ! Compute the GR&R
    !     rst = ems_gauge_r_r(x, tol = 1.0d3, alpha = 0.05d0)

    !     ! Compare some critical results
    !     if (abs(part2part - rst%part_variation) > tol) then
    !         check = .false.
    !         print '(AE10.3AE10.3)', "TEST_GRR - 2: Part-Part Variation - Expected: ", &
    !             part2part, ", but found: ", rst%part_variation
    !     end if

    !     if (abs(repeatability - rst%repeatability) > tol) then
    !         check = .false.
    !         print '(AE10.3AE10.3)', "TEST_GRR - 2: Repeatability - Expected: ", &
    !             repeatability, ", but found: ", rst%repeatability
    !     end if

    !     if (abs(reproducibility - rst%reproducibility) > tol) then
    !         check = .false.
    !         print '(AE10.3AE10.3)', "TEST_GRR - 2: Reproducibility - Expected: ", &
    !             reproducibility, ", but found: ", rst%reproducibility
    !     end if

    !     if (abs(gauge - rst%gauge_variation) > tol) then
    !         check = .false.
    !         print '(AE10.3AE10.3)', "TEST_GRR - 2: Gauge Variation - Expected: ", &
    !             gauge, ", but found: ", rst%gauge_variation
    !     end if

    !     if (abs(pt - rst%pt_ratio) > tol) then
    !         check = .false.
    !         print '(AE10.3AE10.3)', "TEST_GRR - 2: P/T RATIO - Exepected: ", &
    !             pt, ", but found: ", rst%pt_ratio
    !     end if
    ! end function
end program