! test_grr.f90

program main
    use iso_fortran_env
    use measurements
    implicit none

    ! Parameters
    real(real64), parameter :: tol = 1.0d3

    ! Local Variables
    real(real64), allocatable, dimension(:,:,:) :: x
    type(grr_results) :: rst

    ! Initialize the data set
    x = format_data()

    ! Compute the GR&R
    rst = gage_r_r(x, tol = tol)

    ! Print out the results
    print *, "Part Variation (% of total): ", 1.0d2 * rst%part_variation / rst%total_variation
    print *, "Operator Variation (% of total): ", 1.0d2 * rst%operator_variation / rst%total_variation
    print *, "Part-Operator Variation (% of total): ", 1.0d2 * rst%part_by_operator_variation / rst%total_variation
    print *, "Gage Variation (% of total): ", 1.0d2 * rst%gage_variation / rst%total_variation
    print *, "P/T Ratio: ", rst%pt_ratio

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
        op = [39736.0d0, 40088.0d0, 39952.0d0, 39840.0d0, 39792.0d0, &
            39968.0d0, 39384.0d0, 39648.0d0, 39704.0d0, 39152.0d0, &
            39240.0d0, 39312.0d0, 38832.0d0, 38688.0d0, 38704.0d0, &
            38936.0d0, 38872.0d0, 38928.0d0, 39040.0d0, 38952.0d0, &
            38968.0d0, 39128.0d0, 38928.0d0, 38928.0d0, 38712.0d0, &
            38688.0d0, 38624.0d0]
        x(:,:,1) = transpose(reshape(op, [ntests, nparts]))

        ! Fill in the data for operator 2
        op = [40256.0d0, 39864.0d0, 39792.0d0, 39816.0d0, 39888.0d0, &
            39784.0d0, 39760.0d0, 39944.0d0, 39832.0d0, 39272.0d0, &
            39288.0d0, 39296.0d0, 38728.0d0, 38768.0d0, 38704.0d0, &
            38896.0d0, 38952.0d0, 38968.0d0, 39032.0d0, 38968.0d0, &
            38992.0d0, 39032.0d0, 39008.0d0, 38968.0d0, 38592.0d0, &
            38640.0d0, 38584.0d0]
        x(:,:,2) = transpose(reshape(op, [ntests, nparts]))
    end function
end program