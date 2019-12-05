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
    rst = gage_r_r(x, tol = tol, alpha = 0.25d0)

    ! Print out the results
    print *, ""
    print *, "Part Variation: ", rst%part_variation
    print *, "Operator Variation: ", rst%operator_variation
    print *, "Part-Operator Variation: ", rst%part_by_operator_variation
    print *, "Gage Variation: ", rst%gage_variation
    print *, "Total Variation: ", rst%total_variation
    print *, "Repeatability: ", rst%repeatability
    print *, "Reproducibility: ", rst%reproducibility
    print *, "P/T Ratio: ", rst%pt_ratio
    print *, "F Statistic: ", rst%f_statistic
    print *, "F Test: ", rst%f_test
    print *, ""

contains
    ! The data set is # of parts -by- # of tests -by- # of operators
    function format_data() result(x)
        ! Arguments
        real(real64), allocatable, dimension(:,:,:) :: x

        ! Parameters
        integer(int32), parameter :: nparts = 5
        integer(int32), parameter :: nops = 2
        integer(int32), parameter :: ntests = 2

        ! Local Variables
        real(real64), allocatable, dimension(:) :: op

        ! Allocate space for the output
        allocate(x(nparts, ntests, nops))
        allocate(op(nparts * ntests))

        ! Fill in the data for operator 1
        op = [-2.599d0, -18.210d0, -15.218d0, 3.105d0, -0.272d0, &
            -2.586d0, 2.540d0, -10.616d0, -15.743d0, -2.958d0]
        x(:,:,1) = transpose(reshape(op, [ntests, nparts]))

        ! Fill in the data for operator 2
        op = [-5.528d0, 1.566d0, -3.805d0, 5.464d0, 6.756d0, & 
            -9.071d0, 11.635d0, -11.929d0, 8.022d0, -7.669d0]
        x(:,:,2) = transpose(reshape(op, [ntests, nparts]))
    end function
end program