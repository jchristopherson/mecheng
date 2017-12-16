! test_columns.f90

program main
    use, intrinsic :: iso_fortran_env, only : real64, int32
    use columns
    use constants
    implicit none

    logical :: rst, overall
    overall = .true.

    rst = test_buckling_1()
    if (.not.rst) overall = .false.

    rst = test_buckling_2()
    if (.not.rst) overall = .false.

    call test_array_inputs()


    print *, ""
    if (overall) then
        print '(A)', "COLUMN BUCKLING TESTS PASSED"
    else 
        print '(A)', "COLUMN BUCKLING TESTS FAILED."
    end if

contains
    ! Tests the Euler buckling equation implementation with the following
    ! example:
    !
    ! Material: Aluminum (6061-T651)
    !   - Modulus of Elasticity: 10e6 psi
    !   - Yield Strength: 40 ksi
    ! Geometry (Rectangular Section):
    !   - Length: 200 inches
    !   - Section Width: 1.5 inches
    !   - Section Depth: 0.5 inches
    ! Boundary Conditions: Pinned-Pinned (Effective Length Multiplier = 1)
    function test_buckling_1() result(rst)
        ! Variables
        logical :: rst
        real(real64), parameter :: tol = 1.0d-12
        real(real64), parameter :: modulus = 10.0d6
        real(real64), parameter :: yield = 40.0d3
        real(real64), parameter :: length = 2.0d2
        real(real64), parameter :: width = 1.5d0
        real(real64), parameter :: depth = 0.5d0
        real(real64) :: area, moi, ratio, transition, p, ans

        ! Compute the cross-sectional properties
        area = width * depth
        moi = width * depth**3 / 12.0d0

        ! Compute the slenderness ratio
        ratio = slenderness_ratio(length, moi, area)

        ! Compute the transition slenderness ratio
        transition = buckling_transition(modulus, yield)

        ! Determine the appropriate buckling equation to utilize
        if (ratio < transition) then
            ! Use the J.B. Johnson equation
            p = johnson_buckling(modulus, yield, ratio, area)
        else
            p = euler_buckling(modulus, ratio, area)
        end if

        ! Display the results
        print '(A)', new_line('a') // "COLUMN BUCKLING TEST 1 RESULTS:"
        print '(AF5.3A)', achar(9) // "Cross-Sectional Area: ", area, " in**2"
        print '(AF7.5A)', achar(9) // "Moment of Inertia: ", moi, " in**4"
        print '(AF8.3)', achar(9) // "Slenderness Ratio: ", ratio
        print '(AF6.3)', achar(9) // "Transition Slenderness Ratio: ", &
            transition
        print '(AF6.3A)', achar(9) // "Critical Buckling Load: ", p, " lbf"

        ! Compute the expected value
        ans = pi**2 * modulus * area / ratio**2
        if (abs(ans - p) > tol) then
            rst = .false.
            print '(AF6.3AF6.3A)', "COLUMN BUCKLING TEST 1 FAILED: " // &
                "Expected a buckling load of ", ans, " lbf, but found ", &
                p, " lbf."
        else
            rst = .true.
        end if
    end function

! ------------------------------------------------------------------------------
    ! Tests the Johnson buckling equation implementation with the following
    ! example:
    !
    ! Material: Aluminum (6061-T651)
    !   - Modulus of Elasticity: 10e6 psi
    !   - Yield Strength: 40 ksi
    ! Geometry (Rectangular Section):
    !   - Length: 5 inches
    !   - Section Width: 1.5 inches
    !   - Section Depth: 0.5 inches
    ! Boundary Conditions: Pinned-Pinned (Effective Length Multiplier = 1)
    function test_buckling_2() result(rst)
        ! Variables
        logical :: rst
        real(real64), parameter :: tol = 1.0d-12
        real(real64), parameter :: modulus = 10.0d6
        real(real64), parameter :: yield = 40.0d3
        real(real64), parameter :: length = 5.0d0
        real(real64), parameter :: width = 1.5d0
        real(real64), parameter :: depth = 0.5d0
        real(real64) :: area, moi, ratio, transition, p, ans

        ! Compute the cross-sectional properties
        area = width * depth
        moi = width * depth**3 / 12.0d0

        ! Compute the slenderness ratio
        ratio = slenderness_ratio(length, moi, area)

        ! Compute the transition slenderness ratio
        transition = buckling_transition(modulus, yield)

        ! Determine the appropriate buckling equation to utilize
        if (ratio < transition) then
            ! Use the J.B. Johnson equation
            p = johnson_buckling(modulus, yield, ratio, area)
        else
            p = euler_buckling(modulus, ratio, area)
        end if

        ! Display the results
        print '(A)', new_line('a') // "COLUMN BUCKLING TEST 2 RESULTS:"
        print '(AF5.3A)', achar(9) // "Cross-Sectional Area: ", area, " in**2"
        print '(AF7.5A)', achar(9) // "Moment of Inertia: ", moi, " in**4"
        print '(AF6.3)', achar(9) // "Slenderness Ratio: ", ratio
        print '(AF6.3)', achar(9) // "Transition Slenderness Ratio: ", &
            transition
        print '(AF9.3A)', achar(9) // "Critical Buckling Load: ", p, " lbf"

        ! Compute the expected value
        ans = area * (yield - (yield * ratio / (2.0d0 * pi))**2 * &
            (1.0d0 / modulus))
        if (abs(ans - p) > tol) then
            rst = .false.
            print '(AF9.3AF9.3A)', "COLUMN BUCKLING TEST 2 FAILED: " // &
                "Expected a buckling load of ", ans, " lbf, but found ", &
                p, " lbf."
        else
            rst = .true.
        end if
    end function

! ------------------------------------------------------------------------------
    !
    subroutine test_array_inputs()
        ! Variables
        logical :: rst
        real(real64), parameter :: tol = 1.0d-12
        real(real64), parameter :: modulus = 10.0d6
        real(real64), parameter :: yield = 40.0d3
        real(real64), parameter :: width = 1.5d0
        real(real64), parameter :: depth = 0.5d0
        real(real64) :: area, moi, ratio, transition, check
        real(real64), dimension(10) :: lengths, loads
        integer(int32) :: i

        ! Initialization
        area = width * depth
        moi = width * depth**3 / 12.0d0
        do i = 1, size(lengths)
            lengths(i) = 10.0d0 * i
        end do

        ! Compute the buckling loads
        loads = buckling_load(modulus, yield, lengths, moi, area)

        ! Check by calculating each individually
        do i = 1, size(lengths)
            check = buckling_load(modulus, yield, lengths(i), moi, area)
            print '(AF9.3AF9.3A)', "Computed: ", loads(i), " lbf.  Expected: ",&
                check, " lbf."
        end do

    end subroutine

end program