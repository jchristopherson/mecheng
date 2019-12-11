! arrays.f90

!> @brief \b arrays
!!
!! @par Purpose
!! The arrays module provides general array-specific operations.
module arrays
    use iso_fortran_env
    implicit none
    private
    public :: diff
    public :: cumulative_sum
    public :: exists_in
    public :: index_of
    public :: round
    public :: unwrap

    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    interface diff
        module procedure :: diff_dbl
        module procedure :: diff_sngl
        module procedure :: diff_cmplxdbl
        module procedure :: diff_cmplxsngl
        module procedure :: diff_int8
        module procedure :: diff_int16
        module procedure :: diff_int32
        module procedure :: diff_int64
    end interface

    !> @brief Computes the cumulative sum of an array.
    interface cumulative_sum
        module procedure :: cumsum_dbl
        module procedure :: cumsum_sngl
        module procedure :: cumsum_cmplxdbl
        module procedure :: cumsum_cmplxsngl
        module procedure :: cumsum_int8
        module procedure :: cumsum_int16
        module procedure :: cumsum_int32
        module procedure :: cumsum_int64
    end interface

    !> @brief Tests to see if an item exists within the array.
    interface exists_in
        module procedure :: exists_in_dbl
        module procedure :: exists_in_sngl
        module procedure :: exists_in_cmplxdbl
        module procedure :: exists_in_cmplxsngl
        module procedure :: exists_in_int8
        module procedure :: exists_in_int16
        module procedure :: exists_in_int32
        module procedure :: exists_in_int64
    end interface

    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    interface index_of
        module procedure :: index_of_dbl
        module procedure :: index_of_sngl
        module procedure :: index_of_cmplxdbl
        module procedure :: index_of_cmplxsngl
        module procedure :: index_of_int8
        module procedure :: index_of_int16
        module procedure :: index_of_int32
        module procedure :: index_of_int64
    end interface

    !> @brief Rounds a number to the required precision, but rounds 0.5 dow to
    !! the next lowest value.
    interface round
        module procedure :: round_dbl
        module procedure :: round_sngl
    end interface

    !> @brief Unwraps an array with pi magnitude jumps.
    interface unwrap
        module procedure :: unwrap_dbl
        module procedure :: unwrap_sngl
    end interface

contains
! ------------------------------------------------------------------------------
    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-1 element array.
    pure function diff_dbl(x) result(dx)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        real(real64), allocatable, dimension(:) :: dx

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        if (n == 0) return

        ! Process
        if (n == 1) then
            allocate(dx(1))
            dx = 0.0d0
            return
        end if

        allocate(dx(n - 1))
        do i = 1, n - 1
            dx(i) = x(i+1) - x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-1 element array.
    pure function diff_sngl(x) result(dx)
        ! Arguments
        real(real32), intent(in), dimension(:) :: x
        real(real32), allocatable, dimension(:) :: dx

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        if (n == 0) return

        ! Process
        if (n == 1) then
            allocate(dx(1))
            dx = 0.0
            return
        end if

        allocate(dx(n - 1))
        do i = 1, n - 1
            dx(i) = x(i+1) - x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-1 element array.
    pure function diff_cmplxsngl(x) result(dx)
        ! Arguments
        complex(real32), intent(in), dimension(:) :: x
        complex(real32), allocatable, dimension(:) :: dx

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        if (n == 0) return

        ! Process
        if (n == 1) then
            allocate(dx(1))
            dx = (0.0, 0.0)
            return
        end if

        allocate(dx(n - 1))
        do i = 1, n - 1
            dx(i) = x(i+1) - x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-1 element array.
    pure function diff_cmplxdbl(x) result(dx)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: dx

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        if (n == 0) return

        ! Process
        if (n == 1) then
            allocate(dx(1))
            dx = (0.0d0, 0.0d0)
            return
        end if

        allocate(dx(n - 1))
        do i = 1, n - 1
            dx(i) = x(i+1) - x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-1 element array.
    pure function diff_int8(x) result(dx)
        ! Arguments
        integer(int8), intent(in), dimension(:) :: x
        integer(int8), allocatable, dimension(:) :: dx

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        if (n == 0) return

        ! Process
        if (n == 1) then
            allocate(dx(1))
            dx = 0
            return
        end if

        allocate(dx(n - 1))
        do i = 1, n - 1
            dx(i) = x(i+1) - x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-1 element array.
    pure function diff_int16(x) result(dx)
        ! Arguments
        integer(int16), intent(in), dimension(:) :: x
        integer(int16), allocatable, dimension(:) :: dx

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        if (n == 0) return

        ! Process
        if (n == 1) then
            allocate(dx(1))
            dx = 0
            return
        end if

        allocate(dx(n - 1))
        do i = 1, n - 1
            dx(i) = x(i+1) - x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-1 element array.
    pure function diff_int32(x) result(dx)
        ! Arguments
        integer(int32), intent(in), dimension(:) :: x
        integer(int32), allocatable, dimension(:) :: dx

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        if (n == 0) return

        ! Process
        if (n == 1) then
            allocate(dx(1))
            dx = 0
            return
        end if

        allocate(dx(n - 1))
        do i = 1, n - 1
            dx(i) = x(i+1) - x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the difference between items in array.  The resulting
    !! array is one element shorter than the source array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-1 element array.
    pure function diff_int64(x) result(dx)
        ! Arguments
        integer(int64), intent(in), dimension(:) :: x
        integer(int64), allocatable, dimension(:) :: dx

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        if (n == 0) return

        ! Process
        if (n == 1) then
            allocate(dx(1))
            dx = 0
            return
        end if

        allocate(dx(n - 1))
        do i = 1, n - 1
            dx(i) = x(i+1) - x(i)
        end do
    end function

! ******************************************************************************
    !> @brief Computes the cumulative sum of an array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-element array.
    pure function cumsum_dbl(x) result(y)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        real(real64), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(x)
        allocate(y(n))
        y(1) = x(1)
        do i = 2, n
            ! y(i) = sum(x(1:i))
            y(i) = y(i-1) + x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cumulative sum of an array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-element array.
    pure function cumsum_sngl(x) result(y)
        ! Arguments
        real(real32), intent(in), dimension(:) :: x
        real(real32), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(x)
        allocate(y(n))
        y(1) = x(1)
        do i = 2, n
            ! y(i) = sum(x(1:i))
            y(i) = y(i-1) + x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cumulative sum of an array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-element array.
    pure function cumsum_cmplxdbl(x) result(y)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        complex(real64), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(x)
        allocate(y(n))
        y(1) = x(1)
        do i = 2, n
            ! y(i) = sum(x(1:i))
            y(i) = y(i-1) + x(i)
        end do
    end function
    
! ------------------------------------------------------------------------------
    !> @brief Computes the cumulative sum of an array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-element array.
    pure function cumsum_cmplxsngl(x) result(y)
        ! Arguments
        complex(real32), intent(in), dimension(:) :: x
        complex(real32), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(x)
        allocate(y(n))
        y(1) = x(1)
        do i = 2, n
            ! y(i) = sum(x(1:i))
            y(i) = y(i-1) + x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cumulative sum of an array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-element array.
    pure function cumsum_int8(x) result(y)
        ! Arguments
        integer(int8), intent(in), dimension(:) :: x
        integer(int8), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(x)
        allocate(y(n))
        y(1) = x(1)
        do i = 2, n
            ! y(i) = sum(x(1:i))
            y(i) = y(i-1) + x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cumulative sum of an array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-element array.
    pure function cumsum_int16(x) result(y)
        ! Arguments
        integer(int16), intent(in), dimension(:) :: x
        integer(int16), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(x)
        allocate(y(n))
        y(1) = x(1)
        do i = 2, n
            ! y(i) = sum(x(1:i))
            y(i) = y(i-1) + x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cumulative sum of an array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-element array.
    pure function cumsum_int32(x) result(y)
        ! Arguments
        integer(int32), intent(in), dimension(:) :: x
        integer(int32), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(x)
        allocate(y(n))
        y(1) = x(1)
        do i = 2, n
            ! y(i) = sum(x(1:i))
            y(i) = y(i-1) + x(i)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Computes the cumulative sum of an array.
    !!
    !! @param[in] x The N-element array on which to operate.
    !! @return The resulting N-element array.
    pure function cumsum_int64(x) result(y)
        ! Arguments
        integer(int64), intent(in), dimension(:) :: x
        integer(int64), allocatable, dimension(:) :: y

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(x)
        allocate(y(n))
        y(1) = x(1)
        do i = 2, n
            ! y(i) = sum(x(1:i))
            y(i) = y(i-1) + x(i)
        end do
    end function

! ******************************************************************************
    !> @brief Tests to see if an item exists within the array.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return Returns true if the item is found in the array; else, false.
    pure function exists_in_dbl(x, item) result(check)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(in) :: item
        logical :: check

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: tol

        ! Initialization
        check = .false.
        n = size(x)
        if (n < 1) return
        tol = sqrt(epsilon(tol))

        ! Process
        do i = 1, n
            if (abs(x(i) - item) < tol) then
                check = .true.
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an item exists within the array.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return Returns true if the item is found in the array; else, false.
    pure function exists_in_sngl(x, item) result(check)
        ! Arguments
        real(real32), intent(in), dimension(:) :: x
        real(real32), intent(in) :: item
        logical :: check

        ! Local Variables
        integer(int32) :: i, n
        real(real32) :: tol

        ! Initialization
        check = .false.
        n = size(x)
        if (n < 1) return
        tol = sqrt(epsilon(tol))

        ! Process
        do i = 1, n
            if (abs(x(i) - item) < tol) then
                check = .true.
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an item exists within the array.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return Returns true if the item is found in the array; else, false.
    pure function exists_in_cmplxdbl(x, item) result(check)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        complex(real64), intent(in) :: item
        logical :: check

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: tol

        ! Initialization
        check = .false.
        n = size(x)
        if (n < 1) return
        tol = sqrt(epsilon(tol))

        ! Process
        do i = 1, n
            if (abs(x(i) - item) < tol) then
                check = .true.
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an item exists within the array.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return Returns true if the item is found in the array; else, false.
    pure function exists_in_cmplxsngl(x, item) result(check)
        ! Arguments
        complex(real32), intent(in), dimension(:) :: x
        complex(real32), intent(in) :: item
        logical :: check

        ! Local Variables
        integer(int32) :: i, n
        real(real32) :: tol

        ! Initialization
        check = .false.
        n = size(x)
        if (n < 1) return
        tol = sqrt(epsilon(tol))

        ! Process
        do i = 1, n
            if (abs(x(i) - item) < tol) then
                check = .true.
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an item exists within the array.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return Returns true if the item is found in the array; else, false.
    pure function exists_in_int8(x, item) result(check)
        ! Arguments
        integer(int8), intent(in), dimension(:) :: x
        integer(int8), intent(in) :: item
        logical :: check

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        check = .false.
        n = size(x)
        if (n < 1) return

        ! Process
        do i = 1, n
            if (x(i) == item) then
                check = .true.
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an item exists within the array.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return Returns true if the item is found in the array; else, false.
    pure function exists_in_int16(x, item) result(check)
        ! Arguments
        integer(int16), intent(in), dimension(:) :: x
        integer(int16), intent(in) :: item
        logical :: check

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        check = .false.
        n = size(x)
        if (n < 1) return

        ! Process
        do i = 1, n
            if (x(i) == item) then
                check = .true.
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an item exists within the array.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return Returns true if the item is found in the array; else, false.
    pure function exists_in_int32(x, item) result(check)
        ! Arguments
        integer(int32), intent(in), dimension(:) :: x
        integer(int32), intent(in) :: item
        logical :: check

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        check = .false.
        n = size(x)
        if (n < 1) return

        ! Process
        do i = 1, n
            if (x(i) == item) then
                check = .true.
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if an item exists within the array.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return Returns true if the item is found in the array; else, false.
    pure function exists_in_int64(x, item) result(check)
        ! Arguments
        integer(int64), intent(in), dimension(:) :: x
        integer(int64), intent(in) :: item
        logical :: check

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        check = .false.
        n = size(x)
        if (n < 1) return

        ! Process
        do i = 1, n
            if (x(i) == item) then
                check = .true.
                exit
            end if
        end do
    end function

! ******************************************************************************
    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return If found, the index of @p item; else, 0 if not found.
    pure function index_of_dbl(x, item) result(ind)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(in) :: item
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: tol

        ! Initialization
        n = size(x)
        ind = 0
        if (n < 1) return
        tol = sqrt(epsilon(tol))

        ! Process
        do i = 1, n
            if (abs(x(i) - item) < tol) then
                ind = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return If found, the index of @p item; else, 0 if not found.
    pure function index_of_sngl(x, item) result(ind)
        ! Arguments
        real(real32), intent(in), dimension(:) :: x
        real(real32), intent(in) :: item
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n
        real(real32) :: tol

        ! Initialization
        n = size(x)
        ind = 0
        if (n < 1) return
        tol = sqrt(epsilon(tol))

        ! Process
        do i = 1, n
            if (abs(x(i) - item) < tol) then
                ind = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return If found, the index of @p item; else, 0 if not found.
    pure function index_of_cmplxdbl(x, item) result(ind)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: x
        complex(real64), intent(in) :: item
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n
        real(real64) :: tol

        ! Initialization
        n = size(x)
        ind = 0
        if (n < 1) return
        tol = sqrt(epsilon(tol))

        ! Process
        do i = 1, n
            if (abs(x(i) - item) < tol) then
                ind = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return If found, the index of @p item; else, 0 if not found.
    pure function index_of_cmplxsngl(x, item) result(ind)
        ! Arguments
        complex(real32), intent(in), dimension(:) :: x
        complex(real32), intent(in) :: item
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n
        real(real32) :: tol

        ! Initialization
        n = size(x)
        ind = 0
        if (n < 1) return
        tol = sqrt(epsilon(tol))

        ! Process
        do i = 1, n
            if (abs(x(i) - item) < tol) then
                ind = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return If found, the index of @p item; else, 0 if not found.
    pure function index_of_int8(x, item) result(ind)
        ! Arguments
        integer(int8), intent(in), dimension(:) :: x
        integer(int8), intent(in) :: item
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        ind = 0
        if (n < 1) return

        ! Process
        do i = 1, n
            if (x(i) == item) then
                ind = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return If found, the index of @p item; else, 0 if not found.
    pure function index_of_int16(x, item) result(ind)
        ! Arguments
        integer(int16), intent(in), dimension(:) :: x
        integer(int16), intent(in) :: item
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        ind = 0
        if (n < 1) return

        ! Process
        do i = 1, n
            if (x(i) == item) then
                ind = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return If found, the index of @p item; else, 0 if not found.
    pure function index_of_int32(x, item) result(ind)
        ! Arguments
        integer(int32), intent(in), dimension(:) :: x
        integer(int32), intent(in) :: item
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        ind = 0
        if (n < 1) return

        ! Process
        do i = 1, n
            if (x(i) == item) then
                ind = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Locates the first occurrence of an item in the array, and then 
    !! returns its index.
    !!
    !! @param[in] x The array to search.
    !! @param[in] item The item of interest.
    !! @return If found, the index of @p item; else, 0 if not found.
    pure function index_of_int64(x, item) result(ind)
        ! Arguments
        integer(int64), intent(in), dimension(:) :: x
        integer(int64), intent(in) :: item
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n

        ! Initialization
        n = size(x)
        ind = 0
        if (n < 1) return

        ! Process
        do i = 1, n
            if (x(i) == item) then
                ind = i
                exit
            end if
        end do
    end function

! ******************************************************************************
    !> @brief Rounds a number to the required precision, but rounds 0.5 dow to
    !! the next lowest value.
    !!
    !! @param[in] x The value on which to operate.
    !! @param[in] p The precision with which to round.
    !! @return The result of the operation.
    pure elemental function round_dbl(x, p) result(y)
        ! Arguments
        real(real64), intent(in) :: x
        integer(int32), intent(in) :: p
        real(real64) :: y

        ! Local Variables
        real(real64) :: scale, val

        ! Compute the scling factor
        scale = 10.0d0**(-p)

        ! Apply the scaling factor, and round accordingly
        val = x * scale + 0.49d0
        y = floor(val) / scale
    end function

! ------------------------------------------------------------------------------
    !> @brief Rounds a number to the required precision, but rounds 0.5 dow to
    !! the next lowest value.
    !!
    !! @param[in] x The value on which to operate.
    !! @param[in] p The precision with which to round.
    !! @return The result of the operation.
    pure elemental function round_sngl(x, p) result(y)
        ! Arguments
        real(real32), intent(in) :: x
        integer(int32), intent(in) :: p
        real(real32) :: y

        ! Local Variables
        real(real32) :: scale, val

        ! Compute the scling factor
        scale = 10.0**(-p)

        ! Apply the scaling factor, and round accordingly
        val = x * scale + 0.49
        y = floor(val) / scale
    end function

! ******************************************************************************
    subroutine unwrap_dbl(x)
        ! Modules
        use constants, only : pi

        ! Arguments
        real(real64), intent(inout), dimension(:) :: x

        ! Local Variables
        integer(int32) :: i, n
        real(real64), allocatable, dimension(:) :: dp, dpcorr
        real(real64) :: cutoff

        ! Initialization
        n = size(x)
        cutoff = pi

        ! Compute the incremental variations
        dp = diff(x)

        ! Compute how many times 2*pi we're off, and round to the
        ! nearest integer with the tie-breaker rounding n + 0.5
        ! down to n
        dpcorr = round(dp / (2.0d0 * pi), -1)

        ! Stop the jump from happening if dp < cutoff
        do i = 1, n - 1
            if (abs(dp(i)) < cutoff) dpcorr(i) = 0.0d0
        end do

        ! Integrate the corrections
        x(2:n) = x(2:n) - (2.0d0 * pi) * cumulative_sum(dpcorr)
    end subroutine

! ------------------------------------------------------------------------------
    subroutine unwrap_sngl(x)
        ! Arguments
        real(real32), intent(inout), dimension(:) :: x

        ! Constants
        real(real32), parameter :: pi = 3.14159265359

        ! Local Variables
        integer(int32) :: i, n
        real(real32), allocatable, dimension(:) :: dp, dpcorr
        real(real32) :: cutoff

        ! Initialization
        n = size(x)
        cutoff = pi

        ! Compute the incremental variations
        dp = diff(x)

        ! Compute how many times 2*pi we're off, and round to the
        ! nearest integer with the tie-breaker rounding n + 0.5
        ! down to n
        dpcorr = round(dp / (2.0 * pi), -1)

        ! Stop the jump from happening if dp < cutoff
        do i = 1, n - 1
            if (abs(dp(i)) < cutoff) dpcorr(i) = 0.0
        end do

        ! Integrate the corrections
        x(2:n) = x(2:n) - (2.0 * pi) * cumulative_sum(dpcorr)
    end subroutine

! ------------------------------------------------------------------------------
end module