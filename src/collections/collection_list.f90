! collection_list.f90

!> @brief \b collection_list
!!
!! @par Purpose
!! Defines a generic list type.
module collection_list
    use, intrinsic :: iso_fortran_env, only : int32
    use ferror, only : errors
    use collection_errors
    use collection_containers
    implicit none
    private
    public :: list
    public :: persistent_list

! ******************************************************************************
! CONSTANTS
! ------------------------------------------------------------------------------
    !> @brief The default buffer size.
    integer(int32), parameter :: DEFAULT_BUFFER_SIZE = 10

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief A generic list container.
    !!
    !! @par Remarks
    !! This list does not manage or constrain object lifetime.  Contents of this
    !! list may go out of scope, dependent upon how this collection is used.
    !! For a persistent list, see the persistent_list type.
    !!
    !! @par Example
    !! Consider the following example illustrating basic use of the 
    !! list type.  Notice, the list type keeps a pointer to the actual item 
    !! stored.  As such, if the item is changed, so is the item in the list.
    !! The example below provides an illustration of this behavior.  If this
    !! behavior is not desired, the persistent_list type is likely a better
    !! choice.
    !!
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env, only : int32
    !!     use collection_list
    !!     implicit none
    !! 
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 20
    !! 
    !!     ! Local Variables
    !!     type(list) :: lst
    !!     integer(int32) :: i, j
    !!     class(*), pointer :: ptr
    !! 
    !!     ! Build the list
    !!     do i = 1, n
    !!         call lst%push(i)
    !!     end do
    !! 
    !!     ! Display the list
    !!     do j = 1, n
    !!         ptr => lst%get(j)
    !!         select type (ptr)
    !!         type is (integer(int32))
    !!             print '(AI0AI0)', "Index ", j, ": ", ptr
    !!         end select
    !!     end do
    !! end program
    !! @endcode
    !!
    !! @par
    !! The above program produces the following output.
    !! 
    !! @par
    !! @code{.txt}
    !! Index 1: 21
    !! Index 2: 21
    !! Index 3: 21
    !! Index 4: 21
    !! Index 5: 21
    !! Index 6: 21
    !! Index 7: 21
    !! Index 8: 21
    !! Index 9: 21
    !! Index 10: 21
    !! Index 11: 21
    !! Index 12: 21
    !! Index 13: 21
    !! Index 14: 21
    !! Index 15: 21
    !! Index 16: 21
    !! Index 17: 21
    !! Index 18: 21
    !! Index 19: 21
    !! Index 20: 21
    !! @endcode
    type list
    private
        !> A collection of container objects.
        type(container), allocatable, dimension(:) :: m_list
        !> The actual number of items in m_list
        integer(int32) :: m_count = 0
    contains
        !> @brief Gets the number of items in the list.
        procedure, public :: get_count => list_get_count
        !> @brief Gets the capacity of the list.
        procedure, public :: get_capacity => list_get_capacity
        !> @brief Sets the capacity of the list.
        procedure, public :: set_capacity => list_set_capacity
        !> @brief Gets an item from the list.
        procedure, public :: get => list_get
        !> @brief Sets an item into the list.
        procedure, public :: set => list_set
        !> @brief Pushes an item onto the end of the list.
        procedure, public :: push => list_push
        !> @brief Pops the last item from the end of the list.
        procedure, public :: pop => list_pop
        !> @brief Inserts an item into the list.
        procedure, public :: insert => list_insert
        !> @brief Removes an item from the list.
        procedure, public :: remove => list_remove
        !> @brief Clears the contents of the list.
        procedure, public :: clear => list_clear
        !> @brief A private routine used to store items in the list.
        procedure, private :: store => list_store_item
    end type

! ------------------------------------------------------------------------------
    !> @brief A generic, persistent list type.
    !!
    !! @par Remarks
    !! This list is identical to its base list class with the exception that a
    !! copy of the supplied object is made, and stored.  As a result, the list
    !! can, to an extent, control the lifetime(s) of the stored item(s).
    !!
    !! @par Example
    !! Consider the following example illustrating basic use of the 
    !! persistent_list type.
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env, only : int32
    !!     use collection_list
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 20
    !!
    !!     ! Local Variables
    !!     type(persistent_list) :: lst
    !!     integer(int32) :: i, j
    !!     class(*), pointer :: ptr
    !!
    !!     ! Build the list
    !!     do i = 1, n
    !!         call lst%push(i)
    !!     end do
    !!
    !!     ! Display the list
    !!     do j = 1, n
    !!         ptr => lst%get(j)
    !!         select type (ptr)
    !!         type is (integer(int32))
    !!             print '(AI0AI0)', "Index ", j, ": ", ptr
    !!         end select
    !!     end do
    !! end program
    !! @endcode
    !!
    !! @par
    !! The above program produces the following output.
    !! 
    !! @par
    !! @code{.txt}
    !! Index 1: 1
    !! Index 2: 2
    !! Index 3: 3
    !! Index 4: 4
    !! Index 5: 5
    !! Index 6: 6
    !! Index 7: 7
    !! Index 8: 8
    !! Index 9: 9
    !! Index 10: 10
    !! Index 11: 11
    !! Index 12: 12
    !! Index 13: 13
    !! Index 14: 14
    !! Index 15: 15
    !! Index 16: 16
    !! Index 17: 17
    !! Index 18: 18
    !! Index 19: 19
    !! Index 20: 20
    !! @endcode
    type, extends(list) :: persistent_list
    contains
        !> @brief Cleans up resources held by the list.
        final :: plist_destroy
        !> @brief Sets an item into the list.
        procedure, public :: set => plist_set
        !> @brief Pops the last item from the end of the list.
        procedure, public :: pop => plist_pop
        !> @brief Removes an item from the list.
        procedure, public :: remove => plist_remove
        !> @brief Clears the contents of the list.
        procedure, public :: clear => plist_clear
        !> @brief Pushes an item onto the end of the list.
        procedure, public :: push => plist_push
        !> @brief Inserts an item into the list.
        procedure, public :: insert => plist_insert
    end type

contains
! ******************************************************************************
! LIST MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Gets the number of items in the list.
    !!
    !! @param[in] this The list object.
    !! @return The number of items stored in the list.
    pure function list_get_count(this) result(x)
        class(list), intent(in) :: this
        integer(int32) :: x
        x = this%m_count
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the capacity of the list.
    !!
    !! @param[in] this The list object.
    !! @return The capacity of the list.
    pure function list_get_capacity(this) result(x)
        class(list), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_list)) then
            x = size(this%m_list)
        else
            x = 0
        end if
    end function

! --------------------
    !> @brief Sets the capacity of the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] n The desired capacity of the list.  This value must not be
    !!  less than the number of items already stored in the list.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_INVALID_INPUT_ERROR: Occurs if @p n is less than the number 
    !!      of items already stored in the list.
    !! - COLLECTION_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is 
    !!      available.
    subroutine list_set_capacity(this, n, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag, m
        type(container), allocatable, dimension(:) :: copy
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        m = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (n < m) then
            call errmgr%report_error("list_set_capacity", &
                "Reducing the capacity of the list is not allowed.", &
                COLLECTION_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        if (allocated(this%m_list)) then
            allocate(copy(m), stat = flag)
            if (flag == 0) then
                copy = this%m_list
                deallocate(this%m_list)
                allocate(this%m_list(n), stat = flag)
                if (flag == 0) this%m_list(1:m) = copy
            end if
        else
            allocate(this%m_list(n), stat = flag)
        end if
        if (flag /= 0) then
            call errmgr%report_error("list_set_capacity", &
                "Insufficient memory available.", & 
                COLLECTION_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets an item from the list.
    !!
    !! @param[in] this The list object.
    !! @param[in] i The index of the item.
    !! @return A pointer to the requested item.
    function list_get(this, i) result(x)
        class(list), intent(in) :: this
        integer(int32), intent(in) :: i
        class(*), pointer :: x
        type(container) :: cntr
        cntr = this%m_list(i)
        x => cntr%item
    end function

! --------------------
    !> @brief Sets an item into the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index of the item.
    !! @param[in] x The item to place into the list.
    subroutine list_set(this, i, x)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in), target :: x

        ! Store the object
        call this%store(i, x)
    end subroutine

    !> @brief Stores an item in the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index of the item.
    !! @param[in] x The item to place into the list.
    subroutine list_store_item(this, i, x)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in), target :: x

        ! Store the object
        type(container) :: obj
        obj%item => x
        this%m_list(i) = obj
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an item onto the end of the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] x The object to add to the list.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is 
    !!      available.
    subroutine list_push(this, x, err)
        ! Arguments
        class(list), intent(inout) :: this
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Ensure we've got sufficient capacity; else, increase accordingly
        if (this%get_capacity() <= this%get_count() + 1) then
            call this%set_capacity(this%get_count() + DEFAULT_BUFFER_SIZE, err)
        end if

        ! Store the value
        this%m_count = this%m_count + 1
        call this%store(this%get_count(), x)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pops the last item from the end of the list.
    !!
    !! @param[in,out] this The list object.
    subroutine list_pop(this)
        ! Arguments
        class(list), intent(inout) :: this

        ! Process
        if (this%m_count > 0) then
            this%m_count = this%m_count - 1
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Inserts an item into the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index at which to insert the item.  Items at, and beyond
    !!  this index are shifted back in the list.
    !! @param[in] x The item to insert.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_INVALID_INPUT_ERROR: Occurs if @p i is less than or equal to
    !!      0, or if @p i is larger than 1 element beyond the current size of
    !!      the list.
    !! - COLLECTION_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is 
    !!      available.
    subroutine list_insert(this, i, x, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: j, n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (i <= 0 .or. i > n + 1) then
            call errmgr%report_error("list_insert", &
                "The supplied index is outside the bounds of the list.", &
                COLLECTION_INVALID_INPUT_ERROR)
            return
        end if

        ! Ensure sufficient capacity
        if (this%get_capacity() <= n + 1) then
            call this%set_capacity(n + DEFAULT_BUFFER_SIZE, errmgr)
            if (errmgr%has_error_occurred()) return
        end if

        ! Shift everything back by one element, and insert the specified item
        this%m_count = this%m_count + 1
        do j = n, i, -1
            call this%store(j + 1, this%get(j))
        end do
        call this%store(i, x)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Removes an item from the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index of the item to remove.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_INVALID_INPUT_ERROR: Occurs if @p i is less than or equal 
    !!      to 0, or if @p i is larger than 1 element beyond the current size of
    !!      the list.
    !! - COLLECTION_INVALID_OPERATION_ERROR: Occurs if attempting to remove an 
    !!      item when there are no items left in the list to remove.
    subroutine list_remove(this, i, err)
        ! Arguments
        class(list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there are items to remove
        if (n == 0) then
            call errmgr%report_error("list_remove", &
                "Attempted to remove items from an already empty list.", &
                COLLECTION_INVALID_OPERATION_ERROR)
            return
        end if

        ! Input Check
        if (i <= 0 .or. i > n) then
            call errmgr%report_error("list_remove", &
                "The supplied index is outside the bounds of the list.", &
                COLLECTION_INVALID_INPUT_ERROR)
            return
        end if

        ! Shift everything down by one element
        this%m_list(i:n-1) = this%m_list(i+1:n)
        this%m_count = this%m_count - 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Clears the contents of the list.
    !!
    !! @param[in,out] this The list object.
    subroutine list_clear(this)
        class(list), intent(inout) :: this
        this%m_count = 0
    end subroutine

! ******************************************************************************
! PERSISTENT_LIST MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Sets an item into the list.
    !!
    !! @param[in,out] this The persistent_list object.
    !! @param[in] i The index of the item.
    !! @param[in] x The item to place into the list.
    subroutine plist_set(this, i, x)
        ! Arguments
        class(persistent_list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in), target :: x

        ! Local Variables
        class(*), pointer :: cpy, old

        ! Create a copy and store the object
        old => this%get(i)
        if (associated(old)) then
            deallocate(old)
            nullify(old)
        end if
        allocate(cpy, source = x)
        call this%list%set(i, cpy)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Cleans up resources held by the list.
    !!
    !! @param[in,out] this The persistent_list object.
    subroutine plist_destroy(this)
        type(persistent_list), intent(inout) :: this
        call this%clear()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pops the last item from the end of the list.
    !!
    !! @param[in,out] this The persistent_list object.
    subroutine plist_pop(this)
        ! Arguments
        class(persistent_list), intent(inout) :: this

        ! Local Variables
        class(*), pointer :: ptr

        ! Process
        if (this%m_count > 0) then
            ptr => this%m_list(this%m_count)%item
            if (associated(ptr)) then
                deallocate(ptr)
                nullify(ptr)
            end if
            this%m_count = this%m_count - 1
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Removes an item from the list.
    !!
    !! @param[in,out] this The persistent_list object.
    !! @param[in] i The index of the item to remove.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_INVALID_INPUT_ERROR: Occurs if @p i is less than or equal 
    !!      to 0, or if @p i is larger than 1 element beyond the current size of
    !!      the list.
    !! - COLLECTION_INVALID_OPERATION_ERROR: Occurs if attempting to remove an 
    !!      item when there are no items left in the list to remove.
    subroutine plist_remove(this, i, err)
        ! Arguments
        class(persistent_list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(*), pointer :: ptr

        ! Process
        if (i > 0 .and. i <= this%get_count()) then
            ptr => this%get(i)
            if (associated(ptr)) then
                deallocate(ptr)
                nullify(ptr)
            end if
        end if
        call this%list%remove(i, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Clears the contents of the list.
    !!
    !! @param[in,out] this The persistent_list object.
    subroutine plist_clear(this)
        ! Arguments
        class(persistent_list), intent(inout) :: this
        
        ! Local Variables
        class(*), pointer :: ptr
        integer(int32) :: i, n

        ! Process
        n = this%get_count()
        do i = 1, n
            ptr => this%m_list(i)%item
            if (associated(ptr)) then
                deallocate(ptr)
                nullify(ptr)
            end if
        end do
        this%m_count = 0
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an item onto the end of the list.
    !!
    !! @param[in,out] this THe persistent_list object.
    !! @param[in] x The object to add to the list.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is 
    !!      available.
    subroutine plist_push(this, x, err)
        ! Arguments
        class(persistent_list), intent(inout) :: this
        class(*), intent(in) :: x
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        class(*), pointer :: cpy

        ! Process
        allocate(cpy, source = x)
        call this%list%push(cpy, err)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Inserts an item into the list.
    !!
    !! @param[in,out] this The list object.
    !! @param[in] i The index at which to insert the item.  Items at, and beyond
    !!  this index are shifted back in the list.
    !! @param[in] x The item to insert.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_INVALID_INPUT_ERROR: Occurs if @p i is less than or equal to
    !!      0, or if @p i is larger than 1 element beyond the current size of
    !!      the list.
    !! - COLLECTION_OUT_OF_MEMORY_ERROR: Occurs if insufficient memory is 
    !!      available.
    subroutine plist_insert(this, i, x, err)
        ! Arguments
        class(persistent_list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(*), pointer :: cpy
        
        ! Process
        allocate(cpy, source = x)
        call this%list%insert(i, cpy, err)
    end subroutine

! ------------------------------------------------------------------------------
end module