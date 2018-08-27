! collection_linked_list.f90

!> @brief \b collection_linked_list
!!
!! @par Purpose
!! Provides various forms of linked list types.
module collection_linked_list
    use, intrinsic :: iso_fortran_env, only : int32
    use collection_containers, only : dll_node
    use collection_errors
    use ferror, only : errors
    implicit none

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a doubly-linked-list type.
    !!
    !! @par Remarks
    !! This list does not manage or constrain object lifetime.  Contents of this
    !! list may go out of scope, dependent upon how this collection is used.
    !! For a persistent list, see the persistent_linked_list type.
    !!
    !! @par Example
    !! Consider the following example illustrating basic use of the 
    !! linked_list type.  Notice, the list type keeps a pointer to the actual
    !! item stored.  As such, if the item is changed, so is the item in the
    !! list.  The example below provides an illustration of this behavior.  If
    !! this behavior is not desired, the persistent_linked_list type is likely a
    !! better choice.
    !!
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env, only : int32
    !!     use collection_linked_list
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 20
    !!
    !!     ! Local Variables
    !!    type(persistent_linked_list) :: lst
    !!     integer(int32) :: i
    !!     class(*), pointer :: ptr
    !!     logical :: flag
    !!   
    !!     ! Build the list
    !!     do i = 1, n
    !!         call lst%push(i)
    !!     end do
    !!
    !!     ! Display the list
    !!     flag = .true.
    !!     call lst%move_to_first()
    !!     do while (flag)
    !!         ptr => lst%current()
    !!         flag = lst%next()
    !!         select type (ptr)
    !!         type is (integer(int32))
    !!             print '(I0)', ptr
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
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! 21
    !! @endcode
    type linked_list
    private
        !> The number of items in the list.
        integer(int32) :: m_count = 0
        !> A pointer to the first node in the list.
        type(dll_node), pointer :: m_first => null()
        !> A pointer to the last node in the list.
        type(dll_node), pointer :: m_last => null()
        !> A pointer to the current node.
        type(dll_node), pointer :: m_node => null()
    contains
        !> @brief Cleans up resources held by the list.
        final :: ll_destroy
        !> @brief Gets the number of items in the list.
        procedure, public :: get_count => ll_get_count
        !> @brief Gets a pointer to the currently selected item in the list.
        procedure, public :: current => ll_get_current
        !> @brief Moves to the next item in the list.
        procedure, public :: next => ll_next
        !> @brief Moves to the previous item in the list.
        procedure, public :: previous => ll_previous
        !> @brief Tests to see if the current node is the first node in the 
        !! list.
        procedure, public :: is_first => ll_is_first
        !> @brief Tests to see if the current node is the last node in the list.
        procedure, public :: is_last => ll_is_last
        !> @brief Moves to the beginning of the list.
        procedure, public :: move_to_first => ll_move_to_first
        !> @brief Moves to the end of the list.
        procedure, public :: move_to_last => ll_move_to_last
        !> @brief Pushes an item onto the end of the list.
        procedure, public :: push => ll_push
        !> @brief Pops the last item from the end of the list.
        procedure, public :: pop => ll_pop
        !> @brief Inserts an item into the list at the specified location.
        procedure, public :: insert => ll_insert
        !> @brief Remvoes an item from the list.
        procedure, public :: remove => ll_remove
        !> @brief Clears the list of all items.
        procedure, public :: clear => ll_clear
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a generic, persistent linked list type.
    !!
    !! @par Remarks
    !! This list is identical to its base linked_list class with the exception 
    !! that a copy of the supplied object is made, and stored.  As a result, 
    !! the list can, to an extent, control the lifetime(s) of the stored 
    !! item(s).
    !!
    !! @par Example
    !! Consider the following example illustrating basic use of the 
    !! persistent_linked_list type.
    !! @code{.f90}
    !! program example
    !!     use, intrinsic :: iso_fortran_env, only : int32
    !!     use collection_linked_list
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: n = 20
    !!
    !!     ! Local Variables
    !!    type(persistent_linked_list) :: lst
    !!     integer(int32) :: i
    !!     class(*), pointer :: ptr
    !!     logical :: flag
    !!   
    !!     ! Build the list
    !!     do i = 1, n
    !!         call lst%push(i)
    !!     end do
    !!
    !!     ! Display the list
    !!     flag = .true.
    !!     call lst%move_to_first()
    !!     do while (flag)
    !!         ptr => lst%current()
    !!         flag = lst%next()
    !!         select type (ptr)
    !!         type is (integer(int32))
    !!             print '(I0)', ptr
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
    !! 1
    !! 2
    !! 3
    !! 4
    !! 5
    !! 6
    !! 7
    !! 8
    !! 9
    !! 10
    !! 11
    !! 12
    !! 13
    !! 14
    !! 15
    !! 16
    !! 17
    !! 18
    !! 19
    !! 20
    !! @endcode
    type, extends(linked_list) :: persistent_linked_list
    contains
        !> @brief Cleans up resources held by the list.
        final :: pll_destroy
        !> @brief Pushes an item onto the end of the list.
        procedure, public :: push => pll_push
        !> @brief Pops the last item from the end of the list.
        procedure, public :: pop => pll_pop
        !> @brief Inserts an item into the list at the specified location.
        procedure, public :: insert => pll_insert
        !> @brief Remvoes an item from the list.
        procedure, public :: remove => pll_remove
        !> @brief Clears the list of all items.
        procedure, public :: clear => pll_clear
    end type

contains
! ******************************************************************************
! LINKED_LIST MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Cleans up resources held by the list.
    !!
    !! @param[in,out] this The linked_list object.
    subroutine ll_destroy(this)
        type(linked_list), intent(inout) :: this
        call this%clear()
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the number of items in the list.
    !!
    !! @param[in] this The linked_list object.
    !! @return The number of items in the list.
    pure function ll_get_count(this) result(x)
        class(linked_list), intent(in) :: this
        integer(int32) :: x
        x = this%m_count
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a pointer to the currently selected item in the list.
    !!
    !! @param[in] this The linked_list object.
    !! @return A pointer to the currently selected item.  Notice, this may be 
    !!  null in the event of an empty collection.
    function ll_get_current(this) result(x)
        class(linked_list), intent(in) :: this
        class(*), pointer :: x
        if (associated(this%m_node)) then
            x => this%m_node%item
        else
            nullify(x)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Moves to the next item in the list.
    !!
    !! @param[in] this The linked_list object.
    !! @return Returns true if the move was successful; else, false.
    function ll_next(this) result(x)
        ! Arguments
        class(linked_list), intent(inout) :: this
        logical :: x

        ! Local Variables
        type(dll_node), pointer :: ptr

        ! Process
        x = .false.
        if (associated(this%m_node)) then
            ptr => this%m_node%next
            if (associated(ptr)) then
                x = .true.
                this%m_node => ptr
            end if
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Moves to the previous item in the list.
    !!
    !! @param[in] this The linked_list object.
    !! @return Returns true if the move was successful; else, false.
    function ll_previous(this) result(x)
        ! Arguments
        class(linked_list), intent(inout) :: this
        logical :: x

        ! Local Variables
        type(dll_node), pointer :: ptr

        ! Process
        x = .false.
        if (associated(this%m_node)) then
            ptr => this%m_node%previous
            if (associated(ptr)) then
                x = .true.
                this%m_node => ptr
            end if
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if the current node is the first node in the list.
    !!
    !! @param[in] this The linked_list object.
    !! @return Returns true if the current item is the first node in the list.
    pure function ll_is_first(this) result(x)
        ! Arguments
        class(linked_list), intent(in) :: this
        logical :: x

        ! Process
        x = .true.
        if (associated(this%m_node)) then
            if (associated(this%m_node%previous)) x = .false.
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Tests to see if the current node is the last node in the list.
    !!
    !! @param[in] this The linked_list object.
    !! @return Returns true if the current item is the last node in the list.
    pure function ll_is_last(this) result(x)
        ! Arguments
        class(linked_list), intent(in) :: this
        logical :: x

        ! Process
        x = .true.
        if (associated(this%m_node)) then
            if (associated(this%m_node%next)) x = .false.
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Moves to the beginning of the list.
    !!
    !! @param[in,out] this The linked_list object.
    subroutine ll_move_to_first(this)
        class(linked_list), intent(inout) :: this
        this%m_node => this%m_first
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Moves to the end of the list.
    !!
    !! @param[in,out] this The linked_list object.
    subroutine ll_move_to_last(this)
        class(linked_list), intent(inout) :: this
        this%m_node => this%m_last
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pushes an item onto the end of the list.
    !!
    !! @param[in,out] this The linked_list object.
    !! @param[in] x The item to store.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - COLLECTION_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    subroutine ll_push(this, x, err)
        ! Arguments
        class(linked_list), intent(inout) :: this
        class(*), intent(in), target :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        type(dll_node), pointer :: node
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        allocate(node, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("ll_push", &
                "Insufficient memory available.", &
                COLLECTION_OUT_OF_MEMORY_ERROR)
            return
        end if
        node%item => x
        nullify(node%next)
        if (associated(this%m_last)) then
            ! There are already nodes in the collection, and as such,
            ! this is the new last node
            this%m_last%next => node
            node%previous => this%m_last
            this%m_last => node
        else
            ! There are not any nodes in the collection.  This node is both
            ! the first, and the last node in the collection.
            nullify(node%previous)
            this%m_first => node
            this%m_last => node
            this%m_node => node
        end if
        this%m_count = this%m_count + 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pops the last item from the end of the list.
    !!
    !! @param[in,out] this The linked_list object.
    subroutine ll_pop(this)
        ! Arguments
        class(linked_list), intent(inout) :: this

        ! Local Variables
        type(dll_node), pointer :: ptr, prev

        ! Process
        if (this%get_count() > 0) then
            this%m_count = this%m_count - 1
            ptr => this%m_last
            if (associated(ptr)) then
                prev => ptr%previous
                deallocate(ptr)
                nullify(ptr)
                if (associated(prev)) nullify(prev%next)
                this%m_last => prev
            end if
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Inserts an item into the list at the specified location.
    !!
    !! @param[in,out] this The linked_list object.
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
    subroutine ll_insert(this, i, x, err)
        ! Arguments
        class(linked_list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in), target :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: j, n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(dll_node), pointer :: ptr, prev, node
        integer(int32) :: flag

        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (i <= 0 .or. i > n + 1) then
            call errmgr%report_error("ll_insert", &
                "The supplied index is outside the bounds of the list.", &
                COLLECTION_INVALID_INPUT_ERROR)
            return
        end if

        ! Find the right node
        if (i == 1) then
            ! We've got a new first node
            allocate(node, stat = flag)
            if (flag /= 0) then
                call errmgr%report_error("ll_insert", &
                    "Insufficient memory available.", &
                    COLLECTION_OUT_OF_MEMORY_ERROR)
                return
            end if
            node%item => x
            node%next => this%m_first
            nullify(node%previous)
            if (n > 0) then
                this%m_first%previous => node
                this%m_first => node
            end if
        else if (i == n + 1) then
            ! Just push a node onto the end of the list
            allocate(node, stat = flag)
            if (flag /= 0) then
                call errmgr%report_error("ll_insert", &
                    "Insufficient memory available.", &
                    COLLECTION_OUT_OF_MEMORY_ERROR)
                return
            end if
            node%item => x
            nullify(node%next)
            if (associated(this%m_last)) then
                ! There are already nodes in the collection
                this%m_last%next => node
                node%previous => this%m_last
                this%m_last => node
            else
                ! This is the first and only node in the collection
                nullify(node%previous)
                this%m_first => node
                this%m_last => node
                this%m_node => node
            end if
        else
            ! We're somewhere in the middle
            allocate(node, stat = flag)
            if (flag /= 0) then
                call errmgr%report_error("ll_insert", &
                    "Insufficient memory available.", &
                    COLLECTION_OUT_OF_MEMORY_ERROR)
                return
            end if
            ptr => this%m_first
            do j = 2, i
                prev => ptr
                ptr => ptr%next
            end do
            node%item => x
            node%next => ptr
            node%previous => prev
            prev%next => node
            ptr%previous => node
        end if
        this%m_count = this%m_count + 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Remvoes an item from the list.
    !!
    !! @param[in,out] this The linked_list object.
    !! @param[in] i The index of the item to remove.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_INVALID_INPUT_ERROR: Occurs if @p i is less than or equal to
    !!      0, or if @p i is larger than 1 element beyond the current size of
    !!      the list.
    subroutine ll_remove(this, i, err)
        ! Arguments
        class(linked_list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: j, n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(dll_node), pointer :: ptr, next, prev

        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there are items to remove
        if (n == 0) then
            call errmgr%report_error("ll_remove", &
                "Attempted to remove items from an already empty list.", &
                COLLECTION_INVALID_OPERATION_ERROR)
            return
        end if

        ! Input Check
        if (i <= 0 .or. i > n) then
            call errmgr%report_error("ll_remove", &
                "The supplied index is outside the bounds of the list.", &
                COLLECTION_INVALID_INPUT_ERROR)
            return
        end if

        ! Process
        if (i == 1) then
            ! Remove the first node
            ptr => this%m_first
            next => ptr%next
            deallocate(ptr)
            nullify(ptr)
            if (associated(next)) then
                nullify(next%previous)
                this%m_first => next
            else
                nullify(this%m_first)
                nullify(this%m_last)
            end if
        else if (i == n) then
            ! Remove the last node
            ptr => this%m_last
            prev => ptr%previous
            deallocate(ptr)
            nullify(ptr)
            if (associated(prev)) then
                nullify(prev%next)
                this%m_last => prev
            else
                nullify(this%m_first)
                nullify(this%m_last)
            end if
        else
            ! Remove something in the middle
            ptr => this%m_first
            do j = 2, i
                prev => ptr
                ptr => ptr%next
            end do
            next => ptr%next
            prev%next => next
            next%previous => prev
            deallocate(ptr)
            nullify(ptr)
        end if
        this%m_count = this%m_count - 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Clears the list of all items.
    !!
    !! @param[in,out] this The linked_list object.
    subroutine ll_clear(this)
        class(linked_list), intent(inout) :: this
        type(dll_node), pointer :: ptr, next
        ptr => this%m_first
        do
            if (.not.associated(ptr)) exit
            next => ptr%next
            deallocate(ptr)
            nullify(ptr)
            ptr => next
        end do
        nullify(this%m_first)
        nullify(this%m_last)
    end subroutine

! ******************************************************************************
! PERSISTENT_LINKED_LIST MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Pushes an item onto the end of the list.
    !!
    !! @param[in,out] this The persistent_linked_list object.
    !! @param[in] x The item to store.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - COLLECTION_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      available.
    subroutine pll_push(this, x, err)
        ! Arguments
        class(persistent_linked_list), intent(inout) :: this
        class(*), intent(in), target :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(*), pointer :: cpy
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        allocate(cpy, source = x, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pll_push", &
                "Insufficient memory available.", &
                COLLECTION_OUT_OF_MEMORY_ERROR)
            return
        end if
        call this%linked_list%push(cpy, errmgr)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Pops the last item from the end of the list.  The stored item will
    !! be disposed of as part of the execution of the subroutine.
    !!
    !! @param[in,out] this The persistent_linked_list object.
    subroutine pll_pop(this)
        ! Arguments
        class(persistent_linked_list), intent(inout) :: this

        ! Local Variables
        class(*), pointer :: ptr

        ! Process
        ptr => this%m_last
        call this%linked_list%pop()
        if (associated(ptr)) then
            deallocate(ptr)
            nullify(ptr)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Inserts an item into the list at the specified location.
    !!
    !! @param[in,out] this The persistent_linked_list object.
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
    subroutine pll_insert(this, i, x, err)
        ! Arguments
        class(persistent_linked_list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(*), intent(in), target :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(*), pointer :: cpy
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        allocate(cpy, source = x, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("pll_insert", &
                "Insufficient memory available.", &
                COLLECTION_OUT_OF_MEMORY_ERROR)
            return
        end if
        call this%linked_list%insert(i, cpy, errmgr)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Remvoes an item from the list.  The item will be disposed of as 
    !! part of the execution of the subroutine.
    !!
    !! @param[in,out] this The persistent_linked_list object.
    !! @param[in] i The index of the item to remove.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !! - COLLECTION_INVALID_INPUT_ERROR: Occurs if @p i is less than or equal to
    !!      0, or if @p i is larger than 1 element beyond the current size of
    !!      the list.
    subroutine pll_remove(this, i, err)
        ! Arguments
        class(persistent_linked_list), intent(inout) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: j, n
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(*), pointer :: ptr
        type(dll_node), pointer :: node

        ! Initialization
        n = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (i <= 0 .or. i > n) then
            call errmgr%report_error("pll_remove", &
                "The supplied index is outside the bounds of the list.", &
                COLLECTION_INVALID_INPUT_ERROR)
            return
        end if

        ! Find the item being removed
        node => this%m_first
        do j = 2, i
            node => node%next
        end do
        ptr => node%item
        call this%linked_list%remove(i, errmgr)
        if (associated(ptr)) then
            deallocate(ptr)
            nullify(ptr)
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Clears the list of all items.
    !!
    !! @param[in,out] this The persistent_linked_list object.
    subroutine pll_clear(this)
        class(persistent_linked_list), intent(inout) :: this
        type(dll_node), pointer :: node, next
        class(*), pointer :: ptr
        node => this%m_first
        do
            if (.not.associated(node)) exit
            next => node%next
            ptr => node%item
            deallocate(node)
            nullify(node)
            node => next
            if (associated(ptr)) then
                deallocate(ptr)
                nullify(ptr)
            end if
        end do
        nullify(this%m_first)
        nullify(this%m_last)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Cleans up all resources held by the persistent_linked_list.
    !!
    !! @param[in,out] this The persistent_linked_list object.
    subroutine pll_destroy(this)
        type(persistent_linked_list), intent(inout) :: this
        call this%clear()
    end subroutine

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end module
