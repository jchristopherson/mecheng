! collection_containers.f90

!> @brief \b collection_containers
!!
!! @par Purpose
!! This module provides container types aiding in the construction of generic
!! collections.
module collection_containers
    implicit none
    private
    public :: container
    public :: dll_node

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief A container type allowing storage of most any Fortran type.
    type container
        !> A pointer to a polymorphic variable allowing storage of any type.
        class(*), pointer :: item => null()
    end type

! ------------------------------------------------------------------------------
    !> @brief A node for a doubly-linked-list (DLL) type.
    type dll_node
        !> A pointer to a polymorphic variable allowing storage of any type.
        class(*), pointer :: item => null()
        !> A pointer to the previous node in the list.
        type(dll_node), pointer :: previous => null()
        !> A pointer to the next node in the list.
        type(dll_node), pointer :: next => null()
    end type

end module