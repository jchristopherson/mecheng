! collection_errors.f90

!> @brief \b collection_errors
!!
!! @par Purpose
!! This modules provides error codes supporting the collections library.
module collection_errors
    use, intrinsic :: iso_fortran_env, only : int32
    implicit none

    !> @brief Occurs if there is insufficient memory available for the
    !! requested operation.
    integer(int32), parameter :: COLLECTION_OUT_OF_MEMORY_ERROR = 1000
    !> @brief Occurs if an invalid input is provided.
    integer(int32), parameter :: COLLECTION_INVALID_INPUT_ERROR = 1001
    !> @brief Occurs if an attempt is made to perform an invalid operation.
    integer(int32), parameter :: COLLECTION_INVALID_OPERATION_ERROR = 1002
end module
