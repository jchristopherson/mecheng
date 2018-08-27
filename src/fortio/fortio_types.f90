! fortio_types.f90

!> @brief \b fortio_types
!!
!! @par Purpose
!! Provides types and constants used by the FORTIO library.
module fortio_types
    use, intrinsic :: iso_fortran_env, only : int32
    implicit none

! ******************************************************************************
! ERROR CODES
! ------------------------------------------------------------------------------
    !> @brief Denotes a generic I/O error.
    integer(int32), parameter :: FIO_FILE_IO_ERROR = 1000
    !> @brief Denotes a situation where the user attempts to write to an 
    !!  unopened file.
    integer(int32), parameter :: FIO_UNOPENED_ERROR = 1001
    !> @brief Denotes an attempted read effort beyond the end of a file.
    integer(int32), parameter :: FIO_END_OF_FILE_ERROR = 1002
    !> @brief Denotes a memory availability issue.
    integer(int32), parameter :: FIO_OUT_OF_MEMORY_ERROR = 1003
    !> @brief Denotes an unsupported file version.
    integer(int32), parameter :: FIO_UNSUPPORTED_VERSION_ERROR = 1004
end module
