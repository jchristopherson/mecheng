! neural_net_constants.f90

module neural_net_constants
    use iso_fortran_env
    use collection_errors
    implicit none

    !> An out-of-memory error flag.
    integer(int32), parameter :: NN_OUT_OF_MEMORY_ERROR = COLLECTION_OUT_OF_MEMORY_ERROR
    !> An invalid input error flag.
    integer(int32), parameter :: NN_INVALID_INPUT_ERROR = COLLECTION_INVALID_INPUT_ERROR
    !> An error flag denoting an invalid network construction.
    integer(int32), parameter :: NN_INVALID_NETWORK_ERROR = 1003
    !> An error flag denoting an invalid layer construction.
    integer(int32), parameter :: NN_INVALID_LAYER_ERROR = 1004
    !> A null pointer error flag.
    integer(int32), parameter :: NN_NULL_POINTER_ERROR = 1005
    !> An array size error flag.
    integer(int32), parameter :: NN_ARRAY_SIZE_ERROR = 1006
    !> An error flag denoting an uninitialized object error.
    integer(int32), parameter :: NN_UNINITIALIZED_ERROR = 1007
    !> An error flag indicating the supplied index was outside the bounds of the collection.
    integer(int32), parameter :: NN_INDEX_OUT_OF_RANGE_ERROR = 1008
end module