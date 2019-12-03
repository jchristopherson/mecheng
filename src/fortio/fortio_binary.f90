! fortio_binary.f90

!> @brief \b fortio_binary
!!
!! @par Purpose
!! Provides mechanisms to read and write unformatted binary files.
module fortio_binary
    use, intrinsic :: iso_fortran_env
    use fortio_types
    use ferror, only : errors
    implicit none
    private
    public :: is_little_endian
    public :: swap_bytes
    public :: binary_file_manager
    public :: binary_writer
    public :: binary_reader
    public :: binary_formatter

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief The default buffer size for the binary_formatter type.
    integer(int32), parameter :: BINARY_FORMATTER_DEFAULT_SIZE = 2048

    !> @brief Defines a type for formatting binary information.
    type binary_formatter
    private
        !> @brief A buffer into which data may be serialized.
        integer(int8), allocatable, dimension(:) :: m_buffer
        !> @brief The actual number of items in the buffer.
        integer(int32) :: m_count = 0
        !> @brief Determines if the buffer should be written using little or
        !! big endian formatting.
        logical :: m_littleEndian = .true.
    contains
        !> @brief Initializes the binary_formatter object.
        procedure, public :: initialize => bf_init
        !> @brief Gets a logical value determining if the buffer should be filled in
        !! little or big endian format.
        procedure, public :: get_use_little_endian => bf_get_is_little_endian
        !> @brief Sets a logical value determining if the buffer should be filled in
        !! little or big endian format.
        procedure, public :: set_use_little_endian => bf_set_is_little_endian
        !> @brief Adjusts the capacity of bhe binary_formatter to the
        !! requested amount.
        procedure, public :: set_capacity => bf_resize_buffer
        !> @brief Gets the capacity of the binary_formatter, in bytes.
        procedure, public :: get_capacity => bf_get_capacity
        !> @brief Gets the number of bytes stored within the binary_formatter.
        procedure, public :: get_count => bf_get_count
        !> @brief Gets a copy of the internal buffer.
        procedure, public :: get_buffer => bf_get_buffer
        !> @brief Gets a byte-level value in the formatted buffer.
        procedure, public :: get => bf_get_buffer_item
        !> @brief Sets a byte-level value in the formatted buffer.
        procedure, public :: set => bf_set_buffer_item
        !> @brief Appends an item onto the binary_formatter buffer.
        generic, public :: add => bf_add_dbl, bf_add_dbl_array, &
            bf_add_dbl_matrix, bf_add_sngl, bf_add_sngl_array, &
            bf_add_sngl_matrix, bf_add_cmplx64, bf_add_cmplx64_array, &
            bf_add_cmplx64_matrix, bf_add_cmplx32, bf_add_cmplx32_array, &
            bf_add_cmplx32_matrix, bf_add_int16, bf_add_int16_array, &
            bf_add_int16_matrix, bf_add_int64, bf_add_int64_array, &
            bf_add_int64_matrix

        ! TO DO:
        ! - add items to buffer routines
        !   - all real, complex, and integer types for
        !       scalar, 1D, and 2D arrays
        ! - retrieve the buffer
        !   - all real, complex, and integer types for
        !       scalar, 1D, and 2D arrays
        procedure :: bf_add_dbl
        procedure :: bf_add_dbl_array
        procedure :: bf_add_dbl_matrix
        procedure :: bf_add_sngl
        procedure :: bf_add_sngl_array
        procedure :: bf_add_sngl_matrix
        procedure :: bf_add_cmplx64
        procedure :: bf_add_cmplx64_array
        procedure :: bf_add_cmplx64_matrix
        procedure :: bf_add_cmplx32
        procedure :: bf_add_cmplx32_array
        procedure :: bf_add_cmplx32_matrix
        procedure :: bf_add_int16
        procedure :: bf_add_int16_array
        procedure :: bf_add_int16_matrix
        procedure :: bf_add_int32
        procedure :: bf_add_int32_array
        procedure :: bf_add_int32_matrix
        procedure :: bf_add_int64
        procedure :: bf_add_int64_array
        procedure :: bf_add_int64_matrix
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for managing binary file I/O.
    type binary_file_manager
    private
        !> @brief File ID.
        integer(int32) :: m_id = 0
        !> @brief Position.
        integer(int32) :: m_position = 1
        !> @brief Is the stream open?
        logical :: m_streamOpen = .false.
        !> @brief Uses little endian format?
        logical :: m_littleEndian = .true.
    contains
        !> @brief Returns true if the file is currently open; else, false if it
        !! is not.
        procedure, public :: is_open => bfm_is_open
        !> @brief Gets the current position in the file.
        procedure, public :: get_position => bfm_get_position
        !> @brief Sets the position within the file.
        procedure, public :: set_position => bfm_set_position
        !> @brief Moves the current position to the start of the file.
        procedure, public :: move_to_start => bfm_move_to_start
        !> @brief Gets a logical value determining if the file writes in little
        !! or big endian format.
        procedure, public :: get_is_little_endian => bfm_get_is_little_endian
        !> @brief Sets a logical value determining if the file should write in
        !! little or big endian format.
        procedure, public :: set_is_little_endian => bfm_set_is_little_endian
        !> @brief Closes the currently open file.
        procedure, public :: close => bfm_close
        !> @brief Cleans up the resources used by the binary_file_manager 
        !! instance.
        final :: bfm_clean_up
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for writing of unformatted binary files.
    type, extends(binary_file_manager) :: binary_writer
    private
    contains
        !> @brief Opens a new file for writing.
        procedure, public :: open => bw_init
        !> @brief Writes an item to the file.
        generic, public :: write => bw_write_dbl, bw_write_dbl_1d, &
            bw_write_dbl_2d, bw_write_sngl, bw_write_sngl_1d, &
            bw_write_sngl_2d, bw_write_int16, bw_write_int16_1d, &
            bw_write_int16_2d, bw_write_int32, bw_write_int32_1d, &
            bw_write_int32_2d, bw_write_int8, bw_write_int8_1d, &
            bw_write_int8_2d, bw_write_cmplx64, bw_write_cmplx64_1d, &
            bw_write_cmplx64_2d, bw_write_cmplx32, bw_write_cmplx32_1d, &
            bw_write_cmplx32_2d, bw_write_string

        procedure :: bw_write_dbl
        procedure :: bw_write_dbl_1d
        procedure :: bw_write_dbl_2d
        procedure :: bw_write_sngl
        procedure :: bw_write_sngl_1d
        procedure :: bw_write_sngl_2d
        procedure :: bw_write_int16
        procedure :: bw_write_int16_1d
        procedure :: bw_write_int16_2d
        procedure :: bw_write_int32
        procedure :: bw_write_int32_1d
        procedure :: bw_write_int32_2d
        procedure :: bw_write_int64
        procedure :: bw_write_int64_1d
        procedure :: bw_write_int64_2d
        procedure :: bw_write_int8
        procedure :: bw_write_int8_1d
        procedure :: bw_write_int8_2d
        procedure :: bw_write_cmplx64
        procedure :: bw_write_cmplx64_1d
        procedure :: bw_write_cmplx64_2d
        procedure :: bw_write_cmplx32
        procedure :: bw_write_cmplx32_1d
        procedure :: bw_write_cmplx32_2d
        procedure :: bw_write_string
    end type

! ------------------------------------------------------------------------------
    !> @brief Provides a mechanism for reading unformatted binary files.
    type, extends(binary_file_manager) :: binary_reader
    private
    contains
        !> @brief Opens a binary file for reading.
        procedure, public :: open => br_init
        !> @brief Reads a 64-bit floating-point value from the currently open 
        !! file.
        procedure, public :: read_real64 => br_read_dbl
        !> @brief Reads a 32-bit floating-point value from the currently open 
        !! file.
        procedure, public :: read_real32 => br_read_sngl
        !> @brief Reads a 16-bit integer value from the currently open file.
        procedure, public :: read_int16 => br_read_int16
        !> @brief Reads a 32-bit integer value from the currently open file.
        procedure, public :: read_int32 => br_read_int32
        !> @brief Reads a 64-bit integer value from the currently open file.
        procedure, public :: read_int64 => br_read_int64
        !> @brief Reads a complex-valued, 64-bit floating-point value from the 
        !! currently open file.
        procedure, public :: read_complex64 => br_read_cmplx64
        !> @brief Reads a complex-valued, 32-bit floating-point value from the 
        !! currently open file.
        procedure, public :: read_complex32 => br_read_cmplx32
        !> @brief Reads an 8-bit integer value from the currently open file.
        procedure, public :: read_int8 => br_read_int8
        !> @brief Reads a single character from the currently open file.
        procedure, public :: read_char => br_read_char
        !> @brief Reads an array of character values from the currently open 
        !! file.
        generic, public :: read_char_array => br_read_char_array, &
            br_read_char_array_i16, br_read_char_array_i64
        !> @brief Reads an array of 8-bit integer values from the currently
        !!  open file.
        generic, public :: read_int8_array => br_read_int8_array_i16, &
            br_read_int8_array_i32, br_read_int8_array_i64
        !> @brief Reads an array of 16-bit integer values from the currently
        !!  open file.
        generic, public :: read_int16_array => br_read_int16_array_i16, &
            br_read_int16_array_i32, br_read_int16_array_i64
        !> @brief Reads an array of 32-bit integer values from the currently
        !!  open file.
        generic, public :: read_int32_array => br_read_int32_array_i16, &
            br_read_int32_array_i32, br_read_int32_array_i64
        !> @brief Reads an array of 64-bit integer values from the currently
        !!  open file.
        generic, public :: read_int64_array => br_read_int64_array_i16, &
            br_read_int64_array_i32, br_read_int64_array_i64
        !> @brief Reads an array 32-bit floating-point values from the currently 
        !!  open file.
        generic, public :: read_real32_array => br_read_sngl_array_i16, &
            br_read_sngl_array_i32, br_read_sngl_array_i64
        !> @brief Reads an array 64-bit floating-point values from the currently 
        !!  open file.
        generic, public :: read_real64_array => br_read_dbl_array_i16, &
            br_read_dbl_array_i32, br_read_dbl_array_i64
        !> @brief Tests to see if the current position denotes the end-of-file.
        procedure, public :: end_of_file => br_eof

        procedure :: br_read_char_array
        procedure :: br_read_char_array_i16
        procedure :: br_read_char_array_i64
        procedure :: br_read_int8_array_i16
        procedure :: br_read_int8_array_i32
        procedure :: br_read_int8_array_i64
        procedure :: br_read_int16_array_i16
        procedure :: br_read_int16_array_i32
        procedure :: br_read_int16_array_i64
        procedure :: br_read_int32_array_i16
        procedure :: br_read_int32_array_i32
        procedure :: br_read_int32_array_i64
        procedure :: br_read_int64_array_i16
        procedure :: br_read_int64_array_i32
        procedure :: br_read_int64_array_i64
        procedure :: br_read_sngl_array_i16
        procedure :: br_read_sngl_array_i32
        procedure :: br_read_sngl_array_i64
        procedure :: br_read_dbl_array_i16
        procedure :: br_read_dbl_array_i32
        procedure :: br_read_dbl_array_i64
    end type


! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a value.
    interface swap_bytes
        module procedure :: swap_bytes_double
        module procedure :: swap_bytes_single
        module procedure :: swap_bytes_int16
        module procedure :: swap_bytes_int32
        module procedure :: swap_bytes_int64
        module procedure :: swap_bytes_cmplx64
        module procedure :: swap_bytes_cmplx32
    end interface


contains
! ******************************************************************************
! PUBLIC ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Determines if the current machine is little-endian or big-endian.
    !!
    !! @return Returns true if the current machine is little-endian; else, false
    !! if big-endian.
    pure function is_little_endian() result(x)
        ! Arguments
        logical :: x

        ! Process
        integer(int8) :: j(2)
        integer(int16) :: i
        equivalence(i, j)
        i = 1
        x = j(1) == 1
    end function

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 64-bit floating-point value.
    !!
    !! @param[in,out] x On input, the value whose byte order is to be swapped.
    !!  On output, the resulting byte-swapped value.
    elemental subroutine swap_bytes_double(x)
        ! Arguments
        real(real64), intent(inout) :: x

        ! Local Variables
        integer(int8) :: ii(8), jj(8)
        real(real64) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(8)
        jj(2) = ii(7)
        jj(3) = ii(6)
        jj(4) = ii(5)
        jj(5) = ii(4)
        jj(6) = ii(3)
        jj(7) = ii(2)
        jj(8) = ii(1)
        x = t
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 32-bit floating-point value.
    !!
    !! @param[in,out] x On input, the value whose byte order is to be swapped.
    !!  On output, the resulting byte-swapped value.
    elemental subroutine swap_bytes_single(x)
        ! Arguments
        real(real32), intent(inout) :: x

        ! Local Variables
        integer(int8) :: ii(4), jj(4)
        real(real32) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(4)
        jj(2) = ii(3)
        jj(3) = ii(2)
        jj(4) = ii(1)
        x = t
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 16-bit integer value.
    !!
    !! @param[in,out] x On input, the value whose byte order is to be swapped.
    !!  On output, the resulting byte-swapped value.
    elemental subroutine swap_bytes_int16(x)
        ! Arguments
        integer(int16), intent(inout) :: x

        ! Local Variables
        integer(int8) :: ii(2), jj(2)
        integer(int16) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(2)
        jj(2) = ii(1)
        x = t
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 32-bit integer value.
    !!
    !! @param[in,out] x On input, the value whose byte order is to be swapped.
    !!  On output, the resulting byte-swapped value.
    elemental subroutine swap_bytes_int32(x)
        ! Arguments
        integer(int32), intent(inout) :: x

        ! Local Variables
        integer(int8) :: ii(4), jj(4)
        integer(int32) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(4)
        jj(2) = ii(3)
        jj(3) = ii(2)
        jj(4) = ii(1)
        x = t
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 64-bit integer value.
    !!
    !! @param[in,out] x On input, the value whose byte order is to be swapped.
    !!  On output, the resulting byte-swapped value.
    elemental subroutine swap_bytes_int64(x)
        ! Arguments
        integer(int64), intent(inout) :: x

        ! Local Variables
        integer(int8) :: ii(8), jj(8)
        integer(int64) :: s, t

        ! Process
        equivalence(s, ii)
        equivalence(t, jj)
        s = x
        jj(1) = ii(8)
        jj(2) = ii(7)
        jj(3) = ii(6)
        jj(4) = ii(5)
        jj(5) = ii(4)
        jj(6) = ii(3)
        jj(7) = ii(2)
        jj(8) = ii(1)
        x = t
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 64-bit complex-value.
    !!
    !! @param[in,out] x On input, the value whose byte order is to be swapped.
    !!  On output, the resulting byte-swapped value.
    elemental subroutine swap_bytes_cmplx64(x)
        ! Arguments
        complex(real64), intent(inout) :: x

        ! Local Variables
        real(real64) :: re, im

        ! Process
        re = real(x)
        im = aimag(x)
        call swap_bytes(re)
        call swap_bytes(im)
        x = cmplx(re, im, real64)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Swaps the byte order for a 32-bit complex-value.
    !!
    !! @param[in,out] x On input, the value whose byte order is to be swapped.
    !!  On output, the resulting byte-swapped value.
    elemental subroutine swap_bytes_cmplx32(x)
        ! Arguments
        complex(real32), intent(inout) :: x

        ! Local Variables
        real(real32) :: re, im

        ! Process
        re = real(x)
        im = aimag(x)
        call swap_bytes(re)
        call swap_bytes(im)
        x = cmplx(re, im, real32)
    end subroutine


! ******************************************************************************
! BINARY_FORMATTER MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Initializes the binary_formatter object.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_init(this, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Allocate buffer space
        if (allocated(this%m_buffer)) deallocate(this%m_buffer)
        this%m_count = 0
        allocate(this%m_buffer(BINARY_FORMATTER_DEFAULT_SIZE), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("bf_init", &
                "Insufficient memory available.", &
                FIO_OUT_OF_MEMORY_ERROR)
            return
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a logical value determining if the buffer should be filled in
    !! little or big endian format.
    !!
    !! @param[in] this The binary_formatter object.
    !!
    !! @return Returns true if little endian formatting is used; else,
    !!  false for big endian formatting.
    pure function bf_get_is_little_endian(this) result(x)
        class(binary_formatter), intent(in) :: this
        logical :: x
        x = this%m_littleEndian
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets a logical value determining if the buffer should be filled in
    !! little or big endian format.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x A logical value, that if true, enforces the little endian
    !!  format; else, false for the big endian format.
    subroutine bf_set_is_little_endian(this, x)
        class(binary_formatter), intent(inout) :: this
        logical, intent(in) :: x
        this%m_littleEndian = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Adjusts the capacity of bhe binary_formatter to the
    !! requested amount.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] newsize The new size of the buffer, in bytes.
    !! @param[in,out] err  An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    !!  - FIO_INVALID_INPUT_ERROR: Occurs if @p newsize is negative.
    subroutine bf_resize_buffer(this, newsize, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int32), intent(in) :: newsize
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int8), allocatable, dimension(:) :: copy
        integer(int32) :: flag, n
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Verify newsize is >= 0
        if (newsize < 0) then
            call errmgr%report_error("bf_resize_buffer", &
                "A non-negative buffer size is required.", &
                FIO_INVALID_INPUT_ERROR)
            return
        end if

        ! If the buffer hasn't been allocated, simply allocate it
        if (.not.allocated(this%m_buffer)) then
            allocate(this%m_buffer(newsize), stat = flag)
            if (flag /= 0) goto 100
            this%m_count = 0
            return
        end if

        ! Ensure that there's something to copy
        if (this%m_count > 0) then
            allocate(copy(this%m_count), stat = flag)
            if (flag /= 0) goto 100
            copy = this%m_buffer(1:this%m_count)
        end if

        ! Free the buffer, and then reallocate at the requested size
        deallocate(this%m_buffer)
        allocate(this%m_buffer(newsize), stat = flag)
        if (flag /= 0) goto 100

        ! Copy data back into the buffer
        n = min(this%m_count, newsize)
        this%m_buffer(1:n) = copy(1:n)
        this%m_count = n

        ! End
        return

        ! Out of memory error
    100 continue
        call errmgr%report_error("bf_resize_buffer", &
            "Insufficient memory available.", &
            FIO_OUT_OF_MEMORY_ERROR)
        return
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets the capacity of the binary_formatter, in bytes.
    !!
    !! @param[in] this The binary_formatter object.
    !!
    !! @return The capacity.
    pure function bf_get_capacity(this) result(n)
        class(binary_formatter), intent(in) :: this
        integer(int32) :: n
        n = 0
        if (allocated(this%m_buffer)) n = size(this%m_buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the number of bytes stored within the binary_formatter.
    !!
    !! @param[in] this The binary_formatter object.
    !!
    !! @return The number of bytes stored.
    pure function bf_get_count(this) result(n)
        class(binary_formatter), intent(in) :: this
        integer(int32) :: n
        n = this%m_count
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets a copy of the internal buffer.
    !!
    !! @param[in] this The binary_formatter object.
    !!
    !! @return The buffer copy.
    pure function bf_get_buffer(this) result(x)
        class(binary_formatter), intent(in) :: this
        integer(int8), allocatable, dimension(:) :: x
        integer(int32) :: n
        n = this%get_count()
        x = this%m_buffer(1:n)
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets byte-level access to the formatted buffer.
    !!
    !! @param[in] this The binary_formatter object.
    !! @param[in] index The one-based index of the value to return.
    !!
    !! @return The buffer content at the requested location.
    pure function bf_get_buffer_item(this, index) result(x)
        class(binary_formatter), intent(in) :: this
        integer(int32), intent(in) :: index
        integer(int8) :: x
        x = this%m_buffer(index)
    end function

! --------------------
    !> @brief Sets a byte-level value in the formatted buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] index The one-based index of the value to return.
    !! @param[in] x The value to place into the buffer.
    subroutine bf_set_buffer_item(this, index, x)
        class(binary_formatter), intent(inout) :: this
        integer(int32), intent(in) :: index
        integer(int8), intent(in) :: x
        this%m_buffer(index) = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends a 64-bit floating-point value to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The value to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_dbl(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        real(real64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: sizeInBits, sizeInBytes, newSize, istart, iend
        logical littleEndian, writeLittleEndian
        real(real64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        sizeInBits = storage_size(x)
        sizeInBytes = sizeInBits / 8

        ! Ensure there's enough remaining capacity for everything
        if (sizeInBytes > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), this%get_count() + newSize)
            if (newSize == 0) newSize = BINARY_FORMATTER_DEFAULT_SIZE
            call this%set_capacity(newSize, err)
        end if

        ! Append the item to the end of the buffer
        istart = this%get_count() + 1
        iend = istart + sizeInBytes - 1
        if (littleEndian) then
            ! The machine is little endian
            y = x
            if (.not.writeLittleEndian) call swap_bytes(y)
        else
            ! The machine is big endian
            y = x
            if (writeLittleEndian) call swap_bytes(y)
        end if
        this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        this%m_count = this%m_count + sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 64-bit floating-point array to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The array to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_dbl_array(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        real(real64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        n = size(x)
        sizeInBits = storage_size(x(1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = sizeInBits / 8 + n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the array
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do i = 1, n
            istart = iend + 1
            iend = istart + sizeInBytes - 1
            if (littleEndian) then
                y = x(i)
                if (.not.writeLittleEndian) call swap_bytes(y)
            else
                y = x(i)
                if (writeLittleEndian) call swap_bytes(y)
            end if
            this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        end do
        this%m_count = this%m_count + n * sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 64-bit floating-point matrix to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The matrix to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_dbl_matrix(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        real(real64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, m, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        real(real64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        m = size(x, 1)
        n = size(x, 2)
        sizeInBits = storage_size(x(1,1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = 2 * sizeInBits / 8 + m * n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the matrix
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = m
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = m
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        istart = iend + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do j = 1, n
            do i = 1, m
                istart = iend + 1
                iend = istart + sizeInBytes - 1
                if (littleEndian) then
                    y = x(i,j)
                    if (.not.writeLittleEndian) call swap_bytes(y)
                else
                    y = x(i,j)
                    if (writeLittleEndian) call swap_bytes(y)
                end if
                this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
            end do
        end do
        this%m_count = this%m_count + m * n * sizeInBytes
    end subroutine

! ------------------------------------------------------------------------------v
    !> @brief Appends a 32-bit floating-point value to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The value to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_sngl(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        real(real32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: sizeInBits, sizeInBytes, newSize, istart, iend
        logical littleEndian, writeLittleEndian
        real(real32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        sizeInBits = storage_size(x)
        sizeInBytes = sizeInBits / 8

        ! Ensure there's enough remaining capacity for everything
        if (sizeInBytes > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), this%get_count() + newSize)
            if (newSize == 0) newSize = BINARY_FORMATTER_DEFAULT_SIZE
            call this%set_capacity(newSize, err)
        end if

        ! Append the item to the end of the buffer
        istart = this%get_count() + 1
        iend = istart + sizeInBytes - 1
        if (littleEndian) then
            ! The machine is little endian
            y = x
            if (.not.writeLittleEndian) call swap_bytes(y)
        else
            ! The machine is big endian
            y = x
            if (writeLittleEndian) call swap_bytes(y)
        end if
        this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        this%m_count = this%m_count + sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 32-bit floating-point array to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The array to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_sngl_array(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        real(real32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        real(real32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        n = size(x)
        sizeInBits = storage_size(x(1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = sizeInBits / 8 + n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the array
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do i = 1, n
            istart = iend + 1
            iend = istart + sizeInBytes - 1
            if (littleEndian) then
                y = x(i)
                if (.not.writeLittleEndian) call swap_bytes(y)
            else
                y = x(i)
                if (writeLittleEndian) call swap_bytes(y)
            end if
            this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        end do
        this%m_count = this%m_count + n * sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 32-bit floating-point matrix to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The matrix to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_sngl_matrix(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        real(real32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, m, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        real(real32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        m = size(x, 1)
        n = size(x, 2)
        sizeInBits = storage_size(x(1,1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = 2 * sizeInBits / 8 + m * n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the matrix
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = m
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = m
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        istart = iend + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do j = 1, n
            do i = 1, m
                istart = iend + 1
                iend = istart + sizeInBytes - 1
                if (littleEndian) then
                    y = x(i,j)
                    if (.not.writeLittleEndian) call swap_bytes(y)
                else
                    y = x(i,j)
                    if (writeLittleEndian) call swap_bytes(y)
                end if
                this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
            end do
        end do
        this%m_count = this%m_count + m * n * sizeInBytes
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends a 64-bit complex-valued, floating-point value to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The value to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_cmplx64(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        complex(real64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: sizeInBits, sizeInBytes, newSize, istart, iend
        logical littleEndian, writeLittleEndian
        complex(real64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        sizeInBits = storage_size(x)
        sizeInBytes = sizeInBits / 8

        ! Ensure there's enough remaining capacity for everything
        if (sizeInBytes > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), this%get_count() + newSize)
            if (newSize == 0) newSize = BINARY_FORMATTER_DEFAULT_SIZE
            call this%set_capacity(newSize, err)
        end if

        ! Append the item to the end of the buffer
        istart = this%get_count() + 1
        iend = istart + sizeInBytes - 1
        if (littleEndian) then
            ! The machine is little endian
            y = x
            if (.not.writeLittleEndian) call swap_bytes(y)
        else
            ! The machine is big endian
            y = x
            if (writeLittleEndian) call swap_bytes(y)
        end if
        this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        this%m_count = this%m_count + sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 64-bit complex-valued, floating-point array to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The array to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_cmplx64_array(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        complex(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        complex(real64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        n = size(x)
        sizeInBits = storage_size(x(1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = sizeInBits / 8 + n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the array
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do i = 1, n
            istart = iend + 1
            iend = istart + sizeInBytes - 1
            if (littleEndian) then
                y = x(i)
                if (.not.writeLittleEndian) call swap_bytes(y)
            else
                y = x(i)
                if (writeLittleEndian) call swap_bytes(y)
            end if
            this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        end do
        this%m_count = this%m_count + n * sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 64-bit complex-valued, floating-point matrix to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The matrix to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_cmplx64_matrix(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        complex(real64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, m, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        complex(real64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        m = size(x, 1)
        n = size(x, 2)
        sizeInBits = storage_size(x(1,1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = 2 * sizeInBits / 8 + m * n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the matrix
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = m
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = m
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        istart = iend + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do j = 1, n
            do i = 1, m
                istart = iend + 1
                iend = istart + sizeInBytes - 1
                if (littleEndian) then
                    y = x(i,j)
                    if (.not.writeLittleEndian) call swap_bytes(y)
                else
                    y = x(i,j)
                    if (writeLittleEndian) call swap_bytes(y)
                end if
                this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
            end do
        end do
        this%m_count = this%m_count + m * n * sizeInBytes
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends a 32-bit complex-valued, floating-point value to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The value to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_cmplx32(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        complex(real32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: sizeInBits, sizeInBytes, newSize, istart, iend
        logical littleEndian, writeLittleEndian
        complex(real32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        sizeInBits = storage_size(x)
        sizeInBytes = sizeInBits / 8

        ! Ensure there's enough remaining capacity for everything
        if (sizeInBytes > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), this%get_count() + newSize)
            if (newSize == 0) newSize = BINARY_FORMATTER_DEFAULT_SIZE
            call this%set_capacity(newSize, err)
        end if

        ! Append the item to the end of the buffer
        istart = this%get_count() + 1
        iend = istart + sizeInBytes - 1
        if (littleEndian) then
            ! The machine is little endian
            y = x
            if (.not.writeLittleEndian) call swap_bytes(y)
        else
            ! The machine is big endian
            y = x
            if (writeLittleEndian) call swap_bytes(y)
        end if
        this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        this%m_count = this%m_count + sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 32-bit complex-valued, floating-point array to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The array to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_cmplx32_array(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        complex(real32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        complex(real32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        n = size(x)
        sizeInBits = storage_size(x(1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = sizeInBits / 8 + n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the array
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do i = 1, n
            istart = iend + 1
            iend = istart + sizeInBytes - 1
            if (littleEndian) then
                y = x(i)
                if (.not.writeLittleEndian) call swap_bytes(y)
            else
                y = x(i)
                if (writeLittleEndian) call swap_bytes(y)
            end if
            this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        end do
        this%m_count = this%m_count + n * sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 32-bit complex-valued, floating-point matrix to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The matrix to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_cmplx32_matrix(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        complex(real32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, m, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        complex(real32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        m = size(x, 1)
        n = size(x, 2)
        sizeInBits = storage_size(x(1,1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = 2 * sizeInBits / 8 + m * n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the matrix
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = m
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = m
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        istart = iend + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do j = 1, n
            do i = 1, m
                istart = iend + 1
                iend = istart + sizeInBytes - 1
                if (littleEndian) then
                    y = x(i,j)
                    if (.not.writeLittleEndian) call swap_bytes(y)
                else
                    y = x(i,j)
                    if (writeLittleEndian) call swap_bytes(y)
                end if
                this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
            end do
        end do
        this%m_count = this%m_count + m * n * sizeInBytes
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends a 16-bit integer value to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The value to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int16(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int16), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: sizeInBits, sizeInBytes, newSize, istart, iend
        logical littleEndian, writeLittleEndian
        integer(int16) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        sizeInBits = storage_size(x)
        sizeInBytes = sizeInBits / 8

        ! Ensure there's enough remaining capacity for everything
        if (sizeInBytes > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), this%get_count() + newSize)
            if (newSize == 0) newSize = BINARY_FORMATTER_DEFAULT_SIZE
            call this%set_capacity(newSize, err)
        end if

        ! Append the item to the end of the buffer
        istart = this%get_count() + 1
        iend = istart + sizeInBytes - 1
        if (littleEndian) then
            ! The machine is little endian
            y = x
            if (.not.writeLittleEndian) call swap_bytes(y)
        else
            ! The machine is big endian
            y = x
            if (writeLittleEndian) call swap_bytes(y)
        end if
        this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        this%m_count = this%m_count + sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 16-bit integer array to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The array to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int16_array(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int16), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        integer(int16) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        n = size(x)
        sizeInBits = storage_size(x(1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = sizeInBits / 8 + n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the array
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do i = 1, n
            istart = iend + 1
            iend = istart + sizeInBytes - 1
            if (littleEndian) then
                y = x(i)
                if (.not.writeLittleEndian) call swap_bytes(y)
            else
                y = x(i)
                if (writeLittleEndian) call swap_bytes(y)
            end if
            this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        end do
        this%m_count = this%m_count + n * sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 16-bit integer matrix to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The matrix to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int16_matrix(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int16), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, m, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        integer(int16) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        m = size(x, 1)
        n = size(x, 2)
        sizeInBits = storage_size(x(1,1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = 2 * sizeInBits / 8 + m * n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the matrix
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = m
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = m
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        istart = iend + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do j = 1, n
            do i = 1, m
                istart = iend + 1
                iend = istart + sizeInBytes - 1
                if (littleEndian) then
                    y = x(i,j)
                    if (.not.writeLittleEndian) call swap_bytes(y)
                else
                    y = x(i,j)
                    if (writeLittleEndian) call swap_bytes(y)
                end if
                this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
            end do
        end do
        this%m_count = this%m_count + m * n * sizeInBytes
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends a 32-bit integer value to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The value to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int32(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: sizeInBits, sizeInBytes, newSize, istart, iend
        logical littleEndian, writeLittleEndian
        integer(int32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        sizeInBits = storage_size(x)
        sizeInBytes = sizeInBits / 8

        ! Ensure there's enough remaining capacity for everything
        if (sizeInBytes > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), this%get_count() + newSize)
            if (newSize == 0) newSize = BINARY_FORMATTER_DEFAULT_SIZE
            call this%set_capacity(newSize, err)
        end if

        ! Append the item to the end of the buffer
        istart = this%get_count() + 1
        iend = istart + sizeInBytes - 1
        if (littleEndian) then
            ! The machine is little endian
            y = x
            if (.not.writeLittleEndian) call swap_bytes(y)
        else
            ! The machine is big endian
            y = x
            if (writeLittleEndian) call swap_bytes(y)
        end if
        this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        this%m_count = this%m_count + sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 32-bit integer array to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The array to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int32_array(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        integer(int32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        n = size(x)
        sizeInBits = storage_size(x(1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = sizeInBits / 8 + n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the array
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do i = 1, n
            istart = iend + 1
            iend = istart + sizeInBytes - 1
            if (littleEndian) then
                y = x(i)
                if (.not.writeLittleEndian) call swap_bytes(y)
            else
                y = x(i)
                if (writeLittleEndian) call swap_bytes(y)
            end if
            this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        end do
        this%m_count = this%m_count + n * sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 32-bit integer matrix to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The matrix to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int32_matrix(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, m, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        integer(int32) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        m = size(x, 1)
        n = size(x, 2)
        sizeInBits = storage_size(x(1,1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = 2 * sizeInBits / 8 + m * n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the matrix
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = m
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = m
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        istart = iend + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do j = 1, n
            do i = 1, m
                istart = iend + 1
                iend = istart + sizeInBytes - 1
                if (littleEndian) then
                    y = x(i,j)
                    if (.not.writeLittleEndian) call swap_bytes(y)
                else
                    y = x(i,j)
                    if (writeLittleEndian) call swap_bytes(y)
                end if
                this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
            end do
        end do
        this%m_count = this%m_count + m * n * sizeInBytes
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends a 64-bit integer value to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The value to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int64(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: sizeInBits, sizeInBytes, newSize, istart, iend
        logical littleEndian, writeLittleEndian
        integer(int64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        sizeInBits = storage_size(x)
        sizeInBytes = sizeInBits / 8

        ! Ensure there's enough remaining capacity for everything
        if (sizeInBytes > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), this%get_count() + newSize)
            if (newSize == 0) newSize = BINARY_FORMATTER_DEFAULT_SIZE
            call this%set_capacity(newSize, err)
        end if

        ! Append the item to the end of the buffer
        istart = this%get_count() + 1
        iend = istart + sizeInBytes - 1
        if (littleEndian) then
            ! The machine is little endian
            y = x
            if (.not.writeLittleEndian) call swap_bytes(y)
        else
            ! The machine is big endian
            y = x
            if (writeLittleEndian) call swap_bytes(y)
        end if
        this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        this%m_count = this%m_count + sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 64-bit integer array to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The array to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int64_array(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        integer(int64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        n = size(x)
        sizeInBits = storage_size(x(1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = sizeInBits / 8 + n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the array
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do i = 1, n
            istart = iend + 1
            iend = istart + sizeInBytes - 1
            if (littleEndian) then
                y = x(i)
                if (.not.writeLittleEndian) call swap_bytes(y)
            else
                y = x(i)
                if (writeLittleEndian) call swap_bytes(y)
            end if
            this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
        end do
        this%m_count = this%m_count + n * sizeInBytes
    end subroutine

! --------------------
    !> @brief Appends a 64-bit integer matrix to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The matrix to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int64_matrix(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, j, m, n, sizeInBits, sizeInBytes, overallSize, &
            newSize, istart, iend, iy
        logical littleEndian, writeLittleEndian
        integer(int64) :: y

        ! Determine if the machine utilizes little endian or big endian
        littleEndian = is_little_endian()

        ! Determine how the user wishes to write the data
        writeLittleEndian = this%get_use_little_endian()

        ! Determine the size of the item, in bytes
        m = size(x, 1)
        n = size(x, 2)
        sizeInBits = storage_size(x(1,1))
        sizeInBytes = sizeInBits / 8

        ! Account for array size information
        sizeInBits = storage_size(sizeInBits)
        overallSize = 2 * sizeInBits / 8 + m * n * sizeInBytes

        ! Ensure there's enough capacity remaining
        if (overallSize > (this%get_capacity() - this%get_count())) then
            newSize = max(2 * this%get_capacity(), &
                this%get_count() + overallSize)
            if (newSize == 0) &
                newSize = max(BINARY_FORMATTER_DEFAULT_SIZE, overallSize)
            call this%set_capacity(newSize, err)
        end if

        ! Append the array size, and then the matrix
        istart = this%get_count() + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = m
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = m
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        istart = iend + 1
        iend = istart + sizeInBits / 8 - 1
        if (littleEndian) then
            iy = n
            if (.not.writeLittleEndian) call swap_bytes(iy)
        else
            iy = n
            if (writeLittleEndian) call swap_bytes(iy)
        end if
        this%m_buffer(istart:iend) = transfer(iy, this%m_buffer(istart:iend))

        do j = 1, n
            do i = 1, m
                istart = iend + 1
                iend = istart + sizeInBytes - 1
                if (littleEndian) then
                    y = x(i,j)
                    if (.not.writeLittleEndian) call swap_bytes(y)
                else
                    y = x(i,j)
                    if (writeLittleEndian) call swap_bytes(y)
                end if
                this%m_buffer(istart:iend) = transfer(y, this%m_buffer(istart:iend))
            end do
        end do
        this%m_count = this%m_count + m * n * sizeInBytes
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends an 8-bit integer value to the buffer.
    !!
    !! @param[in,out] this The binary_formatter object.
    !! @param[in] x The value to add.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
    !!      available.
    subroutine bf_add_int8(this, x, err)
        ! Arguments
        class(binary_formatter), intent(inout) :: this
        integer(int8), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: newSize

        ! Process
        if (this%get_capacity() - this%get_count() == 0) then
            newSize = max(2 * this%get_capacity(), &
                BINARY_FORMATTER_DEFAULT_SIZE)
            call this%set_capacity(newSize, err)
        end if
        this%m_count = this%m_count + 1
        this%m_buffer(this%m_count) = x
    end subroutine

! ******************************************************************************
! BINARY_FILE_MANAGER MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Returns true if the file is currently open; else, false if it
    !! is not.
    !!
    !! @param[in] this The binary_file_manager object.
    !!
    !! @return True if the file is open; else, false.
    pure function bfm_is_open(this) result(x)
        ! Arguments
        class(binary_file_manager), intent(in) :: this
        logical :: x

        ! Process
        x = this%m_streamOpen
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the current position in the file.
    !!
    !! @param[in] this The binary_file_manager object.
    !!
    !! @return The current position within the file, in bytes.
    pure function bfm_get_position(this) result(x)
        ! Arguments
        class(binary_file_manager), intent(in) :: this
        integer(int32) :: x

        ! Process
        x = this%m_position
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets the current position within the file.
    !!
    !! @param[in,out] this The binary_file_manager object.
    !! @param[in] x The file position, in bytes.
    subroutine bfm_set_position(this, x)
        ! Arguments
        class(binary_file_manager), intent(inout) :: this
        integer(int32), intent(in) :: x

        ! Local Variables
        integer(int32) :: p

        ! Quick Return
        if (.not.this%m_streamOpen) return

        ! Initialization
        if (x < 1) then
            p = 1
        else
            p = x
        end if

        ! Process
        this%m_position = p
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Moves the current position to the start of the file.
    !!
    !! @param[in,out] this The binary_file_manager object.
    subroutine bfm_move_to_start(this)
        ! Arguments
        class(binary_file_manager), intent(inout) :: this

        ! Quick Return
        if (.not.this%m_streamOpen) return

        ! Process
        this%m_position = 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Gets a logical value determining if the file is in little
    !! or big endian format.
    !!
    !! @param[in] this The binary_file_manager object.
    !!
    !! @return Returns true if the file is little endian format; else,
    !!  false if the file big endian format.
    pure function bfm_get_is_little_endian(this) result(x)
        class(binary_file_manager), intent(in) :: this
        logical :: x
        x = this%m_littleEndian
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets a logical value determining if the file is in
    !! little or big endian format.
    !!
    !! @param[in,out] this The binary_file_manager object.
    !! @param[in] x A logical value, that if true, enforces the little endian
    !!  format; else, false for the big endian format.
    subroutine bfm_set_is_little_endian(this, x)
        class(binary_file_manager), intent(inout) :: this
        logical, intent(in) :: x
        this%m_littleEndian = x
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Closes the currently open stream.
    !!
    !! @param[in,out] this The binary_file_manager object.
    !! @param[in] delete An optional input, that if set to true, causes the
    !!  file to be deleted.  The default is false such that the file remains.
    subroutine bfm_close(this, delete)
        ! Arguments
        class(binary_file_manager), intent(inout) :: this
        logical, intent(in), optional :: delete

        ! Local Variables
        logical :: d

        ! Initialization
        d = .false.
        if (present(delete)) d = delete

        ! Process
        if (this%m_streamOpen) then
            if (d) then
                close(this%m_id, status = "delete")
            else
                close(this%m_id)
            end if
            this%m_id = 0
            this%m_streamOpen = .false.
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Cleans up the resources used by the binary_file_manager instance
    !! ensuring the file is closed.
    !!
    !! @param[in,out] this The binary_file_manager object.
    subroutine bfm_clean_up(this)
        type(binary_file_manager), intent(inout) :: this
        call this%close(.false.)
    end subroutine


! ******************************************************************************
! BINARY_WRITER MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Opens a binary file for writing.
    !!
    !! @param[in,out] this The binary_writer object.
    !! @param[in] fname The name of the file to open.
    !! @param[in] append An optional argument that, if specified, determines
    !!  if the file should be appended.  If not supplied, and a file exists,
    !!  the file will be overwritten.  If no file exists, it simply will be
    !!  created.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if the file could not be opened.
    subroutine bw_init(this, fname, append, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        character(len = *), intent(in) :: fname
        logical, intent(in), optional :: append
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: append2File

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        append2File = .false.
        if (present(append)) append2File = append

        ! Close if already open
        call this%close()

        ! Open the new stream
        if (append2File) then
            open(newunit = this%m_id, file = fname, form = "unformatted", &
                access = "stream", position = "append", iostat = flag)
        else
            open(newunit = this%m_id, file = fname, form = "unformatted", &
                access = "stream", iostat = flag)
        end if
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("bw_init", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_streamOpen = .true.
        this%m_position = 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 64-bit floating-point value to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The value to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_dbl(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        real(real64) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_dbl", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + storage_size(x) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 1-dimensional array of 64-bit floating-point values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_dbl_1d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        real(real64), dimension(size(x)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_dbl_1d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + size(x) * storage_size(x(1)) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 2-dimensional array of 64-bit floating-point values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_dbl_2d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, m, n
        real(real64), dimension(size(x, 1), size(x, 2)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        m = size(x, 1)
        n = size(x, 2)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_dbl_2d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do i = 1, n
                write(this%m_id, pos = this%m_position) x(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            do i = 1, n
                write(this%m_id, pos = this%m_position) y(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 32-bit floating-point value to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The value to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_sngl(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        real(real32) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_sngl", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + storage_size(x) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 1-dimensional array of 32-bit floating-point values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_sngl_1d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        real(real32), dimension(size(x)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_sngl_1d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + size(x) * storage_size(x(1)) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 2-dimensional array of 32-bit floating-point values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_sngl_2d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        real(real32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, m, n
        real(real32), dimension(size(x, 1), size(x, 2)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        m = size(x, 1)
        n = size(x, 2)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_sngl_2d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do i = 1, n
                write(this%m_id, pos = this%m_position) x(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            do i = 1, n
                write(this%m_id, pos = this%m_position) y(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 16-bit integer value to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The value to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int16(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int16), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int16) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int16", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + storage_size(x) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 1-dimensional array of 16-bit integer values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int16_1d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int16), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int16), dimension(size(x)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int16_1d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + size(x) * storage_size(x(1)) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 2-dimensional array of 16-bit integer values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int16_2d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int16), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, m, n
        integer(int16), dimension(size(x, 1), size(x, 2)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        m = size(x, 1)
        n = size(x, 2)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int16_2d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do i = 1, n
                write(this%m_id, pos = this%m_position) x(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            do i = 1, n
                write(this%m_id, pos = this%m_position) y(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 32-bit integer value to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The value to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int32(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int32", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + storage_size(x) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 1-dimensional array of 32-bit integer values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int32_1d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32), dimension(size(x)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int32_1d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + size(x) * storage_size(x(1)) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 2-dimensional array of 32-bit integer values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int32_2d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, m, n
        integer(int32), dimension(size(x, 1), size(x, 2)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        m = size(x, 1)
        n = size(x, 2)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int32_2d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do i = 1, n
                write(this%m_id, pos = this%m_position) x(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            do i = 1, n
                write(this%m_id, pos = this%m_position) y(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 64-bit integer value to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The value to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int64(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int64) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + storage_size(x) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 1-dimensional array of 64-bit integer values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int64_1d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int64), dimension(size(x)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int64_1d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            write(this%m_id, pos = this%m_position) x
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
        end if

        ! Increment the position
        this%m_position = this%m_position + size(x) * storage_size(x(1)) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 2-dimensional array of 64-bit integer values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int64_2d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, m, n
        integer(int64), dimension(size(x, 1), size(x, 2)) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        m = size(x, 1)
        n = size(x, 2)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int64_2d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do i = 1, n
                write(this%m_id, pos = this%m_position) x(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            y = x
            call swap_bytes(y)
            do i = 1, n
                write(this%m_id, pos = this%m_position) y(:,i)
                this%m_position = this%m_position + m * storage_size(x(1,1)) / 8
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes an 8-bit integer value to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The value to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int8(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int8), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int8", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        write(this%m_id, pos = this%m_position) x

        ! Increment the position
        this%m_position = this%m_position + storage_size(x) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 1-dimensional array of 8-bit integer values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int8_1d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int8), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: n
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        n = size(x)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int8_1d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        write(this%m_id, pos = this%m_position) x

        ! Increment the position
        this%m_position = this%m_position + n * storage_size(x) / 8
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 2-dimensional array of 8-bit integer values
    !! to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_int8_2d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        integer(int8), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: i, m, n
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        m = size(x, 1)
        n = size(x, 2)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_int8_2d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        do i = 1, n
            write(this%m_id, pos = this%m_position) x(:,i)
            this%m_position = this%m_position + m * storage_size(x) / 8
        end do
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 64-bit, complex-valued, floating-point value to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The value to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_cmplx64(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real64), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        real(real64) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_cmplx64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then

            write(this%m_id, pos = this%m_position) real(x, real64)
            this%m_position = this%m_position + storage_size(x) / 8

            write(this%m_id, pos = this%m_position) aimag(x)
            this%m_position = this%m_position + storage_size(x) / 8
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then

            y = real(x, real64)
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
            this%m_position = this%m_position + storage_size(x) / 8

            y = aimag(x)
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
            this%m_position = this%m_position + storage_size(x) / 8
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 1-dimensional array of 64-bit, complex-valued, 
    !! floating-point values to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_cmplx64_1d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, n
        real(real64) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        n = size(x)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_cmplx64_1d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do i = 1, n
                write(this%m_id, pos = this%m_position) real(x(i), real64)
                this%m_position = this%m_position + storage_size(y) / 8

                write(this%m_id, pos = this%m_position) aimag(x(i))
                this%m_position = this%m_position + storage_size(y) / 8
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            do i = 1, n
                y = real(x(i), real64)
                call swap_bytes(y)
                write(this%m_id, pos = this%m_position) y
                this%m_position = this%m_position + storage_size(y) / 8

                y = aimag(x(i))
                call swap_bytes(y)
                write(this%m_id, pos = this%m_position) y
                this%m_position = this%m_position + storage_size(y) / 8
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 2-dimensional array of 64-bit ,complex-valued, 
    !! floating-point values to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_cmplx64_2d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real64), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, j, m, n
        real(real64) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        m = size(x, 1)
        n = size(x, 2)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_cmplx64_2d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do j = 1, n
                do i = 1, m
                    write(this%m_id, pos = this%m_position) real(x(i,j), real64)
                    this%m_position = this%m_position + storage_size(y) / 8

                    write(this%m_id, pos = this%m_position) aimag(x(i,j))
                    this%m_position = this%m_position + storage_size(y) / 8
                end do
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            do j = 1, n
                do i = 1, m
                    y = real(x(i,j), real64)
                    call swap_bytes(y)
                    write(this%m_id, pos = this%m_position) y
                    this%m_position = this%m_position + storage_size(y) / 8

                    y = aimag(x(i,j))
                    call swap_bytes(y)
                    write(this%m_id, pos = this%m_position) y
                    this%m_position = this%m_position + storage_size(y) / 8
                end do
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 32-bit, complex-valued, floating-point value to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The value to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_cmplx32(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real32), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        real(real32) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_cmplx32", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then

            write(this%m_id, pos = this%m_position) real(x, real32)
            this%m_position = this%m_position + storage_size(x) / 8

            write(this%m_id, pos = this%m_position) aimag(x)
            this%m_position = this%m_position + storage_size(x) / 8
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then

            y = real(x, real32)
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
            this%m_position = this%m_position + storage_size(x) / 8

            y = aimag(x)
            call swap_bytes(y)
            write(this%m_id, pos = this%m_position) y
            this%m_position = this%m_position + storage_size(x) / 8
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 1-dimensional array of 32-bit, complex-valued, 
    !! floating-point values to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_cmplx32_1d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real32), intent(in), dimension(:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, n
        real(real32) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        n = size(x)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_cmplx32_1d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do i = 1, n
                write(this%m_id, pos = this%m_position) real(x(i), real32)
                this%m_position = this%m_position + storage_size(y) / 8

                write(this%m_id, pos = this%m_position) aimag(x(i))
                this%m_position = this%m_position + storage_size(y) / 8
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            do i = 1, n
                y = real(x(i), real32)
                call swap_bytes(y)
                write(this%m_id, pos = this%m_position) y
                this%m_position = this%m_position + storage_size(y) / 8

                y = aimag(x(i))
                call swap_bytes(y)
                write(this%m_id, pos = this%m_position) y
                this%m_position = this%m_position + storage_size(y) / 8
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a 2-dimensional array of 32-bit, complex-valued, 
    !! floating-point values to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The array to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_cmplx32_2d(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        complex(real32), intent(in), dimension(:,:) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        logical :: littleEndian
        integer(int32) :: i, j, m, n
        real(real32) :: y
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        m = size(x, 1)
        n = size(x, 2)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_cmplx32_2d", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        if ((this%m_littleEndian .and. littleEndian) .or. &
                (.not.this%m_littleEndian .and. .not.littleEndian)) then
            do j = 1, n
                do i = 1, m
                    write(this%m_id, pos = this%m_position) real(x(i,j), real32)
                    this%m_position = this%m_position + storage_size(y) / 8

                    write(this%m_id, pos = this%m_position) aimag(x(i,j))
                    this%m_position = this%m_position + storage_size(y) / 8
                end do
            end do
        else if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            do j = 1, n
                do i = 1, m
                    y = real(x(i,j), real32)
                    call swap_bytes(y)
                    write(this%m_id, pos = this%m_position) y
                    this%m_position = this%m_position + storage_size(y) / 8

                    y = aimag(x(i,j))
                    call swap_bytes(y)
                    write(this%m_id, pos = this%m_position) y
                    this%m_position = this%m_position + storage_size(y) / 8
                end do
            end do
        end if
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes a character array to the file.
    !!
    !! @param[in,out] this The binary_writer class.
    !! @param[in] x The string to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine bw_write_string(this, x, err)
        ! Arguments
        class(binary_writer), intent(inout) :: this
        character(len = *), intent(in) :: x
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("bw_write_string", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Write to the file
        write(this%m_id, pos = this%m_position) x
        this%m_position = this%m_position + len(x) * storage_size(x(1:1)) / 8
    end subroutine

! ******************************************************************************
! BINARY_READER MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Opens a binary file for reading.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] fname The name of the file to open.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if the file could not be opened.
    subroutine br_init(this, fname, err)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        character(len = *), intent(in) :: fname
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        integer(int32) :: flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Close if already open
        call this%close()

        ! Process
        open(newunit = this%m_id, file = fname, form = "unformatted", &
            access = "stream", iostat = flag)
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_init", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_streamOpen = .true.
        this%m_position = 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reads a 64-bit floating-point value from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_dbl(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        real(real64) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        x = 0.0d0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_dbl", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) x
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_dbl", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_dbl", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
        if ((this%m_littleEndian .and. .not.littleEndian) .or. &
            (.not.this%m_littleEndian .and. littleEndian)) then
            call swap_bytes(x)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a 32-bit floating-point value from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_sngl(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        real(real32) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        x = 0.0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_sngl", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) x
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_sngl", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_sngl", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
        if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            call swap_bytes(x)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a 16-bit integer value from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int16(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        integer(int16) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        x = 0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_int16", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) x
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_int16", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_int16", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
        if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            call swap_bytes(x)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a 32-bit integer value from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int32(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        integer(int32) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        x = 0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_int32", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) x
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_int32", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_int32", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
        if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            call swap_bytes(x)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a 64-bit integer value from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int64(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        integer(int64) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        x = 0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_int64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) x
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_int64", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_int64", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
        if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                (.not.this%m_littleEndian .and. littleEndian)) then
            call swap_bytes(x)
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a complex-valued, 64-bit floating-point value from the 
    !! currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_cmplx64(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        complex(real64) :: x

        ! Parameters
        real(real64), parameter :: zero = 0.0d0

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag
        real(real64) :: xr, xi

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        x = cmplx(zero, zero, real64)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_cmplx64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) xr
        if (flag == 0) then
            this%m_position = this%m_position + storage_size(x) / 8
            read(this%m_id, pos = this%m_position, iostat = flag) xi
        end if
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_cmplx64", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_cmplx64", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
        if ((this%m_littleEndian .and. .not.littleEndian) .or. &
            (.not.this%m_littleEndian .and. littleEndian)) then
            call swap_bytes(xr)
            call swap_bytes(xi)
        end if
        x = cmplx(xr, xi, real64)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a complex-valued, 32-bit floating-point value from the 
    !! currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_cmplx32(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        complex(real32) :: x

        ! Parameters
        real(real32), parameter :: zero = 0.0

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag
        real(real32) :: xr, xi

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        x = cmplx(zero, zero, real32)

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_cmplx32", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) xr
        if (flag == 0) then
            this%m_position = this%m_position + storage_size(x) / 8
            read(this%m_id, pos = this%m_position, iostat = flag) xi
        end if
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_cmplx32", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_cmplx32", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
        if ((this%m_littleEndian .and. .not.littleEndian) .or. &
            (.not.this%m_littleEndian .and. littleEndian)) then
            call swap_bytes(xr)
            call swap_bytes(xi)
        end if
        x = cmplx(xr, xi, real32)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an 8-bit integer value from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int8(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        integer(int8) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        x = 0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_int8", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) x
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_int8", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_int8", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 8-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int8_array_i64(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int64), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int8), dimension(n) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int64) :: i
        integer(int32) :: flag
        integer(int8) :: temp

        ! Quick Return
        if (n <= 0) return

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        x = 0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_int8_array_i64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            read(this%m_id, pos = this%m_position, iostat = flag) temp
            if (flag < 0) then
                ! End Of File
                call errmgr%report_error(&
                    "br_read_int8_array_i64", "End of file encountered.", &
                    FIO_END_OF_FILE_ERROR)
                return
            else if (flag > 0) then
                ! Somethings wrong
                write(errmsg, "(AI0A)") &
                    "An error was encountered while attempting to " // &
                    "perform the read operation.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("br_read_int8_array_i64", trim(errmsg), &
                    FIO_FILE_IO_ERROR)
                return
            end if
            this%m_position = this%m_position + storage_size(temp) / 8
            x(i) = temp
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 8-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int8_array_i16(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int16), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int8), dimension(n) :: x

        ! Process
        x = br_read_int8_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 8-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int8_array_i32(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int8), dimension(n) :: x

        ! Process
        x = br_read_int8_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a single character from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The character.
    function br_read_char(this, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        character :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: flag

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_char", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        read(this%m_id, pos = this%m_position, iostat = flag) x
        if (flag < 0) then
            ! End Of File
            call errmgr%report_error(&
                "br_read_char", "End of file encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        else if (flag > 0) then
            ! Somethings wrong
            write(errmsg, "(AI0A)") &
                "An error was encountered while attempting to " // &
                "perform the read operation.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("br_read_char", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_position = this%m_position + storage_size(x) / 8
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of characters from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] nchar The number of characters to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The character array.
    function br_read_char_array_i64(this, nchar, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int64), intent(in) :: nchar
        class(errors), intent(inout), optional, target :: err
        character(len = nchar) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int64) :: i
        integer(int32) :: flag
        character :: tempchar

        ! Quick Return
        if (nchar == 0) return

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_char_array_i64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        do i = 1, nchar
            read(this%m_id, pos = this%m_position, iostat = flag) tempchar
            if (flag < 0) then
                ! End Of File
                call errmgr%report_error(&
                    "br_read_char_array_i64", "End of file encountered.", &
                    FIO_END_OF_FILE_ERROR)
                return
            else if (flag > 0) then
                ! Somethings wrong
                write(errmsg, "(AI0A)") &
                    "An error was encountered while attempting to " // &
                    "perform the read operation.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("br_read_char_array_i64", &
                    trim(errmsg), FIO_FILE_IO_ERROR)
                return
            end if
            this%m_position = this%m_position + storage_size(tempchar) / 8
            x(i:i) = tempchar
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of characters from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] nchar The number of characters to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The character array.
    function br_read_char_array_i16(this, nchar, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int16), intent(in) :: nchar
        class(errors), intent(inout), optional, target :: err
        character(len = nchar) :: x

        ! Process
        x = br_read_char_array_i64(this, int(nchar, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of characters from the currently open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] nchar The number of characters to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The character array.
    function br_read_char_array(this, nchar, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int32), intent(in) :: nchar
        class(errors), intent(inout), optional, target :: err
        character(len = nchar) :: x

        ! Process
        x = br_read_char_array_i64(this, int(nchar, int64), err)
    end function
    
! ------------------------------------------------------------------------------
    !> @brief Tests to see if the current position denotes the end-of-file.
    !!
    !! @param[in] this The binary_reader object.
    !!
    !! @return Returns true if the current position is the end of the file;
    !!  else, false.
    function br_eof(this) result(x)
        ! Arguments
        class(binary_reader), intent(in) :: this
        logical :: x

        ! Local Variables
        integer(int8) :: temp
        integer(int32) :: flag

        ! Process - do not increment the position!
        read(this%m_id, pos = this%m_position, iostat = flag) temp
        x = flag < 0
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 16-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int16_array_i64(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int64), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int16), dimension(n) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int64) :: i
        integer(int32) :: flag
        integer(int16) :: temp

        ! Quick Return
        if (n <= 0) return

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        x = 0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_int16_array_i64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            read(this%m_id, pos = this%m_position, iostat = flag) temp
            if (flag < 0) then
                ! End Of File
                call errmgr%report_error(&
                    "br_read_int16_array_i64", "End of file encountered.", &
                    FIO_END_OF_FILE_ERROR)
                return
            else if (flag > 0) then
                ! Somethings wrong
                write(errmsg, "(AI0A)") &
                    "An error was encountered while attempting to " // &
                    "perform the read operation.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("br_read_int16_array_i64", trim(errmsg), &
                    FIO_FILE_IO_ERROR)
                return
            end if
            this%m_position = this%m_position + storage_size(temp) / 8
            x(i) = temp
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 16-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int16_array_i16(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int16), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int16), dimension(n) :: x

        ! Process
        x = br_read_int16_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 16-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int16_array_i32(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int16), dimension(n) :: x

        ! Process
        x = br_read_int16_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 32-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_int32_array_i64(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int64), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int32), dimension(n) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int64) :: i
        integer(int32) :: flag, temp

        ! Quick Return
        if (n <= 0) return

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        x = 0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_int32_array_i64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            read(this%m_id, pos = this%m_position, iostat = flag) temp
            if (flag < 0) then
                ! End Of File
                call errmgr%report_error(&
                    "br_read_int32_array_i64", "End of file encountered.", &
                    FIO_END_OF_FILE_ERROR)
                return
            else if (flag > 0) then
                ! Somethings wrong
                write(errmsg, "(AI0A)") &
                    "An error was encountered while attempting to " // &
                    "perform the read operation.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("br_read_int32_array_i64", trim(errmsg), &
                    FIO_FILE_IO_ERROR)
                return
            end if
            this%m_position = this%m_position + storage_size(temp) / 8
            x(i) = temp
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 8-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_int32_array_i16(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int16), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int32), dimension(n) :: x

        ! Process
        x = br_read_int32_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 32-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_int32_array_i32(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int32), dimension(n) :: x

        ! Process
        x = br_read_int32_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 64-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_int64_array_i64(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int64), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int64), dimension(n) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int64) :: i
        integer(int32) :: flag
        integer(int64) :: temp

        ! Quick Return
        if (n <= 0) return

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        x = 0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_int64_array_i64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            read(this%m_id, pos = this%m_position, iostat = flag) temp
            if (flag < 0) then
                ! End Of File
                call errmgr%report_error(&
                    "br_read_int64_array_i64", "End of file encountered.", &
                    FIO_END_OF_FILE_ERROR)
                return
            else if (flag > 0) then
                ! Somethings wrong
                write(errmsg, "(AI0A)") &
                    "An error was encountered while attempting to " // &
                    "perform the read operation.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("br_read_int64_array_i64", trim(errmsg), &
                    FIO_FILE_IO_ERROR)
                return
            end if
            this%m_position = this%m_position + storage_size(temp) / 8
            x(i) = temp
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 64-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int64_array_i16(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int16), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int64), dimension(n) :: x

        ! Process
        x = br_read_int64_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an array of 64-bit integer values from the currently open 
    !!  file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The value.
    function br_read_int64_array_i32(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        integer(int64), dimension(n) :: x

        ! Process
        x = br_read_int64_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a array 32-bit floating-point values from the currently 
    !! open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_sngl_array_i64(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int64), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        real(real32), dimension(n) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag
        integer(int64) :: i
        real(real32) :: temp

        ! Quick Return
        if (n == 0) return

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        temp = 0.0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_sngl_array_i64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            read(this%m_id, pos = this%m_position, iostat = flag) temp
            if (flag < 0) then
                ! End Of File
                call errmgr%report_error(&
                    "br_read_sngl_array_i64", "End of file encountered.", &
                    FIO_END_OF_FILE_ERROR)
                return
            else if (flag > 0) then
                ! Somethings wrong
                write(errmsg, "(AI0A)") &
                    "An error was encountered while attempting to " // &
                    "perform the read operation.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("br_read_sngl_array_i64", trim(errmsg), &
                    FIO_FILE_IO_ERROR)
                return
            end if
            this%m_position = this%m_position + storage_size(temp) / 8
            if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                    (.not.this%m_littleEndian .and. littleEndian)) then
                call swap_bytes(temp)
            end if
            x(i) = temp
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a array 32-bit floating-point values from the currently 
    !! open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_sngl_array_i32(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        real(real32), dimension(n) :: x

        ! Process
        x = br_read_sngl_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a array 32-bit floating-point values from the currently 
    !! open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_sngl_array_i16(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int16), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        real(real32), dimension(n) :: x

        ! Process
        x = br_read_sngl_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a array 64-bit floating-point values from the currently 
    !! open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_dbl_array_i64(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int64), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        real(real64), dimension(n) :: x

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        logical :: littleEndian
        integer(int32) :: flag
        integer(int64) :: i
        real(real64) :: temp

        ! Quick Return
        if (n == 0) return

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        littleEndian = is_little_endian()
        temp = 0.0d0

        ! Ensure there's an open stream to which we may write
        if (.not.this%m_streamOpen) then
            call errmgr%report_error("br_read_dbl_array_i64", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            read(this%m_id, pos = this%m_position, iostat = flag) temp
            if (flag < 0) then
                ! End Of File
                call errmgr%report_error(&
                    "br_read_dbl_array_i64", "End of file encountered.", &
                    FIO_END_OF_FILE_ERROR)
                return
            else if (flag > 0) then
                ! Somethings wrong
                write(errmsg, "(AI0A)") &
                    "An error was encountered while attempting to " // &
                    "perform the read operation.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("br_read_dbl_array_i64", trim(errmsg), &
                    FIO_FILE_IO_ERROR)
                return
            end if
            this%m_position = this%m_position + storage_size(temp) / 8
            if ((this%m_littleEndian .and. .not.littleEndian) .or. &
                    (.not.this%m_littleEndian .and. littleEndian)) then
                call swap_bytes(temp)
            end if
            x(i) = temp
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a array 64-bit floating-point values from the currently 
    !! open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_dbl_array_i32(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int32), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        real(real64), dimension(n) :: x

        ! Process
        x = br_read_dbl_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a array 64-bit floating-point values from the currently 
    !! open file.
    !!
    !! @param[in,out] this The binary_reader object.
    !! @param[in] n The number of values to read.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if an error occurs when attempting to read
    !!      the data.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the read is requested at or passed 
    !!      the end of the file.
    !!
    !! @return The values.
    function br_read_dbl_array_i16(this, n, err) result(x)
        ! Arguments
        class(binary_reader), intent(inout) :: this
        integer(int16), intent(in) :: n
        class(errors), intent(inout), optional, target :: err
        real(real64), dimension(n) :: x

        ! Process
        x = br_read_dbl_array_i64(this, int(n, int64), err)
    end function

! ------------------------------------------------------------------------------
end module
