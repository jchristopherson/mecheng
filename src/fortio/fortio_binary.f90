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

! ******************************************************************************
! TYPES
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
        !> @brief Tests to see if the current position denotes the end-of-file.
        procedure, public :: end_of_file => br_eof
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
end module
