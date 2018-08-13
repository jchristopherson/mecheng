! fortio_text.f90

!> @brief \b fortio_text
!!
!! @par Purpose
!! Provides mechanisms to read and write text files.
module fortio_text
    use, intrinsic :: iso_fortran_env
    use fortio_types
    use ferror, only : errors
    use strings
    implicit none
    private
    public :: text_file_manager
    public :: text_writer
    public :: text_reader
    public :: read_numeric_text_file

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for managing text file I/O.
    type text_file_manager
    private
        !> @brief File ID.
        integer(int32) :: m_id = 0
        !> @brief Is the stream open?
        logical :: m_streamOpen = .false.
    contains
        !> @brief Returns true if the file is currently open; else, false if it
        !! is not.
        procedure, public :: is_open => tfm_is_open
        !> @brief Closes the currently open file.
        procedure, public :: close => tfm_close
        !> @brief Cleans up the resources used by the text_file_manager
        !! instance.
        final :: tfm_clean_up
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for writing text files.
    type, extends(text_file_manager) :: text_writer
    private
    contains
        !> @brief Opens a text file for writing.
        procedure, public :: open => tw_init
        !> Writes text to the file, but does not advance to the next line.
        generic, public :: write => tw_write_txt
        !> Writes text to the file, but does advance to the next line.
        generic, public :: write_line => tw_write_txt_line

        procedure :: tw_write_txt
        procedure :: tw_write_txt_line
    end type

! ------------------------------------------------------------------------------
    !> @brief Defines a mechanism for reading text files.
    type, extends(text_file_manager) :: text_reader
    private
        !> The current file position.
        integer(int32) :: m_position = 0
        !> The filename.
        character(len = :), allocatable :: m_fname
    contains
        !> @brief Opens a text file for reading.
        procedure, public :: open => tr_init
        !> @brief Reads the entire contents of an ASCII text file into a string.
        procedure, public :: read_all => tr_read_full_file
        !> @brief Reads a single character from an ASCII text file.
        procedure, public :: read_char => tr_read_char
        !> @brief Reads a single line from an ASCII text file.
        procedure, public :: read_line => tr_read_line
        !> @brief Gets the current position in the file.
        procedure, public :: get_position => tr_get_position
        !> @brief Sets the current position in the file.
        procedure, public :: set_position => tr_set_position
        !> @brief Moves the current position to the start of the file.
        procedure, public :: move_to_start => tr_move_to_start
        !> @brief Tests to see if the current position denotes the end-of-file.
        procedure, public :: end_of_file => tr_eof
    end type

contains
! ******************************************************************************
! TEXT_FILE_MANAGER MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Returns true if the file is currently open; else, false if it
    !! is not.
    !!
    !! @param[in] this The text_file_manager object.
    !!
    !! @return True if the file is open; else, false.
    pure function tfm_is_open(this) result(x)
        ! Arguments
        class(text_file_manager), intent(in) :: this
        logical :: x

        ! Process
        x = this%m_streamOpen
    end function

! ------------------------------------------------------------------------------
    !> @brief Closes the currently open stream.
    !!
    !! @param[in,out] this The text_file_manager object.
    !! @param[in] delete An optional input, that if set to true, causes the
    !!  file to be deleted.  The default is false such that the file remains.
    subroutine tfm_close(this, delete)
        ! Arguments
        class(text_file_manager), intent(inout) :: this
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
    !> @brief Cleans up the resources used by the text_file_manager instance
    !! ensuring the file is closed.
    !!
    !! @param[in,out] this The text_file_manager object.
    subroutine tfm_clean_up(this)
        type(text_file_manager), intent(inout) :: this
        call this%close(.false.)
    end subroutine

! ******************************************************************************
! TEXT_WRITER
! ------------------------------------------------------------------------------
    !> @brief Opens a text file for writing.
    !!
    !! @param[in,out] this The text_writer object.
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
    subroutine tw_init(this, fname, append, err)
        ! Arguments
        class(text_writer), intent(inout) :: this
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

        ! Open the file
        if (append2File) then
            open(newunit = this%m_id, file = fname, position = "append", &
                iostat = flag)
        else
            open(newunit = this%m_id, file = fname, iostat = flag)
        end if
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be opened/created.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("tw_init", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_streamOpen = .true.
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes text to the file, but does not advance to the next line.
    !!
    !! @param[in] this The text_writer object.
    !! @param[in] txt The text to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine tw_write_txt(this, txt, err)
        ! Arguments
        class(text_writer), intent(in) :: this
        character(len = *), intent(in) :: txt
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (.not.this%is_open()) then
            call errmgr%report_error("tw_write_txt", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        write(this%m_id, '(A)', advance = 'no') txt
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Writes text to the file, but does advance to the next line.
    !!
    !! @param[in] this The text_writer object.
    !! @param[in] txt The text to write.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file has not yet been opened.
    subroutine tw_write_txt_line(this, txt, err)
        ! Arguments
        class(text_writer), intent(in) :: this
        character(len = *), intent(in) :: txt
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (.not.this%is_open()) then
            call errmgr%report_error("tw_write_txt_line", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Process
        write(this%m_id, '(A)') txt
    end subroutine

! ******************************************************************************
! TEXT_READER MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Opens a text file for reading.
    !!
    !! @param[in,out] this The text_reader object.
    !! @param[in] fname The name of the file to open.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if the file could not be opened.
    subroutine tr_init(this, fname, err)
        ! Arguments
        class(text_reader), intent(inout) :: this
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
            call errmgr%report_error("tr_init", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
        this%m_streamOpen = .true.
        this%m_position = 1
        this%m_fname = fname
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Reads the entire contents of an ASCII text file into a string.
    !!
    !! @param[in] this The text_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file hasn't been opened.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory
    !!      available.
    !!  - FIO_FILE_IO_ERROR: Occurs if the file could not be read.
    !!
    !! @return The string containing the file contents.  Notice, the line
    !!  termination characters have not been stripped out of the string.
    !!
    !! @par Remarks
    !! Notice, the position indicator is not referenced, or utilized, for this
    !! read operation.  Regardless of its status, the entire file is read.
    function tr_read_full_file(this, err) result(x)
        ! REF:
        ! https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/542533

        ! Arguments
        class(text_reader), intent(in) :: this
        class(errors), intent(inout), optional, target :: err
        character(len = :), allocatable :: x

        ! Local Variables
        integer(int32) :: fsize, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the file is open
        if (.not.this%is_open()) then
            call errmgr%report_error("tr_read_full_file", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Determine the file size, and allocate a buffer
        inquire(file = this%m_fname, size = fsize)
        if (fsize == 0) return
        allocate(character(len = fsize) :: x, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("tr_read_full_file", &
                "Insufficient memory available.", FIO_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Read the file
        read(this%m_id, pos = 1, iostat = flag) x
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be read.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("tr_read_full_file", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a single character from an ASCII text file.
    !!
    !! @param[in,out] this The text_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file hasn't been opened.
    !!  - FIO_FILE_IO_ERROR: Occurs if the file could not be read.
    !!  - FIO_END_OF_FILE_ERROR: Occurs if the end-of-file is encountered.
    !!
    !! @return The character.
    !!
    !! @par Remarks
    !! On output, the position indicator is incremented by one character.
    function tr_read_char(this, err) result(x)
        ! Arguments
        class(text_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        character :: x

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

        ! Ensure the file is open
        if (.not.this%is_open()) then
            call errmgr%report_error("tr_read_char", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Read the character
        read(this%m_id, pos = this%m_position, iostat = flag) x
        if (flag > 0) then
            write(errmsg, "(AI0A)") &
                "The file could not be read.  Error code ", flag, &
                " was encountered."
            call errmgr%report_error("tr_read_char", trim(errmsg), &
                FIO_FILE_IO_ERROR)
            return
        else if (flag < 0) then
            call errmgr%report_error("tr_read_char", &
                "The end of the file has been encountered.", &
                FIO_END_OF_FILE_ERROR)
            return
        end if

        ! Increment the position
        ! FYI: storage_size returns the storage size of x in bits, but position
        ! is in bytes; hence division by 8
        this%m_position = this%m_position + storage_size(x) / 8
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads a single line from an ASCII text file.
    !!
    !! @param[in] this The text_reader object.
    !! @param[out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_UNOPENED_ERROR: Occurs if the file hasn't been opened.
    !!  - FIO_FILE_IO_ERROR: Occurs if the file could not be read.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory
    !!      available.
    !!
    !! @return The string containing the line contents.
    !!
    !! @par Remarks
    !! On output, the position indicator is incremented to account for the
    !! length of the line, including any termination characters.
    function tr_read_line(this, err) result(x)
        ! Arguments
        class(text_reader), intent(inout) :: this
        class(errors), intent(inout), optional, target :: err
        character(len = :), allocatable :: x

        ! Local Variables
        integer(int32) :: i, flag, fsize
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        character(len = :), allocatable :: buffer
        character :: c, eol, cr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        cr = char(13) ! Carriage Return Character
        eol = new_line(eol)
        i = 0

        ! Ensure the file is open
        if (.not.this%is_open()) then
            call errmgr%report_error("tr_read_line", &
                "The file is not opened.", FIO_UNOPENED_ERROR)
            return
        end if

        ! Allocate space for a buffer that is sufficiently sized to hold an
        ! entire line.
        inquire(file = this%m_fname, size = fsize)
        if (fsize == 0) return
        allocate(character(len = fsize) :: buffer, stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("tr_read_full_file", &
                "Insufficient memory available.", FIO_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Read in each character until we reach EOF or EOL
        do
            read(this%m_id, pos = this%m_position, iostat = flag) c
            if (flag < 0) then
                ! EOF reached
                exit
            else if (flag > 0) then
                write(errmsg, "(AI0A)") &
                    "The file could not be read.  Error code ", flag, &
                    " was encountered."
                call errmgr%report_error("tr_read_line", trim(errmsg), &
                    FIO_FILE_IO_ERROR)
                return
            end if

            ! Increment the file position
            ! FYI: storage_size returns the storage size of x in bits, but
            ! position is in bytes; hence division by 8
            this%m_position = this%m_position + storage_size(c) / 8

            ! If we encounter a carriage return, simply cycle the loop
            if (c == cr) cycle

            ! See if we're at the end of the line yet
            if (c == eol) exit

            ! Store the value
            i = i + 1
            buffer(i:i) = c
        end do

        ! Trim the buffer to fit tightly
        x = buffer(1:i)
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the current position in the file.
    !!
    !! @param[in] this The text_reader object.
    !!
    !! @return The current position within the file, in bytes.
    pure function tr_get_position(this) result(x)
        class(text_reader), intent(in) :: this
        integer(int32) :: x
        x = this%m_position
    end function

! ------------------------------------------------------------------------------
    !> @brief Sets the current position within the file.
    !!
    !! @param[in,out] this The text_reader object.
    !! @param[in] x The file position, in bytes.
    subroutine tr_set_position(this, x)
        ! Arguments
        class(text_reader), intent(inout) :: this
        integer(int32), intent(in) :: x

        ! Local Variables
        integer(int32) :: p

        ! Quick Return
        if (.not.this%is_open()) return

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
    !! @param[in,out] this The text_reader object.
    subroutine tr_move_to_start(this)
        ! Arguments
        class(text_reader), intent(inout) :: this

        ! Quick Return
        if (.not.this%is_open()) return

        ! Process
        this%m_position = 1
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Tests to see if the current position denotes the end-of-file.
    !!
    !! @param[in] this The text_reader object.
    !!
    !! @return Returns true if the current position is the end of the file;
    !!  else, false.
    function tr_eof(this) result(x)
        ! Arguments
        class(text_reader), intent(in) :: this
        logical :: x

        ! Local Variables
        integer(int8) :: temp
        integer(int32) :: flag

        ! Process - do not increment the position!
        read(this%m_id, pos = this%m_position, iostat = flag) temp
        x = flag < 0
    end function

! ******************************************************************************
! NEW ADDITION (V2.1.0) ADDED 26-MAY 2018 - JAC
! ------------------------------------------------------------------------------
    !> @brief Reads a text file containing numeric data.  The file may also
    !! contain non-numeric data such as column headers, data descriptors, etc.
    !! The file may also contain non-numeric interuptions along the way.
    !! However, non-numeric content is stripped from the output of this routine,
    !! and only the numeric contents are returned.
    !!
    !! @param[in] fname The complete name of the file to open, including the
    !!  full path and extension.
    !! @param[in] delim The delimiter character used to seperate numeric values.
    !!  This parameter is optional.  If no delimiter character is specified, a
    !!  comma will be assumed.
    !! @param[in,out] err An optional argument, that if supplied, provides a
    !!  mechanism for handling any errors that may occur during the execution
    !!  of this routine.  The following errors are possible.
    !!  - FIO_FILE_IO_ERROR: Occurs if the file could not be read.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
    !!      avialable.
    !!
    !! @return A matrix containing the numeric data from the file.
    function read_numeric_text_file(fname, delim, err) result(x)
        ! Arguments
        character(len = *), intent(in) :: fname
        character, intent(in), optional :: delim
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: x

        ! Local Variables
        type(text_reader) :: reader
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = :), allocatable :: line
        integer(int32) :: filesize, i, j, ncols, nrows, flag, nitems
        type(string), allocatable, dimension(:) :: items
        logical :: numeric
        real(real64) :: temp
        real(real64), allocatable, dimension(:,:) :: buffer

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Initialization
        call reader%open(fname, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Determine the size of the file, in bytes
        inquire(file = fname, size = filesize)

        ! Cycle through the contents of the file until and end of file situation
        ! is encountered
        i = 0
        do
            ! Ensure we haven't reached the end of the file
            if (reader%end_of_file()) exit

            ! Read in a line
            line = reader%read_line(errmgr)
            if (errmgr%has_error_occurred()) return

            ! If the line is empty, just move on
            if (len(line) == 0) cycle

            ! Split the line by the delimiter character
            items = split_string(line, delim)
            nitems = size(items)

            ! Test to see if the string is numeric, or if there are non-numeric
            ! characters.  If there are non-numeric characters, simply skip
            ! to the next line.
            numeric = .true.
            do j = 1, nitems
                read(items(j)%str, *, iostat = flag) temp
                if (flag /= 0) then
                    numeric = .false.
                    exit
                end if
            end do
            if (.not.numeric) cycle

            ! Store the data
            if (i == 0) then
                ! This is the first time through the loop.  Estimate the
                ! number of rows of data by assuming the file is completely
                ! numeric, and that each number occupies 8 bytes.
                ncols = nitems
                nrows = filesize * 8 / ncols
                allocate(buffer(nrows, ncols), stat = flag)
                if (flag /= 0) then
                    call errmgr%report_error("read_numeric_text_file", &
                        "Insufficient memory available.", &
                        FIO_OUT_OF_MEMORY_ERROR)
                    return
                end if
            end if
            i = i + 1

            ! Store the data.  Notice, each string in items has already been
            ! shown to be convertable to a numeric value.  As such, no
            ! additional testing is needed at this point.
            do j = 1, min(ncols, nitems)
                call string_to_number(items(j)%str, temp)
                buffer(i,j) = temp
            end do
            if (ncols > nitems) x(i,nitems + 1:ncols) = 0.0d0
        end do

        ! Trim down the buffer if necessary
        x = buffer(1:i,:)
    end function

! ------------------------------------------------------------------------------
end module
