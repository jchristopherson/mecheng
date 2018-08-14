! fortio_hbm.f90

!> @brief \b fortio_hbm
!!
!! @par Purpose
!! This module provides types and routines for supporting the HBM Catman
!! binary format.
module fortio_hbm
    use iso_fortran_env
    use ferror
    use fortio_binary
    use fortio_types
    implicit none
    private
    public :: hbm_data_channel
    public :: hbm_data_file
    public :: read_catman_file
    public :: hbm_data_to_matrix

! ******************************************************************************
! CONSTANTS
! ------------------------------------------------------------------------------
    integer(int16), parameter :: RESERVED_STRING_COUNT = 32
    integer(int16), parameter :: RESERVED_STRING_LENGTH = 256

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> A data type encapsulating the contents of an HBM data channel.
    type hbm_data_channel
        !> The channel name.
        character(len = :), allocatable :: name
        !> The unit of measure.
        character(len = :), allocatable :: unit
        !> Any user comments.
        character(len = :), allocatable :: comments
        !> The channel format (0 = numeric, 1 = string, 2 = binary object)
        integer(int16) :: data_format
        !> The data width, in bytes.
        integer(int16) :: data_width
        !> The date of the measurement.
        real(real64) :: date
        !> Sensor information.
        character(len = :), allocatable :: sensor_info
        !> The channel location in the HBM database.
        integer(int16) :: channel_location
        !> The channel length.
        integer(int64) :: channel_length
        !> The data.
        real(real64), allocatable, dimension(:) :: values
    end type

! ------------------------------------------------------------------------------
    !> A data type encapsulating the contents of an HBM Catman data file.
    type hbm_data_file
        !> The filename.
        character(len = :), allocatable :: filename
        !> The file version.
        integer(int16) :: version
        !> File comments.
        character(len = :), allocatable :: comments
        !> The number of data channels.
        integer(int16) :: channel_count
        !> The maximum channel length.
        integer(int64) :: max_channel_length
        !> The reduction factor in the event of file compression.
        integer(int64) :: reduction_factor
        !> A list of reserved strings.
        character(len = RESERVED_STRING_LENGTH), &
            dimension(RESERVED_STRING_COUNT) :: reserved_strings
        !> A list of channel data.
        type(hbm_data_channel), allocatable, dimension(:) :: channels
    end type

contains
! ------------------------------------------------------------------------------
    !> @brief Returns the contents of an HBM data file in the form of a
    !! matrix.
    !!
    !! @param[in] x The hbm_data_file object containing the file contents.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory 
    !!      available.
    !!
    !! @return The resulting matrix with each channel in its own column.
    function hbm_data_to_matrix(x, err) result(rst)
        ! Arguments
        class(hbm_data_file), intent(in) :: x
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: rst

        ! Local Variables
        integer(int64) :: j, m, n, chanlength
        integer(int32) :: info
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        n = int(x%channel_count, int64)
        m = 0
        do j = 1, n
            m = max(m, x%channels(j)%channel_length)
        end do
        allocate(rst(m, n), stat = info)
        if (info /= 0) then
            call errmgr%report_error("hbm_data_to_matrix", &
                "Insufficient memory available.", FIO_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Process
        do j = 1, n
            chanlength = min(m, x%channels(j)%channel_length)
            rst(1:chanlength,j) = x%channels(j)%values(1:chanlength)
        end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Reads an HBM Catman data file.  
    !!
    !! @param[in] fname The complete filename, including the full path and
    !!  file extension.
    !! @param[in,out] err An optional errors-based object that if provided can be
    !!  used to retrieve information relating to any errors encountered during
    !!  execution.  If not provided, a default implementation of the errors
    !!  class is used internally to provide error handling.  Possible errors and
    !!  warning messages that may be encountered are as follows.
    !!  - FIO_FILE_IO_ERROR: Occurs if the file could not be opened.
    !!  - FIO_UNSUPPORTED_VERSION_ERROR: Occurs if the file is not of a 
    !!      supported version.
    !!  - FIO_OUT_OF_MEMORY_ERROR: Occurs if there isn't sufficient memory 
    !!      available.
    !!
    !! @return The contents of the file.
    function read_catman_file(fname, err) result(rst)
        ! Arguments
        character(len = *), intent(in) :: fname
        class(errors), intent(inout), optional, target :: err
        type(hbm_data_file) :: rst

        ! Parameters
        integer(int16), parameter :: MINIMUM_VERSION = 5010

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        type(binary_reader) :: file
        character(len = 256) :: errmsg
        integer(int64) :: li, dataOffset, offsetChannel, dummyLong
        integer(int16) :: si, sj, commentLength, dummyShort, lres(RESERVED_STRING_COUNT)
        character(len = :), allocatable :: restring
        character :: dummyChar
        real(real64) :: dummyDouble
        integer(int32) :: info

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        rst%filename = fname

        ! Open the file
        call file%open(fname, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Determine the file version & then ensure the version is supported
        rst%version = file%read_int16(errmgr)
        if (errmgr%has_error_occurred()) return

        if (rst%version < MINIMUM_VERSION) then
            write(errmsg, '(AI0A)') "Version ", rst%version, " is not supported."
            call errmgr%report_error("read_catman_file", trim(errmsg), &
                FIO_UNSUPPORTED_VERSION_ERROR)
            return
        end if

        ! Determine the data offset, and file comment length
        dataOffset = file%read_int64(errmgr)
        if (errmgr%has_error_occurred()) return

        commentLength = file%read_int16(errmgr)
        if (errmgr%has_error_occurred()) return

        ! Read in the comments
        allocate(character(len = commentLength) :: rst%comments)
        do si = 1, commentLength
            rst%comments(si:si) = file%read_char(errmgr)
            if (errmgr%has_error_occurred()) return
        end do

        ! Read in the file, taking care for version specifics
        if (rst%version == MINIMUM_VERSION) then ! Version 4.5
            ! # of channels
            rst%channel_count = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Maximum channel length
            rst%max_channel_length = file%read_int64(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Offset channel
            offsetChannel = file%read_int64(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Reduction factor
            rst%reduction_factor = file%read_int64(errmgr)
            if (errmgr%has_error_occurred()) return
        else ! Version 5.0+
            do si = 1, RESERVED_STRING_COUNT
                lres(si) = file%read_int16(errmgr)
                if (errmgr%has_error_occurred()) return
                if (allocated(restring)) deallocate(restring)
                allocate(character(len = lres(si)) :: restring)
                do sj = 1, lres(si)
                    restring(sj:sj) = file%read_char(errmgr)
                    if (errmgr%has_error_occurred()) return
                end do
                dummyShort = min(RESERVED_STRING_LENGTH, lres(si))
                rst%reserved_strings(si) = restring(1:dummyShort)
            end do

            ! # of channels
            rst%channel_count = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Maximum channel length
            rst%max_channel_length = file%read_int64(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Offset channel
            offsetChannel = file%read_int64(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Reduction factor
            rst%reduction_factor = file%read_int64(errmgr)
            if (errmgr%has_error_occurred()) return
        end if

        ! Allocate space for each channel object
        allocate(rst%channels(rst%channel_count))

        ! Read the channel header
        do si = 1, rst%channel_count
            rst%channels(si)%channel_location = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            
            rst%channels(si)%channel_length = file%read_int64(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Channel Name
            dummyShort = file%read_int16(errmgr) ! Channel name length
            if (errmgr%has_error_occurred()) return
            if (dummyShort > 0) then
                allocate(character(len = dummyShort) :: rst%channels(si)%name)
                do sj = 1, dummyShort
                    rst%channels(si)%name(sj:sj) = file%read_char(errmgr)
                    if (errmgr%has_error_occurred()) return
                end do
            end if

            ! Unit of Measure
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            if (dummyShort > 0) then
                allocate(character(len = dummyShort) :: rst%channels(si)%unit)
                do sj = 1, dummyShort
                    rst%channels(si)%unit(sj:sj) = file%read_char(errmgr)
                    if (errmgr%has_error_occurred()) return
                end do
            end if

            ! Channel Comment
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            if (dummyShort > 0) then
                allocate(character(len = dummyShort) :: rst%channels(si)%comments)
                do sj = 1, dummyShort
                    rst%channels(si)%comments(sj:sj) = file%read_char(errmgr)
                    if (errmgr%has_error_occurred()) return
                end do
            end if

            ! Channel Format
            rst%channels(si)%data_format = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            
            rst%channels(si)%data_width = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Date/Time Info
            rst%channels(si)%date = file%read_real64(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Extended Header Info
            dummyLong = file%read_int64(errmgr)
            if (errmgr%has_error_occurred()) return
            do li = 1, dummyLong
                dummyChar = file%read_char(errmgr)
                if (errmgr%has_error_occurred()) return
            end do

            ! Linearization Info
            dummyChar = file%read_char(errmgr)
            if (errmgr%has_error_occurred()) return

            ! User Scaling
            dummyChar = file%read_char(errmgr)
            if (errmgr%has_error_occurred()) return

            ! # of points used for user scale
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            do sj = 1, dummyShort
                dummyDouble = file%read_real64(errmgr)
                if (errmgr%has_error_occurred()) return
            end do

            ! Thermo Type
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Length of formula
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            do sj = 1, dummyShort
                dummyChar = file%read_char(errmgr)
                if (errmgr%has_error_occurred()) return
            end do

            ! Sensor Information
            if (rst%version >= 5012) then
                dummyLong = file%read_int64(errmgr)
                if (errmgr%has_error_occurred()) return
                if (dummyLong > 0) then
                    allocate(character(len = dummyLong) :: rst%channels(si)%sensor_info)
                    do li = 1, dummyLong
                        rst%channels(si)%sensor_info(li:li) = file%read_char(errmgr)
                        if (errmgr%has_error_occurred()) return
                    end do
                end if
            end if
        end do

        ! Move the beginning of the actual data
        call file%set_position(dataOffset)

        ! Read in the data
        do si = 1, rst%channel_count
            dummyLong = rst%channels(si)%channel_length
            if (dummyLong <= 0) cycle
            allocate(rst%channels(si)%values(dummyLong), stat = info)
            if (info /= 0) then
                call errmgr%report_error("read_catman_file", &
                    "Insufficient memory available.", FIO_OUT_OF_MEMORY_ERROR)
                return
            end if
            do li = 1, dummyLong
                rst%channels(si)%values(li) = file%read_real64(errmgr)
                if (errmgr%has_error_occurred()) return
            end do
        end do
    end function

end module