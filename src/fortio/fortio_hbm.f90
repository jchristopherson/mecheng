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
        integer(int32) :: channel_length
        !> The data.
        real(real64), allocatable, dimension(:) :: values
        !> Header data.
        character(len = :), allocatable :: header
        !> Linearization mode.
        integer(int8) :: linearization_mode
        !> User scaling information.
        integer(int8) :: user_scale
        !> User scaling points.
        real(real64), allocatable, dimension(:) :: user_scale_points
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
        integer(int32) :: max_channel_length
        !> The reduction factor in the event of file compression.
        integer(int32) :: reduction_factor
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
        integer(int32) :: j, m, n, chanlength, info
        type(errors), target :: deferr
        class(errors), pointer :: errmgr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        n = x%channel_count
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
        integer(int32) :: dataOffset, dummyLong
        integer(int32), allocatable, dimension(:) :: offsetChannel
        integer(int16) :: si, commentLength, dummyShort, lres(RESERVED_STRING_COUNT)
        character(len = :), allocatable :: restring
        character(len = :), allocatable :: dummyString
        integer(int8) :: dummyByte

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
        dataOffset = file%read_int32(errmgr)
        if (errmgr%has_error_occurred()) return

        commentLength = file%read_int16(errmgr)
        if (errmgr%has_error_occurred()) return

        ! Read in the comments
        rst%comments = file%read_char_array(commentLength, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Read in the file, taking care for version specifics
        if (rst%version == MINIMUM_VERSION) then ! Version 4.5
            ! # of channels
            rst%channel_count = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Maximum channel length
            rst%max_channel_length = file%read_int32(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Offset channel
            offsetChannel = file%read_int32(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Reduction factor
            rst%reduction_factor = file%read_int32(errmgr)
            if (errmgr%has_error_occurred()) return
        else ! Version 5.0+
            lres = 0
            do si = 1, RESERVED_STRING_COUNT
                lres(si) = file%read_int16(errmgr)
                if (errmgr%has_error_occurred()) return

                restring = file%read_char_array(lres(si), errmgr)
                if (errmgr%has_error_occurred()) return

                dummyShort = min(RESERVED_STRING_LENGTH, lres(si))
                rst%reserved_strings(si) = restring(1:dummyShort)
            end do

            ! # of channels
            rst%channel_count = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Maximum channel length
            rst%max_channel_length = file%read_int32(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Offset channel
            offsetChannel = file%read_int32_array(rst%channel_count, errmgr)
            if (errmgr%has_error_occurred()) return

            ! Reduction factor
            rst%reduction_factor = file%read_int32(errmgr)
            if (errmgr%has_error_occurred()) return
        end if

        ! Allocate space for each channel object
        allocate(rst%channels(rst%channel_count))

        ! Read the channel header
        do si = 1, rst%channel_count
            rst%channels(si)%channel_location = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            
            rst%channels(si)%channel_length = file%read_int32(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Channel Name
            dummyShort = file%read_int16(errmgr) ! Channel name length
            if (errmgr%has_error_occurred()) return
            
            rst%channels(si)%name = file%read_char_array(dummyShort, errmgr)
            if (errmgr%has_error_occurred()) return

            ! Unit of Measure
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            
            rst%channels(si)%unit = file%read_char_array(dummyShort, errmgr)
            if (errmgr%has_error_occurred()) return

            ! Channel Comment
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            
            rst%channels(si)%comments = file%read_char_array(dummyShort, errmgr)
            if (errmgr%has_error_occurred()) return

            ! Channel Format
            rst%channels(si)%data_format = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return
            
            ! Data Width
            rst%channels(si)%data_width = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Date/Time Info
            rst%channels(si)%date = file%read_real64(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Extended Header Info
            dummyLong = file%read_int32(errmgr)
            if (errmgr%has_error_occurred()) return

            rst%channels(si)%header = file%read_char_array(dummyLong, errmgr)
            if (errmgr%has_error_occurred()) return

            ! Linearization Info
            rst%channels(si)%linearization_mode = file%read_int8(errmgr)
            if (errmgr%has_error_occurred()) return

            ! User Scaling
            rst%channels(si)%user_scale = file%read_int8(errmgr)
            if (errmgr%has_error_occurred()) return

            ! # of points used for user scale
            dummyByte = file%read_int8(errmgr)
            if (errmgr%has_error_occurred()) return

            rst%channels(si)%user_scale_points = file%read_real64_array(int(dummyByte, int32), errmgr)
            if (errmgr%has_error_occurred()) return

            ! Thermo Type
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            ! Length of formula
            dummyShort = file%read_int16(errmgr)
            if (errmgr%has_error_occurred()) return

            dummyString = file%read_char_array(dummyShort, errmgr)
            if (errmgr%has_error_occurred()) return

            ! Sensor Information
            if (rst%version >= 5012) then
                dummyLong = file%read_int32(errmgr)
                if (errmgr%has_error_occurred()) return

                rst%channels(si)%sensor_info = file%read_char_array(dummyLong, errmgr)
                if (errmgr%has_error_occurred()) return
            end if
        end do

        ! Move the beginning of the actual data
        call file%set_position(dataOffset + 1)

        ! Read in the data
        do si = 1, rst%channel_count
            dummyLong = rst%channels(si)%channel_length
            rst%channels(si)%values = file%read_real64_array(dummyLong, errmgr)
            if (errmgr%has_error_occurred()) return
        end do
    end function

! ------------------------------------------------------------------------------

end module