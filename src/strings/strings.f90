! strings.f90

module strings
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private
    public :: string
    public :: string_builder
    public :: split_string
    public :: string_to_number
    public :: remove_chars
    public :: remove_substring
    public :: remove_char_range
    public :: replace_chars
    public :: to_string
    public :: cstr_2_fstr
    public :: fstr_2_cstr

! ******************************************************************************
! TYPES
! ------------------------------------------------------------------------------
    !> @brief Defines a string type.
    type string
        !> The string.
        character(len = :), allocatable :: str
    end type

! ------------------------------------------------------------------------------
    !> @brief Provides a mechanism for building strings.
    type string_builder
    private
        !> The actual length of the string.
        integer(int32) :: m_length = 0
        !> The character buffer.
        character(len = :), allocatable :: m_buffer
    contains
        !> @brief Initializes the string_builder object.
        procedure, public :: initialize => sb_init
        !> @brief Appends to the string.
        procedure, public :: append => sb_append
        !> @brief Returns the contents as a single string.
        procedure, public :: to_string => sb_to_string
        !> @brief Gets the current length of the string being built.
        procedure, public :: get_length => sb_get_length
    end type

! ******************************************************************************
! INTERFACES
! ------------------------------------------------------------------------------
    !> @brief Converts a string to a numeric value.
    interface string_to_number
        module procedure :: string_to_double
        module procedure :: string_to_single
        module procedure :: string_to_int16
        module procedure :: string_to_int32
        module procedure :: string_to_int64
    end interface

! ------------------------------------------------------------------------------
    !> @brief Converts an item to a string.
    interface to_string
        module procedure :: to_string_int8
        module procedure :: to_string_int16
        module procedure :: to_string_int32
        module procedure :: to_string_int64
        module procedure :: to_string_single
        module procedure :: to_string_single_fmt
        module procedure :: to_string_double
        module procedure :: to_string_double_fmt
        module procedure :: to_string_cmplx_32
        module procedure :: to_string_cmplx_32_fmt
        module procedure :: to_string_cmplx_64
        module procedure :: to_string_cmplx_64_fmt
    end interface

contains
! ******************************************************************************
! STRING_BUILDER MEMBERS
! ------------------------------------------------------------------------------
    !> @brief Initializes the string_builder object.
    !!
    !! @param[in,out] this The string_builder object.
    subroutine sb_init(this)
        ! Arguments
        class(string_builder), intent(inout) :: this

        ! Initialization
        this%m_length = 0
        if (.not.allocated(this%m_buffer)) &
            allocate(character(len = 128) :: this%m_buffer)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Appends to the string.
    !!
    !! @param[in,out] this The string_builder object.
    !! @param[in] txt The string to append.
    subroutine sb_append(this, txt)
        ! Arguments
        class(string_builder), intent(inout) :: this
        character(len = *), intent(in) :: txt

        ! Local Variables
        integer(int32) :: space, n, start, finish, nb
        character(len = :), allocatable :: temp

        ! Process
        if (.not.allocated(this%m_buffer)) call this%initialize()
        space = len(this%m_buffer) - this%m_length
        n = len(txt)
        if (space < n) then
            ! Reallocate a larger buffer
            nb = len(this%m_buffer)
            allocate(character(len = nb + max(n, 128)) :: temp)
            temp(1:nb) = this%m_buffer
            this%m_buffer = temp
        end if
        start = this%m_length + 1
        finish = start + n - 1
        this%m_buffer(start:finish) = txt(1:n)
        this%m_length = this%m_length + n
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Returns the contents as a single string.
    !!
    !! @param[in] this The string_builder object.
    !! @return The string.
    pure function sb_to_string(this) result(txt)
        class(string_builder), intent(in) :: this
        character(len = :), allocatable :: txt
        txt = this%m_buffer(1:this%m_length)
    end function

! ------------------------------------------------------------------------------
    !> @brief Gets the current length of the string being built.
    !!
    !! @param[in] this The string_builder object.
    !! @return The length.
    pure function sb_get_length(this) result(n)
        class(string_builder), intent(in) :: this
        integer(int32) :: n
        n = this%m_length
    end function

! ******************************************************************************
! STRING OPERATIONS
! ------------------------------------------------------------------------------
    !> @brief Splits a string at every occurrence of a delimiter character.
    !!
    !! @param[in] txt The string to split.
    !! @param[in] delim An optional input that defines the delimiter character.
    !!  The default is a comma.
    !!
    !! @return A collection of strings.  The delimiter character is not
    !!  returned.
    pure function split_string(txt, delim) result(rst)
        ! Arguments
        character(len = *), intent(in) :: txt
        character, intent(in), optional :: delim
        type(string), allocatable, dimension(:) :: rst

        ! Local Variables
        integer(int32) :: i, j, k, n
        character :: delimiter
        character(len = len(txt)) :: buffer
        type(string) :: stbuffer(len(txt))

        ! Initialization
        n = len(txt)
        if (present(delim)) then
            delimiter = delim
        else
            delimiter = ","
        end if

        ! Process
        j = 0
        k = 0
        do i = 1, n
            if (txt(i:i) /= delimiter) then
                j = j + 1
                buffer(j:j) = txt(i:i)
            else
                ! Reached a delimiter character
                if (j == 0) cycle
                k = k + 1
                stbuffer(k)%str = buffer(1:j)
                j = 0
            end if
        end do

        ! Catch the last string (between the final delimiter and end of txt)
        if (j /= 0) then
            k = k + 1
            stbuffer(k)%str = buffer(1:j)
        end if

        ! End
        rst = stbuffer(1:k)
    end function

! ******************************************************************************
! STRING TO NUMBER ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Converts a string to a 64-bit floating-point value.
    !!
    !! @param[in] str The string to convert.
    !! @param[out] x The resulting value.
    !! @param[out] info An optional parameter that returns true if the
    !!  conversion was successful; else, returns false.
    subroutine string_to_double(str, x, info)
        ! Arguments
        character(len = *), intent(in) :: str
        real(real64), intent(out) :: x
        logical, intent(out), optional :: info

        ! Local Variables
        integer(int32) :: flag

        ! Process
        read(str, *, iostat = flag) x
        if (present(info)) info = (flag == 0)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Converts a string to a 32-bit floating-point value.
    !!
    !! @param[in] str The string to convert.
    !! @param[out] x The resulting value.
    !! @param[out] info An optional parameter that returns true if the
    !!  conversion was successful; else, returns false.
    subroutine string_to_single(str, x, info)
        ! Arguments
        character(len = *), intent(in) :: str
        real(real32), intent(out) :: x
        logical, intent(out), optional :: info

        ! Local Variables
        integer(int32) :: flag

        ! Process
        read(str, *, iostat = flag) x
        if (present(info)) info = (flag == 0)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Converts a string to a 16-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !! @param[out] x The resulting value.
    !! @param[out] info An optional parameter that returns true if the
    !!  conversion was successful; else, returns false.
    subroutine string_to_int16(str, x, info)
        ! Arguments
        character(len = *), intent(in) :: str
        integer(int16), intent(out) :: x
        logical, intent(out), optional :: info

        ! Local Variables
        integer(int32) :: flag

        ! Process
        read(str, *, iostat = flag) x
        if (present(info)) info = (flag == 0)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Converts a string to a 32-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !! @param[out] x The resulting value.
    !! @param[out] info An optional parameter that returns true if the
    !!  conversion was successful; else, returns false.
    subroutine string_to_int32(str, x, info)
        ! Arguments
        character(len = *), intent(in) :: str
        integer(int32), intent(out) :: x
        logical, intent(out), optional :: info

        ! Local Variables
        integer(int32) :: flag

        ! Process
        read(str, *, iostat = flag) x
        if (present(info)) info = (flag == 0)
    end subroutine

! ------------------------------------------------------------------------------
    !> @brief Converts a string to a 64-bit integer value.
    !!
    !! @param[in] str The string to convert.
    !! @param[out] x The resulting value.
    !! @param[out] info An optional parameter that returns true if the
    !!  conversion was successful; else, returns false.
    subroutine string_to_int64(str, x, info)
        ! Arguments
        character(len = *), intent(in) :: str
        integer(int64), intent(out) :: x
        logical, intent(out), optional :: info

        ! Local Variables
        integer(int32) :: flag

        ! Process
        read(str, *, iostat = flag) x
        if (present(info)) info = (flag == 0)
    end subroutine

! ******************************************************************************
! STRING MODIFICATION ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Removes all occurrences of a specific character from a string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] c The character to remove.
    !!
    !! @return The modified string.
    pure function remove_chars(str, c) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        character, intent(in) :: c
        character(len = :), allocatable :: rst

        ! Local Variables
        character(len = len(str)) :: buffer
        integer(int32) :: i, j, n

        ! Process
        n = len(str)
        j = 0
        do i = 1, n
            if (str(i:i) /= c) then
                j = j + 1
                buffer(j:j) = str(i:i)
            end if
        end do
        rst = buffer(1:j)
    end function

! ------------------------------------------------------------------------------
    !> @brief Removes all occurrences of a substring from a string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] s The substring to remove.
    !!
    !! @return The modified string.
    pure function remove_substring(str, s) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str, s
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: i, start, finish, nstr, ns, nb
        character(len = len(str)) :: buffer

        ! Process
        nstr = len(str)
        ns = len(s)
        nb = nstr
        buffer = str(1:nstr)
        i = 1
        do
            start = index(buffer(i:nstr), s)
            if (start == 0) exit
            finish = start + ns - 1
            nb = nb - ns
            buffer(start:nb) = buffer(finish+1:nstr)
        end do
        rst = buffer(1:nb)
    end function

! ------------------------------------------------------------------------------
    !> @brief Removes a range of characters from a string.
    !!
    !! @param[in] str The string on which to operate.
    !! @param[in] i1 The lower index of the range to remove.
    !! @param[in] i2 The upper index of the range to remove.
    !!
    !! @return The modified string.
    pure function remove_char_range(str, i1, i2) result(rst)
        ! Arguments
        character(len = *), intent(in) :: str
        integer(int32), intent(in) :: i1, i2
        character(len = :), allocatable :: rst

        ! Local Variables
        integer(int32) :: start, finish, nstr, nb
        character(len = len(str)) :: buffer

        ! Process
        start = min(i1, i2)
        finish = max(i1, i2, 0)
        nstr = len(str)
        nb = nstr - (finish - start + 1)
        buffer(1:i1-1) = str(1:start-1)
        buffer(i1:nb) = str(finish+1:nstr)
        rst = buffer(1:nb)
    end function

! ------------------------------------------------------------------------------
    !> @brief Replaces all occurrences of a character in a string with a new
    !! character.
    !!
    !! @param[in,out] str On input, the string to modify.  On output, the
    !!  modified string.
    !! @param[in] oldc The character to search for.
    !! @param[in] newc The character with which to replace @p oldc.
    subroutine replace_chars(str, oldc, newc)
        ! Arguments
        character(len = *), intent(inout) :: str
        character, intent(in) :: oldc, newc

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = len(str)
        do i = 1, n
            if (str(i:i) == oldc) str(i:i) = newc
        end do
    end subroutine

! ******************************************************************************
! STRING CONVERSION ROUTINES
! ------------------------------------------------------------------------------
    !> @brief Converts an 8-bit integer to a string.
    !!
    !! @param[in] x The value to convert.
    !! @return The resulting string.
    pure function to_string_int8(x) result(str)
        ! Arguments
        integer(int8), intent(in) :: x
        character(len = :), allocatable :: str

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(I0)') x
        str = trim(buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 16-bit integer to a string.
    !!
    !! @param[in] x The value to convert.
    !! @return The resulting string.
    pure function to_string_int16(x) result(str)
        ! Arguments
        integer(int16), intent(in) :: x
        character(len = :), allocatable :: str

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(I0)') x
        str = trim(buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 32-bit integer to a string.
    !!
    !! @param[in] x The value to convert.
    !! @return The resulting string.
    pure function to_string_int32(x) result(str)
        ! Arguments
        integer(int32), intent(in) :: x
        character(len = :), allocatable :: str

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(I0)') x
        str = trim(buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 64-bit integer to a string.
    !!
    !! @param[in] x The value to convert.
    !! @return The resulting string.
    pure function to_string_int64(x) result(str)
        ! Arguments
        integer(int64), intent(in) :: x
        character(len = :), allocatable :: str

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(I0)') x
        str = trim(buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 32-bit floating-point value to a string.
    !!
    !! @param[in] x The value to convert.
    !! @return The resulting string.
    pure function to_string_single(x) result(str)
        ! Arguments
        real(real32), intent(in) :: x
        character(len = :), allocatable :: str

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, *) x
        str = trim(buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 32-bit floating-point value to a string.
    !!
    !! @param[in] x The value to convert.
    !! @param[in] fmt The formatting to apply to the value (e.g. "F6.2").
    !! @return The resulting string.
    pure function to_string_single_fmt(x, fmt) result(str)
        ! Arguments
        real(real32), intent(in) :: x
        character(len = *), intent(in) :: fmt
        character(len = :), allocatable :: str

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(' // fmt // ')') x
        str = trim(buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 64-bit floating-point value to a string.
    !!
    !! @param[in] x The value to convert.
    !! @return The resulting string.
    pure function to_string_double(x) result(str)
        ! Arguments
        real(real64), intent(in) :: x
        character(len = :), allocatable :: str

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, *) x
        str = trim(buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 64-bit floating-point value to a string.
    !!
    !! @param[in] x The value to convert.
    !! @param[in] fmt The formatting to apply to the value (e.g. "F6.2").
    !! @return The resulting string.
    pure function to_string_double_fmt(x, fmt) result(str)
        ! Arguments
        real(real64), intent(in) :: x
        character(len = *), intent(in) :: fmt
        character(len = :), allocatable :: str

        ! Local Variables
        character(len = 128) :: buffer

        ! Process
        write(buffer, '(' // fmt // ')') x
        str = trim(buffer)
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 32-bit complex value to a string.
    !!
    !! @param[in] x The value to convert.
    !! @return The resulting string.
    pure function to_string_cmplx_32(x) result(str)
        ! Arguments
        complex(real32), intent(in) :: x
        character(len = :), allocatable :: str

        ! Parameters
        real(real32), parameter :: zero = 0.0

        ! Process
        if (aimag(x) == zero) then
            str = to_string(real(x))
        else if (aimag(x) >= zero) then
            str = to_string(real(x)) // " + " // to_string(aimag(x)) // "i"
        else
            str = to_string(real(x)) // " - " // to_string(abs(aimag(x))) // "i"
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 32-bit complex value to a string.
    !!
    !! @param[in] x The value to convert.
    !! @param[in] fmt The formatting to apply to the value (e.g. "F6.2").
    !! @return The resulting string.
    pure function to_string_cmplx_32_fmt(x, fmt) result(str)
        ! Arguments
        complex(real32), intent(in) :: x
        character(len = *), intent(in) :: fmt
        character(len = :), allocatable :: str

        ! Parameters
        real(real32), parameter :: zero = 0.0

        ! Process
        if (aimag(x) == zero) then
            str = to_string(real(x))
        else if (aimag(x) >= zero) then
            str = to_string(real(x), fmt) // " + " // &
                to_string(aimag(x), fmt) // "i"
        else
            str = to_string(real(x), fmt) // " - " // &
                to_string(abs(aimag(x)), fmt) // "i"
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 64-bit complex value to a string.
    !!
    !! @param[in] x The value to convert.
    !! @return The resulting string.
    pure function to_string_cmplx_64(x) result(str)
        ! Arguments
        complex(real64), intent(in) :: x
        character(len = :), allocatable :: str

        ! Parameters
        real(real64), parameter :: zero = 0.0d0

        ! Process
        if (aimag(x) == zero) then
            str = to_string(real(x, real64))
        else if (aimag(x) >= zero) then
            str = to_string(real(x, real64)) // " + " // &
                to_string(aimag(x)) // "i"
        else
            str = to_string(real(x, real64)) // " - " // &
                to_string(abs(aimag(x))) // "i"
        end if
    end function

! ------------------------------------------------------------------------------
    !> @brief Converts a 64-bit complex value to a string.
    !!
    !! @param[in] x The value to convert.
    !! @param[in] fmt The formatting to apply to the value (e.g. "F6.2").
    !! @return The resulting string.
    pure function to_string_cmplx_64_fmt(x, fmt) result(str)
        ! Arguments
        complex(real64), intent(in) :: x
        character(len = *), intent(in) :: fmt
        character(len = :), allocatable :: str

        ! Parameters
        real(real64), parameter :: zero = 0.0d0

        ! Process
        if (aimag(x) == zero) then
            str = to_string(real(x, real64))
        else if (aimag(x) >= zero) then
            str = to_string(real(x, real64), fmt) // " + " // &
                to_string(aimag(x), fmt) // "i"
        else
            str = to_string(real(x, real64), fmt) // " - " // &
                to_string(abs(aimag(x)), fmt) // "i"
        end if
    end function

! ******************************************************************************
! C STRING CONVERSIONS
! ------------------------------------------------------------------------------
    !> @brief Copies a C string (null terminated) to a Fortran string.
    !!
    !! @param[in] cstr The null-terminated C string.
    !! @param[in] maxlength An optional input that specifies the maximum length
    !!  of cstr allowed.  Setting this parameter helps prevent issues in 
    !!  determining the length of the C string if the null terminator character
    !!  is forgotten.  The default is 10,000 characters.
    !! @return The Fortran copy.
    function cstr_2_fstr(cstr, maxlength) result(fstr)
        ! Arguments
        character(kind = c_char), intent(in) :: cstr(*)
        integer(int32), intent(in), optional :: maxlength
        character(len = :), allocatable :: fstr

        ! Local Variables
        integer :: i, n, maxchar

        ! Initialization
        if (present(maxlength)) then
            maxchar = maxlength
            if (maxchar < 1) maxchar = 10000
        else
            maxchar = 10000
        end if

        ! Determine the length of the C string
        n = 0
        do i = 1, maxchar
            if (cstr(i) == c_null_char) exit
            n = n + 1
        end do

        ! Process
         if (n == 0) return
         allocate(character(len = n) :: fstr)
         do i = 1, n
            fstr(i:i) = cstr(i)
         end do
    end function

! ------------------------------------------------------------------------------
    !> @brief Copies a Fortran string into a C string.
    !!
    !! @param[in] fstr The Fortran string to copy.
    !! @param[out] cstr The null-terminated C string.
    !! @param[in,out] csize On input, the size of the character buffer @p cstr.
    !!  On output, the actual number of characters (not including the null
    !!  character) writen to @p cstr.
    subroutine fstr_2_cstr(fstr, cstr, csize)
        ! Arguments
        character(len = *), intent(in) :: fstr
        character(kind = c_char), intent(out) :: cstr(*)
        integer, intent(inout) :: csize

        ! Local Variables
        integer :: i, n

        ! Process
        n = min(len(fstr), csize - 1) ! -1 allows room for the null char
        do i = 1, n
            cstr(i) = fstr(i:i)
        end do
        cstr(n+1) = c_null_char
        csize = n
    end subroutine
end module
