! fplot_plot_data_bar.f90

submodule (fplot_core) fplot_plot_data_bar
contains
! ------------------------------------------------------------------------------
pure module function pdb_get_count(this) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32) :: x
    if (allocated(this%m_barData)) then
        x = size(this%m_barData, 1)
    else
        x = 0
    end if
end function

! ------------------------------------------------------------------------------
pure module function pdb_get_data(this, index, col) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32), intent(in) :: index, col
    real(real64) :: x
    if (allocated(this%m_barData)) then
        x = this%m_barData(index, col)
    else
        x = 0.0d0
    end if
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_data(this, index, col, x)
    class(plot_data_bar), intent(inout) :: this
    integer(int32), intent(in) :: index, col
    real(real64), intent(in) :: x
    if (allocated(this%m_barData)) then
        this%m_barData(index, col) = x
    end if
end subroutine

! ------------------------------------------------------------------------------
pure module function pdb_get_data_set(this, col) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32), intent(in) :: col
    real(real64), allocatable, dimension(:) :: x
    if (allocated(this%m_barData)) then
        x = this%m_barData(:,col)
    else
        allocate(x(0))
    end if
end function

! ------------------------------------------------------------------------------
pure module function pdb_get_label(this, index) result(x)
    class(plot_data_bar), intent(in) :: this
    integer(int32), intent(in) :: index
    character(len = :), allocatable :: x
    if (allocated(this%m_axisLabels)) then
        x = this%m_axisLabels(i)%str
    else
        x = ""
    end if
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_label(this, index, txt)
    class(plot_data_bar), intent(inout) :: this
    integer(int32) :: index
    character(len = *), intent(in) :: txt
    if (allocated(this%m_axisLabels)) then
        this%m_axisLabels(index)%str = txt
    end if
end subroutine

! ------------------------------------------------------------------------------
pure module function pdb_get_use_labels(this) result(x)
    class(plot_data_bar), intent(in) :: this
    logical :: x
    x = this%m_useAxisLabels
end function

! ------------------------------------------------------------------------------
module subroutine pdb_set_use_labels(this, x)
    class(plot_data_bar), intent(inout) :: this
    logical, intent(in) :: x
    this%m_useAxisLabels = x
end subroutine

! ------------------------------------------------------------------------------
module function pdb_get_cmd(this) result(x)
    ! Arguments
    class(plot_data_bar), intent(in) :: this
    character(len = :), allocatable :: x

    ! Local Variables
    type(string_builder) :: str
    integer(int32) :: n, ncols
    type(color) :: clr

    ! Initialization
    call str%initialize()

    ! Tic Labels
    if (this%get_use_labels() .and. allocated(this%m_barData) .and. &
            allocated(this%m_axisLabels)) then
        ncols = size(this%m_barData, 2)
        if (ncols == 1) then
            call str%append(" using 2:xtic(1) ")
        else
            call str%append(" using 2:")
            call str%append(to_string(ncols))
            call str%append(":xtic(1) ")
        end if
    end if

    ! Title
    n = len_trim(this%get_name())
    if (n > 0) then
        call str%append(' with boxes title "')
        call str%append(this%get_name())
        call str%append('"')
    else
        call str%append(' with boxes notitle')
    end if

    ! Color
    clr = this%get_line_color()
    call str%append(' lc rgb "#')
    call str%append(clr%to_hex_string())
    call str%append('"')

    ! Define the axes structure
    call str%append(" ")
    call str%append(this%get_axes_string())

    ! End
    x = str%to_string()
end function

! ------------------------------------------------------------------------------
module function pdb_get_data_cmd(this) result(x)
    ! Arguments
    class(plot_data_bar), intent(in) :: this
    character(len = :), allocatable :: x
end function

! ------------------------------------------------------------------------------
module function pdb_get_axes_cmd(this) result(x)
    ! Arguments
    class(plot_data_bar), intent(in) :: this
    character(len = :), allocatable :: x
end function

! ------------------------------------------------------------------------------
end submodule
