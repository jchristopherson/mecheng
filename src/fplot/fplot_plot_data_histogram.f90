! fplot_plot_data_histogram.f90

submodule (fplot_core) fplot_plot_data_histogram
contains
! ------------------------------------------------------------------------------
module function pdhist_get_data_cmd(this) result(x)
    ! Arguments
    class(plot_data_histogram), intent(in) :: this
    character(len = :), allocatable :: x

    ! Local Variables
    type(string_builder) :: str

    ! Local Variables
    real(real64), allocatable, dimension(:) :: bins

    ! Data Format Info (REF: http://www.gnuplotting.org/manpage-gnuplot-4-6/#Q1-1-147)
    ! - Column 1: Name to associate with each box
    ! - Columns 2-N+1: Define's # of items per box for each of the N boxes
    ! - Each row defines N boxes

    ! Bin the data
    bins = this%bin_data()

    ! Perhaps better to only display the data, and provide a bin function to
    ! the fplot_histogram object similar to: https://stackoverflow.com/questions/2471884/histogram-using-gnuplot

    ! Obtain any labels for each bin

    ! Write the data

    ! End
    x = str%to_string()
end function

! ------------------------------------------------------------------------------
! REF: 
! - https://stackoverflow.com/questions/2471884/histogram-using-gnuplot
!
module function pdhist_bin_data(this) result(bins)
    ! Arguments
    class(plot_data_histogram), intent(in) :: this
    real(real64), allocatable, dimension(:) :: bins

    ! Local Variables
    integer(int32) :: i, npts, nbins
    real(real64) :: maxX, minX, val, width

    ! Initialization
    npts = this%get_count()

    ! Quick Return
    if (npts == 0) then
        allocate(bins(0))
        return
    end if

    ! Determine the max and min values in the data set
    val = this%get_raw_data(1)
    maxX = val
    minX = val
    do i = 2, npts
        val = this%get_raw_data(i)
        if (val > maxX) maxX = val
        if (val < minX) minX = val
    end do

    ! Determine the number of bins based upon the requested bin width
    width = this%get_bin_width()
    nbins = max(ceiling((maxX - minX) / width, kind = int32), 1)

    ! Allocate & store the output
    
end function

! ------------------------------------------------------------------------------
pure module function pdhist_get_bin_width(this) result(x)
    class(plot_data_histogram), intent(in) :: this
    real(real64) :: x
    x = this%m_binWidth
end function

! ------------------------------------------------------------------------------
module subroutine pdhist_set_bin_width(this, x)
    class(plot_data_histogram), intent(inout) :: this
    real(real64), intent(in) :: x
    this%m_binWidth = x
end subroutine

! ------------------------------------------------------------------------------
module function pdhist_get_raw_data(this) result(x)
    ! Arguments
    class(plot_data_histogram), intent(in) :: this
    real(real64), allocatable, dimension(:) :: x

    ! Process
    if (allocated(this%m_rawData)) then
        x = this%m_rawData
    end if
end function

! ------------------------------------------------------------------------------
pure module function pdhist_get_data_count(this) result(x)
    class(plot_data_histogram), intent(in) :: this
    integer(int32) :: x
    if (allocated(this%m_rawData)) then
        x = size(this%m_rawData)
    else
        x = 0
    end if
end function

! ------------------------------------------------------------------------------
pure module function pdhist_get_data(this, index) result(x)
    class(plot_data_histogram), intent(in) :: this
    integer(int32), intent(in) :: index
    real(real64) :: x
    if (allocated(this%m_rawData)) then
        x = this%m_rawData(index)
    else
        x = 0.0d0
    end if
end function

! ------------------------------------------------------------------------------
end submodule
