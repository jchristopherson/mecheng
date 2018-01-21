! signals_windows.f90

submodule (signals) signals_windows
contains
! ------------------------------------------------------------------------------
    pure module function bartlett_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 1.0d0 - abs(2.0d0 * bin / (winsize - 1.0d0) - 1.0d0)
    end function

! ------------------------------------------------------------------------------
    pure module function welch_window(bin, winsize) result(x)
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 1.0d0 - (2.0d0 * bin / (winsize - 1.0d0) - 1.0d0)**2
    end function

! ------------------------------------------------------------------------------
    pure module function hann_window(bin, winsize) result(x)
        use constants
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 0.5d0 * (1.0d0 - cos(2.0d0 * pi * bin / winsize))
    end function

! ------------------------------------------------------------------------------
    pure module function hamming_window(bin, winsize) result(x)
        use constants
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 0.54d0 - 0.46d0 * cos(2.0d0 * pi * bin / winsize)
    end function

! ------------------------------------------------------------------------------
    pure module function blackman_window(bin, winsize) result(x)
        use constants
        integer(int32), intent(in) :: bin, winsize
        real(real64) :: x
        x = 0.42d0 - 0.5d0 * cos(2.0d0 * pi * bin / (winsize - 1.0d0) + &
            8.0d-2 * cos(4.0d0 * pi * bin / (winsize - 1.0d0)))
    end function
        
end submodule