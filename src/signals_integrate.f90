! signals_integrate.f90

submodule (signals) signals_integrate
contains
! ------------------------------------------------------------------------------
    module function integrate_a(x, y, c) result(f)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x, y
        real(real64), intent(in), optional :: c
        real(real64), dimension(size(y)) :: f

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = min(size(x), size(y))
        if (present(c)) then
            f(1) = c
        else
            f(1) = 0.0d0
        end if
        do i = 2, n
            f(i) = f(i-1) + (x(i) - x(i-1)) * y(i)
        end do
    end function

! ------------------------------------------------------------------------------
    module function integrate_b(dx, y, c) result(f)
        ! Arguments
        real(real64), intent(in) :: dx
        real(real64), intent(in), dimension(:) :: y
        real(real64), intent(in), optional :: c
        real(real64), dimension(size(y)) :: f

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        n = size(y)
        if (present(c)) then
            f(1) = c
        else
            f(1) = 0.0d0
        end if
        do i = 2, n
            f(i) = f(i-1) + dx * y(i)
        end do
    end function

end submodule