! vibrations_fit.f90

submodule (vibrations) vibrations_fit
contains
! ------------------------------------------------------------------------------
    pure function partial_fraction_basis_function(s, a, p) result(phi)
        ! Arguments
        complex(real64), intent(in) :: s
        complex(real64), intent(in), dimension(:) :: a
        integer(int32), intent(in) :: p
        complex(real64) :: phi

        ! Process
        phi = 1.0d0 / (s + a(p))
    end function

! ------------------------------------------------------------------------------
    pure function frequency_basis_function(s, a, p) result(phi)
        ! Arguments
        complex(real64), intent(in) :: s
        complex(real64), intent(in), dimension(:) :: a
        integer(int32), intent(in) :: p
        complex(real64) :: phi

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)
        complex(real64), parameter :: one = (1.0d0, 0.0d0)
        complex(real64), parameter :: ten = (1.0d1, 0.0d0)
        complex(real64), parameter :: p1 = (1.0d-1, 0.0d0)

        ! Local Variables
        integer(int32) :: i, ep
        complex(real64) :: temp

        ! Compute the product of s / (s + a(i)) from i = 1 to p - 1
        if (p == 1) then
            phi = zero
        else if (p == 2) then
            phi = s / (s + a(1))
        else
            temp = one
            ep = 0
            do i = 1, p - 1
                temp = temp * (s / (s + a(i)))
                if (temp == zero) then
                    phi = zero
                    return
                end if
                
                do while (abs(temp) < 1.0d0)
                    temp = ten * temp
                    ep = ep - 1
                end do

                do while (abs(temp) > 1.0d1)
                    temp = p1 * temp
                    ep = ep + 1
                end do
            end do
            phi = temp * ten**ep
        end if

        ! Compute the full basis function
        phi = phi * abs(a(p)) / (s + a(p))
    end function

! ------------------------------------------------------------------------------
    pure function evaluate_polynomial(phi, coeff) result(r)
        ! Arguments
        complex(real64), intent(in), dimension(:) :: phi! N element array of 
                                                        ! basis function values
        real(real64), intent(in), dimension(:) :: coeff ! N-element coefficient array
                                                        ! (the highest power coefficient is one)
        complex(real64) :: r                            ! value of the polynomial

        ! Parameters
        complex(real64), parameter :: zero = (0.0d0, 0.0d0)

        ! Local Variables
        integer(int32) :: i, n, nz

        ! Initialization
        n = size(coeff) ! If n < size(z), the order of the polynomial in
                        ! the denominator (source of phi) is greater than
                        ! that of the numerator.  This is OK as this is
                        ! equivalent to stating that the numerator 
                        ! polynomial is of equal order, but contains
                        ! zero-valued coefficients for the higher order
                        ! terms.
        nz = size(phi)  ! For the non-standard case where the order of
                        ! the numerator is greater than that of the
                        ! denominator.
        n = min(n, nz)

        ! Process
        r = zero
        do i = 1, n
            ! Process
            r = r + coeff(i) * phi(i)
        end do
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
