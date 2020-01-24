! c_polynomial.f90

!> @brief A module containing 
module c_polynomial
    use iso_c_binding
    use nonlin_polynomials
    use linalg_core
    implicit none
contains
    !> @brief Fits a polynomial of the specified order to the supplied data.
    !!
    !! @param[in] order The desired order of the polynomial.  This must be at least
    !!  one less than the number of data points (@p npts).
    !! @param[in] thruzero Set to true to force the polynomial thru zero; else, false.
    !! @param[in] npts The number of data points.
    !! @param[in] x An @p npts element array containing the x-coordinate data.
    !! @param[in] y An @p npts element array containing the y-coordinate data.
    !! @param[out] c An @p order + 1 element array where the polynomial coefficients
    !!  will be written.  If all zero, the polynomial could not be constructed because
    !!  the requested order was not appropriate for the data set.  If successful, the
    !!  coefficients will be returned in ascending order.
    subroutine fit_poly(order, thruzero, npts, x, y, c) bind(C, name = "fit_poly")
        ! Arguments
        integer(c_int), intent(in), value :: order, npts
        logical(c_bool), intent(in), value :: thruzero
        real(c_double), intent(in) :: x(npts), y(npts)
        real(c_double), intent(out) :: c(order + 1)

        ! Local Variables
        type(polynomial) :: poly
        real(c_double) :: yc(npts)
        integer(c_int) :: i

        ! Set the output to all zeros
        c = 0.0d0

        ! Check the order
        if (npts < 2) return
        if (order < 1 .or. order >= npts) return

        ! Create a copy of y to avoid overwriting the input
        yc = y

        ! Fit the polynomial
        if (thruzero) then
            call poly%fit_thru_zero(x, yc, order)
        else
            call poly%fit(x, yc, order)
        end if

        ! Populate the coefficient array
        do i = 1, order + 1
            c(i) = poly%get(i)
        end do
    end subroutine

    !> @brief Evalautes a polynomial at the requested points.
    !!
    !! @param[in] order The order of the polynomial.
    !! @param[in] c An @p order + 1 element array containing the polynomial
    !!  coefficients in ascending order.
    !! @param[in] npts The number of points to evaluate.
    !! @param[in] x An @p npts element array containing the values at which
    !!  to evaluate the polynomial.
    !! @param[out] y An @p npts element array where the output of the polynomial
    !!  at the points in @p x will be written.
    subroutine eval_poly(order, c, npts, x, y) bind(C, name = "eval_poly")
        ! Arguments
        integer(c_int), intent(in), value :: order, npts
        real(c_double), intent(in) :: c(order + 1), x(npts)
        real(c_double), intent(out) :: y(npts)

        ! Local Variables
        integer(c_int) :: i
        type(polynomial) :: p

        ! Ensure order and npts are appropriate

        ! Populate the polynomial object
        call p%initialize(order)
        do i = 1, order + 1
            call p%set(i, c(i))
        end do

        ! Evaluate the polynomial
        y = p%evaluate(x)
    end subroutine

    !> @brief Evalautes a polynomial at the requested points.
    !!
    !! @param[in] order The order of the polynomial.
    !! @param[in] c An @p order + 1 element array containing the polynomial
    !!  coefficients in ascending order.
    !! @param[in] npts The number of points to evaluate.
    !! @param[in] x An @p npts element array containing the values at which
    !!  to evaluate the polynomial.
    !! @param[out] y An @p npts element array where the output of the polynomial
    !!  at the points in @p x will be written.
    subroutine eval_poly_cmplx(order, c, npts, x, y) bind(C, name = "eval_poly_cmplx")
        ! Arguments
        integer(c_int), intent(in), value :: order, npts
        real(c_double), intent(in) :: c(order + 1)
        complex(c_double), intent(in) :: x(npts)
        complex(c_double), intent(out) :: y(npts)

        ! Local Variables
        integer(c_int) :: i
        type(polynomial) :: p

        ! Ensure order and npts are appropriate

        ! Populate the polynomial object
        call p%initialize(order)
        do i = 1, order + 1
            call p%set(i, c(i))
        end do

        ! Evaluate the polynomial
        y = p%evaluate(x)
    end subroutine

    !> @brief Computes the roots of a polynomial.
    !!
    !! @param[in] order The order of the polynomial.
    !! @param[in] c An @p order + 1 element array containing the polynomial
    !!  coefficients in ascending order.
    !! @param[out] rts An @p order element array where the roots of the
    !!  polynomial will be written.
    subroutine poly_roots(order, c, rts) bind(C, name = "poly_roots")
        ! Arguments
        integer(c_int), intent(in), value :: order
        real(c_double), intent(in) :: c(order + 1)
        complex(c_double), intent(out) :: rts(order)

        ! Local Variables
        integer(c_int) :: i
        type(polynomial) :: p
        real(c_double), allocatable, dimension(:,:) :: cmtx

        ! Populate the polynomial object
        call p%initialize(order)
        do i = 1, order + 1
            call p%set(i, c(i))
        end do

        ! Compute the companion matrix of the polynomial
        cmtx = p%companion_mtx()

        ! Compute the eigenvalues of the companion matrix.  The
        ! eigenvalues are the roots of the polynomial.
        call eigen(cmtx, rts)
    end subroutine
end module
