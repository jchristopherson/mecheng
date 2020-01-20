! calibrate_example.f90

program example
    use iso_fortran_env
    use calibrate
    use fplot_core
    use strings
    implicit none

    ! Local Variables
    type(calibration) :: cal
    integer(int32) :: i
    real(real64) :: xp(21), yp(21), yc(21), err(21), c(4)
    type(plot_2d) :: plt
    type(plot_data_2d) :: ds1, ds2
    class(plot_axis), pointer :: xAxis, yAxis
    type(plot_label) :: lbl, lblmax, lblmin

    ! Data to fit
    xp = [0.0d0, 0.1d0, 0.2d0, 0.3d0, 0.4d0, 0.5d0, 0.6d0, 0.7d0, 0.8d0, &
        0.9d0, 1.0d0, 1.1d0, 1.2d0, 1.3d0, 1.4d0, 1.5d0, 1.6d0, 1.7d0, &
        1.8d0, 1.9d0, 2.0d0]
    yp = [1.216737514d0, 1.250032542d0, 1.305579195d0, 1.040182335d0, &
        1.751867738d0, 1.109716707d0, 2.018141531d0, 1.992418729d0, &
        1.807916923d0, 2.078806005d0, 2.698801324d0, 2.644662712d0, &
        3.412756702d0, 4.406137221d0, 4.567156645d0, 4.999550779d0, &
        5.652854194d0, 6.784320119d0, 8.307936836d0, 8.395126494d0, &
        10.30252404d0]

    ! Populate the calibration instance
    call cal%initialize()
    do i = 1, size(xp)
        call cal%append([yp(i), xp(i)])
    end do

    ! Fit the calibration polynomial - 3rd order
    call cal%fit_polynomial(3)

    ! Evaluate the calibration at each data point
    yc = cal%evaluate_at_cal_points()

    ! Evaluate the calibration errors
    err = cal%compute_errors()

    ! Get the coefficients
    c = cal%get_polynomial_coefficients()

    ! Plot the polynomial
    call plt%initialize()
    call plt%set_font_size(14)
    call plt%set_show_gridlines(.false.)
    
    xAxis => plt%get_x_axis()
    call xAxis%set_title("x")

    yAxis => plt%get_y_axis()
    call yAxis%set_title("f(x)")

    call ds1%define_data(xp, yp)
    call ds1%set_draw_line(.false.)
    call ds1%set_draw_markers(.true.)
    call ds1%set_line_width(2.0)
    call plt%push(ds1)

    call ds2%define_data(xp, yc)
    call ds2%set_line_width(2.0)
    call plt%push(ds2)

    call lbl%set_text("f(x) = " // &
        to_string(c(4), "F6.4") // " x^3 - " // &
        to_string(abs(c(3)), "F6.4") // " x^2 + " // &
        to_string(c(2), "F6.4") // " x + " // &
        to_string(c(1), "F6.4"))
    call lbl%set_position([0.1, 9.0, 0.0])
    call plt%push_label(lbl)

    call lblmax%set_text("Max Error: " // to_string(maxval(err), "F6.4"))
    call lblmax%set_position([0.1, 8.25, 0.0])
    call plt%push_label(lblmax)

    call lblmin%set_text("Min Error: " // to_string(minval(err), "F7.4"))
    call lblmin%set_position([0.1, 7.5, 0.0])
    call plt%push_label(lblmin)

    call plt%draw()
end program