! test_neural_network.f90

program test
    use iso_fortran_env
    use neural_networks
    implicit none

    ! Local Variables
    logical :: check, overall

    ! Initialization
    overall = .true.

    ! Tests
    check = test_neural_net_basics()
    if (.not.check) overall = .false.

    call train_network_test()


    ! Check
    if (overall) then
        print '(A)', "NEURAL_NETWORK TESTS PASSED."
    else
        print '(A)', "NEURAL_NETWORK TESTS FAILED."
    end if

contains
    function test_neural_net_basics() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        integer(int32), parameter :: inputs = 10
        integer(int32), parameter :: hidden_layers = 8
        integer(int32), parameter :: hidden = 10
        integer(int32), parameter :: outputs = 4

        ! Local Variables
        type(neural_network) :: network
        real(real64) :: invals(inputs)
        real(real64), allocatable, dimension(:) :: outvals

        ! Initialize
        rst = .true.
        call network%initialize(inputs, hidden_layers, hidden, outputs)

        ! Ensure the proper network size parameters
        if (network%get_input_count() /= inputs) then
            rst = .false.
            print '(AI0AI0A)', "ERROR: Expected ", inputs, &
                " inputs, but found ", network%get_input_count(), "."
            return
        end if

        ! Run the network
        call random_number(invals)
        outvals = network%run(invals)
        if (size(outvals) /= outputs) then
            rst = .false.
            print '(AI0AI0A)', "ERROR: Expected ", outputs, &
                " outputs, but found ", size(outvals), "."
            return
        end if
    end function

    ! Train a network to fit a data set
    subroutine train_network_test()
        ! Required Modules
        use fplot_core
        use curvefit_regression

        ! Local Variables
        integer(int32), parameter :: npts = 34
        integer(int32), parameter :: nchan = 2
        integer(int32), parameter :: niter = 500
        real(real64), parameter :: learning_rate = 1.0d-1
        type(neural_network) :: network
        real(real64) :: loads(npts, nchan), bridge(npts, nchan), &
            delta(nchan), residuals(niter), ev, axialFS, torqueFS, &
            nnOutput(npts, nchan), llsmtx(nchan, nchan), &
            llsOutput(npts, nchan), sloads(npts, nchan), sbridge(npts, nchan), &
            temp(nchan)
        integer(int32) :: i, j, k, indices(npts)
        type(multiplot) :: plt
        type(plot_2d) :: plt1, plt2
        type(plot_data_2d) :: d1nn, d2nn, derr, d1lls, d2lls
        class(plot_axis), pointer :: xAxis1, yAxis1, xAxis2, yAxis2
        class(legend), pointer :: lgnd

        ! Populate the applied loads matrix
        loads = reshape([0.0d0, 3000.0d0, 6000.0d0, 7500.0d0, 9000.0d0, 12000.0d0, &
            15000.0d0, 7500.0d0, 0.0d0, 0.0d0, -3000.0d0, -6000.0d0, -7500.0d0, &
            -9000.0d0, -12000.0d0, -15000.0d0, -7500.0d0, 0.0d0, 0.0d0, 0.0d0, &
            0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
            0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
            0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0, &
            0.0d0, 0.0d0, 0.0d0, 67.7908728d0, 135.5817456d0, 203.3726184d0, &
            271.1634912d0, 338.954364d0, 203.3726184d0, 0.0d0, 0.0d0, &
            -67.7908728d0, -135.5817456d0, -203.3726184d0, -271.1634912d0, &
            -338.954364d0, -203.3726184d0, 0.0d0], [npts, nchan])

        ! Populate the bridge output (measured) matrix
        bridge = reshape([0.0d0, 0.38905d0, 0.77816d0, 0.97269d0, 1.16714d0, &
            1.556d0, 1.94484d0, 0.9726d0, -0.00001d0, 0.0d0, -0.388886d0, &
            -0.77775d0, -0.97215d0, -1.16654d0, -1.55533d0, -1.9441d0, -0.97171d0, &
            0.00004d0, 0.0d0, -0.00044d0, -0.0013d0, -0.0024d0, -0.00382d0, &
            -0.00528d0, -0.00257d0, 0.00015d0, 0.0d0, 0.00144d0, 0.00306d0, &
            0.00446d0, 0.00567d0, 0.00688d0, 0.00451d0, -0.00002d0, 0.0d0, &
            0.00122d0, 0.00259d0, 0.0029d0, 0.00314d0, 0.00338d0, 0.00356d0, &
            0.00477d0, -0.00001d0, 0.0d0, 0.00021d0, 0.00051d0, 0.00069d0, &
            0.00088d0, 0.0013d0, 0.00175d0, 0.00058d0, 0.00003d0, 0.0d0, &
            0.27156d0, 0.54329d0, 0.81507d0, 1.08682d0, 1.35881d0, 0.81553d0, &
            0.0001d0, 0.0d0, -0.27145d0, -0.54312d0, -0.81493d0, -1.0868d0, &
            -1.35879d0, -0.81548d0, 0.0d0], [npts, nchan])

        ! Determine the full scale values
        axialFS = maxval(loads(:,1))
        torqueFS = maxval(loads(:,2))

        ! For a comparison, compute a linear least-squares model
        llsmtx = linear_least_squares(transpose(bridge), transpose(loads))
        llsOutput = matmul(bridge, transpose(llsmtx)) - loads
        llsOutput(:,1) = 1.0d2 * llsOutput(:,1) / axialFS
        llsOutput(:,2) = 1.0d2 * llsOutput(:,2) / torqueFS

        ! Shuffle the training data set
        do i = 1, npts
            indices(i) = i
        end do
        call shuffle(indices)
        do i = 1, npts
            k = indices(i)
            sbridge(k,:) = bridge(i,:)
            sbridge(i,:) = bridge(k,:)

            sloads(k,:) = loads(i,:)
            sloads(i,:) = loads(k,:)
        end do

        ! Set up and train the network
        call network%initialize(2, 1, 4, 2)
        do i = 1, niter
            ev = 0.0d0
            do j = 1, npts
                call network%training_step(sbridge(j,:), sloads(j,:), &
                    learning_rate, delta)
                ev = max(ev, norm2(delta))
            end do
            residuals(i) = ev
        end do

        ! Apply the trained network to the data set
        do i = 1, npts
            nnOutput(i,:) = network%run(bridge(i,:)) - loads(i,:)

            ! Convert to a percentage of full scale value
            nnOutput(i,1) = 1.0d2 * nnOutput(i,1) / axialFS
            nnOutput(i,2) = 1.0d2 * nnOutput(i,2) / torqueFS
        end do

        ! Plot the results
        call plt%initialize(2, 1)
        call plt1%initialize()
        call plt2%initialize()
        call plt%set_font_size(14)
        xAxis1 => plt1%get_x_axis()
        yAxis1 => plt1%get_y_axis()
        xAxis2 => plt2%get_x_axis()
        yAxis2 => plt2%get_y_axis()
        lgnd => plt1%get_legend()

        call xAxis1%set_title("Index")
        call yAxis1%set_title("Measured Load [% FS]")
        call xAxis2%set_title("Iteration")
        call yAxis2%set_title("Residual")
        call lgnd%set_is_visible(.true.)
        call lgnd%set_horizontal_position(LEGEND_RIGHT)
        call lgnd%set_vertical_position(LEGEND_CENTER)
        call lgnd%set_draw_inside_axes(.false.)

        call d1nn%set_name("Axial - NN")
        call d1nn%define_data(nnOutput(:,1))

        call d2nn%set_name("Torsional - NN")
        call d2nn%define_data(nnOutput(:,2))

        call d1lls%set_name("Axial - LLS")
        call d1lls%define_data(llsOutput(:,1))

        call d2lls%set_name("Torsional - LLS")
        call d2lls%define_data(llsOutput(:,2))

        call derr%define_data(residuals)

        call plt1%push(d1nn)
        call plt1%push(d2nn)
        call plt1%push(d1lls)
        call plt1%push(d2lls)
        call plt2%push(derr)
        call plt%set(1, 1, plt1)
        call plt%set(2, 1, plt2)
        call plt%draw()
    end subroutine
end program