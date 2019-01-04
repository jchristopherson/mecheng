! test_neural_network.f90

program test
    use iso_fortran_env
    use neural_networks
    use test_nn_gradient
    implicit none

    ! Local Variables
    logical :: check, overall

    ! Initialization
    overall = .true.

    ! Tests
    check = test_neural_net_basics()
    if (.not.check) overall = .false.

    check = test_network_output()
    if (.not.check) overall = .false.

    check = test_network_output_2()
    if (.not.check) overall = .false.

    check = test_gradient_vector()
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
        integer(int32), parameter :: hidden_layers = 4
        integer(int32), parameter :: hidden = 8
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




    function sigmoid(w, x, b) result(z)
        real(real64), intent(in) :: w, x, b
        real(real64) :: z
        z = 1.0d0 / (1.0d0 + exp(-w * x - b))
    end function




    function test_network_output() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        integer(int32), parameter :: ntests = 5
        real(real64), parameter :: tol = 1.0d-6

        ! Local Variables
        type(neural_network) :: network
        integer(int32) :: i
        real(real64) :: temp, weights(2), bias(2), &
            inputs(ntests), answers(ntests), output(ntests, 1), ins(ntests, 1)

        ! Initialization
        rst = .true.
        weights = [0.5d0, 2.0d0]
        bias = [-1.0d0, -5.0d0]
        call random_number(inputs)

        ! Construct a network of 1 hidden layer with one neuron, and a single
        ! output.
        call network%initialize(1, 1, 1, 1)

        ! Ensure the correct number of bias and weighting terms
        if (network%get_weight_count() /= size(weights)) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK_OUTPUT): Expected ", &
                size(weights), " weighting factors, but found ", &
                network%get_weight_count(), "."
            return
        end if
        if (network%get_bias_count() /= size(bias)) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK_OUTPUT): Expected ", &
                size(bias), " bias terms, but found ", &
                network%get_bias_count(), "."
            return
        end if

        ! Set the bias and weighting terms
        call network%set_weights(weights)
        call network%set_bias(bias)

        ! Construct the result for the given input
        do i = 1, ntests
            ! Evaluate the first hidden layer
            temp = sigmoid(weights(1), inputs(i), bias(1))

            ! Evaluate the output layer
            answers(i) = sigmoid(weights(2), temp, bias(2))
        end do

        ! Now, use the neural_network object to perform the evaluation
        ins(:,1) = inputs
        do i = 1, ntests
            output(i,:) = network%run(ins(i,:))
        end do

        ! Compare results
        do i = 1, ntests
            if (abs(output(i,1) - answers(i)) > tol) then
                rst = .false.
                print '(AF9.7AF9.7A)', &
                    "TEST FAILED (TEST_NETWORK_OUTPUT): Expected ", &
                    answers(i), ", but found ", output(i,1), "."
            end if
        end do
    end function




    function test_network_output_2() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        integer(int32), parameter :: ntests = 5
        real(real64), parameter :: tol = 1.0d-6

        ! Local Variables
        type(neural_network) :: network
        integer(int32) :: i
        real(real64) :: in1, in2, temp1, temp2, weights(6), bias(3), &
            inputs(ntests, 2), outputs(ntests, 1), answers(ntests)

        ! Initialization
        rst = .true.
        call random_number(weights)
        call random_number(bias)
        call random_number(inputs)

        ! Construct a network of 2 inputs, 1 hidden layer with 2 neurons, and
        ! a single output.
        call network%initialize(2, 1, 2, 1)

        ! Ensure the correct number of bias and weighting terms
        if (network%get_weight_count() /= size(weights)) then
            rst = .false.
            print '(AI0AI0A)', &
                "TEST FAILED (TEST_NETWORK_OUTPUT_2): Expected ", &
                size(weights), " weighting factors, but found ", &
                network%get_weight_count(), "."
            return
        end if
        if (network%get_bias_count() /= size(bias)) then
            rst = .false.
            print '(AI0AI0A)', &
                "TEST FAILED (TEST_NETWORK_OUTPUT_2): Expected ", &
                size(bias), " bias terms, but found ", &
                network%get_bias_count(), "."
            return
        end if

        ! Construct the weighting and bias vectors
        call network%set_weights(weights)
        call network%set_bias(bias)

        ! Define the solution
        do i = 1, ntests
            ! Define the inputs to the two hidden neurons
            in1 = weights(1) * inputs(i,1) + weights(3) * inputs(i,2) + bias(1)
            in2 = weights(2) * inputs(i,1) + weights(4) * inputs(i,2) + bias(2)

            ! Evaluate the hidden neurons
            temp1 = 1.0d0 / (1.0d0 + exp(-in1))
            temp2 = 1.0d0 / (1.0d0 + exp(-in2))

            ! Compute the results of the output layer
            in1 = weights(5) * temp1 + weights(6) * temp2 + bias(3)
            answers(i) = 1.0d0 / (1.0d0 + exp(-in1))

            ! Print the output
            print '(AI0)', "TEST ", i
            print '(AF9.7)', achar(9) // "HIDDEN NEURON 1: ", temp1
            print '(AF9.7)', achar(9) // "HIDDEN NEURON 2: ", temp2
            print '(AF9.7)', achar(9) // "NETWORK OUTPUT: ", answers(i)
            print *, ""
        end do

        ! Evaluate the network
        do i = 1, ntests
            outputs(i,:) = network%run(inputs(i,:))
        end do

        ! Compare the results
        do i = 1, ntests
            if (abs(outputs(i,1) - answers(i)) > tol) then
                rst = .false.
                print '(AF9.7AF9.7A)', &
                    "TEST FAILED (TEST_NETWORK_OUTPUT_2): Expected ", &
                    answers(i), ", but found ", outputs(i,1), "."
            end if
        end do
    end function




    ! Train a network to fit a data set
    subroutine train_network_test()
        ! Required Modules
        use fplot_core
        use curvefit_regression
        use nonlin_least_squares

        ! Local Variables
        integer(int32), parameter :: npts = 34
        integer(int32), parameter :: nchan = 2
        type(neural_network) :: network
        real(real64) :: loads(npts, nchan), bridge(npts, nchan), &
            axialFS, torqueFS, nnOutput(npts, nchan), llsmtx(nchan, nchan), &
            llsOutput(npts, nchan), sloads(npts, nchan), sbridge(npts, nchan)
        integer(int32) :: i
        type(plot_2d) :: plt
        type(plot_data_2d) :: d1nn, d2nn, d1lls, d2lls
        class(plot_axis), pointer :: xAxis, yAxis
        class(legend), pointer :: lgnd
        type(least_squares_solver) :: solver
        procedure(cost_function), pointer :: cf
        procedure(cost_function_derivative), pointer :: df

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

        ! Scale the training data by full scale values such that no value is
        ! in excess of one.  Also shift the data such that all target values
        ! are positive.
        sbridge = bridge
        sloads(:,1) = loads(:,1) / (2 * axialFS)
        sloads(:,2) = loads(:,2) / (2 * torqueFS)
        sloads = sloads + 0.5d0

        ! Set up and train the network
        call network%initialize(2, 2, 2, 2)
        cf => quadratic_cost_function
        df => diff_quadratic_cost_function
        call solver%set_print_status(.true.)
        call network%train(solver, cf, df, sbridge, sloads)

        ! Apply the trained network to the data set
        do i = 1, npts
            ! Evaluate the network
            nnOutput(i,:) = network%run(bridge(i,:))

            ! Convert to a percentage of full scale value
            nnOutput(i,1) = 1.0d2 * (2.0d0 * nnOutput(i,1) - loads(i,1) / axialFS)
            nnOutput(i,2) = 1.0d2 * (2.0d0 * nnOutput(i,2) - loads(i,2) / torqueFS)
        end do

        ! Plot the results
        call plt%initialize()
        call plt%set_font_size(14)
        xAxis => plt%get_x_axis()
        yAxis => plt%get_y_axis()
        lgnd => plt%get_legend()

        call xAxis%set_title("Index")
        call yAxis%set_title("Measured Load [% FS]")
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

        call plt%push(d1nn)
        call plt%push(d2nn)
        call plt%push(d1lls)
        call plt%push(d2lls)
        call plt%draw()
    end subroutine



end program