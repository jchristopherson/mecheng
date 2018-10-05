! test_ff_network.f90

module test_ff_network
    use iso_fortran_env
    use neural_net_core
    use nonlin_least_squares
    implicit none

    type layer_obj
        class(layer), pointer :: item
    end type

    type layer_container
    private
        type(layer_obj), allocatable, dimension(:) :: m_layers
    contains
        final :: lc_final
        procedure, public :: get => lc_get
        procedure, public :: count => lc_count
        procedure, public :: initialize => lc_init
    end type

contains
    function test_network_1() result(rst)
        ! Parameters

        ! Local Variables
        logical :: rst
        integer(int32), dimension(4) :: indices
        type(feedforward_network) :: network
        type(sigmoid_neuron) :: neuronModel
        type(layer) :: layerModel
        integer(int32) :: i, j
        class(layer), pointer :: lptr
        class(neuron), pointer :: nptr

        ! Initialization
        rst = .true.
        
        ! Establish a 4 layer network with the following
        ! neuron count:
        ! - Layer 1: 3
        ! - Layer 2: 8
        ! - Layer 3: 6
        ! - Layer 4: 1
        indices = [3, 8, 6, 1]

        ! Initialize the network
        call network%initialize(indices, layerModel, neuronModel)

        ! Check to ensure the layer structure is correct
        if (network%get_count() /= 4) then
            rst = .false.
            print '(AI0A)', "TEST FAILED (TEST_NETWORK_1): " // &
                "Incorrect number of layers.  Expected 4, but found ", &
                network%get_count(), "."
            return
        end if

        ! Cycle over each layer and determine if it has the correct number
        ! of neurons, and that each neuron has the correct number of inputs.
        do i = 1, network%get_count()
            ! Get a pointer to the layer
            lptr => network%get_layer(i)

            ! Ensure the proper number of neurons
            if (lptr%get_count() /= indices(i)) then
                rst = .false.
                print '(AI0AI0AI0A)', "TEST FAILED (TEST_NETWORK_1): " // &
                    "Improper neuron count on layer ", i, &
                    ".  Expected to find ", indices(i), &
                    " neurons, but found ", lptr%get_count(), &
                    " instead."
                return
            end if

            ! Ensure each neuron accepts the appropriate number of inputs
            do j = 1, lptr%get_count()
                ! Get a pointer to the neuron
                nptr => lptr%get_neuron(j)

                ! Ensure the proper number of inputs.  The input layer should only
                ! accept 1 input per neuron.
                if (i == 1) then
                    if (nptr%get_input_count() /= 1) then
                        rst = .false.
                        print '(AI0A)', "TEST FAILED (TEST_NETWORK_1): " // &
                            "Improper neuron input count on the input layer.  " // &
                            "Expected a single input, but found ", &
                            nptr%get_input_count(), "."
                        return
                    end if
                else
                    if (nptr%get_input_count() /= indices(i-1)) then
                        rst = .false.
                        print '(AI0AI0AI0AI0A)', "TEST FAILED (TEST_NETWORK_1): " // &
                            "Improper neuron input count on layer ", i, " neuron ", j, &
                            ".  Expected to find ", indices(i-1), " inputs, but found ", &
                            nptr%get_input_count(), " instead."
                        return
                    end if
                end if
            end do
        end do
    end function

    function test_network_2() result(rst)
        ! Local Variables
        logical :: rst
        type(feedforward_network) :: network
        type(sigmoid_neuron) :: neuronModel
        type(layer) :: layerModel
        integer(int32) :: i, indices(4)
        real(real64), allocatable, dimension(:) :: ins, outs

        ! Initialization
        rst = .true.

        ! Establish a 4 layer network
        indices = [20, 50, 60, 10]

        ! Construct the network
        call network%initialize(indices, layerModel, neuronModel)

        ! Randomize the weighting and bias factors for each neuron
        call network%randomize()

        ! Evaluate the network
        allocate(ins(indices(1)))
        call random_number(ins)
        outs = network%evaluate(ins)

        ! Print the inputs and outputs
        print '(A)', "NETWORK INPUTS:"
        do i = 1, size(ins)
            print *, ins(i)
        end do
        print '(A)', "NETWORK OUTPUTS:"
        do i = 1, size(outs)
            print *, outs(i)
        end do
    end function

    function test_layer_container() result(rst)
        ! Local Variables
        logical :: rst
        type(layer_container) :: lyrs
        class(layer), pointer :: lyr, lyr1, lyr2
        integer(int32) :: i
        type(sigmoid_neuron) :: model

        ! Initialization
        rst = .true.
        call lyrs%initialize(4)

        ! Initialize a few layers
        do i = 1, lyrs%count()
            lyr => lyrs%get(i)
            call lyr%initialize(i, model)
        end do

        ! Ensure layer 1 and layer 2 are different
        lyr1 => lyrs%get(1)
        lyr2 => lyrs%get(2)
        if (lyr1%get_count() == lyr2%get_count()) then
            rst = .false.
            print '(A)', "TEST FAILED (TEST_LAYER_CONTAINER): No differentiation between different layers."
            return
        end if
    end function

    function test_network_count() result(rst)
        ! Local Variables
        logical :: rst
        type(feedforward_network) :: network
        type(sigmoid_neuron) :: neuronModel
        type(layer) :: layerModel
        integer(int32), dimension(3) :: lyrs
        integer(int32) :: nCoeff, nCoeffAns

        ! Initialization
        rst = .true.
        lyrs = [1, 3, 2]
        call network%initialize(lyrs, layerModel, neuronModel)

        ! Define how many coefficients are expected
        nCoeffAns = lyrs(1) * 2 + lyrs(2) * (lyrs(1) + 1) + lyrs(3) * (lyrs(2) + 1)

        ! Compute the number of coefficients
        nCoeff = network%get_coefficient_count()

        if (nCoeff /= nCoeffAns) then
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK_COUNT): Expected to find ", nCoeffAns, &
                " coefficients, but found ", nCoeff, " instead."
            return
        end if
    end function

    function test_network_coeff_transfer() result(rst)
        ! Local Variables
        logical :: rst
        type(feedforward_network) :: network
        type(sigmoid_neuron) :: neuronModel
        type(layer) :: layerModel
        integer(int32) :: lyrs(3), ncoeffs, i
        real(real64), allocatable, dimension(:) :: coeffs, extracted

        ! Parameters
        real(real64), parameter :: tol = 1.0d-12

        ! Initialization
        rst = .true.
        lyrs = [10, 100, 5]
        call network%initialize(lyrs, layerModel, neuronModel)

        ! Determine the number of coefficients, and define a coefficient array
        ncoeffs = network%get_coefficient_count()
        allocate(coeffs(ncoeffs))
        call random_number(coeffs)

        ! Assign the coefficients to the network
        call network%populate(coeffs)

        ! Extract the coefficients and compare to the input
        extracted = network%get_coefficients()
        if (size(extracted) /= ncoeffs) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK_COEFF_TRANSFER): Expected ", ncoeffs, &
                ", but found ", size(extracted), "."
            return
        end if
        do i = 1, ncoeffs
            if (abs(extracted(i) - coeffs(i)) > tol) then
                rst = .false.
                print '(AI0A)', "TEST FAILED (TEST_NETWORK_COEFF_TRANSFER): Coefficient mismatch at index ", i, "."
            end if
        end do
    end function

    subroutine ff_fit_example_1()
        ! Required Modules
        use fplot_core

        ! Local Variables
        type(feedforward_network) :: network
        type(layer) :: layerModel
        type(sigmoid_neuron) :: neuronModel
        type(least_squares_solver) :: solver
        integer(int32) :: lyrs(3), i
        real(real64), dimension(8,1) :: sensorOutput, calLoads, fittedData, fitErrors

        type(plot_2d) :: graph
        type(plot_data_2d) :: ds1
        class(plot_axis), pointer :: xAxis, yAxis

        ! Define the layer structure
        ! - # of inputs: 1
        ! - # of outputs: 1
        lyrs = [1, 20, 1]
        call network%initialize(lyrs, layerModel, neuronModel)

        ! Define the data
        calLoads = reshape([0.0d0, 600.7160569d0, 1200.631306d0, 1800.362883d0, 2400.497199d0, &
            3000.197944d0, 1800.510307d0, -0.210683425d0], [8, 1])
        sensorOutput = reshape([0.0d0, 0.302820934d0, 0.605285451d0, 0.907848517d0, &
            1.210417199d0, 1.512626867d0, 0.907987061d0, -5.86675d-05], [8, 1])

        ! Define solver parameters
        call solver%set_print_status(.true.)

        ! Attempt to fit the data
        call network%fit(solver, sensorOutput, calLoads)

        ! Apply the fitted network to the data
        do i = 1, size(sensorOutput, 1)
            fittedData(i,:) = network%evaluate(sensorOutput(i,:))
        end do

        ! Compute the errors in the fit
        fitErrors = fittedData - calLoads

        ! Create a plot of the data
        call graph%initialize()
        call graph%set_font_size(14)
        xAxis => graph%get_x_axis()
        yAxis => graph%get_y_axis()

        call xAxis%set_title("Applied [N]")
        call yAxis%set_title("Error [N]")

        call ds1%set_line_width(2.0)
        call ds1%define_data(calLoads(:,1), fitErrors(:,1))

        call graph%push(ds1)
        call graph%draw()
    end subroutine

! ------------------------------------------------------------------------------
    function lc_get(this, i) result(x)
        class(layer_container), intent(in) :: this
        integer(int32), intent(in) :: i
        class(layer), pointer :: x
        x => this%m_layers(i)%item
    end function

    pure function lc_count(this) result(x)
        class(layer_container), intent(in) :: this
        integer(int32) :: x
        x = size(this%m_layers)
    end function

    subroutine lc_init(this, n)
        class(layer_container), intent(inout) :: this
        integer(int32), intent(in) :: n
        type(layer) :: model
        integer(int32) :: i

        allocate(this%m_layers(n))
        do i = 1, n
            allocate(this%m_layers(i)%item, source = model)
        end do
    end subroutine

    subroutine lc_final(this)
        type(layer_container), intent(inout) :: this
        integer(int32) :: i, n
        if (allocated(this%m_layers)) then
            n = size(this%m_layers)
        else
            n = 0
        end if
        do i = 1, n
            if (associated(this%m_layers(i)%item)) deallocate(this%m_layers(i)%item)
        end do
    end subroutine

end module