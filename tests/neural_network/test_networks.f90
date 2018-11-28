! test_networks.f90

module test_networks
    use iso_fortran_env
    use neural_network_core
    implicit none
contains
    function test_network() result(rst)
        ! Parameters
        integer(int32), parameter :: nlayers = 5

        ! Local Variables
        logical :: rst
        type(layer) :: model
        type(network) :: net
        type(cross_entropy_helper) :: hlpr
        integer(int32) :: lyrs(nlayers), nneurons, i
        real(real64), allocatable, dimension(:) :: xIn, xOut
        real(real64), allocatable, dimension(:,:) :: back
        procedure(cost_function), pointer :: fcn

        ! Initialization
        rst = .true.
        lyrs = [10, 20, 100, 40, 3]
        allocate(xIn(lyrs(1)))
        call random_number(xIn)
        call net%initialize(lyrs, model)

        ! Ensure the proper number of layers were created
        if (net%get_count() /= nlayers) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK): Expected to find ", &
                nlayers, " layers, but found ", net%get_count(), " instead."
            return
        end if

        ! Check the input and output count
        if (net%get_input_count() /= lyrs(1)) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK): Expected to find ", &
                lyrs(1), " inputs, but found ", net%get_input_count(), " instead."
            return
        end if

        if (net%get_output_count() /= lyrs(nlayers)) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK): Expected to find ", &
                lyrs(nlayers), " outputs, but found ", net%get_output_count(), &
                " instead."
            return
        end if

        ! Ensure we've got the proper number of neurons in the network
        nneurons = sum(lyrs)
        if (net%get_neuron_count() /= nneurons) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK): Expected to find ", &
                nneurons, " neurons, but found ", net%get_neuron_count(), " instead."
            return
        end if

        ! Initialize the weighting and bias factors
        call net%randomize()

        ! Evaluate the network
        xOut = net%evaluate(xIn)

        if (size(xOut) /= lyrs(nlayers)) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK): Expected ", lyrs(nlayers), &
                " elements in the output array, but found ", size(xOut), "."
            return
        end if

        ! Compute the network backpropagation
        call hlpr%initialize(xIn, xOut)
        back = net%backpropagate(hlpr)

        if (size(back, 1) /= net%get_weighting_factor_count()) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_NETWORK): Expected ", &
                net%get_weighting_factor_count(), &
                " elements in the backpropagation array, but found ", size(back), "."
            return
        end if
    end function

end module
