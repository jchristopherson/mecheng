! test_ff_network.f90

module test_ff_network
    use iso_fortran_env
    use neural_net_core
    implicit none

contains
    function test_network_1() result(rst)
        ! Parameters

        ! Local Variables
        logical :: rst
        integer(int32), dimension(4) :: indices
        type(feedforward_network) :: network
        type(sigmoid_neuron) :: neuronModel

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
        call network%initialize(indices, neuronModel)
    end function
end module