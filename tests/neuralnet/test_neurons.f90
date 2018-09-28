! test_neurons.f90

module test_neurons
    use iso_fortran_env
    use neural_net_core
    implicit none

contains
    ! Tests the ability of the neuron evaluation ability.  Returns true if
    ! the test was successful; else, false.
    function test_sigmoid_neuron_eval() result(rst)
        ! Parameters
        integer(int32), parameter :: nInputs = 1000
        real(real64), parameter :: tol = 1.0d-8

        ! Local Variables
        logical :: rst
        type(sigmoid_neuron) :: nrn
        real(real64), dimension(nInputs) :: inputs, weights
        real(real64) :: nrnOutput, z, ans

        ! Initialization
        rst = .true.
        call random_number(inputs)
        call nrn%initialize(nInputs, .true.)
        call random_number(z)
        call nrn%set_bias(z)

        ! Get the array of weighting factors
        weights = nrn%get_weights();

        ! Evaluate the neuron
        nrnOutput = nrn%evaluate(inputs)

        ! Compute the solution
        z = dot_product(weights, inputs) + nrn%get_bias()
        ans = 1.0d0 / (1.0d0 + exp(-z))

        ! Test
        if (abs(nrnOutput - ans) > tol) then
            rst = .false.
            print '(AEN12.4AEN12.4A)', "TEST FAILED: Sigmoid neuron evaluation test failed.  " // &
                "Expected an output of ", ans, ", but found an output of ", nrnOutput, "."
            return
        end if
    end function
end module
