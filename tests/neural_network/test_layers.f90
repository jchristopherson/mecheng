! test_layers.f90

module test_layers
    use iso_fortran_env
    use neural_network_core
    implicit none

contains
    function test_layer() result(rst)
        ! Parameters
        integer(int32), parameter :: nNeurons = 100
        integer(int32), parameter :: nInputs = 500

        ! Local Variables
        logical :: rst
        type(layer) :: lyr
        real(real64), allocatable :: b(:), w(:,:)

        ! Initialization
        rst = .true.
        call lyr%initialize(nInputs, nNeurons)

        ! Ensure the proper number of inputs and neurons were allocated
        if (lyr%get_input_count() /= nInputs) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_LAYER): Expected ", &
                nInputs, " inputs, but found ", lyr%get_input_count(), "."
            return
        end if

        if (lyr%get_neuron_count() /= nNeurons) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_LAYER): Expected ", &
                nNeurons, " neurons, but found ", lyr%get_neuron_count(), "."
            return
        end if

        ! Randomize the weighting factors and bias terms
        call lyr%randomize()

        ! Retrieve the weighting factors and bias terms - verify the dimensions
        w = lyr%get_weights()
        b = lyr%get_bias_vector()

        if (size(b) /= nNeurons) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED (TEST_LAYER): The bias vector is " // &
                "not sized appropriately.  Expected a vector of ", nNeurons, &
                " elements, but found a vector of ", size(b), " elements."
            return
        end if

        if (size(w, 1) /= nNeurons .or. size(w, 2) /= nInputs) then
            rst = .false.
            print '(AI0AI0AI0AI0A)', "TEST FAILED (TEST_LAYER): The weighting matrix is " // &
                "not sized appropriately.  Expected a matrix of ", nNeurons, " x ", nInputs, &
                " elements, but found a matrix of ", size(w, 1), " x ", size(w, 2), &
                " elements."
            return
        end if

        ! Test w * x + b
    end function
end module