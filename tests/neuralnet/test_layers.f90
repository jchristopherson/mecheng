! test_layers.f90

module test_layers
    use iso_fortran_env
    use neural_net_core
    implicit none

contains
    function test_layer() result(rst)
        ! Parameters
        integer(int32), parameter :: nNeurons = 1000
        real(real64), parameter :: tol = 1.0d-12

        ! Local Variables
        logical :: rst
        type(layer) :: lyr
        type(sigmoid_neuron) :: model
        class(neuron), pointer :: n1, n2
        real(real64) :: x(2)

        ! Initialization
        rst = .true.
        call lyr%initialize(nNeurons, model)
        call random_number(x)

        ! Test to ensure the proper # of neurons were established
        if (lyr%get_count() /= nNeurons) then
            rst = .false.
            print '(AI0AI0A)', "TEST FAILED: An incorrect number of neurons " // &
                "was found in the layer.  Expected ", nNeurons, ", but found ", &
                lyr%get_count(), "."
            return
        end if

        ! Get a few neurons, and then ensure they really are seperate objects
        n1 => lyr%get_neuron(1)
        n2 => lyr%get_neuron(nNeurons)

        call n1%set_bias(x(1))
        call n2%set_bias(x(2))

        ! Ensure the bias from n1 and n2 are different
        if (abs(n1%get_bias() - n2%get_bias()) < tol) then
            rst = .false.
            print '(A)', "TEST FAILED: The layer collection did not populate " // &
                "with independent neurons."
            return
        end if
    end function

end module