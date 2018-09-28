! test_neuralnet.f90

program test
    use test_neurons
    use test_layers
    use test_ff_network
    implicit none

    ! Local Variables
    logical :: rst, overall

    ! Introduction
    print *, ""
    print '(A)', "**********************************************"
    print '(A)', "WELCOME TO THE NEURAL_NETWORK TEST APPLICATION"
    print '(A)', "**********************************************"
    print *, ""

    ! Initialization
    overall = .true.

    ! Tests
    rst = test_sigmoid_neuron_eval()
    if (.not.rst) overall = .false.

    rst = test_layer()
    if (.not.rst) overall = .false.

    rst = test_network_1()
    if (.not.rst) overall = .false.


    ! Report
    if (overall) then
        print '(A)', "ALL TESTS PASSED."
    else
        print '(A)', "TESTS HAVE FAILED."
    end if
    print *, ""
end program