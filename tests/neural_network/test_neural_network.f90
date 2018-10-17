! test_neural_network.f90

program main
    use test_layers
    use test_networks
    implicit none

    ! Local Variables
    logical :: rst, overall

    ! Process
    overall = .true.

    rst = test_layer()
    if (.not.rst) overall = .false.

    rst = test_network()
    if (.not.rst) overall = .false.

    ! End
    if (rst) then
        print '(A)', "NEURAL_NETWORK TESTS PASSED."
    else
        print '(A)', "NEURAL_NETWORK TESTS FAILED."
    end if
end program
