! test_neural_network.f90

program test
    use iso_fortran_env
    use neural_networks
    implicit none

    ! Parameters
    integer(int32), parameter :: inputs = 10
    integer(int32), parameter :: hidden_layers = 2
    integer(int32), parameter :: hidden = 10
    integer(int32), parameter :: outputs = 1

    ! Local Variables
    logical :: check, overall

    ! Initialization
    overall = .true.

    ! Tests
    check = test_neural_net_basics()
    if (.not.check) overall = .false.


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

        ! Local Variables
        type(neural_network) :: network

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
    end function
end program