! test_neural_network.f90

program test
    use iso_fortran_env
    use neural_networks
    implicit none

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

        ! Parameters
        integer(int32), parameter :: inputs = 10
        integer(int32), parameter :: hidden_layers = 8
        integer(int32), parameter :: hidden = 10
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
end program