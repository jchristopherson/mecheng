! genann_example_1.f90

program example
    use iso_c_binding
    use genann_core
    implicit none

    ! Parameters
    integer(c_int), parameter :: num_inputs = 2
    integer(c_int), parameter :: num_hidden_layers = 1
    integer(c_int), parameter :: num_hidden_neurons = 2
    integer(c_int), parameter :: num_outputs = 1
    integer(c_int), parameter :: num_training = 500
    real(c_double), parameter :: learn_rate = 5.0d0

    ! Local Variables
    type(c_ptr) :: nn_ptr, optr
    type(genann), pointer :: ann
    real(c_double) :: inputs(2, 4), outputs(1, 4)
    real(c_double), pointer :: rst(:)
    integer(c_int) :: i

    ! Create the neural network
    nn_ptr = genann_init(num_inputs, num_hidden_layers, &
        num_hidden_neurons, num_outputs)
    if (.not.c_associated(nn_ptr)) then
        print '(A)', "Could not initialize the network."
        return
    end if
    call c_f_pointer(nn_ptr, ann)

    ! Deffine the expected input and output data for the XOR function
    inputs = reshape([&
        0.0d0, 0.0d0, &
        0.0d0, 1.0d0, &
        1.0d0, 0.0d0, &
        1.0d0, 1.0d0], [2, 4])
    outputs = reshape([0.0d0, 1.0d0, 1.0d0, 0.0d0], [1, 4])

    ! Train the network
    do i = 1, num_training
        call genann_train(ann, inputs(:,1), outputs(:,1), learn_rate)
        call genann_train(ann, inputs(:,2), outputs(:,2), learn_rate)
        call genann_train(ann, inputs(:,3), outputs(:,3), learn_rate)
        call genann_train(ann, inputs(:,4), outputs(:,4), learn_rate)
    end do
    
    ! Display the output of the network
    do i = 1, size(inputs, 2)
        ! Compute the outputs, and get a usable Fortran array
        optr = genann_run(ann, inputs(:,i))
        call c_f_pointer(optr, rst, [num_outputs])

        ! Display the results
        print '(AF5.3AF5.3AF5.3AF5.3)', &
            "Input 1: ", inputs(1,i), &
            ", Input 2: ", inputs(2, i), &
            ", Output: ", rst(1), &
            ", Expected Output: ", outputs(1,i)
    end do

    ! Clean up
    call genann_free(ann)
end program
