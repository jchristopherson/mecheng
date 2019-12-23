! genann.f90

!> @brief This module provides a straight Fortran interface to 
!! the C library GENANN.  GENANN is availalbe at
!! https://github.com/codeplea/genann.
module genann_core
    use iso_c_binding
    implicit none

    !> @brief The "C" genann structure containing parameters defining the
    !! neural network.
    type, bind(C) :: genann
        !> @brief The number of inputs.
        integer(c_int) :: inputs
        !> @brief The number of hidden layers.
        integer(c_int) :: hidden_layers
        !> @brief The number of neurons per hidden layer.
        integer(c_int) :: hidden
        !> @brief The number of outputs.
        integer(c_int) :: outputs
        !> @brief A pointer to the activation function to use for hidden
        !! neurons.
        type(c_funptr) :: activation_hidden
        !> @brief The activation function to use for the output neurons.
        type(c_funptr) :: activation_output
        !> @brief The total number of weights.
        integer(c_int) :: total_weights
        !> @brief THe total number neurons + inputs and the size of the 
        !! output buffer.
        integer(c_int) :: total_neurons
        !> @brief A pointer to the double-precision weighting factor matrix
        !! (total_weights long).
        type(c_ptr) :: weights
        !> @brief A pointer to the  double-precision output of each 
        !! neuron (total_neurons long).
        type(c_ptr) :: output
        !> @brief A pointer to the double-precision hidden and output 
        !! differences (total_neurons - inputs long).
        type(c_ptr) :: delta
    end type

    interface
        !> @brief Defines the signature of a GENANN activation function.
        !!
        !! @param[in] ann The network object.
        !! @param[in] a The input value to the neuron.
        !! @return The output value of the neuron.
        function genann_actfun(ann, a) result(rst)
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double), intent(in), value :: a
            real(c_double) :: rst
        end function

        !> @brief Creates and return a new genann neural network object.
        !!
        !! @param[in] inputs The number of input neurons.
        !! @param[in] hidden_layers The number of hidden layers.
        !! @param[in] hidden The number of neurons per hidden layer.
        !! @param[in] outputs The number of output neurons.
        !!
        !! @return A C-pointer to a new genann object.
        function genann_init(inputs, hidden_layers, hidden, outputs) result(ann) bind(C, name = "genann_init")
            use iso_c_binding
            integer(c_int), intent(in) :: inputs, hidden_layers, hidden, outputs
            type(c_ptr) :: ann
        end function

        !> @brief Frees the resources held by a genann neural network object.
        !!
        !! @param[in,out] ann The genann object to clean up.
        subroutine genann_free(ann) bind(C, name = "genann_free")
            use iso_c_binding
            import genann
            type(genann), intent(inout) :: ann
        end subroutine

        !> @brief Sets weights randomly.  This routine is called by
        !! genann_init.
        !!
        !! @param[in,out] ann
        subroutine genann_randomize(ann) bind(C, name = "genann_randomize")
            use iso_c_binding
            import genann
            type(genann), intent(inout) :: ann
        end subroutine

        !> @brief Saves the neural network object to file.
        !!
        !! @param[in] ann The genann object.
        !! @param[in] output The output stream.  This is a C FILE pointer.
        subroutine genann_write(ann, output) bind(C, name = "genann_write")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            type(c_ptr), intent(in), value :: output
        end subroutine

        !> @brief Reads and creates a neural network object from file.
        !!
        !! @param[in] input The input stream.  This is a C FILE pointer.
        !!
        !! @return A C-pointer to a new genann object.
        function genann_read(input) result(ann) bind(C, name = "genann_read")
            use iso_c_binding
            type(c_ptr), intent(in), value :: input
            type(c_ptr) :: ann
        end function

        !> @brief Returns a copy of the supplied neural network object.
        !!
        !! @param[in] ann The genann object.
        !!
        !! @return A C-pointer to a new genann object.
        function genann_copy(ann) result(cpy) bind(C, name = "genann_copy")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            type(c_ptr) :: cpy
        end function

        !> @brief Runs the feed-forward algorithm to determine the output
        !! of the neural network.  A pointer is returned to a block of 
        !! internally managed memory containing the output array.
        !!
        !! @param[in] ann The genann object to run.
        !! @param[in] inputs The input array with which to feed the network.
        !!
        !! @return A pointer to an internally managed block of memory containing
        !! a double-precision array containing the output values of the network.
        function genann_run(ann, inputs) result(rst) &
                bind(C, name = "genann_run")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double), intent(in) :: inputs(*)
            type(c_ptr) :: rst
        end function

        !> @brief Runs a single back-propogation update to train the network.
        !!
        !! @param[in] ann The genann object to train.
        !! @param[in] inputs An array of input values to use when training the
        !!  network.
        !! @param[in] outputs An array of desired outputs corresponding to the
        !!  supplied inputs.
        !! @param[in] learning_rate The rate at which to allow the network to
        !!  learn.
        subroutine genann_train(ann, inputs, outputs, learning_rate) &
                bind(C, name = "genann_train")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double), intent(in) :: inputs(*), outputs(*)
            real(c_double), intent(in), value :: learning_rate
        end subroutine

        !> @brief Defines a sigmoid GENANN activation function.
        !!
        !! @param[in] ann The network object.
        !! @param[in] a The input value to the neuron.
        !! @return The output value of the neuron.
        function genann_act_sigmoid(ann, a) result(rst) &
                bind(C, name = "genann_act_sigmoid")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double), intent(in), value :: a
            real(c_double) :: rst
        end function

        !> @brief Defines a cached sigmoid GENANN activation function.
        !!
        !! @param[in] ann The network object.
        !! @param[in] a The input value to the neuron.
        !! @return The output value of the neuron.
        function genann_act_sigmoid_cached(ann, a) result(rst) &
                bind(C, name = "genann_act_sigmoid_cached")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double), intent(in), value :: a
            real(c_double) :: rst
        end function

        !> @brief Defines a threshold GENANN activation function.
        !!
        !! @param[in] ann The network object.
        !! @param[in] a The input value to the neuron.
        !! @return The output value of the neuron.
        function genann_act_threshold(ann, a) result(rst) &
                bind(C, name = "genann_act_threshold")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double), intent(in), value :: a
            real(c_double) :: rst
        end function

        !> @brief Defines a linear GENANN activation function.
        !!
        !! @param[in] ann The network object.
        !! @param[in] a The input value to the neuron.
        !! @return The output value of the neuron.
        function genann_act_linear(ann, a) result(rst) &
                bind(C, name = "genann_act_linear")
            use iso_c_binding
            import genann
            type(genann), intent(in) :: ann
            real(c_double), intent(in), value :: a
            real(c_double) :: rst
        end function
    end interface
end module
