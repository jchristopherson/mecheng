! neural_net_sigmoid.f90

submodule (neural_net_core) neural_net_sigmoid
contains
    pure module function sn_eval(this, wx) result(y)
        ! Arguments
        class(sigmoid_neuron), intent(in) :: this
        real(real64), intent(in), dimension(:) :: wx
        real(real64) :: y

        ! Local Variables
        integer(int32) :: m, n, mn

        ! Initialization
        m = size(wx)
        n = this%get_input_count()
        mn = min(m, n) ! Ideally m and n are the same, but this will handle the case if they're not without throwing an error

        ! Process
        y = 1.0d0 / (1.0d0 + exp(-(dot_product(this%m_weights(1:mn), wx(1:mn)) + this%get_bias())))
    end function

end submodule
