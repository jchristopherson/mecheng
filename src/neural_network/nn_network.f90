! nn_network.f90

submodule (neural_network_core) nn_network
contains
    module subroutine net_init(this, layers, model, err)
        ! Arguments
        class(network), intent(inout) :: this
        integer(int32), intent(in) :: layers
        class(layer), intent(in) :: model
        class(errors), intent(inout), target, optional :: err

        ! Initialization
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
    end subroutine
end submodule
