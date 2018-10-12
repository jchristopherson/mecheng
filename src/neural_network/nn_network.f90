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
        character(len = 256) :: errmsg
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Check
        if (size(layers) < 3) then
            write(errmsg, '(AI0A)') "There must be at least 3 layers, including " // &
                "the input and output layers, but ", size(layers), " were requested."
            call errmgr%report_error("net_init", trim(errmsg), NN_INVALID_INPUT_ERROR)
            return
        end if
    end subroutine
end submodule
