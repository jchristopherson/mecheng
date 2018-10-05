! neural_net_network.f90

submodule (neural_net_core) neural_net_network
contains
! ------------------------------------------------------------------------------
    module subroutine network_init(this, lyrs, lmodel, nmodel, err)
        ! Arguments
        class(neural_network), intent(inout) :: this
        integer(int32), intent(in), dimension(:) :: lyrs
        class(layer), intent(in) :: lmodel
        class(neuron), intent(in) :: nmodel
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        integer(int32) :: i, n, nin, flag
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        class(neuron), allocatable :: nrn
        class(layer), pointer :: ptr

        ! Initialization
        n = size(lyrs)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (n < 3) then
            write(errmsg, '(AI0A)') &
                "3 layers were expected for this network type, but ", &
                n, " were found."
            call errmgr%report_error("network_init", trim(errmsg), &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Memory Allocation
        if (this%get_count() /= 0) call this%clear()
        if (this%get_capacity() < n) call this%set_capacity(n, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Establish the network structure
        do i = 1, n
            ! Ensure there is at least 1 neuron in the current layer
            if (lyrs(i) < 1) then
                write(errmsg, '(AI0AI0A)') "Layer ", i, &
                    " was expected to have at least 1 neuron, but ", &
                    lyrs(i), " were found."
                call errmgr%report_error("network_init", trim(errmsg), &
                    NN_INVALID_INPUT_ERROR)
                return
            end if

            ! Place the layer into the collection of layers.  The push
            ! method will create a copy of the lmodel, and store its copy.
            ! As such, it is irrelevant what we do with lmodel after
            ! this call.
            call this%push(lmodel, errmgr)
            if (errmgr%has_error_occurred()) return
        end do

        ! Set up each layer structure
        nin = 1 ! The number of inputs accepted by each neuron of the current layer
        do i = 1, n
            ! Update the input count for the model neuron for this layer
            if (allocated(nrn)) deallocate(nrn)
            allocate(nrn, source = nmodel, stat = flag)
            if (flag /= 0) then
                call errmgr%report_error("network_init", &
                    "Insufficient memory available.", &
                    NN_OUT_OF_MEMORY_ERROR)
                return
            end if
            call nrn%initialize(nin, err = errmgr)
            if (errmgr%has_error_occurred()) return

            ! Define the layer structure
            ptr => this%get_layer(i)
            call ptr%initialize(lyrs(i), nrn, errmgr)

            ! Update the input number count
            nin = lyrs(i)
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module function network_get(this, i, err) result(ptr)
        ! Arguments
        class(neural_network), intent(in) :: this
        integer(int32), intent(in) :: i
        class(errors), intent(inout), target, optional :: err
        class(layer), pointer :: ptr

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(*), pointer :: genptr
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        nullify(ptr)

        ! Error Checking
        if (i < 1 .or. i > this%get_count()) then
            call errmgr%report_error("network_get", &
                "The supplied index was outside the bounds of the collection.", &
                NN_INDEX_OUT_OF_RANGE_ERROR)
            return
        end if

        ! Process
        genptr => this%get(i)
        select type (genptr)
        class is (layer)
            ptr => genptr
        end select
    end function

! ------------------------------------------------------------------------------
    module subroutine network_randomize(this, err)
        ! Arguments
        class(neural_network), intent(inout) :: this
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        integer(int32) :: i, j, nlyrs
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(layer), pointer :: lyr
        class(neuron), pointer :: nrn
        character(len = 256) :: errmsg

        ! Initialization
        nlyrs = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Error Checking
        if (nlyrs < 1) then
            call errmgr%report_error("network_randomize", &
                "The network has not been properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        ! Process
        !
        ! TO DO: Investigate how to run this loop in parallel
        do i = 1, nlyrs
            ! Obtain a pointer to the layer
            lyr => this%get_layer(i)

            ! Ensure the layer pointer is valid
            if (.not.associated(lyr)) then
                write(errmsg, '(AI0A)') "Layer ", i, "is undefined."
                call errmgr%report_error("network_randomize", &
                    trim(errmsg), NN_NULL_POINTER_ERROR)
                return
            end if

            ! Cycle over each neuron in the layer
            do j = 1, lyr%get_count()
                ! Obtain a pointer to the neuron
                nrn => lyr%get_neuron(j)

                ! Ensure the neuron pointer is valid
                if (.not.associated(nrn)) then
                    write(errmsg, '(AI0AI0A)') "Neuron ", j, &
                        " in layer ", i, " is undefined."
                    call errmgr%report_error("network_randomize", &
                        trim(errmsg), NN_NULL_POINTER_ERROR)
                    return
                end if

                ! Randomize the neuron
                call nrn%randomize(errmgr)
                if (errmgr%has_error_occurred()) return
            end do
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine network_fit(this, solver, inputs, outputs, res, err)
        ! Required Modules
        use nonlin_core
        use nonlin_least_squares

        ! Arguments
        class(neural_network), intent(inout) :: this
        class(least_squares_solver), intent(inout) :: solver
        real(real64), intent(in), dimension(:) :: inputs, outputs
        real(real64), intent(out), dimension(:), target, optional :: res
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: nlayers, nin, nout, flag
        logical :: valid
        class(layer), pointer :: lyr
        real(real64), allocatable, dimension(:) :: coeffs
        real(real64), allocatable, dimension(:), target :: rsd
        real(real64), pointer, dimension(:) :: residuals
        type(vecfcn_helper) :: obj
        procedure(vecfcn), pointer :: fcn
        type(iteration_behavior) :: iter
        
        ! Initialization
        nlayers = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        
        ! Validate the network
        valid = this%validate(errmsg)
        if (.not.valid) then
            call errmgr%report_error("network_fit", trim(errmsg), NN_INVALID_NETWORK_ERROR)
            return
        end if

        ! Ensure the inputs and outputs arrays are properly sized
        lyr => this%get_layer(1)
        nin = lyr%get_count()
        if (size(inputs) /= nin) then
            write(errmsg, '(AI0AI0A)') "The input array size (", &
                size(inputs), ") does not match the number of neurons (", &
                nin, ") in the input layer."
            call errmgr%report_error("network_fit", trim(errmsg), &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        lyr => this%get_layer(nlayers)
        nout = lyr%get_count()
        if (size(outputs) /= nout) then
            write(errmsg, '(AI0AI0A)') "The output array size (", &
                size(outputs), ") does not match the number of neurons (", &
                nout, ") in the output layer."
            call errmgr%report_error("network_fit", trim(errmsg), &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Extract the coefficients for the network as they're currently
        ! defined
        coeffs = this%get_coefficients(errmgr)
        if (errmgr%has_error_occurred()) return

        ! Allocate space for the residual information
        if (present(res)) then
            if (size(res) /= nout) then
                write(errmsg, '(AI0AI0A)') "The residual output array is " // &
                    "improperly sized.  An array of size ", nout, &
                    " was expected, but an array of size ", size(res), &
                    " was found."
                call errmgr%report_error("network_fit", & 
                    trim(errmsg), NN_ARRAY_SIZE_ERROR)
                return
            end if
            residuals => res
        else
            allocate(rsd(nout), stat = flag)
            if (flag /= 0) then
                call errmgr%report_error("network_fit", &
                    "Insufficient memory available.", &
                    NN_OUT_OF_MEMORY_ERROR)
                return
            end if
            residuals => rsd
        end if
        
        ! Set up the solver, and then determine the best-fit coefficients
        fcn => fit_routine
        call obj%set_fcn(fcn, size(coeffs), nout)

        ! Compute the solution
        call solver%solve(obj, coeffs, residuals, iter, errmgr)
        if (errmgr%has_error_occurred()) return

        ! Ensure the correct coefficients are utilized
        call this%populate(coeffs)

        ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% !
    contains
        subroutine fit_routine(x, f)
            ! Arguments
            real(real64), intent(in), dimension(:) :: x
            real(real64), intent(out), dimension(:) :: f

            ! Populate the network with coefficients
            call this%populate(x)

            ! Evaluate the network using the given inputs, and compare against
            ! the desired outputs
            f = this%evaluate(inputs) - outputs
        end subroutine
    end subroutine

! ------------------------------------------------------------------------------
    module subroutine network_populate(this, x, err)
        ! Arguments
        class(neural_network), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x
        class(errors), intent(inout), target, optional :: err

        ! Local Variables
        integer(int32) :: i, j, k, nlayers, nneurons, ncoeffs, nin
        class(layer), pointer :: lyr
        class(neuron), pointer :: nrn
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        nlayers = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the network is valid
        if (.not.this%validate(errmsg)) then
            call errmgr%report_error("network_populate", trim(errmsg), NN_INVALID_NETWORK_ERROR)
            return
        end if

        ! Ensure the input array is properly sized
        ncoeffs = this%get_coefficient_count()
        if (size(x) /= ncoeffs) then
            write(errmsg, '(AI0AI0A)') &
                "The coefficient array is improperly sized.  An array of ", &
                ncoeffs, " elements was expected, but an array of ", size(x), &
                " was found."
            call errmgr%report_error("network_populate", trim(errmsg),  &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        k = 1
        do i = 1, nlayers
            lyr => this%get_layer(i)
            nneurons = lyr%get_count()
            do j = 1, nneurons
                nrn => lyr%get_neuron(j)
                nin = nrn%get_input_count()
                call nrn%set_weights(x(k:k+nin-1))
                call nrn%set_bias(x(k+nin))
                k = k + nin + 1
            end do
        end do
    end subroutine

! ------------------------------------------------------------------------------
    module function network_extract(this, err) result(x)
        ! Arguments
        class(neural_network), intent(in) :: this
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:) :: x

        ! Local Variables
        integer(int32) :: i, j, k, nlayers, nneurons, ncoeffs, nin, flag
        class(layer), pointer :: lyr
        class(neuron), pointer :: nrn
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        
        ! Initialization
        nlayers = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the network is valid
        if (.not.this%validate(errmsg)) then
            call errmgr%report_error("network_extract", trim(errmsg), NN_INVALID_NETWORK_ERROR)
            return
        end if

        ! Determine the number of coefficients, and then allocate space
        ncoeffs = this%get_coefficient_count()
        allocate(x(ncoeffs), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("network_extract", &
                "Insufficient memory available.", &
                NN_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Cycle over and collect each coefficient
        k = 1
        do i = 1, nlayers
            lyr => this%get_layer(i)
            nneurons = lyr%get_count()
            do j = 1, nneurons
                nrn => lyr%get_neuron(j)
                nin = nrn%get_input_count()
                x(k:k+nin-1) = nrn%get_weights()
                x(k+nin) = nrn%get_bias()
                k = k + nin + 1
            end do
        end do
    end function

! ------------------------------------------------------------------------------
    module function network_validate(this, msg) result(rst)
        ! Arguments
        class(neural_network), intent(in) :: this
        character(len = *), intent(out) :: msg
        logical :: rst

        ! Local Variables
        class(layer), pointer :: lyr
        class(neuron), pointer :: nrn
        integer(int32) :: i, j, n, nlayers, nin

        ! Initialization
        rst = .false.
        nlayers = this%get_count()

        ! Process
        if (nlayers < 1) then
            write(msg, '(AI0A)') "Only ", nlayers, " were found."
            return
        end if
        do i = 1, nlayers
            ! Get a pointer to the layer object
            lyr => this%get_layer(i)

            ! Ensure the pointer is valid
            if (.not.associated(lyr)) then
                write(msg, '(AI0A)') "Layer ", i, " is not defined."
                return
            end if

            ! Ensure there's something in the layer
            n = lyr%get_count()
            if (n < 1) then
                write(msg, '(AI0A)') "Layer ", i, " is not properly initialized."
                return
            end if

            ! Cycle over each neuron
            nin = 0
            do j = 1, n
                ! Get a pointer to the neuron
                nrn => lyr%get_neuron(j)

                ! Ensure the pointer is valid
                if (.not.associated(nrn)) then
                    write(msg, '(AI0AI0A)') "Neuron ", j, " in layer ", i, &
                        " is not properly initialized."
                    return
                end if

                ! Ensure there are enough inputs
                nin = nrn%get_input_count()
                if (nin < 1) then
                    write(msg, '(AI0AI0A)') "Neuron ", j, " in layer ", i, &
                        " does not allow for sufficient inputs."
                    return
                end if
            end do
        end do

        ! If we've made it this far, everything's OK
        rst = .true.
    end function

! ------------------------------------------------------------------------------
    module function network_get_num_coeff(this, err) result(ncoeff)
        ! Arguments
        class(neural_network), intent(in) :: this
        class(errors), intent(inout), target, optional :: err
        integer(int32) :: ncoeff

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, j, nlayers, n, nin
        class(layer), pointer :: lyr
        class(neuron), pointer :: nrn
        
        ! Initialization
        ncoeff = 0
        nlayers = this%get_count()
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Validate the network
        if (.not.this%validate(errmsg)) then
            call errmgr%report_error("network_get_num_coeff", &
                trim(errmsg), NN_INVALID_NETWORK_ERROR)
            return
        end if

        ! Count coefficients
        ncoeff = 0
        do i = 1, nlayers
            ! Get a pointer to the layer object
            lyr => this%get_layer(i)

            ! Ensure there's something in the layer
            n = lyr%get_count()

            ! Cycle over each neuron
            nin = 0
            do j = 1, n
                ! Get a pointer to the neuron
                nrn => lyr%get_neuron(j)

                ! Ensure there are enough inputs
                nin = nrn%get_input_count()

                ! Count the coefficients
                ncoeff = ncoeff + nin + 1   ! +1 accounts for the bias term
            end do
        end do
    end function

! ------------------------------------------------------------------------------
end submodule
