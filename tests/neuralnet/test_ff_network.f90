! test_ff_network.f90

module test_ff_network
    use iso_fortran_env
    use neural_net_core
    implicit none

    type layer_obj
        class(layer), pointer :: item
    end type

    type layer_container
    private
        type(layer_obj), allocatable, dimension(:) :: m_layers
    contains
        final :: lc_final
        procedure, public :: get => lc_get
        procedure, public :: count => lc_count
        procedure, public :: initialize => lc_init
    end type

contains
    function test_network_1() result(rst)
        ! Parameters

        ! Local Variables
        logical :: rst
        integer(int32), dimension(4) :: indices
        type(feedforward_network) :: network
        type(sigmoid_neuron) :: neuronModel
        type(layer) :: layerModel
        integer(int32) :: i, j
        class(layer), pointer :: lptr
        class(neuron), pointer :: nptr

        ! Initialization
        rst = .true.
        
        ! Establish a 4 layer network with the following
        ! neuron count:
        ! - Layer 1: 3
        ! - Layer 2: 8
        ! - Layer 3: 6
        ! - Layer 4: 1
        indices = [3, 8, 6, 1]

        ! Initialize the network
        call network%initialize(indices, layerModel, neuronModel)

        ! Check to ensure the layer structure is correct
        if (network%get_count() /= 4) then
            rst = .false.
            print '(AI0A)', "TEST FAILED (TEST_NETWORK_1): " // &
                "Incorrect number of layers.  Expected 4, but found ", &
                network%get_count(), "."
            return
        end if

        ! Cycle over each layer and determine if it has the correct number
        ! of neurons, and that each neuron has the correct number of inputs.
        do i = 1, network%get_count()
            ! Get a pointer to the layer
            lptr => network%get_layer(i)

            ! Ensure the proper number of neurons
            if (lptr%get_count() /= indices(i)) then
                rst = .false.
                print '(AI0AI0AI0A)', "TEST FAILED (TEST_NETWORK_1): " // &
                    "Improper neuron count on layer ", i, &
                    ".  Expected to find ", indices(i), &
                    " neurons, but found ", lptr%get_count(), &
                    " instead."
                return
            end if

            ! Ensure each neuron accepts the appropriate number of inputs
            do j = 1, lptr%get_count()
                ! Get a pointer to the neuron
                nptr => lptr%get_neuron(j)

                ! Ensure the proper number of inputs.  The input layer should only
                ! accept 1 input per neuron.
                if (i == 1) then
                    if (nptr%get_input_count() /= 1) then
                        rst = .false.
                        print '(AI0A)', "TEST FAILED (TEST_NETWORK_1): " // &
                            "Improper neuron input count on the input layer.  " // &
                            "Expected a single input, but found ", &
                            nptr%get_input_count(), "."
                        return
                    end if
                else
                    if (nptr%get_input_count() /= indices(i-1)) then
                        rst = .false.
                        print '(AI0AI0AI0AI0A)', "TEST FAILED (TEST_NETWORK_1): " // &
                            "Improper neuron input count on layer ", i, " neuron ", j, &
                            ".  Expected to find ", indices(i-1), " inputs, but found ", &
                            nptr%get_input_count(), " instead."
                        return
                    end if
                end if
            end do
        end do
    end function

    function test_layer_container() result(rst)
        ! Local Variables
        logical :: rst
        type(layer_container) :: lyrs
        class(layer), pointer :: lyr, lyr1, lyr2
        integer(int32) :: i
        type(sigmoid_neuron) :: model

        ! Initialization
        rst = .true.
        call lyrs%initialize(4)

        ! Initialize a few layers
        do i = 1, lyrs%count()
            lyr => lyrs%get(i)
            call lyr%initialize(i, model)
        end do

        ! Ensure layer 1 and layer 2 are different
        lyr1 => lyrs%get(1)
        lyr2 => lyrs%get(2)
        if (lyr1%get_count() == lyr2%get_count()) then
            rst = .false.
            print '(A)', "TEST FAILED (TEST_LAYER_CONTAINER): No differentiation between different layers."
            return
        end if
    end function 

! ------------------------------------------------------------------------------
    function lc_get(this, i) result(x)
        class(layer_container), intent(in) :: this
        integer(int32), intent(in) :: i
        class(layer), pointer :: x
        x => this%m_layers(i)%item
    end function

    pure function lc_count(this) result(x)
        class(layer_container), intent(in) :: this
        integer(int32) :: x
        x = size(this%m_layers)
    end function

    subroutine lc_init(this, n)
        class(layer_container), intent(inout) :: this
        integer(int32), intent(in) :: n
        type(layer) :: model
        integer(int32) :: i

        allocate(this%m_layers(n))
        do i = 1, n
            allocate(this%m_layers(i)%item, source = model)
        end do
    end subroutine

    subroutine lc_final(this)
        type(layer_container), intent(inout) :: this
        integer(int32) :: i, n
        if (allocated(this%m_layers)) then
            n = size(this%m_layers)
        else
            n = 0
        end if
        do i = 1, n
            if (associated(this%m_layers(i)%item)) deallocate(this%m_layers(i)%item)
        end do
    end subroutine

end module