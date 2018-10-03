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
        call network%initialize(indices, neuronModel)
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