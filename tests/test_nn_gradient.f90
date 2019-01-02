! test_nn_gradient.f90

module test_nn_gradient
    use iso_fortran_env
    use neural_networks
    use nonlin_core
    implicit none
contains
    function test_gradient_vector() result(rst)
        ! Arguments
        logical :: rst

        ! Parameters
        integer(int32), parameter :: nin = 2
        integer(int32), parameter :: nout = 1
        integer(int32), parameter :: nhidden = 1
        integer(int32), parameter :: nhiddennode = 2

        ! Local Variables
        type(neural_network) :: network
        real(real64) :: xin(nin), xout(nout), xexpect(nout)
        integer(int32) :: nweights, nbias, ntotal
        type(fcnnvar_helper) :: grad_obj
        procedure(fcnnvar), pointer :: fcn
        real(real64), allocatable, dimension(:) :: g, gexpect, weights
        procedure(cost_function_derivative), pointer :: dcf

        ! Initialization
        rst = .true.
        call network%initialize(nin, nhidden, nhiddennode, nout)
        call random_number(xin)
        call random_number(xout)
        nweights = network%get_weight_count()
        nbias = network%get_bias_count()
        ntotal = nbias + nweights
        allocate(gexpect(ntotal))
        fcn => gradfcn
        call grad_obj%set_fcn(fcn, ntotal)
        dcf => diff_cross_entropy_cost_function

        ! Compute the gradient via backpropagation
        g = network%compute_gradient(xin, xexpect, dcf = dcf)
        print '(A)', "Computed Gradient:"
        print *, g

        ! Compute the expected gradient vector via finite differences
        weights = network%get_weights()
        call grad_obj%gradient(weights, gexpect)

        ! TEMP - Print out the gradient
        print '(A)', "Expected Gradient:"
        print *, gexpect

    contains
        function gradfcn(ws) result(cs)
            ! Arguments
            real(real64), intent(in), dimension(:) :: ws
            real(real64) :: cs

            ! Set the weighting factors
            call network%set_weights(ws(1:nweights))
            call network%set_bias(ws(nweights+1:size(ws)))
            
            ! Evaluate the network
            xout = network%run(xin)

            ! Evaluate the cost function
            cs = 0.5d0 * sum((xexpect - xout)**2)
        end function
    end function
end module