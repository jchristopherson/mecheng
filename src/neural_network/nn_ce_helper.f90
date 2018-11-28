! nn_ce_helper.f90

submodule (neural_network_core) nn_ce_helper
contains
    module function ce_cost_fcn_vec(this, a, err) result(c)
        ! Arguments
        class(cross_entropy_helper), intent(in) :: this
        real(real64), intent(in), dimension(:) :: a
        class(errors), intent(inout), target, optional :: err
        real(real64) :: c

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, m, n
        
        ! Initialization
        m = size(a)
        c = 0.0d0
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (.not.allocated(this%m_outputs)) then
            call errmgr%report_error("ce_cost_fcn_vec", &
                "The learning_helper object is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        if (m /= size(this%m_outputs, 1)) then
            write(errmsg, '(AI0AI0A)') "The input array was expected to be of length ", &
                size(this%m_outputs, 1), ", but was found to be of length ", m, "."
            call errmgr%report_error("ce_cost_fcn_vec", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        n = size(this%m_outputs, 1) * size(this%m_outputs, 2)
        do i = 1, m
            c = c + sum(this%m_outputs(i,:) * log(a(i)) + &
                (1.0d0 - this%m_outputs(i,:)) * log(1.0d0 - a(i)))
        end do
        c = -c / real(n, real64)
    end function




    module function ce_cost_fcn_mtx(this, a, err) result(c)
        ! Arguments
        class(cross_entropy_helper), intent(in) :: this
        real(real64), intent(in), dimension(:,:) :: a
        class(errors), intent(inout), target, optional :: err
        real(real64) :: c

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: i, m, n, mn
        
        ! Initialization
        m = size(a, 1)
        n = size(a, 2)
        mn = m * n
        c = 0.0d0
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (.not.allocated(this%m_outputs)) then
            call errmgr%report_error("ce_cost_fcn_mtx", &
                "The learning_helper object is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        if (m /= size(this%m_outputs, 1) .or. n /= size(this%m_outputs, 2)) then
            write(errmsg, '(AI0AI0AI0AI0A)') "The input matrix was expected to be of size ", &
                size(this%m_outputs, 1), " x ", size(this%m_outputs, 2), &
                 ", but was found to be of size ", m, " x ", n, "."
            call errmgr%report_error("ce_cost_fcn_mtx", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            c = c + sum(this%m_outputs(:,i) * log(a(:,i)) + &
                (1.0d0 - this%m_outputs(:,i)) * log(1.0d0 - a(:,i)))
        end do
        c = -c / real(mn, real64)
    end function


    module function ce_cost_fcn_grad_vec(this, a, err) result(g)
        ! Arguments
        class(cross_entropy_helper), intent(in) :: this
        real(real64), intent(in), dimension(:) :: a
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:) :: g

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: m, n
        
        ! Initialization
        m = size(a)
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (.not.allocated(this%m_outputs)) then
            call errmgr%report_error("ce_cost_fcn_grad_vec", &
                "The learning_helper object is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        if (m /= size(this%m_outputs, 1)) then
            write(errmsg, '(AI0AI0A)') "The input array was expected to be of length ", &
                size(this%m_outputs, 1), ", but was found to be of length ", m, "."
            call errmgr%report_error("ce_cost_fcn_grad_vec", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        n = size(this%m_outputs, 1) * size(this%m_outputs, 2)
        g = (a - this%m_outputs(:,1)) / (a * (1.0d0 - a) * real(n, real64))
    end function

    module function ce_cost_fcn_grad_mtx(this, a, err) result(g)
        ! Arguments
        class(cross_entropy_helper), intent(in) :: this
        real(real64), intent(in), dimension(:,:) :: a
        class(errors), intent(inout), target, optional :: err
        real(real64), allocatable, dimension(:,:) :: g

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        character(len = 256) :: errmsg
        integer(int32) :: m, n, mn
        
        ! Initialization
        m = size(a, 1)
        n = size(a, 2)
        mn = m * n
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Input Checking
        if (.not.allocated(this%m_outputs)) then
            call errmgr%report_error("ce_cost_fcn_grad_mtx", &
                "The learning_helper object is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        if (m /= size(this%m_outputs, 1) .or. n /= size(this%m_outputs, 2)) then
            write(errmsg, '(AI0AI0AI0AI0A)') "The input matrix was expected to be of size ", &
                size(this%m_outputs, 1), " x ", size(this%m_outputs, 2), &
                 ", but was found to be of size ", m, " x ", n, "."
            call errmgr%report_error("ce_cost_fcn_grad_mtx", trim(errmsg), &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        g = (a - this%m_outputs) / (a * (1.0d0 - a) * real(mn, real64))
    end function

end submodule
