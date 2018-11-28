! nn_learning_helper.f90

submodule (neural_network_core) nn_learning_helper
contains
    module subroutine lh_init_vec(this, x, y, err)
        ! Arguments
        class(learning_helper), intent(inout) :: this
        real(real64), intent(in), dimension(:) :: x, y
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        if (allocated(this%m_inputs)) deallocate(this%m_inputs)
        if (allocated(this%m_outputs)) deallocate(this%m_outputs)
        allocate(this%m_inputs(size(x), 1), stat = flag)
        if (flag == 0) allocate(this%m_outputs(size(y), 1), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("lh_init", &
                "Insufficient memory available.", NN_OUT_OF_MEMORY_ERROR)
            return
        end if
        this%m_inputs(:,1) = x
        this%m_outputs(:,1) = y
    end subroutine



    module subroutine lh_init_mtx(this, x, y, err)
        ! Arguments
        class(learning_helper), intent(inout) :: this
        real(real64), intent(in), dimension(:,:) :: x, y
        class(errors), intent(inout), optional, target :: err

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        integer(int32) :: flag
        
        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Process
        if (allocated(this%m_inputs)) deallocate(this%m_inputs)
        if (allocated(this%m_outputs)) deallocate(this%m_outputs)
        allocate(this%m_inputs(size(x, 1), size(x, 2)), stat = flag)
        if (flag == 0) allocate(this%m_outputs(size(y, 1), size(y, 2)), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("lh_init", &
                "Insufficient memory available.", NN_OUT_OF_MEMORY_ERROR)
            return
        end if
        this%m_inputs = x
        this%m_outputs = y
    end subroutine




    pure module function lh_get_x_data(this) result(x)
        class(learning_helper), intent(in) :: this
        real(real64), allocatable, dimension(:,:) :: x
        if (allocated(this%m_inputs)) x= this%m_inputs
    end function




    pure module function lh_get_y_data(this) result(x)
        class(learning_helper), intent(in) :: this
        real(real64), allocatable, dimension(:,:) :: x
        if (allocated(this%m_outputs)) x= this%m_outputs
    end function



    
    
    module function lh_cost_fcn_vec(this, a, err) result(c)
        ! Arguments
        class(learning_helper), intent(in) :: this
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
            call errmgr%report_error("lh_cost_fcn_vec", &
                "The learning_helper object is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        if (m /= size(this%m_outputs, 1)) then
            write(errmsg, '(AI0AI0A)') "The input array was expected to be of length ", &
                size(this%m_outputs, 1), ", but was found to be of length ", m, "."
            call errmgr%report_error("lh_cost_fcn_vec", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        n = size(this%m_outputs, 1) * size(this%m_outputs, 2)
        do i = 1, m
            c = c + sum((this%m_outputs(i,:) - a(i))**2)
        end do
        c = c / (2.0d0 * real(n, real64))
    end function




    module function lh_cost_fcn_mtx(this, a, err) result(c)
        ! Arguments
        class(learning_helper), intent(in) :: this
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
            call errmgr%report_error("lh_cost_fcn_mtx", &
                "The learning_helper object is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        if (m /= size(this%m_outputs, 1) .or. n /= size(this%m_outputs, 2)) then
            write(errmsg, '(AI0AI0AI0AI0A)') "The input matrix was expected to be of size ", &
                size(this%m_outputs, 1), " x ", size(this%m_outputs, 2), &
                 ", but was found to be of size ", m, " x ", n, "."
            call errmgr%report_error("lh_cost_fcn_mtx", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        do i = 1, n
            c = c + sum((this%m_outputs(:,i) - a(:,i))**2)
        end do
        c = c / (2.0d0 * real(mn, real64))
    end function


    module function lh_cost_fcn_grad_vec(this, a, err) result(g)
        ! Arguments
        class(learning_helper), intent(in) :: this
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
            call errmgr%report_error("lh_cost_fcn_grad_vec", &
                "The learning_helper object is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        if (m /= size(this%m_outputs, 1)) then
            write(errmsg, '(AI0AI0A)') "The input array was expected to be of length ", &
                size(this%m_outputs, 1), ", but was found to be of length ", m, "."
            call errmgr%report_error("lh_cost_fcn_grad_vec", trim(errmsg), NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        n = size(this%m_outputs, 1) * size(this%m_outputs, 2)
        g = (a - this%m_outputs(:,1)) / n
    end function

    module function lh_cost_fcn_grad_mtx(this, a, err) result(g)
        ! Arguments
        class(learning_helper), intent(in) :: this
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
            call errmgr%report_error("lh_cost_fcn_grad_mtx", &
                "The learning_helper object is not properly initialized.", &
                NN_UNINITIALIZED_ERROR)
            return
        end if

        if (m /= size(this%m_outputs, 1) .or. n /= size(this%m_outputs, 2)) then
            write(errmsg, '(AI0AI0AI0AI0A)') "The input matrix was expected to be of size ", &
                size(this%m_outputs, 1), " x ", size(this%m_outputs, 2), &
                 ", but was found to be of size ", m, " x ", n, "."
            call errmgr%report_error("lh_cost_fcn_grad_mtx", trim(errmsg), &
                NN_ARRAY_SIZE_ERROR)
            return
        end if

        ! Process
        g = (a - this%m_outputs) / mn
    end function




    pure module function lh_get_input_count(this) result(x)
        class(learning_helper), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_inputs)) then
            x = size(this%m_inputs, 1)
        else
            x = 0
        end if
    end function




    pure module function lh_get_output_count(this) result(x)
        class(learning_helper), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_outputs)) then
            x = size(this%m_outputs, 1)
        else
            x = 0
        end if
    end function




    pure module function lh_get_data_set_count(this) result(x)
        class(learning_helper), intent(in) :: this
        integer(int32) :: x
        if (allocated(this%m_inputs)) then
            x = size(this%m_inputs, 2)
        else
            x = 0
        end if
    end function
    
end submodule
