! vibrations_lti.f90

submodule (vibrations) vibrations_lti
contains
! ------------------------------------------------------------------------------
    module function lti_get_zeros(this, err) result(z)
        ! Arguments
        class(LTI), intent(in) :: this
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:) :: z

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the transfer function is properly defined

        ! Compute the zeros - roots of the numerator
        z = this%numerator%roots(errmgr)
    end function

! ------------------------------------------------------------------------------
    module function lti_get_poles(this, err) result(p)
        ! Arguments
        class(LTI), intent(in) :: this
        class(errors), intent(inout), optional, target :: err
        complex(real64), allocatable, dimension(:) :: p

        ! Local Variables
        class(errors), pointer :: errmgr
        type(errors), target :: deferr

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if

        ! Ensure the transfer function is properly defined

        ! Compute the poles - roots of the denominator
        p = this%denominator%roots(errmgr)
    end function

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------

! ------------------------------------------------------------------------------
end submodule
