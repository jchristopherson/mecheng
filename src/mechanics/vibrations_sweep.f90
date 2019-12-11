! vibrations_sweep.f90

submodule (vibrations) vibrations_sweep
contains
! ------------------------------------------------------------------------------
    module function compute_frequency_sweep(fcn, freq, xo, opt, err) result(rst)
        ! Arguments
        procedure(harmonic_ode_fcn), intent(in), pointer :: fcn
        real(real64), intent(in), dimension(:) :: freq, xo
        type(frequency_sweep_options), intent(in), optional :: opt
        class(errors), intent(inout), optional, target :: err
        real(real64), allocatable, dimension(:,:) :: rst

        ! Local Variables
        logical :: status
        integer(int32) :: neqn, nfreq, ncycle, scycle, flag
        real(real64), allocatable, dimension(:) :: ic, w1, w2
        class(errors), pointer :: errmgr
        type(errors), target :: deferr
        class(ode_integrator), pointer :: integrator
        type(ode_auto), target :: defintegrator

        ! Initialization
        if (present(err)) then
            errmgr => err
        else
            errmgr => deferr
        end if
        neqn = size(xo)
        nfreq = size(freq)

        ! Establish settings
        if (present(opt)) then
            integrator => opt%integrator
            ncycle = opt%forced_cycle_count
            scycle = opt%measured_cycle_count
            status = opt%display_status
        else
            integrator => defintegrator
            ncycle = 50
            scycle = 10
            status = .false.
        end if

        ! Verify the settings
        if (.not.associated(integrator)) then
            call errmgr%report_error("compute_frequency_sweep", &
                "No itegrator is defined.", MECH_NULL_REFERENCE_ERROR)
            return
        end if
        if (ncycle < 2) then
            call errmgr%report_error("compute_frequency_sweep", &
                "At least 2 forcing cycles must be employed.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (scycle >= ncycle) then
            call errmgr%report_error("compute_frequency_sweep", &
                "The number of sampling cycles cannot match or exceed " // &
                "the number of forcing cycles.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if
        if (scycle < 1) then
            call errmgr%report_error("compute_frequency_sweep", &
                "At least 1 sample cycle must be employed.", &
                MECH_INVALID_INPUT_ERROR)
            return
        end if

        ! Local Memory Allocation
        allocate(ic(neqn), stat = flag)
        if (flag == 0) allocate(w1(neqn), stat = flag)
        if (flag == 0) allocate(w2(neqn), stat = flag)
        if (flag == 0) allocate(rst(nfreq, neqn), stat = flag)
        if (flag /= 0) then
            call errmgr%report_error("compute_frequency_sweep", &
                "Insufficient memory available.", &
                MECH_OUT_OF_MEMORY_ERROR)
            return
        end if

        ! Process
        ic = xo
        call sweep_engine(integrator, fcn, freq, ic, ncycle, scycle, &
            status, rst, w1, w2, errmgr)
    end function

! ------------------------------------------------------------------------------
    ! Sweeps through the supplied frequency vector, and computes the amplitude
    ! of the resulting outputs.
    !
    ! Inputs:
    ! - integrator: The integrator to use.  The reset routine of the integrator
    !       is utilized between iterations to ensure the integrator is properly
    !       aligned for each iteration.
    ! - fcn: A pointer to the routine containing the ODE's to integrate.
    ! - freq: An N-element array containing the frequency points, in rad/s.
    ! - xo: An M-element array containing the initial conditions for each of
    !       the M ODE's.
    ! - ncycle: The number of cycles of forced oscillations the system of ODE's
    !       is forced through.
    ! - scycle: The number of forced oscillation cycles at the end of the signal
    !       that are used to determine the output.
    ! - status: Set to true to update status on each iteration; else, false for
    !       no updates.
    ! - err: The error handler object.
    !
    ! Output:
    ! - xo: The M-element initial condition array is used as a working array to
    !       update initial conditions for each solution step.
    ! - amp: An N-by-M matrix containing the responses from each of the M equations.
    ! - work1: An M-element workspace array.
    ! - work2: An M-element workspace array.
    subroutine sweep_engine(integrator, fcn, freq, xo, ncycle, scycle, status, &
            amp, work1, work2, err)
        use constants

        ! Arguments
        class(ode_integrator), intent(inout) :: integrator
        procedure(harmonic_ode_fcn), intent(in), pointer :: fcn
        real(real64), intent(in), dimension(:) :: freq
        real(real64), intent(inout), dimension(:) :: xo
        integer(int32), intent(in) :: ncycle, scycle
        logical, intent(in) :: status
        real(real64), intent(out), dimension(:,:) :: amp
        real(real64), intent(out), dimension(:) :: work1, work2
        class(errors), intent(inout) :: err

        ! Local Variables
        type(ode_helper) :: ode
        procedure(ode_fcn), pointer :: fcnptr
        integer(int32) :: i, nfreq, neqn, npts, index
        real(real64) :: currentFrequency, tstart, time(2)
        real(real64), allocatable, dimension(:,:) :: rst

        ! Initialization
        nfreq = size(freq)
        neqn = size(xo)

        ! Set up the integrator
        fcnptr => eom
        call ode%define_equations(neqn, fcnptr)

        ! Cycle over each frequency.  This must be done in a serial manner
        ! as each solution segment for a given frequency relies upon the
        ! solution of the prior step to establish its initial conditions.
        do i = 1, nfreq
            ! Establish the current frequency
            currentFrequency = freq(i)

            ! Reset the integrator such that it is good to go for this round
            call integrator%reset()

            ! Establish the solution time vector
            time = [0.0d0, 2.0d0 * pi * ncycle / currentFrequency]

            ! Perform the integration
            rst = integrator%integrate(ode, time, xo, err)
            if (err%has_error_occurred()) return    ! Exit if an error was encountered

            ! Determine the region to search for max/min values
            tstart = 2.0d0 * pi * (ncycle - scycle) / currentFrequency
            index = find(rst(:,1), tstart)

            ! Search over the desired range for the max and min values
            npts = size(rst, 1)
            call find_extremes(rst(index:npts, 2:neqn+1), work1, work2)

            ! Compute the amplitude
            amp(i,:) = 0.5d0 * (work1 - work2)

            ! Establish the new set of initial conditions
            xo = rst(npts, 2:neqn+1)

            ! Display a status update?
            if (status) then
                print '(AI0AI0AES11.4A)', "Iteration: ", i, " of ", nfreq, &
                    ", Frequency: ", currentFrequency, " rad/s."
            end if
        end do


    contains
        ! Actual routine sent to the integrator
        subroutine eom(t, x, dxdt)
            ! Arguments
            real(real64), intent(in) :: t
            real(real64), intent(in), dimension(:) :: x
            real(real64), intent(out), dimension(:) :: dxdt

            ! Call to the user routine
            call fcn(t, x, currentFrequency, dxdt)
        end subroutine
    end subroutine

! ------------------------------------------------------------------------------
    ! Finds the index of the first value in x to be >= to lim.
    pure function find(x, lim) result(ind)
        ! Arguments
        real(real64), intent(in), dimension(:) :: x
        real(real64), intent(in) :: lim
        integer(int32) :: ind

        ! Local Variables
        integer(int32) :: i, n

        ! Process
        ind = 0
        n = size(x)
        do i = 1, n
            if (x(i) >= lim) then
                ind = i
                exit
            end if
        end do
    end function

! ------------------------------------------------------------------------------
    ! Finds the maximum and minimum values in each column of x.
    subroutine find_extremes(x, big, small)
        ! Arguments
        real(real64), intent(in), dimension(:,:) :: x
        real(real64), intent(out), dimension(:) :: big, small

        ! Process
        integer(int32) :: i, j, m, n

        ! Initialization
        m = size(x, 1)
        n = size(x, 2)

        ! Quick Return
        if (m == 0 .or. n == 0) return

        ! Process
        do j = 1, n
            big(j) = x(1,j)
            small(j) = x(1,j)
        end do
        do j = 1, n
            do i = 2, m
                if (x(i,j) > big(j)) big(j) = x(i,j)
                if (x(i,j) < small(j)) small(j) = x(i,j)
            end do
        end do
    end subroutine

! ------------------------------------------------------------------------------

end submodule
