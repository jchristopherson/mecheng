! vibrations.f90

module vibrations
    use iso_fortran_env
    use ferror
    use mechanics_constants
    use curvefit_interp
    use linalg_core
    use constants
    use nonlin_polynomials
    implicit none
    private
    public :: modal_information
    public :: compute_poincare_section
    public :: compute_modal_response
    public :: compute_frequency_response
    public :: bode_settings
    public :: pole_zero_settings
    public :: LTI
    public :: state_space
    public :: modal_info_from_poles

    ! TO DO:
    ! - Primary Oscillation Frequency Finder (Base upon an FFT, and return the largest magnitude non-DC frequency)
    ! - Frequency Sweep Type FRF's

    !> @brief Contains modal information such as frequency and mode shape.
    type modal_information
        !> The modal frequency, in Hz.
        real(real64) :: frequency
        !> The mode shape.
        real(real64), allocatable, dimension(:) :: mode_shape
    end type

    !> @brief Compute the frequency response of a linear vibrating system.
    interface compute_frequency_response
        module procedure :: compute_frequency_response_1
        module procedure :: compute_frequency_response_2
        module procedure :: compute_frequency_response_3
    end interface

    !> @brief A type describing BODE plot settings.
    type :: bode_settings
        !> @brief Set to true to unwrap the phase plot; else, false.
        logical :: unwrap_phase
        !> @brief The name of the font to use.
        character(len = :), allocatable :: font_name
        !> @brief The size of the font, in points.
        integer(int32) :: font_size
        !> @brief The window height, in system units.
        integer(int32) :: window_height
        !> @brief The window width, in system units.
        integer(int32) :: window_width
        !> @brief The plot line width.
        real(real32) :: line_width
    end type

    !> @brief A type describing pole-zero plot settings.
    type :: pole_zero_settings
        !> @brief The name of the font to use.
        character(len = :), allocatable :: font_name
        !> @brief The size of the font, in points.
        integer(int32) :: font_size
        !> @brief The plot line width.
        real(real32) :: line_width
        !> @brief The marker scaling factor.
        real(real32) :: marker_size
        !> @brief Show the legend?
        logical :: show_legend
    end type

    !> @brief Defines a means of describing a continuous-time linear 
    !! time invariant (LTI) system by means of a transfer function.
    type :: LTI
        !> @brief The numerator of the transfer function.
        type(polynomial), public :: numerator
        !> @brief The denominator of the transfer function.
        type(polynomial), public :: denominator
    contains
        !> @brief Computes the zeros of the transfer function.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! complex(real64)(:) function compute_zeros(class(LTI) this, optional class (errors) err)
        !! @endcode
        !!
        !! @param[in] this The LTI object.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_INVALID_TRANSFER_FUNCTION_ERROR: Occurs if the transfer function is invalid.
        !!      See validate for more information.
        !!  - NL_OUT_OF_MEMORY_ERROR: Occurs if local memory must be allocated, and
        !!      there is insufficient memory available.
        !!  - NL_CONVERGENCE_ERROR: Occurs if the algorithm failed to converge.
        !! @return A complex-valued array containing the zero values.
        procedure, public :: compute_zeros => lti_get_zeros
        !> @brief Computes the poles of the transfer function.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! complex(real64)(:) function compute_poles(class(LTI) this, optional class (errors) err)
        !! @endcode
        !!
        !! @param[in] this The LTI object.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_INVALID_TRANSFER_FUNCTION_ERROR: Occurs if the transfer function is invalid.
        !!      See validate for more information.
        !!  - NL_OUT_OF_MEMORY_ERROR: Occurs if local memory must be allocated, and
        !!      there is insufficient memory available.
        !!  - NL_CONVERGENCE_ERROR: Occurs if the algorithm failed to converge.
        !! @return A complex-valued array containing the pole values.
        procedure, public :: compute_poles => lti_get_poles
        !> @brief Validates the structure of the transfer function.  To be valid
        !! the transfer function must obey the following rules:
        !!  - Both the numerator and denominator must be valid polynomials.
        !!  - The order of the denominator must be at least one less than the denominator.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure logical function validate(class(LTI) this)
        !! @endcode
        !!
        !! @param[in] this The LTI object.
        !! @return Returns true if the transfer function is structured appropriately; else,
        !!  false.
        procedure, public :: validate => lti_validate
        !> @brief Evaluates the transfer function at the specified frequency.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! complex(real64) function evaluate(class(LTI) this, real(real64) omega)
        !! @endcode
        !!
        !! @param[in] this The LTI object.
        !! @param[in] omega The frequency, in rad/s.
        !! @return The value of the transfer function at @p omega.
        procedure, public :: evaluate => lti_evaluate
        !> @brief Creates a Bode plot of the transfer function.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine bode(class(LTI) this, real(real64) freq(:), optional type(bode_settings) settings, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The LTI object.
        !! @param[in] freq An N-element array containing the frequency values
        !!  at which to evaluate the transfer function, in Hz.
        !! @param[in] settings An optional input that provides a means of 
        !!  controlling the appearance and behavior of the plot.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_INVALID_TRANSFER_FUNCTION_ERROR: Occurs if the transfer function is invalid.
        !!      See validate for more information.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use vibrations
        !!     use constants
        !!     use fplot_core, only : linspace
        !!     implicit none
        !!
        !!     ! Construct the model to represent a mechanical system with
        !!     ! the following properties:
        !!     ! - Natural Frequency: 50 Hz
        !!     ! - Damping Ratio: 0.1
        !!     ! - Sprung Mass: 20 kg
        !!     ! - Force Amplitude: 1 kN
        !!
        !!     ! Model Parameters
        !!     real(real64), parameter :: fn = 5.0d1
        !!     real(real64), parameter :: zeta = 1.0d-1
        !!     real(real64), parameter :: mass = 2.0d1
        !!     real(real64), parameter :: force = 1.0d3
        !!     integer(int32), parameter :: npts = 1000
        !!
        !!     ! Local Variables
        !!     type(LTI) :: sys
        !!     real(real64) :: wn
        !!     real(real64), allocatable, dimension(:) :: freq
        !!
        !!     ! Build the model noting the equation of motion is:
        !!     ! x" + 2 zeta * wn x' + wn**2 = F / m
        !!     wn = 2.0d0 * pi * fn
        !!     call sys%numerator%initialize([force / mass])
        !!     call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])
        !!
        !!     ! Plot the BODE diagram
        !!     freq = linspace(1.0d0, 1.0d2, npts)
        !!     call sys%bode(freq)
        !! end program
        !! @endcode
        !! The above code produces the following output.
        !! @image html lti_bode_1.png
        procedure, public :: bode => lti_bode
        !> @brief Draws a pole-zero plot for the LTI system.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine pole_zero_plot(class(LTI) this, optional type(pole_zero_settings) settings, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The LTI object.
        !! @param[in] settings An optional input that provides a means of 
        !!  controlling the appearance and behavior of the plot.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_INVALID_TRANSFER_FUNCTION_ERROR: Occurs if the transfer function is invalid.
        !!      See validate for more information.
        !!  - NL_OUT_OF_MEMORY_ERROR: Occurs if local memory must be allocated, and
        !!      there is insufficient memory available.
        !!  - NL_CONVERGENCE_ERROR: Occurs if the algorithm failed to converge.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use vibrations
        !!     use constants
        !!     use fplot_core, only : linspace
        !!     implicit none
        !!
        !!     ! Construct the model to represent a mechanical system with
        !!     ! the following properties:
        !!     ! - Natural Frequency: 50 Hz
        !!     ! - Damping Ratio: 0.1
        !!     ! - Sprung Mass: 20 kg
        !!     ! - Force Amplitude: 1 kN
        !!
        !!     ! Model Parameters
        !!     real(real64), parameter :: fn = 5.0d1
        !!     real(real64), parameter :: zeta = 1.0d-1
        !!     real(real64), parameter :: mass = 2.0d1
        !!     real(real64), parameter :: force = 1.0d3
        !!     integer(int32), parameter :: npts = 1000
        !!
        !!     ! Local Variables
        !!     type(LTI) :: sys
        !!     real(real64) :: wn
        !!
        !!     ! Build the model noting the equation of motion is:
        !!     ! x" + 2 zeta * wn x' + wn**2 = F / m
        !!     wn = 2.0d0 * pi * fn
        !!     call sys%numerator%initialize([force / mass])
        !!     call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])
        !!
        !!     ! Plot the pole-zero diagram
        !!     call sys%pole_zero_plot()
        !! end program
        !! @endcode
        !! The above code produces the following output.
        !! @image html lti_pole_zero_1.png
        procedure, public :: pole_zero_plot => lti_pole_zero_plot
        !> @brief Converts the transfer function form of the LTI system to a
        !! state-space representation.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine to_state_space(class(LTI) this, class(state_space) ss, optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The LTI object.
        !! @param[out] ss The state-space model object to populate.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_INVALID_TRANSFER_FUNCTION_ERROR: Occurs if the transfer function is invalid.
        !!      See validate for more information.
        !!
        !! @par Example
        !! The following example illustrates how to convert an LTI transfer function model
        !! into a state-space model, and then integrate to obtain the time-domain response
        !! of the system.  The result is compared with directly integrating the equations
        !! of motion.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use vibrations
        !!     use constants
        !!     use fplot_core
        !!     use integral_core
        !!     implicit none
        !!
        !!     ! Construct the model to represent a mechanical system with
        !!     ! the following properties:
        !!     ! - Natural Frequency: 50 Hz
        !!     ! - Damping Ratio: 0.1
        !!     ! - Sprung Mass: 20 kg
        !!     ! - Force Amplitude: 1 kN
        !!     ! - Forcing Frequency: 20 Hz
        !!     ! - Sampling Interval: 1 ms
        !!
        !!     ! Model Parameters
        !!     real(real64), parameter :: fn = 5.0d1
        !!     real(real64), parameter :: zeta = 1.0d-1
        !!     real(real64), parameter :: mass = 2.0d1
        !!     real(real64), parameter :: force = 1.0d3
        !!     real(real64), parameter :: freq = 2.0d1
        !!     real(real64), parameter :: dt = 1.0d-3
        !!     integer(int32), parameter :: npts = 1000
        !!
        !!     ! Local Variables
        !!     type(LTI) :: sys
        !!     type(state_space) :: mdl
        !!     integer(int32) :: i
        !!     real(real64) :: wn, xo(2), t(2)
        !!     real(real64), allocatable, dimension(:,:) :: mdlOut, dirOut
        !!     type(ode_helper) :: ssObj, dirObj
        !!     type(ode_auto) :: integrator
        !!     procedure(ode_fcn), pointer :: ssFcn, dirFcn
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: ds1, ds2
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     class(legend), pointer :: lgnd
        !!
        !!     ! Build the model noting the equation of motion is:
        !!     ! x" + 2 zeta * wn x' + wn**2 = F / m
        !!     wn = 2.0d0 * pi * fn
        !!     call sys%numerator%initialize([force / mass])
        !!     call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])
        !!
        !!     ! Convert to a state-space model
        !!     call sys%to_state_space(mdl)
        !!
        !!     ! Display the state space matrices
        !!     print '(AI0AI0A)', "A (", size(mdl%A, 1), "x", size(mdl%A, 2), "): "
        !!     do i = 1, size(mdl%A, 1)
        !!         print *, mdl%A(i,:)
        !!     end do
        !!
        !!     print *, ""
        !!     print '(AI0AI0A)', "B (", size(mdl%B, 1), "x", size(mdl%B, 2), "): "
        !!     do i = 1, size(mdl%B, 1)
        !!         print *, mdl%B(i,:)
        !!     end do
        !!
        !!     print *, ""
        !!     print '(AI0AI0A)', "C (", size(mdl%C, 1), "x", size(mdl%C, 2), "): "
        !!     do i = 1, size(mdl%C, 1)
        !!         print *, mdl%C(i,:)
        !!     end do
        !!
        !!     print *, ""
        !!     print '(AI0AI0A)', "D (", size(mdl%D, 1), "x", size(mdl%D, 2), "): "
        !!     do i = 1, size(mdl%D, 1)
        !!         print *, mdl%D(i,:)
        !!     end do
        !!
        !!     ! Set up the integrator
        !!     ssFcn => state_space_model
        !!     call ssObj%define_equations(2, ssFcn)
        !!
        !!     dirFcn => direct_equations
        !!     call dirObj%define_equations(2, dirFcn)
        !!
        !!     ! Define the initial conditions
        !!     t = [0.0d0, 1.0d0]
        !!     xo = [0.0d0, 0.0d0]
        !!
        !!     ! Perform the integration
        !!     mdlOut = integrator%integrate(ssObj, t, xo)
        !!
        !!     call integrator%reset()
        !!     dirOut = integrator%integrate(dirObj, t, xo)
        !!
        !!     ! Plot the solution
        !!     call plt%initialize()
        !!     call plt%set_font_size(11)
        !!     call plt%set_font_name("Arial")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     yAxis => plt%get_y_axis()
        !!
        !!     call xAxis%set_title("Time (s)")
        !!     call yAxis%set_title("Position (mm)")
        !!
        !!     lgnd => plt%get_legend()
        !!     call lgnd%set_is_visible(.true.)
        !!     call lgnd%set_draw_border(.false.)
        !!     call lgnd%set_draw_inside_axes(.false.)
        !!     call lgnd%set_horizontal_position(LEGEND_CENTER)
        !!
        !!     call ds1%set_name("State-Space Model")
        !!     call ds1%define_data(mdlOut(:,1), 1.0d3 * mdlOut(:,2))
        !!     call plt%push(ds1)
        !!
        !!     call ds2%set_name("Direct")
        !!     call ds2%define_data(dirOut(:,1), 1.0d3 * dirOut(:,2))
        !!     call ds2%set_draw_line(.false.)
        !!     call ds2%set_draw_markers(.true.)
        !!     call plt%push(ds2)
        !!
        !!     call plt%draw()
        !!
        !! contains
        !!     ! The routine utilizing the state-space model
        !!     subroutine state_space_model(t, x, dxdt)
        !!         ! Arguments
        !!         real(real64), intent(in) :: t
        !!         real(real64), intent(in), dimension(:) :: x
        !!         real(real64), intent(out), dimension(:) :: dxdt
        !!
        !!         ! Local Variables
        !!         real(real64) :: u(1)
        !!         real(real64), allocatable, dimension(:) :: y
        !!
        !!         ! Define the forcing function
        !!         u(1) = sin(2.0d0 * pi * freq * t)
        !!
        !!         ! Evaluate the state space model
        !!         dxdt = x    ! Use to store the current state, the model will update
        !!         y = mdl%evaluate(dxdt, u)
        !!     end subroutine
        !!
        !!     ! Direct application of the equations
        !!     subroutine direct_equations(t, x, dxdt)
        !!         ! Arguments
        !!         real(real64), intent(in) :: t
        !!         real(real64), intent(in), dimension(:) :: x
        !!         real(real64), intent(out), dimension(:) :: dxdt
        !!
        !!         ! Define the output
        !!         dxdt(1) = x(2)
        !!         dxdt(2) = (force / mass) * sin(2.0d0 * pi * freq * t) - &
        !!             (2.0d0 * zeta * wn * x(2) + wn**2 * x(1))
        !!     end subroutine
        !! end program
        !! @endcode
        !! The above program produces the following outputs:
        !! @code{.txt}
        !! A (2x2):
        !! -62.831853071795869        1.0000000000000000
        !! -98696.044010893587        0.0000000000000000
        !!
        !! B (2x1):
        !! 0.0000000000000000
        !! 50.000000000000000
        !!
        !! C (1x2):
        !! 1.0000000000000000        0.0000000000000000
        !!
        !! D (1x1):
        !! 0.0000000000000000
        !! @endcode
        !! @html image lti_state_space_1.png
        procedure, public :: to_state_space => lti_to_ss
    end type

    !> @brief Defines a means of describing a time-invariant system by means of 
    !! a state-space representation.
    !!
    !! @par Description
    !! A continuous-time dynamic oscillator can be described by the state-space equations
    !! @par \f$ \frac{dx}{dt} = A x + B u \f$
    !! @par \f$ y = C x + D u \f$,
    !! @par where x is the state vector, and u is the forcing function vector.
    !! @par The state space representation can be related to the transfer function
    !! representation by
    !! @par \f$ H(s) = C(s I - A)^{-1} B + D.
    !! @par In a discrete-time system, the a dynamic oscillator can be described by
    !! the state-space equations
    !! @par \f$ x(k+1) = A x(k) + B u(k) \f$
    !! @par \f$ y(k) = C x(k) + D u(k) \f$,
    !! @par where x is the state vector, and u is the forcing function vector.
    !! @par The state space representation can be related to the transfer function
    !! representation by
    !! @par \f$ H(z) = C(z I - A)^{-1} B + D.
    type :: state_space
    private
        !> @brief The state matrix.  If the system has p inputs, q outputs,
        !! and is described by n state variables, this matrix is then
        !! n-by-n in size.
        real(real64), public, allocatable, dimension(:,:) :: A
        !! @brief The input-to-state matrix.  If the system has p inputs,
        !! q outputs, and is described by n state variables, this matrix is
        !! then n-by-p in size.
        real(real64), public, allocatable, dimension(:,:) :: B
        !> @brief The state-to-output matrix.  If the system has p inputs,
        !! q outputs, and is described by n state variables, this matrix is
        !! then q-by-n in size.
        real(real64), public, allocatable, dimension(:,:) :: C
        !> @brief The feedthrough matrix.  If the system has p inputs,
        !! q outputs, and is described by n state variables, this matrix is
        !! then q-by-p in size.
        real(real64), public, allocatable, dimension(:,:) :: D
    contains
        !> @brief Evaluates the state-space model.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64)(:) function evaluate(class(state_space) this, real(real64) x(:), real(real64) u(:), optional class(errors) err)
        !! @endcode
        !!
        !! @param[in] this The state-space model.
        !! @param[in,out] x On input, the state vector.  On output, for discrete systems,
        !!  the updated state vector; for continuous systems, the derivative of the state
        !!  vector.
        !! @param[in] u The forcing function vector.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_ARRAY_SIZE_ERROR: Occurs if any of the input array dimensions are not 
        !!      compatible with the state-space model matrices.
        !!
        !! @par Example
        !! The following example illustrates how to convert an LTI transfer function model
        !! into a state-space model, and then integrate to obtain the time-domain response
        !! of the system.  The result is compared with directly integrating the equations
        !! of motion.
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use vibrations
        !!     use constants
        !!     use fplot_core
        !!     use integral_core
        !!     implicit none
        !!
        !!     ! Construct the model to represent a mechanical system with
        !!     ! the following properties:
        !!     ! - Natural Frequency: 50 Hz
        !!     ! - Damping Ratio: 0.1
        !!     ! - Sprung Mass: 20 kg
        !!     ! - Force Amplitude: 1 kN
        !!     ! - Forcing Frequency: 20 Hz
        !!     ! - Sampling Interval: 1 ms
        !!
        !!     ! Model Parameters
        !!     real(real64), parameter :: fn = 5.0d1
        !!     real(real64), parameter :: zeta = 1.0d-1
        !!     real(real64), parameter :: mass = 2.0d1
        !!     real(real64), parameter :: force = 1.0d3
        !!     real(real64), parameter :: freq = 2.0d1
        !!     real(real64), parameter :: dt = 1.0d-3
        !!     integer(int32), parameter :: npts = 1000
        !!
        !!     ! Local Variables
        !!     type(LTI) :: sys
        !!     type(state_space) :: mdl
        !!     integer(int32) :: i
        !!     real(real64) :: wn, xo(2), t(2)
        !!     real(real64), allocatable, dimension(:,:) :: mdlOut, dirOut
        !!     type(ode_helper) :: ssObj, dirObj
        !!     type(ode_auto) :: integrator
        !!     procedure(ode_fcn), pointer :: ssFcn, dirFcn
        !!     type(plot_2d) :: plt
        !!     type(plot_data_2d) :: ds1, ds2
        !!     class(plot_axis), pointer :: xAxis, yAxis
        !!     class(legend), pointer :: lgnd
        !!
        !!     ! Build the model noting the equation of motion is:
        !!     ! x" + 2 zeta * wn x' + wn**2 = F / m
        !!     wn = 2.0d0 * pi * fn
        !!     call sys%numerator%initialize([force / mass])
        !!     call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])
        !!
        !!     ! Convert to a state-space model
        !!     call sys%to_state_space(mdl)
        !!
        !!     ! Display the state space matrices
        !!     print '(AI0AI0A)', "A (", size(mdl%A, 1), "x", size(mdl%A, 2), "): "
        !!     do i = 1, size(mdl%A, 1)
        !!         print *, mdl%A(i,:)
        !!     end do
        !!
        !!     print *, ""
        !!     print '(AI0AI0A)', "B (", size(mdl%B, 1), "x", size(mdl%B, 2), "): "
        !!     do i = 1, size(mdl%B, 1)
        !!         print *, mdl%B(i,:)
        !!     end do
        !!
        !!     print *, ""
        !!     print '(AI0AI0A)', "C (", size(mdl%C, 1), "x", size(mdl%C, 2), "): "
        !!     do i = 1, size(mdl%C, 1)
        !!         print *, mdl%C(i,:)
        !!     end do
        !!
        !!     print *, ""
        !!     print '(AI0AI0A)', "D (", size(mdl%D, 1), "x", size(mdl%D, 2), "): "
        !!     do i = 1, size(mdl%D, 1)
        !!         print *, mdl%D(i,:)
        !!     end do
        !!
        !!     ! Set up the integrator
        !!     ssFcn => state_space_model
        !!     call ssObj%define_equations(2, ssFcn)
        !!
        !!     dirFcn => direct_equations
        !!     call dirObj%define_equations(2, dirFcn)
        !!
        !!     ! Define the initial conditions
        !!     t = [0.0d0, 1.0d0]
        !!     xo = [0.0d0, 0.0d0]
        !!
        !!     ! Perform the integration
        !!     mdlOut = integrator%integrate(ssObj, t, xo)
        !!
        !!     call integrator%reset()
        !!     dirOut = integrator%integrate(dirObj, t, xo)
        !!
        !!     ! Plot the solution
        !!     call plt%initialize()
        !!     call plt%set_font_size(11)
        !!     call plt%set_font_name("Arial")
        !!
        !!     xAxis => plt%get_x_axis()
        !!     yAxis => plt%get_y_axis()
        !!
        !!     call xAxis%set_title("Time (s)")
        !!     call yAxis%set_title("Position (mm)")
        !!
        !!     lgnd => plt%get_legend()
        !!     call lgnd%set_is_visible(.true.)
        !!     call lgnd%set_draw_border(.false.)
        !!     call lgnd%set_draw_inside_axes(.false.)
        !!     call lgnd%set_horizontal_position(LEGEND_CENTER)
        !!
        !!     call ds1%set_name("State-Space Model")
        !!     call ds1%define_data(mdlOut(:,1), 1.0d3 * mdlOut(:,2))
        !!     call plt%push(ds1)
        !!
        !!     call ds2%set_name("Direct")
        !!     call ds2%define_data(dirOut(:,1), 1.0d3 * dirOut(:,2))
        !!     call ds2%set_draw_line(.false.)
        !!     call ds2%set_draw_markers(.true.)
        !!     call plt%push(ds2)
        !!
        !!     call plt%draw()
        !!
        !! contains
        !!     ! The routine utilizing the state-space model
        !!     subroutine state_space_model(t, x, dxdt)
        !!         ! Arguments
        !!         real(real64), intent(in) :: t
        !!         real(real64), intent(in), dimension(:) :: x
        !!         real(real64), intent(out), dimension(:) :: dxdt
        !!
        !!         ! Local Variables
        !!         real(real64) :: u(1)
        !!         real(real64), allocatable, dimension(:) :: y
        !!
        !!         ! Define the forcing function
        !!         u(1) = sin(2.0d0 * pi * freq * t)
        !!
        !!         ! Evaluate the state space model
        !!         dxdt = x    ! Use to store the current state, the model will update
        !!         y = mdl%evaluate(dxdt, u)
        !!     end subroutine
        !!
        !!     ! Direct application of the equations
        !!     subroutine direct_equations(t, x, dxdt)
        !!         ! Arguments
        !!         real(real64), intent(in) :: t
        !!         real(real64), intent(in), dimension(:) :: x
        !!         real(real64), intent(out), dimension(:) :: dxdt
        !!
        !!         ! Define the output
        !!         dxdt(1) = x(2)
        !!         dxdt(2) = (force / mass) * sin(2.0d0 * pi * freq * t) - &
        !!             (2.0d0 * zeta * wn * x(2) + wn**2 * x(1))
        !!     end subroutine
        !! end program
        !! @endcode
        !! The above program produces the following outputs:
        !! @code{.txt}
        !! A (2x2):
        !! -62.831853071795869        1.0000000000000000
        !! -98696.044010893587        0.0000000000000000
        !!
        !! B (2x1):
        !! 0.0000000000000000
        !! 50.000000000000000
        !!
        !! C (1x2):
        !! 1.0000000000000000        0.0000000000000000
        !!
        !! D (1x1):
        !! 0.0000000000000000
        !! @endcode
        !! @html image lti_state_space_1.png
        procedure, public :: evaluate => ss_eval
        generic, public :: evaluate_transfer_function => ss_tf_eval, ss_tf_eval_array

        procedure :: ss_tf_eval
        procedure :: ss_tf_eval_array
    end type

    ! TO DO:
    ! - Create a state-space model type

! ******************************************************************************
! VIBRATIONS_POINCARE.F90
! ------------------------------------------------------------------------------
    interface
        !> @brief Computes the values of the dependent variable and its first 
        !! derivative at even intervals for use in generating a Poincare section.
        !!
        !! @param[in] t An n-element array containing the independent variable.
        !! @param[in] x An n-element array containing the dependent variable 
        !!  values corresponding to the values in @p t.
        !! @param[in] v An n-element array containing the first derivative values
        !!  of @p x with respect to @p t.
        !! @param[in] period The sampling period.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_ARRAY_SIZE_ERROR: Occurs if @p x and @p v are not the same size.
        !!  - MECH_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory
        !!      available.
        !!  - CF_NONMONOTONIC_ARRAY_ERROR: Occurs if @p x is not monotonically
        !!      increasing or decreasing.
        !!  - MECH_INVALID_INPUT_ERROR: Occurs if @p period is less than machine 
        !!      precision.
        !!
        !! @return A matrix with two columns.  Column 1 contains the sampled
        !!  dependent variable values, and column 2 contains the corresponding
        !!  first derivative values.
        module function compute_poincare_section(t, x, v, period, err) result(rst)
            real(real64), intent(in), dimension(:) :: t, x, v
            real(real64), intent(in) :: period
            class(errors), intent(inout), optional, target :: err
            real(real64), allocatable, dimension(:,:) :: rst
        end function
    end interface

! ******************************************************************************
! VIBRATIONS_MODAL.F90
! ------------------------------------------------------------------------------
    interface
        !> @brief Computes the modal response of a dynamic system.
        !!
        !! @param[in] m An N-by-N mass matrix.
        !! @param[in] k An N-by-N stiffness matrix.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_ARRAY_SIZE_ERROR: Occurs if either input matrix is not square,
        !!      or if the two matrices are sized differently.
        !!  - LA_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
        !!      available.
        !!  - LA_CONVERGENCE_ERROR: Occurs if the eigen solver fails to converge.
        !!
        !! @return A list of modal information.
        module function compute_modal_response(m, k, err) result(rst)
            real(real64), intent(in), dimension(:,:) :: m, k
            class(errors), intent(inout), optional, target :: err
            type(modal_information), allocatable, dimension(:) :: rst
        end function

        !> @brief Compute the frequency response of a linear vibrating system.
        !!
        !! @param[in] m An N-by-N mass matrix.
        !! @param[in] k An N-by-N stiffness matrix.
        !! @param[in] b An N-by-N damping matrix.
        !! @param[in] f An M-by-N forcing function matrix with each of the M
        !!  rows representing a discrete excitation frequency.
        !! @param[in] freq An M-element array containing the frequency points
        !!  at which to analyze the system.  The units are expected to be rad/s.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_ARRAY_SIZE_ERROR: Occurs if any of the input matrices are not 
        !!      square, or if the matrices are sized differently.
        !!  - MECH_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
        !!      available.
        !!  - LA_CONVERGENCE_ERROR: Occurs if the eigen solver fails to converge.
        !!
        !! @return An M-by-N element matrix where each of the N results
        !!  can be written for all M frequency points of interest.
        module function compute_frequency_response_1(m, k, b, f, freq, err) result(rsp)
            ! Arguments
            real(real64), intent(in), dimension(:,:) :: m, k, b
            complex(real64), intent(in), dimension(:,:) :: f
            real(real64), intent(in), dimension(:) :: freq
            class(errors), intent(inout), optional, target :: err
            complex(real64), allocatable, dimension(:,:) :: rsp
        end function

        !> @brief Compute the frequency response of a linear vibrating system.
        !!
        !! @param[in] m An N-by-N mass matrix.
        !! @param[in] k An N-by-N stiffness matrix.
        !! @param[in] zeta An N-element array containing a damping ratio for
        !!  each of the N equations.
        !! @param[in] f An M-by-N forcing function matrix with each of the M
        !!  rows representing a discrete excitation frequency.
        !! @param[in] freq An M-element array containing the frequency points
        !!  at which to analyze the system.  The units are expected to be rad/s.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_ARRAY_SIZE_ERROR: Occurs if any of the input matrices are not 
        !!      square, or if the matrices are sized differently.
        !!  - MECH_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
        !!      available.
        !!  - LA_CONVERGENCE_ERROR: Occurs if the eigen solver fails to converge.
        !!
        !! @return An M-by-N element matrix where each of the N results
        !!  can be written for all M frequency points of interest.
        module function compute_frequency_response_2(m, k, zeta, f, freq, err) result(rsp)
            ! Arguments
            real(real64), intent(in), dimension(:,:) :: m, k
            complex(real64), intent(in), dimension(:,:) :: f
            real(real64), intent(in), dimension(:) :: zeta, freq
            class(errors), intent(inout), optional, target :: err
            complex(real64), allocatable, dimension(:,:) :: rsp
        end function

        !> @brief Compute the frequency response of a linear vibrating system.
        !!
        !! @param[in] m An N-by-N mass matrix.
        !! @param[in] k An N-by-N stiffness matrix.
        !! @param[in] zeta A damping ratio for each of the N equations.
        !! @param[in] f An M-by-N forcing function matrix with each of the M
        !!  rows representing a discrete excitation frequency.
        !! @param[in] freq An M-element array containing the frequency points
        !!  at which to analyze the system.  The units are expected to be rad/s.
        !! @param[in,out] err An optional errors-based object that if provided can be
        !!  used to retrieve information relating to any errors encountered during
        !!  execution.  If not provided, a default implementation of the errors
        !!  class is used internally to provide error handling.  Possible errors and
        !!  warning messages that may be encountered are as follows.
        !!  - MECH_ARRAY_SIZE_ERROR: Occurs if any of the input matrices are not 
        !!      square, or if the matrices are sized differently.
        !!  - MECH_OUT_OF_MEMORY_ERROR: Occurs if there is insufficient memory 
        !!      available.
        !!  - LA_CONVERGENCE_ERROR: Occurs if the eigen solver fails to converge.
        !!
        !! @return An M-by-N element matrix where each of the N results
        !!  can be written for all M frequency points of interest.
        module function compute_frequency_response_3(m, k, zeta, f, freq, err) result(rsp)
            ! Arguments
            real(real64), intent(in), dimension(:,:) :: m, k
            real(real64), intent(in) :: zeta
            complex(real64), intent(in), dimension(:,:) :: f
            real(real64), intent(in), dimension(:) :: freq
            class(errors), intent(inout), optional, target :: err
            complex(real64), allocatable, dimension(:,:) :: rsp
        end function
    end interface

! ******************************************************************************
! VIBRATIONS_LTI.F90
! ------------------------------------------------------------------------------
    interface
        module function lti_get_zeros(this, err) result(z)
            class(LTI), intent(in) :: this
            class(errors), intent(inout), optional, target :: err
            complex(real64), allocatable, dimension(:) :: z
        end function

        module function lti_get_poles(this, err) result(p)
            class(LTI), intent(in) :: this
            class(errors), intent(inout), optional, target :: err
            complex(real64), allocatable, dimension(:) :: p
        end function

        pure module function lti_validate(this) result(x)
            class(LTI), intent(in) :: this
            logical :: x
        end function

        elemental module function lti_evaluate(this, omega) result(x)
            class(LTI), intent(in) :: this
            real(real64), intent(in) :: omega
            complex(real64) :: x
        end function

        module subroutine lti_bode(this, freq, settings, err)
            class(LTI), intent(in) :: this
            real(real64), intent(in), dimension(:) :: freq
            type(bode_settings), intent(in), optional :: settings
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine lti_pole_zero_plot(this, settings, err)
            class(LTI), intent(in) :: this
            type(pole_zero_settings), intent(in), optional :: settings
            class(errors), intent(inout), optional, target :: err
        end subroutine

        module subroutine lti_to_ss(this, ss, err)
            class(LTI), intent(in) :: this
            class(state_space), intent(out) :: ss
            class(errors), intent(inout), optional, target :: err
        end subroutine
    end interface

    interface
        !> @brief Computes the resonant frequency and damping ratio information
        !! for a system given the complex-valued pole information for the system.
        !!
        !! @param[in] poles An N-element array containing the system poles.
        !! @return An M-by-2 matrix containing the natural frequency values, in Hz,
        !!  in the first column, and the damping ratio values in the second column.
        !!  M is less than or equal to N noting that complex-conjugate pairs are
        !!  representative of only one resonant mode.
        !!
        !! @par Example
        !! @code{.f90}
        !! program example
        !!     use iso_fortran_env
        !!     use vibrations
        !!     use constants
        !!     implicit none
        !!
        !!     ! Construct the model to represent a mechanical system with
        !!     ! the following properties:
        !!     ! - Natural Frequency: 50 Hz
        !!     ! - Damping Ratio: 0.1
        !!     ! - Sprung Mass: 20 kg
        !!     ! - Force Amplitude: 1 kN
        !!
        !!     ! Model Parameters
        !!     real(real64), parameter :: fn = 5.0d1
        !!     real(real64), parameter :: zeta = 1.0d-1
        !!     real(real64), parameter :: mass = 2.0d1
        !!     real(real64), parameter :: force = 1.0d3
        !!     integer(int32), parameter :: npts = 1000
        !!
        !!     ! Local Variables
        !!     type(LTI) :: sys
        !!     real(real64) :: wn
        !!     complex(real64), allocatable, dimension(:) :: poles
        !!     real(real64), allocatable, dimension(:,:) :: info
        !!
        !!     ! Build the model noting the equation of motion is:
        !!     ! x" + 2 zeta * wn x' + wn**2 = F / m
        !!     wn = 2.0d0 * pi * fn
        !!     call sys%numerator%initialize([force / mass])
        !!     call sys%denominator%initialize([wn**2, 2.0d0 * zeta * wn, 1.0d0])
        !!
        !!     ! Compute the system poles
        !!     poles = sys%compute_poles()
        !!
        !!     ! Compute the modal information for the system
        !!     info = modal_info_from_poles(poles)
        !!
        !!     ! Display the resonant frequency and damping ratio
        !!     print '(AI0)', "# of modes found: ", size(info, 1)
        !!     print '(AF5.2A)', "Natural Frequency: ", info(1,1), " Hz"
        !!     print '(AF4.2)', "Damping Ratio: ", info(1, 2)
        !! end program
        !! @endcode
        !! The above program produces the following output.
        !! @code{.txt}
        !! # of modes found: 1
        !! Natural Frequency: 50.00 Hz
        !! Damping Ratio: 0.10
        !! @endcode
        pure module function modal_info_from_poles(poles) result(x)
            complex(real64), intent(in), dimension(:) :: poles
            real(real64), allocatable, dimension(:,:) :: x
        end function
    end interface

! ******************************************************************************
! VIBRATIONS_SS.F90
! ------------------------------------------------------------------------------
    interface
        module function ss_eval(this, x, u, err) result(y)
            class(state_space), intent(in) :: this
            real(real64), intent(inout), dimension(:) :: x
            real(real64), intent(in), dimension(:) :: u
            class(errors), intent(inout), optional, target :: err
            real(real64), allocatable, dimension(:) :: y
        end function

        module function ss_tf_eval(this, s, err) result(h)
            class(state_space), intent(in) :: this
            complex(real64), intent(in) :: s
            class(errors), intent(inout), optional, target :: err
            complex(real64), allocatable, dimension(:,:) :: h
        end function

        module function ss_tf_eval_array(this, s, err) result(h)
            class(state_space), intent(in) :: this
            complex(real64), intent(in), dimension(:) :: s
            class(errors), intent(inout), optional, target :: err
            complex(real64), allocatable, dimension(:,:,:) :: h
        end function
    end interface

end module
