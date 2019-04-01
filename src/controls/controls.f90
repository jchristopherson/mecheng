! controls.f90

! References:
! http://bestune.50megs.com/typeABC.htm
! https://github.com/Hendryputra/PID/blob/master/main.c

module controls
    use iso_fortran_env
    implicit none
    private
    public :: realtime_reset
    public :: realtime_object
    public :: realtime_derivative
    public :: pid

! ******************************************************************************
! CONTROLS_REALTIME.F90
! ------------------------------------------------------------------------------
    !> Defines the structure of a basic realtime object.
    type, abstract :: realtime_object
    private
        ! Update rate, in Hz
        real(real64) :: m_updateRate = 1.024d0
    contains
        !> @brief Resets the realtime_object.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine reset(class(realtime_object) this)
        !! @endcode
        !!
        !! @param[in,out] this The realtime_object object.
        procedure(realtime_reset), public, deferred, pass :: reset
        !> @brief Gets the update rate.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_update_rate(class(realtime_object) this)
        !! @endcode
        !!
        !! @param[in] this The realtime_object.
        !! @return The update rate, in Hz.
        procedure, public :: get_update_rate => rto_get_update_rate
        !> @brief Sets the update rate.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_update_rate(class(realtime_object) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The realtime_object.
        !! @param[in] x The update rate, in Hz.
        procedure, public :: set_update_rate => rto_set_update_rate
    end type

    interface
        !> @brief Resets the state of the realtime object.
        !!
        !! @param[in,out] this The realtime_object.
        subroutine realtime_reset(this)
            import realtime_object
            class(realtime_object), intent(inout) :: this
        end subroutine

        pure module function rto_get_update_rate(this) result(x)
            class(realtime_object), intent(in) :: this
            real(real64) :: x
        end function

        module subroutine rto_set_update_rate(this, x)
            class(realtime_object), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine
    end interface

! ------------------------------------------------------------------------------
    !> @brief Defines a realtime derivative object.
    !!
    !! @par Example
    !! The following example illustrates how to use the realtime_derivative 
    !! object to compute the derivative of a signal.
    !! @code{.f90}
    !! program example
    !!     use iso_fortran_env
    !!     use controls
    !!     use fplot_core
    !!     use constants
    !!     implicit none
    !!
    !!     ! Parameters
    !!     integer(int32), parameter :: npts = 1024
    !!     real(real64), parameter :: fs = 1024.0d0 ! Signal Sample Rate
    !!     real(real64), parameter :: update = 1024.0d0 ! Real Time Object Update Rate
    !!
    !!     ! Local Variables
    !!     integer(int32) :: i
    !!     real(real64) :: dt, t(npts), x(npts), dxdt(npts), ans(npts), omega
    !!     type(realtime_derivative) :: diff
    !!     type(plot_2d) :: plt
    !!     type(plot_data_2d) :: d1, d2
    !!     class(plot_axis), pointer :: xAxis, yAxis
    !!     class(legend), pointer :: lgnd
    !!
    !!     ! Initialization
    !!     x = 0.0d0
    !!     dxdt = 0.0d0
    !!     ans = 0.0d0
    !!     call diff%set_update_rate(update)
    !!
    !!     ! Sample the signal, and compute its derivative
    !!     dt = 1.0d0 / fs
    !!     omega = 2.0d0 * pi * 10.0d0
    !!     do i = 2, npts
    !!         t(i) = t(i-1) + dt
    !!         x(i) = cos(omega * t(i)) * sin(t(i))
    !!         ans(i) = cos(omega * t(i)) * cos(t(i)) - &
    !!             omega * sin(omega * t(i)) * sin(t(i))
    !!         dxdt(i) = diff%evaluate(t(i), x(i))
    !!     end do
    !!
    !!     ! Plot the results
    !!     call plt%initialize()
    !!     call plt%set_font_size(14)
    !!
    !!     lgnd => plt%get_legend()
    !!     call lgnd%set_is_visible(.true.)
    !!     call lgnd%set_horizontal_position(LEGEND_LEFT)
    !!
    !!     xAxis => plt%get_x_axis()
    !!     call xAxis%set_title("t")
    !!
    !!     yAxis => plt%get_y_axis()
    !!     call yAxis%set_title("dx/dt")
    !!
    !!     call d1%set_name("Numerical")
    !!     call d1%set_line_width(2.0)
    !!     call d1%define_data(t, dxdt)
    !!
    !!     call d2%set_name("Analytical")
    !!     call d2%set_line_width(3.0)
    !!     call d2%set_line_style(LINE_DASHED)
    !!     call d2%define_data(t, ans)
    !!
    !!     call plt%push(d1)
    !!     call plt%push(d2)
    !!     call plt%draw()
    !! end program
    !! @endcode
    !! The above program produces the following output
    !! @image html realtime_example_1.png
    type, extends(realtime_object) :: realtime_derivative
    private
        real(real64) :: m_previousSignal = 0.0d0
        real(real64) :: m_previousTime = 0.0d0
    contains
        !> @brief Computes the derivative of a signal.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function evaluate(class(realtime_derivative) this, real(real64) t, real(real64) y)
        !! @endcode
        !!
        !! @param[in,out] this The realtime_derivative object.
        !! @param[in] t The time at which the signal was sampled.
        !! @param[in] y The signal value.
        !! @return The estimate of the derivative.
        procedure, public :: evaluate => rd_evaluate
        !> @brief Resets the controller.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine reset(class(realtime_derivative) this)
        !! @endcode
        !!
        !! @param[in,out] this The realtime_derivative object.
        procedure, public :: reset => rd_reset
    end type

    interface
        module function rd_evaluate(this, t, y) result(dydt)
            class(realtime_derivative), intent(inout) :: this
            real(real64), intent(in) :: t, y
            real(real64) :: dydt
        end function

        module subroutine rd_reset(this)
            class(realtime_derivative), intent(inout) :: this
        end subroutine
    end interface

! ******************************************************************************
! CONTROLS_PID.F90
! ------------------------------------------------------------------------------
    !> @breif Defines a basic PID controller.
    type, extends(realtime_object) :: pid
    private
        !> @brief The proportional gain.
        real(real64), public :: proportional_gain
        !> @brief The integral gain.
        real(real64), public :: integral_gain
        !> @brief The derivative gain.
        real(real64), public :: derivative_gain

        ! PRIVATE VARIABLES
        logical, private :: m_useFilter = .true.
        real(real64), private :: m_upperLimit = 1.0d3
        real(real64), private :: m_lowerLimit = -1.0d3
        real(real64), private :: m_setpoint = 0.0d0
        real(real64), private :: m_previousTime = 0.0d0
        real(real64), private :: m_previousSignal = 0.0d0
        real(real64), private :: m_previousDerivative = 0.0d0
        real(real64), private :: m_previousOutput = 0.0d0
        logical :: m_reset = .true.

    contains
        !> @brief Gets the lower limit on the controller output.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_lower_limit(class(pid) this)
        !! @endcode
        !!
        !! @param[in] x The pid object.
        !! @return The limit value.
        procedure, public :: get_lower_limit => pid_get_lower_limit
        !> @brief Sets the lower limit on the controller output.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_lower_limit(class(pid) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The pid object.
        !! @param[in] x The limit value.
        procedure, public :: set_lower_limit => pid_set_lower_limit
        !> @brief Gets the upper limit on the controller output.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! pure real(real64) function get_upper_limit(class(pid) this)
        !! @endcode
        !!
        !! @param[in] x The pid object.
        !! @return The limit value.
        procedure, public :: get_upper_limit => pid_get_upper_limit
        !> @brief Sets the upper limit on the controller output.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine set_upper_limit(class(pid) this, real(real64) x)
        !! @endcode
        !!
        !! @param[in,out] this The pid object.
        !! @param[in] x The limit value.
        procedure, public :: set_upper_limit => pid_set_upper_limit
        !> @brief Resets the controller.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! subroutine reset(class(pid) this)
        !! @endcode
        !!
        !! @param[in,out] this The pid object.
        procedure, public :: reset => pid_reset
        !> @brief Computes the output of the controller given the input state.
        !!
        !! @par Syntax
        !! @code{.f90}
        !! real(real64) function evaluate(class(pid) this, real(real64) setpoint, real(real64) t, real(real64) y)
        !! @endcode
        !!
        !! @param[in,out] this The pid object.
        !! @param[in] setpoint The target setpoint.
        !! @param[in] t The time at which the signal was sampled.
        !! @param[in] y The system response signal at @p t.
        !! @return The controller output.
        procedure, public :: evaluate => pid_eval_1
    end type

    interface
        pure module function pid_get_lower_limit(this) result(x)
            class(pid), intent(in) :: this
            real(real64) :: x
        end function

        module subroutine pid_set_lower_limit(this, x)
            class(pid), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        pure module function pid_get_upper_limit(this) result(x)
            class(pid), intent(in) :: this
            real(real64) :: x
        end function

        module subroutine pid_set_upper_limit(this, x)
            class(pid), intent(inout) :: this
            real(real64), intent(in) :: x
        end subroutine

        module subroutine pid_reset(this)
            class(pid), intent(inout) :: this
        end subroutine

        module function pid_eval_1(this, setpoint, t, y) result(co)
            class(pid), intent(inout) :: this
            real(real64), intent(in) :: setpoint, y, t
            real(real64) :: co
        end function
    end interface
end module
