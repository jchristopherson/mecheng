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
    public :: LTI

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

    !> @brief Defines a means of describing a continuous-time linear 
    !! time invariant (LTI) system.
    type :: LTI
        !> @brief The numerator.
        type(polynomial), public :: numerator
        !> @brief The denominator.
        type(polynomial), public :: denominator
    contains
        procedure, public :: compute_zeros => lti_get_zeros
        procedure, public :: compute_poles => lti_get_poles
    end type

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
    end interface

end module
