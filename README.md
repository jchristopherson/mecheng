# mecheng
This library provides a collection of engineering calculations I use on a regular basis.

## Comments
This library is currently a work-in-progress as I consolidate various calculation tools into a single location.


## Kinematics Example 1
The following example illustrates the use of quaternions, and compares the results to traditional rotation matrices.

```fortran
program example
    use, intrinsic :: iso_fortran_env, only : real64, int32
    use kinematics
    use constants
    implicit none

    ! Local Variables
    type(quaternion) :: qx, qy, qz, qzyx, qs
    real(real64), dimension(3, 3) :: rx, ry, rz, rzyx, qrzyx
    real(real64), dimension(3) :: s1a, s1b, s1q, s1q2
    real(real64) :: ax, ay, az
    integer(int32) :: i

    ! Initialization
    ax = pi / 4.0d0
    ay = -pi / 3.0d0
    az = pi / 6.0d0
    call qx%from_angle_axis(ax, [1.0d0, 0.0d0, 0.0d0]) ! X Rotation
    call qy%from_angle_axis(ay, [0.0d0, 1.0d0, 0.0d0]) ! Y Rotation
    call qz%from_angle_axis(az, [0.0d0, 0.0d0, 1.0d0]) ! Z Rotation
    rx = rotate_x(ax)
    ry = rotate_y(ay)
    rz = rotate_z(az)
    call random_number(s1a)

    ! Compute a Z-Y-X rotation sequance using both matrices and quaternions
    qzyx = qx * qy * qz
    rzyx = matmul(rx, matmul(ry, rz))

    ! Convert the quaternion to a matrix for display purposes
    call qzyx%normalize()
    qrzyx = qzyx%to_matrix()

    ! Display the rotation matrices
    print '(A)', "Rotation Matrix:"
    do i = 1, 3
        print *, rzyx(i,:)
    end do

    print '(A)', "Quaternion Generated Matrix:"
    do i = 1, 3
        print *, qrzyx(i,:)
    end do

    ! Compute: s1b = Rzyx * s1a
    s1b = matmul(rzyx, s1a)

    ! Apply the transformation via quaternion
    qs = qzyx * s1a * qzyx%conjugate()
    s1q = qs%to_vector()

    ! Another example, using the quaternion transform routine
    s1q2 = qzyx%transform(s1a)

    ! Display the results
    print '(A)', new_line('a') // "s1a:"
    print *, s1a

    print '(A)', "Rzyx * s1a:"
    print *, s1b

    print '(A)', "qzyx * s1a * qzyx':"
    print *, s1q

    print '(A)', "qzyx%transform:"
    print *, s1q2
end program
```
The above program produces the following output.
```text
Rotation Matrix:
  0.43301270189215896      -0.24999999999998507      -0.86602540378447312
 -0.17677669529667200       0.91855865354369426      -0.35355339059324981
  0.88388347648320686       0.30618621784790212       0.35355339059321322
Quaternion Generated Matrix:
  0.43301270189215912      -0.24999999999998501      -0.86602540378447312
 -0.17677669529667206       0.91855865354369437      -0.35355339059324981
  0.88388347648320698       0.30618621784790218       0.35355339059321345

s1a:
  0.99755959009261719       0.56682470761127335       0.96591537549612494
Rzyx * s1a:
 -0.54625745658403368        2.8137964878268695E-003   1.3967830079292356
qzyx * s1a * qzyx':
 -0.54625745658403357        2.8137964878267767E-003   1.3967830079292358
qzyx%transform:
 -0.54625745658403357        2.8137964878267767E-003   1.3967830079292358
```

## Kinematics Example 2
This example illustrates the forward kinematics of a 3 link mechanism.  The
following illustration from Jazar's text "Theory of Applied Robotics, Kinematics, Dynamics, and Control" (Figure 5.5) illustrates the mechanism.
![](images/MechanismExampleJazarText.png?raw=true)
```fortran
program example
    use, intrinsic :: iso_fortran_env, only : real64, int32
    use kinematics
    implicit none

    ! Robot Geometry (Constants)
    real(real64), parameter :: offset1 = 0.0d0
    real(real64), parameter :: offset2 = 3.0d0
    real(real64), parameter :: offset3 = 1.0d0
    real(real64), parameter :: twist1 = 1.5707963267948966d0
    real(real64), parameter :: twist2 = 0.0d0
    real(real64), parameter :: twist3 = 0.0d0

    ! Parameters
    real(real64), parameter :: pi = 3.14159265358979324d0

    ! Local Variables
    real(real64) :: theta1, theta2, theta3, trans1, trans2, trans3
    real(real64), dimension(4, 4) :: T0_1, T1_2, T2_3, T
    real(real64), dimension(4) :: p1, k1
    integer(int32) :: i

    ! Define the joint variables
    theta1 = 0.0d0
    theta2 = 0.0d0
    theta3 = pi / 4.0d0

    trans1 = 0.0d0
    trans2 = -1.0d0
    trans3 = 0.0d0

    ! Construct the matrix defining the first link
    T0_1 = dh_mtx(theta1, trans1, twist1, offset1)
    T1_2 = dh_mtx(theta2, trans2, twist2, offset2)
    T2_3 = dh_mtx(theta3, trans3, twist3, offset3)

    ! Construct the overall linkage transformation matrix
    ! T = T0_1 * T1_2 * T2_3
    T = matmul(T0_1, matmul(T1_2, T2_3))

    ! Display the matrices
    print '(A)', "Link 1 Matrix:"
    do i = 1, 4
        print *, T0_1(i,:)
    end do

    print '(A)', "Link 2 Matrix:"
    do i = 1, 4
        print *, T1_2(i,:)
    end do

    print '(A)', "Link 3 Matrix:"
    do i = 1, 4
        print *, T2_3(i,:)
    end do

    print '(A)', "Overall Matrix:"
    do i = 1, 4
        print *, T(i,:)
    end do

    ! The origin of the end-effector is then located at the following point.
    p1 = matmul(T, [0.0d0, 0.0d0, 0.0d0, 1.0d0])
    print '(A)', "End Effector Location:"
    print *, p1(1:3)

    ! The end-effector z-axis lies along the following vector
    k1 = matmul(T, [0.0d0, 0.0d0, 1.0d0, 1.0d0]) - p1
    print '(A)', "End-Effector Z-Axis:"
    print *, k1(1:3)
end program
```
The above program produces the following output.
```text
Link 1 Matrix:
   1.0000000000000000       -0.0000000000000000        0.0000000000000000        0.0000000000000000
   0.0000000000000000        6.1230317691118863E-017  -1.0000000000000000        0.0000000000000000
   0.0000000000000000        1.0000000000000000        6.1230317691118863E-017   0.0000000000000000
   0.0000000000000000        0.0000000000000000        0.0000000000000000        1.0000000000000000
Link 2 Matrix:
   1.0000000000000000       -0.0000000000000000        0.0000000000000000        3.0000000000000000
   0.0000000000000000        1.0000000000000000       -0.0000000000000000        0.0000000000000000
   0.0000000000000000        0.0000000000000000        1.0000000000000000       -1.0000000000000000
   0.0000000000000000        0.0000000000000000        0.0000000000000000        1.0000000000000000
Link 3 Matrix:
  0.70710678118654757      -0.70710678118654746        0.0000000000000000       0.70710678118654757
  0.70710678118654746       0.70710678118654757       -0.0000000000000000       0.70710678118654746
   0.0000000000000000        0.0000000000000000        1.0000000000000000        0.0000000000000000
   0.0000000000000000        0.0000000000000000        0.0000000000000000        1.0000000000000000
Overall Matrix:
  0.70710678118654757      -0.70710678118654746        0.0000000000000000        3.7071067811865475
   4.3296372853596771E-017   4.3296372853596777E-017  -1.0000000000000000        1.0000000000000000
  0.70710678118654746       0.70710678118654757        6.1230317691118863E-017  0.70710678118654735
   0.0000000000000000        0.0000000000000000        0.0000000000000000        1.0000000000000000
End Effector Location:
   3.7071067811865475        1.0000000000000000       0.70710678118654735
End-Effector Z-Axis:
   0.0000000000000000       -1.0000000000000000        1.1102230246251565E-016
```

## Signals Example 1
```fortran
program example
    use, intrinsic :: iso_fortran_env, only : int32, real64
    use signals
    use constants
    use fplot_core ! Requries the FPLOT library (https://github.com/jchristopherson/fplot)
    implicit none

    ! Parameters
    integer(int32), parameter :: npts = 2000
    integer(int32), parameter :: winsize = 512
    real(real64), parameter :: freq = 512.0d0 ! Signal sampled at 512 Hz

    ! Local Variables
    integer(int32) :: i
    real(real64) :: dt, t(npts), s(npts), sWin(npts)
    complex(real64), allocatable, dimension(:) :: xfrm
    real(real64), allocatable, dimension(:) :: x, f, p, xWin, fWin
    procedure(window_function), pointer :: win
    type(plot_2d) :: plt
    type(plot_data_2d) :: d1, d2, d3
    class(plot_axis), pointer :: xAxis, yAxis, y2Axis

    ! Build the signal
    t(1) = 0.0d0
    dt = 1.0d0 / freq
    do i = 1, npts
        s(i) = 2.0d0 * sin(2.0d0 * pi * 50.0d0 * t(i)) + &
            0.75 * sin(2.0d0 * pi * 120.0d0 * t(i)) + &
            1.25 * sin(2.0d0 * pi * 200.0d0 * t(i))
        sWin(i) = s(i)
        if (i < npts) t(i+1) = t(i) + dt
    end do
    
    ! Plot the signal
    call plt%initialize()
    call plt%set_title("FFT Example - Signal")
    call plt%set_font_size(14)

    xAxis => plt%get_x_axis()
    call xAxis%set_title("Time [sec]")
    
    yAxis => plt%get_y_axis()
    call yAxis%set_title("s(t)")

    call d1%define_data(t, s)
    call d1%set_name("s(t)")
    
    call plt%push(d1)
    call plt%draw()

! ------------------------------------------------------------------------------
    ! Now that the signal has been defined, compute it's FFT, and then display
    ! its magnitude and phase as a function of frequency
    xfrm = rfft(s)  ! Notice, s is modified by this function

    ! We can simply take the absolute value of the resulting complex-valued 
    ! array to obtain the magnitude spectrum of the signal
    x = abs(xfrm)

    ! The phase can be computed by computing the arctangent, and convert from
    ! radians to degrees
    p = atan2(aimag(xfrm), real(xfrm, real64)) * (180.0d0 / pi)

    ! The corresponding frequency values can be determined as follows
    f = frequencies(freq, npts)

! ------------------------------------------------------------------------------
    ! An alternate approach, using a Hamming window
    win => hamming_window
    xWin = signal_magnitude(sWin, win, winsize)
    fWin = frequencies(freq, winsize)
    
! ------------------------------------------------------------------------------
    ! Plot the results.  Amplitude on the primary axis, and phase on the 
    ! secondary axis
    call plt%clear_all()
    call plt%set_use_y2_axis(.true.)

    call plt%set_title("FFT Example")
    call xAxis%set_title("Frequency [Hz]")
    call yAxis%set_title("Signal Magnitude")

    y2Axis => plt%get_y2_axis()
    call y2Axis%set_title("Phase [deg]")

    call d1%define_data(f, x)
    call d1%set_name("Magnitude")
    call d1%set_use_auto_color(.false.)
    call d1%set_line_color(CLR_BLUE)
    call d1%set_line_width(2.0)
    
    call d2%define_data(f, p)
    call d2%set_name("Phase (Y2)")
    call d2%set_use_auto_color(.false.)
    call d2%set_line_color(CLR_GREEN)
    call d2%set_line_width(2.0)
    call d2%set_line_style(LINE_DASHED)
    call d2%set_draw_against_y2(.true.)

    call d3%define_data(fWin, xWin)
    call d3%set_name("Magnitude (Windowed)")
    call d3%set_use_auto_color(.false.)
    call d3%set_line_color(CLR_RED)
    call d3%set_line_width(1.0)
    call d3%set_line_style(LINE_DASH_DOTTED)

    call plt%push(d1)
    call plt%push(d2)
    call plt%push(d3)
    call plt%draw()
end program
```
The above program produces the following plots.
![](images/fft_example_signal.png?raw=true)
![](images/fft_example_spectrum.png?raw=true)

Notice, there is some leakage as the signal amplitudes aren't quite correct in the plot for the non-windowed data.  As is shown, this can be treated with windowing (shown), or using a data set length that would result in better frequency bin alignment (i.e. an integer power of two).

## References
1. Jazar, Reza N., "Theory of Applied Robotics."  New York: Springer, 2007.
2. Rosenberg, Reinhardt M. "Analytical Dynamics of Discrete Systems." New York: Plenum Press, 1977.
