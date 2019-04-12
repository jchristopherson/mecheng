# FORWARD_KINEMATICS_EXAMPLE
add_executable(forward_kinematics_example forward_kinematics_example.f90)
target_link_libraries(forward_kinematics_example mecheng)

# QUATERNION EXAMPLE
add_executable(quaternion_example quaternion_example.f90)
target_link_libraries(quaternion_example mecheng)

# FFT EXAMPLE
add_executable(fft_example fft_example.f90)
target_link_libraries(fft_example mecheng)

# SIGNALS EXAMPLE
add_executable(signals_example signals_example.f90)
target_link_libraries(signals_example mecheng)

# DIFFERENTIATION EXAMPLE
add_executable(diff_example diff_example.f90)
target_link_libraries(diff_example mecheng)

# SIGNALS EXAMPLE #2
add_executable(signals_example_2 signals_example_2.f90)
target_link_libraries(signals_example_2 mecheng)

# INTEGRATION EXAMPLE
add_executable(integrate_example integrate_example.f90)
target_link_libraries(integrate_example mecheng)

# HBM FILE READ EXAMPLE
add_executable(hbm_file_read_example hbm_file_read_example.f90)
target_link_libraries(hbm_file_read_example mecheng)

# PLANE GEOMETRY EXAMPLE 1
add_executable(plane_geometry_example_1 plane_geometry_example_1.f90)
target_link_libraries(plane_geometry_example_1 mecheng)

# LINE EXAMPLE 1
add_executable(line_example_1 line_example_1.f90)
target_link_libraries(line_example_1 mecheng)

# LINE-PLANE EXAMPLE 1
add_executable(plane_line_example_1 plane_line_example_1.f90)
target_link_libraries(plane_line_example_1 mecheng)

# LINE EXAMPLE 2
add_executable(line_example_2 line_example_2.f90)
target_link_libraries(line_example_2 mecheng)

# ODE EXAMPLE 1
add_executable(ode_example_1 ode_example_1.f90)
target_link_libraries(ode_example_1 mecheng)

# ODE EXAMPLE 2
add_executable(ode_example_2 ode_example_2.f90)
target_link_libraries(ode_example_2 mecheng)

# REALTIME EXAMPLE 1
add_executable(realtime_example_1 realtime_example_1.f90)
target_link_libraries(realtime_example_1 mecheng)

# REALTIME EXAMPLE 2
add_executable(realtime_example_2 realtime_example_2.f90)
target_link_libraries(realtime_example_2 mecheng)

# REALTIME EXAMPLE 3
add_executable(realtime_example_3 realtime_example_3.f90)
target_link_libraries(realtime_example_3 mecheng)
