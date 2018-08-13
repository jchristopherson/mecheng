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
