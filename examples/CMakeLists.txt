# ------------------------------------------------------------------------------
# FORWARD_KINEMATICS_EXAMPLE
# ------------------------------------------------------------------------------
set(forward_kinematics_example_source forward_kinematics_example.f90)
add_executable(forward_kinematics_example ${forward_kinematics_example_source})
target_link_libraries(forward_kinematics_example mecheng)


# ------------------------------------------------------------------------------
# QUATERNION EXAMPLE
# ------------------------------------------------------------------------------
set(quaternion_example_source quaternion_example.f90)
add_executable(quaternion_example ${quaternion_example_source})
target_link_libraries(quaternion_example mecheng)

# ------------------------------------------------------------------------------
# FFT EXAMPLE
# ------------------------------------------------------------------------------
set(fft_example_source fft_example.f90)
add_executable(fft_example ${fft_example_source})
target_link_libraries(fft_example mecheng fplot)
