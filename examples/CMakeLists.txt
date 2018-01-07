# ------------------------------------------------------------------------------
# FORWARD_KINEMATICS_EXAMPLE
# ------------------------------------------------------------------------------
option(RUN_FORWARD_KINEMATICS_EXAMPLE "Run forward kinematics example?" OFF)

# Build the executable
set(forward_kinematics_example_source forward_kinematics_example.f90)
add_executable(forward_kinematics_example ${forward_kinematics_example_source})
target_link_libraries(forward_kinematics_example mecheng)

# Run the example
if (RUN_FORWARD_KINEMATICS_EXAMPLE)
    add_custom_command(OUTPUT forward_kinematics_example_cmd
        DEPENDS ${forward_kinematics_example_source}
        COMMAND forward_kinematics_example)
    add_custom_target(run_forward_kinematics_example ALL
        DEPENDS forward_kinematics_example_cmd)
endif()

# ------------------------------------------------------------------------------
# QUATERNION EXAMPLE
# ------------------------------------------------------------------------------
option(RUN_QUATERNION_EXAMPLE "Run quaternion example?" OFF)

# Build the executable
set(quaternion_example_source quaternion_example.f90)
add_executable(quaternion_example ${quaternion_example_source})
target_link_libraries(quaternion_example mecheng)

# Run the example
if (RUN_QUATERNION_EXAMPLE)
    add_custom_command(OUTPUT quaternion_example_cmd
        DEPENDS ${quaternion_example_source}
        COMMAND quaternion_example)
    add_custom_target(run_quaternion_example ALL
        DEPENDS quaternion_example_cmd)
endif()

# ------------------------------------------------------------------------------
# FFT EXAMPLE
# ------------------------------------------------------------------------------
option(RUN_FFT_EXAMPLE "Run FFT example?" ON)

# Build the executable
set(fft_example_source fft_example.f90)
add_executable(fft_example ${fft_example_source})
target_link_libraries(fft_example mecheng fplot)

# Run the example
if (RUN_FFT_EXAMPLE)
    add_custom_command(OUTPUT fft_example_cmd
        DEPENDS ${fft_example_source}
        COMMAND fft_example)
    add_custom_target(run_fft_example ALL
        DEPENDS fft_example_cmd)
endif()
