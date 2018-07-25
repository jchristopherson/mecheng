# Link in dependencies necessary to generate plots
find_package(fplot)
find_package(ferror)
find_package(collections)

# FORWARD_KINEMATICS_EXAMPLE
add_executable(forward_kinematics_example forward_kinematics_example.f90)
target_link_libraries(forward_kinematics_example mecheng)

# QUATERNION EXAMPLE
add_executable(quaternion_example quaternion_example.f90)
target_link_libraries(quaternion_example mecheng)

# FFT EXAMPLE
add_executable(fft_example fft_example.f90)
target_link_libraries(fft_example mecheng fplot)

# SIGNALS EXAMPLE
add_executable(signals_example signals_example.f90)
target_link_libraries(signals_example mecheng fplot)

# DIFFERENTIATION EXAMPLE
add_executable(diff_example diff_example.f90)
target_link_libraries(diff_example mecheng fplot)

# SIGNALS EXAMPLE #2
add_executable(signals_example_2 signals_example_2.f90)
target_link_libraries(signals_example_2 mecheng fplot)

# ------------------------------------------------------------------------------
if (WIN32)
    # Copy shared libraries necessary to generate plots to the runtime directory
    get_target_property(ferror_LibLocation ferror LOCATION)
    add_custom_command(TARGET fft_example POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${ferror_LibLocation} $<TARGET_FILE_DIR:fft_example>
    )

    get_target_property(fplot_LibLocation fplot LOCATION)
    add_custom_command(TARGET fft_example POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${fplot_LibLocation} $<TARGET_FILE_DIR:fft_example>
    )

    get_target_property(collections_LibLocation collections LOCATION)
    add_custom_command(TARGET fft_example POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${collections_LibLocation} $<TARGET_FILE_DIR:fft_example>
    )
endif()
