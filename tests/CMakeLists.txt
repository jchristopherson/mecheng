# ------------------------------------------------------------------------------
# COLUMN TESTS
# ------------------------------------------------------------------------------
# Build the executable
set(test_columns_source test_columns.f90)
add_executable(mecheng_test_columns ${test_columns_source})
target_link_libraries(mecheng_test_columns mecheng)

# Run the tests
add_custom_command(OUTPUT mecheng_test_columns_cmd
  DEPENDS ${test_columns_source}
  COMMAND mecheng_test_columns)
add_custom_target(run_mecheng_column_tests ALL DEPENDS mecheng_test_columns_cmd)

# ------------------------------------------------------------------------------
# KINEMATICS TESTS
# ------------------------------------------------------------------------------
# Build the executable
set(test_kinematics_source test_kinematics.f90)
add_executable(mecheng_test_kinematics ${test_kinematics_source})
target_link_libraries(mecheng_test_kinematics mecheng)

# Run the tests
add_custom_command(OUTPUT mecheng_test_kinematics_cmd
    DEPENDS ${test_kinematics_source}
    COMMAND mecheng_test_kinematics)
add_custom_target(run_mecheng_kinematics_tests ALL
    DEPENDS mecheng_test_kinematics_cmd)


# ------------------------------------------------------------------------------
# INTEGRATION TESTS
# ------------------------------------------------------------------------------
set(test_integration_source test_integration.f90)
add_executable(mecheng_test_integration ${test_integration_source})
target_link_libraries(mecheng_test_integration mecheng)

# Run the tests
add_custom_command(OUTPUT mecheng_test_integration_cmd
    DEPENDS ${test_integration_source}
    COMMAND mecheng_test_integration
)
add_custom_target(
    run_mecheng_integration_tests ALL
    DEPENDS mecheng_test_integration_cmd
)



# ------------------------------------------------------------------------------
# VIBRATIONS TESTS
# ------------------------------------------------------------------------------
set(test_vibrations_source test_vibrations.f90)
add_executable(mecheng_test_vibrations ${test_vibrations_source})
target_link_libraries(mecheng_test_vibrations mecheng)

# Run the tests
add_custom_command(OUTPUT mecheng_test_vibrations_cmd
    DEPENDS ${test_vibrations_source}
    COMMAND mecheng_test_vibrations
)
add_custom_target(
    run_mecheng_vibrations_tests ALL
    DEPENDS mecheng_test_vibrations_cmd
)


# ------------------------------------------------------------------------------
# CONTROLS TESTS
# ------------------------------------------------------------------------------
set(test_controls_source
    test_pid.f90
)
add_executable(mecheng_test_pid ${test_controls_source})
target_link_libraries(mecheng_test_pid mecheng)

# Run the tests
add_custom_command(OUTPUT mecheng_test_pid_cmd
    DEPENDS ${test_controls_source}
    COMMAND mecheng_test_pid
)
add_custom_target(
    run_mecheng_pid_tests ALL
    DEPENDS mecheng_test_pid_cmd
)


# ------------------------------------------------------------------------------
# FPLOT TESTS
# ------------------------------------------------------------------------------
set(test_fplot_source test_fplot.f90)
add_executable(mecheng_test_fplot ${test_fplot_source})
target_link_libraries(mecheng_test_fplot mecheng)

# Run the tests
# add_custom_command(OUTPUT mecheng_test_fplot_cmd
#     DEPENDS ${test_fplot_source}
#     COMMAND mecheng_test_fplot
# )
# add_custom_target(
#     run_mecheng_fplot_tests ALL
#     DEPENDS mecheng_test_fplot_cmd
# )

# ------------------------------------------------------------------------------
# SIGNALS TESTS
# ------------------------------------------------------------------------------
set(test_signals_source test_signals.f90)
add_executable(mecheng_test_signals ${test_signals_source})
target_link_libraries(mecheng_test_signals mecheng)

# Run the tests
add_custom_command(OUTPUT mecheng_test_signals_cmd
    DEPENDS ${test_signals_source}
    COMMAND mecheng_test_signals
)
add_custom_target(
    run_mecheng_signals_tests ALL
    DEPENDS mecheng_test_signals_cmd
)
