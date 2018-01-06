# Locate dependencies
add_subdirectory(external/fplot)

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
# SIGNALS TESTS
# ------------------------------------------------------------------------------
option(RUN_SIGNALS_TESTS "Run signals tests?" OFF)

# Build the executable
set(test_signals_source test_signals.f90)
add_executable(mecheng_test_signals ${test_signals_source})
target_link_libraries(mecheng_test_signals mecheng fplot)

if (RUN_SIGNALS_TESTS)
    # Run the tests
    add_custom_command(OUTPUT mecheng_test_signals_cmd
        DEPENDS ${test_signals_source}
        COMMAND mecheng_test_signals)
    add_custom_target(run_mecheng_signals_tests ALL 
    DEPENDS mecheng_test_signals_cmd)
endif()

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
    