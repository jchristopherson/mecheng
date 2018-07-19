# Define the FFTPACK source files
set(fftpack_sources
    external/modern_fftpack/src/auxiliary_routines.f90
    external/modern_fftpack/src/complex_backward_1d.f90
    external/modern_fftpack/src/complex_backward_2d.f90
    external/modern_fftpack/src/complex_forward_1d.f90
    external/modern_fftpack/src/complex_forward_2d.f90
    external/modern_fftpack/src/complex_initialization_routines.f90
    external/modern_fftpack/src/complex_transform_routines.f90
    external/modern_fftpack/src/cosine_transform_routines.f90
    external/modern_fftpack/src/error_handlers.f90
    external/modern_fftpack/src/fftpack_library.f90
    external/modern_fftpack/src/fftpack_precision.f90
    external/modern_fftpack/src/multiple_complex_backward.f90
    external/modern_fftpack/src/multiple_complex_forward.f90
    external/modern_fftpack/src/multiple_real_backward.f90
    external/modern_fftpack/src/multiple_real_forward.f90
    external/modern_fftpack/src/quarter_cosine_transform_routines.f90
    external/modern_fftpack/src/quarter_sine_transform_routines.f90
    external/modern_fftpack/src/real_backward_1d.f90
    external/modern_fftpack/src/real_backward_2d.f90
    external/modern_fftpack/src/real_forward_1d.f90
    external/modern_fftpack/src/real_forward_2d.f90
    external/modern_fftpack/src/real_initialization_routines.f90
    external/modern_fftpack/src/real_transform_routines.f90
    external/modern_fftpack/src/sine_transform_routines.f90
    external/modern_fftpack/src/type_FFTpack.f90
)

# Define the MECHENG source files
set(mecheng_sources
    constants.f90
    sections.f90
    columns.f90
    strain.f90
    signals.f90
    signals_filter.f90
    signals_fourier.f90
    signals_operations.f90
    signals_windows.f90
    signals_spectral_analysis.f90
    signals_diff.f90
    kinematics.f90
    kinematics_quaternion.f90
    kinematics_transforms.f90
    ${fftpack_sources}
)

# Build the library
add_library(mecheng ${mecheng_sources})
target_link_libraries(mecheng)

# ------------------------------------------------------------------------------
# INSTALLATION INSTRUCTIONS
# ------------------------------------------------------------------------------
# Define target information
set_property(TARGET mecheng PROPERTY VERSION ${MECHENG_VERSION})
set_property(TARGET mecheng PROPERTY SOVERSION ${MECHENG_MAJOR_VERSION})
set_property(TARGET mecheng PROPERTY INTERFACE_mecheng_MAJOR_VERSION ${MECHENG_MAJOR_VERSION})
set_property(TARGET mecheng APPEND PROPERTY COMPATIBLE_INTERFACE_STRING mecheng_MAJOR_VERSION)

# Locate the "include" directories
set(mecheng_INCLUDE_DIRS ${PROJECT_SOURCE_DIR}/include)

# Define the installation instructions
install(TARGETS mecheng EXPORT mechengTargets
    RUNTIME DESTINATION mecheng/bin
    LIBRARY DESTINATION mecheng/lib
    ARCHIVE DESTINATION mecheng/lib
    CONFIGURATIONS Release
    INCLUDES DESTINATION mecheng/include
)
install(
    DIRECTORY ${PROJECT_SOURCE_DIR}/include
    DESTINATION ${CMAKE_INSTALL_PREFIX}/mecheng
    COMPONENT Devel
)

# Include the documentation
install(DIRECTORY ${PROJECT_SOURCE_DIR}/doc/html DESTINATION mecheng/doc)

# Define the version file
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
    "${CMAKE_BINARY_DIR}/mechengConfigVersion.cmake"
    VERSION ${MECHENG_VERSION}
    COMPATIBILITY AnyNewerVersion
)

export(EXPORT mechengTargets
    FILE "${CMAKE_BINARY_DIR}/mechengTargets.cmake"
)

# Define the configuration file
configure_file(
    "${PROJECT_SOURCE_DIR}/mechengConfig.cmake.in"
    "${CMAKE_BINARY_DIR}/mechengConfig.cmake"
    COPYONLY
)

set(ConfigPackageLocation mecheng/lib/cmake/mecheng)
install(
    EXPORT mechengTargets
    FILE mechengTargets.cmake
    DESTINATION ${ConfigPackageLocation}
)
install(
    FILES
        "${CMAKE_BINARY_DIR}/mechengConfig.cmake"
        "${CMAKE_BINARY_DIR}/mechengConfigVersion.cmake"
    DESTINATION
        ${ConfigPackageLocation}
    COMPONENT
        Devel
)
