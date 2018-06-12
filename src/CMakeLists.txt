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
    kinematics.f90
    kinematics_quaternion.f90
    kinematics_transforms.f90
    ${fftpack_sources}
)

# Build the library
add_library(mecheng ${mecheng_sources})
target_link_libraries(mecheng fftpack)
