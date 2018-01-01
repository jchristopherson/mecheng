# Locate the modern_fftpack sources
set(fftpack_sources 
    modern_fftpack/src/auxiliary_routines.f90
    modern_fftpack/src/complex_backward_1d.f90
    modern_fftpack/src/complex_backward_2d.f90
    modern_fftpack/src/complex_forward_1d.f90
    modern_fftpack/src/complex_forward_2d.f90
    modern_fftpack/src/complex_initialization_routines.f90
    modern_fftpack/src/complex_transform_routines.f90
    modern_fftpack/src/cosine_transform_routines.f90
    modern_fftpack/src/error_handlers.f90
    modern_fftpack/src/fftpack_library.f90
    modern_fftpack/src/fftpack_precision.f90
    modern_fftpack/src/multiple_complex_backward.f90
    modern_fftpack/src/multiple_complex_forward.f90
    modern_fftpack/src/multiple_real_backward.f90
    modern_fftpack/src/multiple_real_forward.f90
    modern_fftpack/src/quarter_cosine_transform_routines.f90
    modern_fftpack/src/quarter_sine_transform_routines.f90
    modern_fftpack/src/real_backward_1d.f90
    modern_fftpack/src/real_backward_2d.f90
    modern_fftpack/src/real_forward_1d.f90
    modern_fftpack/src/real_forward_2d.f90
    modern_fftpack/src/real_initialization_routines.f90
    modern_fftpack/src/real_transform_routines.f90
    modern_fftpack/src/sine_transform_routines.f90
    modern_fftpack/src/type_FFTpack.f90)

# Build the library
add_library(fftpack ${fftpack_sources})