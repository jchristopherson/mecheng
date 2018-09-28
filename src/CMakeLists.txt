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

# Define the collections source files
set(collections_sources
    collections/collection_containers.f90
    collections/collection_list.f90
    collections/collection_errors.f90
    collections/collection_linked_list.f90
)

# Define the curvefit sources
set(curvefit_sources
    curvefit/curvefit_calibration.f90
    curvefit/curvefit_core.f90
    curvefit/curvefit_interp.f90
    curvefit/curvefit_regression.f90
    curvefit/curvefit_statistics.f90
)

# Define the ferror sources
set(ferror_sources
    ferror/ferror.f90
)

# Define the fortio sources
set(fortio_sources
    fortio/fortio_types.f90
    fortio/fortio_binary.f90
    fortio/fortio_text.f90
    fortio/fortio_hbm.f90
)

# Define the fplot sources
set(fplot_sources
    fplot/fplot_core.f90
    fplot/fplot_core_routines.f90
    fplot/fplot_errors.f90
    fplot/fplot_colors.f90
    fplot/fplot_terminal.f90
    fplot/fplot_windows_terminal.f90
    fplot/fplot_qt_terminal.f90
    fplot/fplot_wxt_terminal.f90
    fplot/fplot_png_terminal.f90
    fplot/fplot_latex_terminal.f90
    fplot/fplot_plot_data.f90
    fplot/fplot_plot_axis.f90
    fplot/fplot_legend.f90
    fplot/fplot_plot.f90
    fplot/fplot_colormap.f90
    fplot/fplot_scatter_plot_data.f90
    fplot/fplot_plot_data_2d.f90
    fplot/fplot_plot_data_3d.f90
    fplot/fplot_surface_plot_data.f90
    fplot/fplot_plot_2d.f90
    fplot/fplot_plot_3d.f90
    fplot/fplot_surface_plot.f90
    fplot/fplot_axis.f90
    fplot/fplot_label.f90
    fplot/fplot_multiplot.f90
    fplot/fplot_plot_data_error_bars.f90
    fplot/fplot_plot_data_colored.f90
)

# Define the integral sources
set(integral_sources
    integral/integral_core.f90
    integral/integral_integrator_base.f90
    integral/integral_adaptive_integrator.f90
    integral/integral_nonadaptive_integrator.f90
    integral/integral_ode_helper.f90
    integral/integral_ode_integrator.f90
    integral/integral_ode_auto.f90
)

# Define the QUADPACK source files
# REF:
# - http://www.netlib.org/quadpack/
# - https://people.sc.fsu.edu/~jburkardt/f_src/quadpack/quadpack.html
set(quadpack_sources
    integral/QUADPACK/dqagie.f
    integral/QUADPACK/dqags.f
    integral/QUADPACK/dqagse.f
    integral/QUADPACK/dqawc.f
    integral/QUADPACK/dqawce.f
    integral/QUADPACK/dqawf.f
    integral/QUADPACK/dqawfe.f
    integral/QUADPACK/dqawo.f
    integral/QUADPACK/dqawoe.f
    integral/QUADPACK/dqaws.f
    integral/QUADPACK/dqawse.f
    integral/QUADPACK/dqc25c.f
    integral/QUADPACK/dqc25f.f
    integral/QUADPACK/dqc25s.f
    integral/QUADPACK/dqcheb.f
    integral/QUADPACK/dqelg.f
    integral/QUADPACK/dqk15.f
    integral/QUADPACK/dqk15i.f
    integral/QUADPACK/dqk15w.f
    integral/QUADPACK/dqk21.f
    integral/QUADPACK/dqk31.f
    integral/QUADPACK/dqk41.f
    integral/QUADPACK/dqk51.f
    integral/QUADPACK/dqk61.f
    integral/QUADPACK/dqmomo.f
    integral/QUADPACK/dqng.f
    integral/QUADPACK/dqpsrt.f
    integral/QUADPACK/dqwgtc.f
    integral/QUADPACK/dqwgtc.f
    integral/QUADPACK/dqwgtf.f
    integral/QUADPACK/dqwgts.f
    integral/QUADPACK/dqag.f
    integral/QUADPACK/dqage.f
    integral/QUADPACK/dqagi.f
    integral/QUADPACK/dqagp.f
    integral/QUADPACK/dqagpe.f
    integral/QUADPACK/xerror.f90
    integral/QUADPACK/d1mach.f
    integral/QUADPACK/dgtsl.f
)

# Define the ODEPACK sources
set(odepack_sources
    integral/ODEPACK/opkda1.f
    integral/ODEPACK/opkda2.f
    integral/ODEPACK/opkdmain.f
)

# Define the kinematics sources
set(kinematics_sources
    kinematics/kinematics.f90
    kinematics/kinematics_quaternion.f90
    kinematics/kinematics_transforms.f90
)

# Define the LINALG sources
set(linalg_sources
    linalg/linalg_core.f90
    linalg/linalg_constants.f90
    linalg/linalg_eigen.f90
    linalg/linalg_factor.f90
    linalg/linalg_solve.f90
    linalg/linalg_sorting.f90
    linalg/linalg_basic.f90
    linalg/linalg_immutable.f90
)

# Define the BLAS sources
set(blas_sources
    external/lapack/BLAS/SRC/dasum.f
    external/lapack/BLAS/SRC/daxpy.f
    external/lapack/BLAS/SRC/dcopy.f
    external/lapack/BLAS/SRC/ddot.f
    external/lapack/BLAS/SRC/dgbmv.f
    external/lapack/BLAS/SRC/dgemm.f
    external/lapack/BLAS/SRC/dgemv.f
    external/lapack/BLAS/SRC/dger.f
    external/lapack/BLAS/SRC/dnrm2.f
    external/lapack/BLAS/SRC/drot.f
    external/lapack/BLAS/SRC/drotg.f
    external/lapack/BLAS/SRC/drotm.f
    external/lapack/BLAS/SRC/drotmg.f
    external/lapack/BLAS/SRC/dsbmv.f
    external/lapack/BLAS/SRC/dscal.f
    external/lapack/BLAS/SRC/dspmv.f
    external/lapack/BLAS/SRC/dspr.f
    external/lapack/BLAS/SRC/dspr2.f
    external/lapack/BLAS/SRC/dswap.f
    external/lapack/BLAS/SRC/dsymm.f
    external/lapack/BLAS/SRC/dsymv.f
    external/lapack/BLAS/SRC/dsyr.f
    external/lapack/BLAS/SRC/dsyr2.f
    external/lapack/BLAS/SRC/dsyr2k.f
    external/lapack/BLAS/SRC/dsyrk.f
    external/lapack/BLAS/SRC/dtbmv.f
    external/lapack/BLAS/SRC/dtbsv.f
    external/lapack/BLAS/SRC/dtpmv.f
    external/lapack/BLAS/SRC/dtpsv.f
    external/lapack/BLAS/SRC/dtrmm.f
    external/lapack/BLAS/SRC/dtrmv.f
    external/lapack/BLAS/SRC/dtrsm.f
    external/lapack/BLAS/SRC/dtrsv.f
    external/lapack/BLAS/SRC/idamax.f
    external/lapack/BLAS/SRC/lsame.f
    external/lapack/BLAS/SRC/xerbla.f
    external/lapack/BLAS/SRC/dcabs1.f
    external/lapack/BLAS/SRC/dzasum.f
    external/lapack/BLAS/SRC/dznrm2.f
    external/lapack/BLAS/SRC/izamax.f
    external/lapack/BLAS/SRC/zaxpy.f
    external/lapack/BLAS/SRC/zcopy.f
    external/lapack/BLAS/SRC/zdotc.f
    external/lapack/BLAS/SRC/zdotu.f
    external/lapack/BLAS/SRC/zdscal.f
    external/lapack/BLAS/SRC/zrotg.f
    external/lapack/BLAS/SRC/zscal.f
    external/lapack/BLAS/SRC/zswap.f
    external/lapack/BLAS/SRC/zdrot.f
    external/lapack/BLAS/SRC/zgemv.f
    external/lapack/BLAS/SRC/zgbmv.f
    external/lapack/BLAS/SRC/zhemv.f
    external/lapack/BLAS/SRC/zhbmv.f
    external/lapack/BLAS/SRC/zhpmv.f
    external/lapack/BLAS/SRC/ztrmv.f
    external/lapack/BLAS/SRC/ztbmv.f
    external/lapack/BLAS/SRC/ztpmv.f
    external/lapack/BLAS/SRC/ztrsv.f
    external/lapack/BLAS/SRC/ztbsv.f
    external/lapack/BLAS/SRC/ztpsv.f
    external/lapack/BLAS/SRC/zgerc.f
    external/lapack/BLAS/SRC/zgeru.f
    external/lapack/BLAS/SRC/zher.f
    external/lapack/BLAS/SRC/zhpr.f
    external/lapack/BLAS/SRC/zher2.f
    external/lapack/BLAS/SRC/zhpr2.f
    external/lapack/BLAS/SRC/zgemm.f
    external/lapack/BLAS/SRC/zsymm.f
    external/lapack/BLAS/SRC/zsyrk.f
    external/lapack/BLAS/SRC/zsyr2k.f
    external/lapack/BLAS/SRC/ztrmm.f
    external/lapack/BLAS/SRC/ztrsm.f
    external/lapack/BLAS/SRC/zhemm.f
    external/lapack/BLAS/SRC/zherk.f
    external/lapack/BLAS/SRC/zher2k.f
)

set(lapack_sources
    external/lapack/SRC/dbdsqr.f
    external/lapack/SRC/dgbtf2.f
    external/lapack/SRC/dgbtrf.f
    external/lapack/SRC/dgbtrs.f
    external/lapack/SRC/dgebak.f
    external/lapack/SRC/dgebal.f
    external/lapack/SRC/dgebd2.f
    external/lapack/SRC/dgebrd.f
    external/lapack/SRC/dgecon.f
    external/lapack/SRC/dgeev.f
    external/lapack/SRC/dgehd2.f
    external/lapack/SRC/dgehrd.f
    external/lapack/SRC/dgelq2.f
    external/lapack/SRC/dgelqf.f
    external/lapack/SRC/dgels.f
    external/lapack/SRC/dgelss.f
    external/lapack/SRC/dgelsy.f
    external/lapack/SRC/dgeqp3.f
    external/lapack/SRC/dgeqr2.f
    external/lapack/SRC/dgeqrf.f
    external/lapack/SRC/dgesvd.f
    external/lapack/SRC/dgetf2.f
    external/lapack/SRC/dgetrf.f
    external/lapack/SRC/dgetri.f
    external/lapack/SRC/dgetrs.f
    external/lapack/SRC/dggbak.f
    external/lapack/SRC/dggbal.f
    external/lapack/SRC/dggev.f
    external/lapack/SRC/dgghrd.f
    external/lapack/SRC/dhgeqz.f
    external/lapack/SRC/dhseqr.f
    external/lapack/SRC/disnan.f
    external/lapack/SRC/dlabad.f
    external/lapack/SRC/dlabrd.f
    external/lapack/SRC/dlacn2.f
    external/lapack/SRC/dlacpy.f
    external/lapack/SRC/dladiv.f
    external/lapack/SRC/dlae2.f
    external/lapack/SRC/dlaev2.f
    external/lapack/SRC/dlaexc.f
    external/lapack/SRC/dlag2.f
    external/lapack/SRC/dlahqr.f
    external/lapack/SRC/dlahr2.f
    external/lapack/SRC/dlaic1.f
    external/lapack/SRC/dlaisnan.f
    external/lapack/SRC/dlaln2.f
    external/lapack/INSTALL/dlamch.f
    external/lapack/SRC/dlange.f
    external/lapack/SRC/dlanhs.f
    external/lapack/SRC/dlanst.f
    external/lapack/SRC/dlansy.f
    external/lapack/SRC/dlanv2.f
    external/lapack/SRC/dlapy2.f
    external/lapack/SRC/dlapy3.f
    external/lapack/SRC/dlaqp2.f
    external/lapack/SRC/dlaqps.f
    external/lapack/SRC/dlaqr0.f
    external/lapack/SRC/dlaqr1.f
    external/lapack/SRC/dlaqr2.f
    external/lapack/SRC/dlaqr3.f
    external/lapack/SRC/dlaqr4.f
    external/lapack/SRC/dlaqr5.f
    external/lapack/SRC/dlarf.f
    external/lapack/SRC/dlarfb.f
    external/lapack/SRC/dlarfg.f
    external/lapack/SRC/dlarft.f
    external/lapack/SRC/dlarfx.f
    external/lapack/SRC/dlartg.f
    external/lapack/SRC/dlarz.f
    external/lapack/SRC/dlarzb.f
    external/lapack/SRC/dlarzt.f
    external/lapack/SRC/dlas2.f
    external/lapack/SRC/dlascl.f
    external/lapack/SRC/dlaset.f
    external/lapack/SRC/dlasq1.f
    external/lapack/SRC/dlasq2.f
    external/lapack/SRC/dlasq3.f
    external/lapack/SRC/dlasq4.f
    external/lapack/SRC/dlasq5.f
    external/lapack/SRC/dlasq6.f
    external/lapack/SRC/dlasr.f
    external/lapack/SRC/dlasrt.f
    external/lapack/SRC/dlassq.f
    external/lapack/SRC/dlasv2.f
    external/lapack/SRC/dlaswp.f
    external/lapack/SRC/dlasy2.f
    external/lapack/SRC/dlatrd.f
    external/lapack/SRC/dlatrs.f
    external/lapack/SRC/dlatrz.f
    external/lapack/SRC/dorg2l.f
    external/lapack/SRC/dorg2r.f
    external/lapack/SRC/dorgbr.f
    external/lapack/SRC/dorghr.f
    external/lapack/SRC/dorgl2.f
    external/lapack/SRC/dorglq.f
    external/lapack/SRC/dorgql.f
    external/lapack/SRC/dorgqr.f
    external/lapack/SRC/dorgtr.f
    external/lapack/SRC/dorm2r.f
    external/lapack/SRC/dormbr.f
    external/lapack/SRC/dormhr.f
    external/lapack/SRC/dorml2.f
    external/lapack/SRC/dormlq.f
    external/lapack/SRC/dormqr.f
    external/lapack/SRC/dormr3.f
    external/lapack/SRC/dormrz.f
    external/lapack/SRC/dpotf2.f
    external/lapack/SRC/dpotrf.f
    external/lapack/SRC/dpotrf2.f
    external/lapack/SRC/dpotrs.f
    external/lapack/SRC/drscl.f
    external/lapack/SRC/dsteqr.f
    external/lapack/SRC/dsterf.f
    external/lapack/SRC/dsyev.f
    external/lapack/SRC/dsytd2.f
    external/lapack/SRC/dsytrd.f
    external/lapack/SRC/dtgevc.f
    external/lapack/SRC/dtrevc.f
    external/lapack/SRC/dtrexc.f
    external/lapack/SRC/dtrti2.f
    external/lapack/SRC/dtrtri.f
    external/lapack/SRC/dtrtrs.f
    external/lapack/SRC/dtzrzf.f
    external/lapack/SRC/ieeeck.f
    external/lapack/SRC/iladlc.f
    external/lapack/SRC/iladlr.f
    external/lapack/SRC/ilaenv.f
    external/lapack/SRC/iparmq.f
    external/lapack/SRC/dtrevc3.f
    external/lapack/SRC/dgetrf2.f
    external/lapack/SRC/zgetrf.f
    external/lapack/SRC/zgetf2.f
    external/lapack/SRC/zlaswp.f
    external/lapack/SRC/zgetrf2.f
    external/lapack/SRC/zgetrs.f
)

# Define the QRUPDATE source files
set(qrupdate_sources
    linalg/dch1dn.f
    linalg/dch1up.f
    linalg/dlup1up.f
    linalg/dqhqr.f
    linalg/dqr1up.f
    linalg/dqrot.f
    linalg/dqrqh.f
    linalg/dqrtv1.f
)

# Define the mechanics source files
set(mechanics_sources
    mechanics/columns.f90
    mechanics/sections.f90
    mechanics/strain.f90
)

set(nonlin_sources
    nonlin/nonlin_least_squares.f90
    nonlin/nonlin_linesearch.f90
    nonlin/nonlin_optimize.f90
    nonlin/nonlin_polynomials.f90
    nonlin/nonlin_solve.f90
    nonlin/nonlin_constants.f90
    nonlin/nonlin_core.f90
    nonlin/nonlin_vecfcn_helper.f90
    nonlin/nonlin_fcn1var_helper.f90
    nonlin/nonlin_fcnnvar_helper.f90
    nonlin/nonlin_equation_solver.f90
    nonlin/nonlin_equation_solver_1var.f90
    nonlin/nonlin_equation_optimizer.f90
    nonlin/nonlin_optimize_nelder_mead.f90
    nonlin/nonlin_optimize_line_search.f90
    nonlin/nonlin_optimize_bfgs.f90
    nonlin/nonlin_solve_line_search.f90
    nonlin/nonlin_solve_quasi_newton.f90
    nonlin/nonlin_solve_newton.f90
    nonlin/nonlin_solve_brent.f90
    nonlin/nonlin_solve_newton1var.f90
)

set(signals_sources
    signals/signals.f90
    signals/signals_filter.f90
    signals/signals_fourier.f90
    signals/signals_operations.f90
    signals/signals_windows.f90
    signals/signals_spectral_analysis.f90
    signals/signals_diff.f90
    signals/signals_integrate.f90
)

set(strings_sources
    strings/strings.f90
)

set(geometry_sources
    geometry/geometry.f90
)

set(neuralnet_sources
    neuralnet/neural_net_constants.f90
    neuralnet/neural_net_core.f90
    neuralnet/neural_net_neuron.f90
    neuralnet/neural_net_sigmoid.f90
    neuralnet/neural_net_layer.f90
    neuralnet/neural_net_network.f90
    neuralnet/neural_net_ff_network.f90
)

# Define the MECHENG source files
set(mecheng_sources
    constants.f90
    ${fftpack_sources}
    ${collections_sources}
    ${curvefit_sources}
    ${ferror_sources}
    ${fortio_sources}
    ${fplot_sources}
    ${integral_sources}
    ${quadpack_sources}
    ${odepack_sources}
    ${kinematics_sources}
    ${linalg_sources}
    ${blas_sources}
    ${lapack_sources}
    ${qrupdate_sources}
    ${mechanics_sources}
    ${nonlin_sources}
    ${signals_sources}
    ${strings_sources}
    ${geometry_sources}
    ${neuralnet_sources}
)

# Build the library
add_library(mecheng ${mecheng_sources})

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
