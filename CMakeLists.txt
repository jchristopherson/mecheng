# Master CMAKE Build Script
cmake_minimum_required(VERSION 3.7)
project(mecheng C CXX Fortran)

# Define version information
set(mecheng_VERSION_MAJOR 1)
set(mecheng_VERSION_MINOR 0)
set(mecheng_VERSION_PATCH 0)

# Define output directories, if undefined
if (NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  message(STATUS "Library output directories undefined.  Using default directories.")
  if (CMAKE_BUILD_TYPE MATCHES Debug)
    # Debug Build
    if (BUILD_SHARED_LIBS)
      set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
      set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
      set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
      set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug/mod)
    else()
      set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
      set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
      set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
      set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug/mod)
    endif ()
  elseif (CMAKE_BUILD_TYPE MATCHES Release)
    # Release Build
    if (BUILD_SHARED_LIBS)
      set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
      set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
      set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
      set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release/mod)
    else()
      set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
      set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
      set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
      set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release/mod)
    endif ()
  else ()
    # Default Condition
    if (BUILD_SHARED_LIBS)
      set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
      set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
      set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
      set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug/mod)
    else()
      set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
      set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
      set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
      set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug/mod)
    endif ()
  endif ()
endif ()

# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set(CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -Wall -Wno-c-binding-type")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set (CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
  message ("No optimized Fortran compiler flags are known, we just try -O2...")
  set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -Wall")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# Locate the source directory
add_subdirectory(src)

# ------------------------------------------------------------------------------
# TESTS
# ------------------------------------------------------------------------------
set(test_columns_source tests/test_columns.f90)
add_executable(mecheng_test_columns ${test_columns_source})
target_link_libraries(mecheng_test_columns mecheng)
add_custom_command(OUTPUT mecheng_test_columns_cmd 
  DEPENDS ${test_columns_source}
  COMMAND mecheng_test_columns)
add_custom_target(run_mecheng_column_tests ALL DEPENDS mecheng_test_columns_cmd)