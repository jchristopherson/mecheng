# Master CMAKE Build Script
cmake_minimum_required(VERSION 3.7)
project(mecheng C CXX Fortran)

# Define version information
set(MECHENG_MAJOR_VERSION 1)
set(MECHENG_MINOR_VERSION 1)
set(MECHENG_PATCH_VERSION 0)
set(MECHENG_VERSION ${MECHENG_MAJOR_VERSION}.${MECHENG_MINOR_VERSION}.${MECHENG_PATCH_VERSION})
# Set a default build type if none was specified
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Release' as none was specified.")
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
  # Set the possible values of build type for cmake-gui
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release")
endif()

# By default, shared library
option(BUILD_SHARED_LIBS "Build shared libraries" ON)

# Export all symbols on Windows when building shared libraries
SET(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS TRUE)

# Locate the module files
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/include)

# Define output directories, if undefined
if (NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
    message(STATUS "MECHENG library output directories undefined.  Using default directories.")
    if (CMAKE_BUILD_TYPE MATCHES Debug)
        # Debug Build
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
        endif()
    elseif (CMAKE_BUILD_TYPE MATCHES Release)
        # Release Build
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Release)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Release)
        endif()
    else()
        # Default Condition
        if (BUILD_SHARED_LIBS)
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin/Debug)
        else()
            set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
            set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib/Debug)
        endif()
    endif()
endif()


# FFLAGS depend on the compiler
get_filename_component (Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

if (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  # gfortran
  set(CMAKE_Fortran_FLAGS_RELEASE "-funroll-all-loops -fno-f2c -O3 -Wl,--allow-multiple-definition")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-fno-f2c -O0 -g -Wall -Wno-surprising -Wl,--allow-multiple-definition")
elseif (Fortran_COMPILER_NAME MATCHES "ifort.*")
  # ifort (untested)
  set(CMAKE_Fortran_FLAGS_RELEASE "-f77rtl -O3 -Wl,--allow-multiple-definition")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-f77rtl -O0 -g -Wl,--allow-multiple-definition")
else (Fortran_COMPILER_NAME MATCHES "gfortran.*")
  message(STATUS "CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
  message(STATUS "Fortran compiler: " ${Fortran_COMPILER_NAME})
  message(STATUS "No optimized Fortran compiler flags are known, we just try -O2...")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -Wl,--allow-multiple-definition")
  set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -Wall -Wl,--allow-multiple-definition")
endif (Fortran_COMPILER_NAME MATCHES "gfortran.*")

# Locate C/C++ Headers
include_directories(${PROJECT_SOURCE_DIR}/include)

# Build the library
add_subdirectory(src)

# ------------------------------------------------------------------------------
# TESTS
# ------------------------------------------------------------------------------
option(BUILD_MECHENG_TESTS "Build MECHENG tests?" OFF)
if (BUILD_MECHENG_TESTS)
    # Inform the user we're building the tests
    message(STATUS "Building MECHENG tests.")

    # Build the tests
    add_subdirectory(tests)
endif()

# ------------------------------------------------------------------------------
# EXAMPLES
# ------------------------------------------------------------------------------
option(BUILD_MECHENG_EXAMPLES "Build MECHENG examples?" OFF)
if (BUILD_MECHENG_EXAMPLES)
    # Inform the user we're building the tests
    message(STATUS "Building MECHENG examples.")

    # Build the tests
    add_subdirectory(examples)
endif()
