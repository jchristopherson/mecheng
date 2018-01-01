# Locate external dependencies
add_subdirectory(external)

# Define the MECHENG source files
set(mecheng_sources constants.f90 sections.f90 columns.f90 strain.f90)

# Build the library
add_library(mecheng ${mecheng_sources})
