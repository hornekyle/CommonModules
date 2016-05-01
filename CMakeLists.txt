cmake_minimum_required(VERSION 2.6)
enable_language(Fortran)
set(PROJECT "CommonModules")
project(${PROJECT})
enable_testing()

#=====================#
#= Set Default Flags =#
#=====================#

set(CMAKE_Fortran_FLAGS "")
set(CMAKE_Fortran_FLAGS_DEBUG "-g2 -Wall -Wno-maybe-uninitialized -Wtabs -fcheck=all -ffpe-trap=invalid,zero,overflow -fbacktrace -finit-real=snan")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -mtune=native -march=native")

#===============================#
#= Find Packages and Libraries =#
#===============================#

include(FindPkgConfig)
pkg_search_module(PLPLOT REQUIRED plplot-f95 plplotd-f95)
include_directories(${PLPLOT_INCLUDE_DIRS})
link_directories(${PLPLOT_LIBRARY_DIRS})

find_library(NETCDF_LIBRARY netcdff "/usr/lib")
find_path(NETCDF_INCLUDE "netcdf.mod" "/usr/include")

find_library(LAPACK_LIBRARY lapack "/usr/lib")
find_library(UMFPACK_LIBRARY umfpack "/usr/lib")
include_directories(${NETCDF_INCLUDE})

find_library(FFTW3_LIBRARY fftw3 "/usr/lib")
find_path(FFTW3_INCLUDE "fftw3.f03" "/usr/include")
include_directories(${FFTW3_INCLUDE})

#=========================#
#= Create Symlink Target =#
#=========================#

add_custom_target(symlink
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/results" "${CMAKE_BINARY_DIR}/results"
	COMMAND ${CMAKE_COMMAND} -E create_symlink "${CMAKE_SOURCE_DIR}/config" "${CMAKE_BINARY_DIR}/config"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

#======================#
#= Create FORD Target =#
#======================#

set(FORD_EXECUTABLE "ford")
add_custom_target(ford
	COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/pages" "${CMAKE_BINARY_DIR}/pages"
	COMMAND ${CMAKE_COMMAND} -E copy_directory "${CMAKE_SOURCE_DIR}/src" "${CMAKE_BINARY_DIR}/src"
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/project.md" "${CMAKE_BINARY_DIR}"
	COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_SOURCE_DIR}/README.md" "${CMAKE_BINARY_DIR}"
	COMMAND ${FORD_EXECUTABLE} "project.md"
	WORKING_DIRECTORY "${CMAKE_BINARY_DIR}")

#=======================#
#= Create Test Targets =#
#=======================#

set(testKinds_src
	"src/kinds.f90"
	"src/test/testKinds.f90")
add_executable(testKinds ${testKinds_src})
add_test(testKinds testKinds)

set(testTime_src
	"src/kinds.f90"
	"src/time.f90"
	"src/test/testTime.f90")
add_executable(testTime ${testTime_src})
add_test(testTime testTime)

set(testStats_src
	"src/kinds.f90"
	"src/stats.f90"
	"src/test/testStats.f90")
add_executable(testStats ${testStats_src})
add_test(testStats testStats)

set(testArray_src
	"src/array.f90"
	"src/kinds.f90"
	"src/test/testArray.f90")
add_executable(testArray ${testArray_src})
add_test(testArray testArray)

set(testFourier_src
	"src/kinds.f90"
	"src/fftw3.f90"
	"src/fourier.f90"
	"src/test/testFourier.f90")
add_executable(testFourier ${testFourier_src})
target_link_libraries(testFourier ${FFTW3_LIBRARY})
add_test(testFourier testFourier)

set(testText_src
	"src/kinds.f90"
	"src/text.f90"
	"src/time.f90"
	"src/test/testText.f90")
add_executable(testText ${testText_src})
add_test(testText testText)

set(testConfig_src
	"src/config.f90"
	"src/kinds.f90"
	"src/text.f90"
	"src/time.f90"
	"src/test/testConfig.f90")
add_executable(testConfig ${testConfig_src})
add_test(testConfig testConfig)
add_dependencies(testConfig symlink)

set(examplePlplotlib_src
	"src/array.f90"
	"src/kinds.f90"
	"src/plplotlib.f90"
	"src/text.f90"
	"src/time.f90"
	"src/example/examplePlplotlib.f90")
add_executable(examplePlplotlib ${examplePlplotlib_src})
target_link_libraries(examplePlplotlib ${PLPLOT_LIBRARIES})
add_dependencies(examplePlplotlib symlink)


# target_link_libraries(main "gomp")
# target_link_libraries(main ${FFTW3_LIBRARY})
# target_link_libraries(main ${NETCDF_LIBRARY})
# target_link_libraries(main ${LAPACK_LIBRARY})
# target_link_libraries(main ${UMFPACK_LIBRARY})
# target_link_libraries(main ${PLPLOT_LIBRARIES})